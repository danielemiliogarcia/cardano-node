{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Node.CLI (
  -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , ViewMode(..)
  , fromProtocol
  -- * Common CLI
  , CommonCLI(..)
  , parseCommonCLI
  , mergeConfiguration
  -- * Generic
  , command'
  , lastOption
  , lastAutoOption
  , lastIntOption
  , lastDoubleOption
  , lastBoolOption
  , lastWordOption
  , lastTextListOption
  , lastStrOption
  , NodeCLIArguments (..)
  , NodeCommand (..)
  ) where

import           Prelude

import qualified Data.ByteString.Lazy as LB
import           Data.Monoid (Last(..))
import           Data.Semigroup ((<>))
import           Data.String (IsString)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Options.Applicative as OA
import           Options.Applicative hiding (command)

import           Control.Exception
import           Control.Monad.Except

import           Codec.CBOR.Read (deserialiseFromBytes, DeserialiseFailure)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Ouroboros.Consensus.Util
import qualified Ouroboros.Consensus.Protocol as Consensus

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto (RequiresNetworkMagic (..), decodeAbstractHash)
import qualified Cardano.Crypto.Signing as Signing

import           Cardano.Shell.Lib (GeneralException (..))
import           Cardano.Node.Configuration.Partial as Shell.Config
                   ( PartialCardanoConfiguration (..), PartialCore (..) )
import           Cardano.Node.Configuration.Types as Shell.Config
                   ( CardanoConfiguration (..), Core (..)
                   , RequireNetworkMagic (..) )
import           Cardano.Node.Topology (NodeAddress (..), TopologyInfo (..))
import           Cardano.Node.Tracers (TraceConstraints, TraceOptions)

import qualified Cardano.Node.CanonicalJSON as CanonicalJSON

{-------------------------------------------------------------------------------
  Untyped/typed protocol boundary
-------------------------------------------------------------------------------}

data Protocol =
    BFT
  | Praos
  | MockPBFT
  | RealPBFT


data SomeProtocol where
  SomeProtocol :: (RunDemo blk, TraceConstraints blk)
               => Consensus.Protocol blk -> SomeProtocol

fromProtocol :: CardanoConfiguration -> Protocol -> IO SomeProtocol
fromProtocol _ BFT =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = ProtocolMockBFT defaultSecurityParam
fromProtocol _ Praos =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = ProtocolMockPraos defaultDemoPraosParams
fromProtocol _ MockPBFT =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = ProtocolMockPBFT defaultDemoPBftParams
fromProtocol CardanoConfiguration{ccCore} RealPBFT = do
    let Core{ coGenesisFile
            , coGenesisHash
            , coPBftSigThd
            } = ccCore
        genHash = either (throw . ConfigurationError) id $
                  decodeAbstractHash coGenesisHash
        cvtRNM :: RequireNetworkMagic -> RequiresNetworkMagic
        cvtRNM NoRequireNetworkMagic = RequiresNoMagic
        cvtRNM RequireNetworkMagic   = RequiresMagic

    gcE <- runExceptT (Genesis.mkConfigFromFile (cvtRNM $ coRequiresNetworkMagic ccCore) coGenesisFile genHash)
    let gc = case gcE of
          Left err -> throw err -- TODO: no no no!
          Right x -> x

    optionalLeaderCredentials <- readLeaderCredentials gc ccCore

    let
        -- TODO:  make configurable via CLI (requires cardano-shell changes)
        -- These defaults are good for mainnet.
        defSoftVer  = Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1
        defProtoVer = Update.ProtocolVersion 0 2 0
        -- TODO: The plumbing here to make the PBFT options from the
        -- CardanoConfiguration is subtle, it should have its own function
        -- to do this, along with other config conversion plumbing:
        p = ProtocolRealPBFT
              gc
              (fmap PBftSignatureThreshold coPBftSigThd)
              defProtoVer
              defSoftVer
              optionalLeaderCredentials

    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p


readLeaderCredentials :: Genesis.Config
                      -> Shell.Config.Core
                      -> IO (Maybe PBftLeaderCredentials)
readLeaderCredentials gc Shell.Config.Core {
                           coStaticKeySigningKeyFile = Just signingKeyFile
                         , coStaticKeyDlgCertFile    = Just delegCertFile
                         } = do
    signingKeyFileBytes <- LB.readFile signingKeyFile
    delegCertFileBytes  <- LB.readFile delegCertFile

    --TODO: review the style of reporting for input validation failures
    -- If we use throwIO, we should use a local exception type that
    -- wraps the other structured failures and reports them appropriatly
    signingKey <- either throwIO return $
                    deserialiseSigningKey signingKeyFileBytes

    delegCert  <- either (fail . Text.unpack) return $
                    CanonicalJSON.canonicalDecPre delegCertFileBytes

    either throwIO (return . Just)
           (mkPBftLeaderCredentials gc signingKey delegCert)
  where
    deserialiseSigningKey :: LB.ByteString
                          -> Either DeserialiseFailure Signing.SigningKey
    deserialiseSigningKey =
        fmap (Signing.SigningKey . snd)
      . deserialiseFromBytes Signing.fromCBORXPrv

--TODO: fail noisily if only one file is specified without the other
-- since that's obviously a user error.
readLeaderCredentials _gc _ = return Nothing


-- TODO: consider not throwing this, or wrap it in a local error type here
-- that has proper error messages.
instance Exception Genesis.ConfigurationError

-- Node can be run in two modes.
data ViewMode =
    LiveView    -- Live mode with TUI
  | SimpleView  -- Simple mode, just output text.

{-------------------------------------------------------------------------------
  Common CLI
-------------------------------------------------------------------------------}

-- | CLI Arguments common to all Cardano node flavors
data CommonCLI = CommonCLI
  { cliGenesisFile                :: !(Last FilePath)
  , cliGenesisHash                :: !(Last Text)
  , cliStaticKeySigningKeyFile    :: !(Last FilePath)
  , cliStaticKeyDlgCertFile       :: !(Last FilePath)
  , cliPBftSigThd                 :: !(Last Double)
  , cliDBPath                     :: !(Last FilePath)
  , cliSocketPath                 :: !(Last FilePath)
  --TODO cliUpdate                :: !PartialUpdate
  }

parseCommonCLI :: Parser CommonCLI
parseCommonCLI =
    CommonCLI
    <$> lastStrOption
           ( long "genesis-file"
          <> metavar "FILEPATH"
          <> help "The filepath to the genesis file."
           )
    <*> lastStrOption
           ( long "genesis-hash"
          <> metavar "GENESIS-HASH"
          <> help "The genesis hash value."
           )
    <*> lastStrOption
           ( long "signing-key"
          <> metavar "FILEPATH"
          <> help "Path to the signing key."
           )
    <*> lastStrOption
           ( long "delegation-certificate"
          <> metavar "FILEPATH"
          <> help "Path to the delegation certificate."
           )
    <*> lastDoubleOption
           ( long "pbft-signature-threshold"
          <> metavar "DOUBLE"
          <> help "The PBFT signature threshold."
           )
    <*> lastStrOption (
            long "database-path"
         <> metavar "FILEPATH"
         <> help "Directory where the state is stored."
        )
    <*> lastStrOption (
            long "socket-path"
         <> metavar "FILEPATH"
         <> help "The local socket filepath."
        )


{-------------------------------------------------------------------------------
  Configuration merging
-------------------------------------------------------------------------------}

-- | Perform merging of layers of configuration, but for now, only in a trivial way,
--   just for Cardano.Shell.Constants.Types.{Genesis,StaticKeyMaterial}.
--   We expect this process to become generic at some point.
mergeConfiguration
  :: PartialCardanoConfiguration
  -> CommonCLI
  -> PartialCardanoConfiguration
mergeConfiguration pcc cli =
    -- The beauty of this kind of configuration management (using trees of
    -- monoids) is that we can override individual config elements by simply
    -- merging an extra partial config on top. That extra partial config is
    -- built starting from mempty and setting the fields of interest.
    pcc <> commonCLIToPCC cli
  where
    commonCLIToPCC :: CommonCLI -> PartialCardanoConfiguration
    commonCLIToPCC CommonCLI {
                     cliGenesisFile
                   , cliGenesisHash
                   , cliStaticKeySigningKeyFile
                   , cliStaticKeyDlgCertFile
                   , cliPBftSigThd
                   , cliDBPath
                   , cliSocketPath
                   } =
      mempty { pccCore = mempty
                    { pcoGenesisFile             = cliGenesisFile
                    , pcoGenesisHash             = cliGenesisHash
                    , pcoStaticKeySigningKeyFile = cliStaticKeySigningKeyFile
                    , pcoStaticKeyDlgCertFile    = cliStaticKeyDlgCertFile
                    , pcoPBftSigThd              = cliPBftSigThd
                    -- TODO: cliUpdate
                    }
             , pccDBPath = cliDBPath
             , pccSocketPath = cliSocketPath
             }



{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    OA.command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]

-- TODO:  deal with cardano-shell duplication
-- | Lift the parser to an optional @Last@ type.
lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

-- | General @Last@ auto option from @Read@ instance.
lastAutoOption :: Read a => Mod OptionFields a -> Parser (Last a)
lastAutoOption args = lastOption (option auto args)

lastIntOption :: Mod OptionFields Int -> Parser (Last Int)
lastIntOption = lastAutoOption

lastDoubleOption :: Mod OptionFields Double -> Parser (Last Double)
lastDoubleOption = lastAutoOption

lastBoolOption :: Mod OptionFields Bool -> Parser (Last Bool)
lastBoolOption = lastAutoOption

lastWordOption :: Mod OptionFields Word -> Parser (Last Word)
lastWordOption = lastAutoOption

lastTextListOption :: Mod OptionFields [Text] -> Parser (Last [Text])
lastTextListOption = lastAutoOption

lastStrOption :: IsString a => Mod OptionFields a -> Parser (Last a)
lastStrOption args = Last <$> optional (strOption args)


--------------------------------------------------------------------------------
-- Node CLI Types
--------------------------------------------------------------------------------

data NodeCLIArguments = NodeCLIArguments {
    slotDuration :: !SlotLength
  , commonCLI    :: !CommonCLI
  , command      :: !NodeCommand
  }

data NodeCommand =
    SimpleNode  TopologyInfo NodeAddress Protocol ViewMode TraceOptions
  | TxSubmitter TopologyInfo Mock.Tx     Protocol
  | TraceAcceptor