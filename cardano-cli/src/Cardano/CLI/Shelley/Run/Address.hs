module Cardano.CLI.Shelley.Run.Address
  ( runBootStrapAddressGen
  ) where

import           Cardano.Prelude

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Cardano.Api (Address(..), Network, ShelleyKeyDiscriminator(..), byronPubKeyAddress,
                   shelleyGenKeyPair)


runBootStrapAddressGen :: Network -> IO ()
runBootStrapAddressGen network = do
  keyPair <- shelleyGenKeyPair RegularShelleyKey
  let sPa = mkShelleyPubKeyAddress $ mkShelleyPublicKey keyPair
  print sPa


