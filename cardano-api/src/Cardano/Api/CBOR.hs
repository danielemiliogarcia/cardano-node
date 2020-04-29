{-# LANGUAGE PatternSynonyms #-}

module Cardano.Api.CBOR
  ( addressFromCBOR
  , addressToCBOR

  , byronPublicKeyFromCBOR
  , shelleyPublicKeyFromCBOR

  , byronKeyPairFromCBOR
  , byronKeyPairToCBOR
  , shelleyKeyPairToCBOR
  , shelleyKeyPairFromCBOR


  , renderByronPublicKeyToCBOR
  , renderShelleyPublicKeyToCBOR
  , shelleyVerificationKeyFromCBOR
  , shelleyVerificationKeyToCBOR

  , txSignedFromCBOR
  , txSignedToCBOR
  , txUnsignedFromCBOR
  , txUnsignedToCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import           Cardano.Prelude

import           Data.ByteString.Char8 (ByteString)
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Word (Word8)

import           Shelley.Spec.Ledger.Keys (DiscVKey (..), SKey (..), pattern VKey,
                     pattern VKeyGenesis)


addressFromCBOR :: ByteString -> Either ApiError Address
addressFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "Address" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s Address
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        170  -> AddressByron <$> fromCBOR
        171  -> BootStrapAddressShelley <$> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "Address" tag

addressToCBOR :: Address -> ByteString
addressToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      AddressByron ba -> mconcat [ toCBOR (170 :: Word8), toCBOR ba ]
      BootStrapAddressShelley sa -> mconcat [ toCBOR (171 :: Word8), toCBOR sa]

byronKeyPairFromCBOR :: ByteString -> Either ApiError ByronKeyPair
byronKeyPairFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ByronKeyPair" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ByronKeyPair
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        172  -> KeyPairByron <$> fromCBOR <*> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "ByronKeyPair" tag

byronKeyPairToCBOR :: ByronKeyPair -> ByteString
byronKeyPairToCBOR (KeyPairByron vk sk) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (172 :: Word8), toCBOR vk, toCBOR sk ]


shelleyKeyPairFromCBOR :: ByteString -> Either ApiError ShelleyKeyPair
shelleyKeyPairFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ShelleyKeyPair" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ShelleyKeyPair
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        173  -> KeyPairShelley <$> decodeShelleyVerificationKey <*> (SKey <$> decodeSignKeyDSIGN)
        _  -> cborError $ DecoderErrorUnknownTag "ShelleyKeyPair" tag

shelleyKeyPairToCBOR :: ShelleyKeyPair -> ByteString
shelleyKeyPairToCBOR kp =
  case kp of
    KeyPairShelley svk (SKey sk) ->
      CBOR.serializeEncoding' $
            mconcat
              [ toCBOR (173 :: Word8)
              , encodeShelleyVerificationKey svk
              , encodeSignKeyDSIGN sk
              ]
    GenesisKeyPairShelley -> undefined


byronPublicKeyFromCBOR :: ByteString -> Either ApiError ByronPublicKey
byronPublicKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ByronPublicKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ByronPublicKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        174  -> PubKeyByron' <$> networkFromCBOR <*> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "ByronKeyPair" tag

shelleyPublicKeyFromCBOR :: ByteString -> Either ApiError ShelleyPublicKey
shelleyPublicKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ByronPublicKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ShelleyPublicKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        175 -> BootStrapPubKeyShelley' <$> decodeShelleyVerificationKey
        176 -> GenesisPubKeyShelley <$> decodeShelleyVerificationKey
        _  -> cborError $ DecoderErrorUnknownTag "ShelleyKeyPair" tag

renderByronPublicKeyToCBOR :: ByronPublicKey -> ByteString
renderByronPublicKeyToCBOR (PubKeyByron' nw vk) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (174 :: Word8), networkToCBOR nw, toCBOR vk ]

renderShelleyPublicKeyToCBOR :: ShelleyPublicKey -> ByteString
renderShelleyPublicKeyToCBOR (BootStrapPubKeyShelley' (Genesis vk)) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (175 :: Word8), toCBOR vk ]
renderShelleyPublicKeyToCBOR (BootStrapPubKeyShelley' (Regular vk)) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (175 :: Word8), toCBOR vk ]

--publicKeyToCBOR :: PublicKey -> ByteString
--publicKeyToCBOR pk =
--  CBOR.serializeEncoding' $
--    case pk of
--      PubKeyByron nw vk ->
--        mconcat [ toCBOR (174 :: Word8), networkToCBOR nw, toCBOR vk ]
--      BootStrapPubKeyShelley vk ->
--        mconcat [ toCBOR (175 :: Word8), encodeShelleyVerificationKey vk]

txSignedFromCBOR :: ByteString -> Either ApiError TxSigned
txSignedFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "TxSigned" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s TxSigned
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        176  -> TxSignedByron <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
        177  -> pure TxSignedShelley
        _  -> cborError $ DecoderErrorUnknownTag "TxSigned" tag

txSignedToCBOR :: TxSigned -> ByteString
txSignedToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      TxSignedByron btx cbor hash wit ->
        mconcat [ toCBOR (176 :: Word8), toCBOR btx, toCBOR cbor, toCBOR hash, toCBOR wit ]
      TxSignedShelley -> toCBOR (177 :: Word8)

txUnsignedFromCBOR :: ByteString -> Either ApiError TxUnsigned
txUnsignedFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "TxUnsigned" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s TxUnsigned
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        178  -> TxUnsignedByron <$> fromCBOR <*> fromCBOR <*> fromCBOR
        179  -> pure TxUnsignedShelley
        _  -> cborError $ DecoderErrorUnknownTag "TxUnsigned" tag

txUnsignedToCBOR :: TxUnsigned -> ByteString
txUnsignedToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      TxUnsignedByron btx cbor hash ->
        mconcat [ toCBOR (178 :: Word8), toCBOR btx, toCBOR cbor, toCBOR hash ]
      TxUnsignedShelley -> toCBOR (179 :: Word8)

shelleyVerificationKeyFromCBOR :: ByteString -> Either ApiError ShelleyVerificationKey
shelleyVerificationKeyFromCBOR bs =
   first ApiErrorCBOR
    . CBOR.decodeFullDecoder "ShelleyVerificationKey" decodeShelleyVerificationKey
    $ LBS.fromStrict bs

decodeShelleyVerificationKey :: Decoder s ShelleyVerificationKey
decodeShelleyVerificationKey = do
  tag <- CBOR.decodeWord8
  case tag of
    180 -> Genesis <$> (VKeyGenesis <$> decodeVerKeyDSIGN)
    181 -> Regular <$> (VKey <$> decodeVerKeyDSIGN)
    _  -> cborError $ DecoderErrorUnknownTag "ShelleyVerificationKey" tag

shelleyVerificationKeyToCBOR :: ShelleyVerificationKey -> ByteString
shelleyVerificationKeyToCBOR = CBOR.serializeEncoding' . encodeShelleyVerificationKey

encodeShelleyVerificationKey :: ShelleyVerificationKey -> Encoding
encodeShelleyVerificationKey svk =
  case svk of
    Genesis (DiscVKey vk) ->
      mconcat [toCBOR (180 :: Word8), encodeVerKeyDSIGN vk]
    Regular (DiscVKey vk) ->
      mconcat [toCBOR (181 :: Word8), encodeVerKeyDSIGN vk]

-- -------------------------------------------------------------------------------------------------

networkFromCBOR :: Decoder s Network
networkFromCBOR = do
  tag <- CBOR.decodeWord8
  case tag of
    168  -> pure Mainnet
    169  -> Testnet <$> fromCBOR
    _  -> cborError $ DecoderErrorUnknownTag "Network" tag

networkToCBOR :: Network -> Encoding
networkToCBOR nw =
  case nw of
    Mainnet -> mconcat [toCBOR (168 :: Word8)]
    Testnet pid -> mconcat [toCBOR (169 :: Word8), toCBOR pid]
