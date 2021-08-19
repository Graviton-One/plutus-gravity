{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Gravity where

import           Control.Monad                  (void)
import           Data.Aeson                     (FromJSON, ToJSON)
import           GHC.Generics                   (Generic)
import           Ledger                         (Address, Datum(Datum), ScriptContext, TxOutTx, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada                     as Ada
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import           Plutus.Contract
import           Plutus.Contract.Schema         ()
import           Plutus.Trace.Emulator          (EmulatorTrace)
import qualified Plutus.Trace.Emulator          as Trace
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Data.Map                       as Map
import           Schema                         (ToArgument, ToSchema)
import           Wallet.Emulator                (Wallet (..))

import qualified Data.ByteString.Char8          as C
import qualified Prelude
import           Data.Maybe                     (catMaybes)

type GravitySchema =
        Endpoint "updateConsuls" UpdateConsulsParams
        .\/ Endpoint "hashNewConsuls" HashNewConsulsParams

-- | The validation function (DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodHash hs cs

{-# INLINABLE isGoodGuess #-}
isGoodHash :: HashedString -> ClearString -> Bool
isGoodHash (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

gravityValidator :: Validator
gravityValidator = Scripts.validatorScript gravityInstance

data Gravity
instance Scripts.ValidatorTypes Gravity where
    type instance RedeemerType Gravity = String
    type instance DatumType Gravity = HashedString

gravityInstance :: Scripts.TypedValidator Gravity
gravityInstance = Scripts.mkTypedValidator @Gravity
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HashedString @ClearString

hashString :: Prelude.String -> HashedString
hashString = HashedString . sha2_256 . C.pack

clearString :: Prelude.String -> ClearString
clearString = ClearString . C.pack

gravityAddress :: Address
gravityAddress = Ledger.scriptAddress gravityValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: Prelude.String
    , amount     :: Value
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: Prelude.String
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

gravity :: (AsContractError e) => Contract () GravitySchema e ()
gravity = do
  lock `select` guess

lock :: (AsContractError e) => Contract () GravitySchema e ()
lock = do
    logInfo @Prelude.String "Waiting for lock endpoint..."
    LockParams secret amt <- endpoint @"lock" @LockParams
    logInfo @Prelude.String $ "Pay " <> Prelude.show amt <> " to the script"
    let tx         = Constraints.mustPayToTheScript (hashString secret) amt
    void (submitTxConstraints gravityInstance tx)

guess :: (AsContractError e) => Contract () GravitySchema e ()
guess = do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Prelude.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq gravityAddress (Ada.lovelaceValueOf 1)
    -- Wait for a call on the guess endpoint
    logInfo @Prelude.String "Waiting for guess endpoint..."
    GuessParams theGuess <- endpoint @"guess" @GuessParams

    let redeemer = clearString theGuess
        tx       = collectFromScript utxos redeemer

    -- Log a message saying if the secret word was correctly guessed
    let hashedSecretWord = findSecretWordValue utxos
        isCorrectSecretWord = fmap (`isGoodGuess` redeemer) hashedSecretWord == Just True
    if isCorrectSecretWord
       then logWarn "Correct secret word! Submitting the transaction"
       else logWarn "Incorrect secret word, but still submiting the transaction"

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the guess is
    -- wrong.
    logInfo @Prelude.String "Submitting transaction to guess the secret word"
    void (submitTxConstraintsSpending gravityInstance utxos tx)

-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: UtxoMap -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: TxOutTx -> Maybe HashedString
secretWordValue o = do
  dh <- Ledger.txOutDatum $ Ledger.txOutTxOut o
  Datum d <- Map.lookup dh $ Ledger.txData $ Ledger.txOutTxTx o
  PlutusTx.fromData d

lockTrace :: Wallet -> Prelude.String -> EmulatorTrace ()
lockTrace wallet secretWord = do
    hdl <- Trace.activateContractWallet wallet (lock @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"lock" hdl (LockParams secretWord (Ada.adaValueOf 10))
    void $ Trace.waitNSlots 1

guessTrace :: Wallet -> Prelude.String -> EmulatorTrace ()
guessTrace wallet guessWord = do
    hdl <- Trace.activateContractWallet wallet (guess @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"guess" hdl (GuessParams guessWord)
    void $ Trace.waitNSlots 1
