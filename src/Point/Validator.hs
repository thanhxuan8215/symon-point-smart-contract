{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Point.Validator
    ( typedPointValidator
    , Point (..)
    , Points (..)
    , PointAction (..)
    , BalanceAction (..)
    , type ClearString
    , createBalanceList
    , updateBalanceList
    , balanceOf
    ) where

import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Prelude                     (Semigroup (..))
import qualified Prelude                     as P
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Ledger.Constraints          as Constraints
import           Ledger.Ada                  as Ada
import           Text.Printf                 (printf)

import           Point.Utils                 (getCredentials)

minLovelace :: Integer
minLovelace = 2_000_000

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, P.Show, Generic, FromJSON, ToJSON, ToSchema, Eq)
PlutusTx.makeLift ''ClearString

data Point = Point
    { pUser        :: !Address
    , pShop        :: !Address
    , pAmount      :: !Integer
    , pCreatedTime :: !POSIXTime
    } deriving (P.Show, Generic, FromJSON, ToJSON, ToSchema)

instance Eq Point where
    {-# INLINABLE (==) #-}
    a == b = (pUser a == pUser b) &&
             (pShop a == pShop b)

PlutusTx.unstableMakeIsData ''Point     

data Points = Points 
    { psPoints      :: ![Point]
    , psCreatedTime :: !POSIXTime
    , psOwner       :: !PubKeyHash
    } deriving (P.Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Points

data PointAction = Grant | Exchange | Redeem | Cancel
    deriving P.Show

PlutusTx.unstableMakeIsData ''PointAction

data BalanceAction = Update Points
    deriving P.Show    

PlutusTx.unstableMakeIsData ''BalanceAction

data PointParam = PointParam
    { ppCurrency  :: !CurrencySymbol
    , ppTokenName :: !TokenName
    , ppAmount    :: !Integer
    }

{-# INLINABLE mkPointValidator #-}
mkPointValidator :: Points -> BalanceAction -> ScriptContext -> Bool
mkPointValidator pd action ctx =
    traceIfFalse "Wrong input value" correctInputValue &&
    case action of
        Update Points{psPoints = ps, psCreatedTime = _} -> 
            traceIfFalse "negative balance" (checkAmount ps)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signByOwner :: Bool
        signByOwner = txSignedBy info $ psOwner pd

        input :: TxInInfo
        input =
            let
                isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
                    Nothing -> False
                    Just _  -> True
                xs = [i | i <- txInfoInputs info, isScriptInput i]
            in
                case xs of
                    [i] -> i
                    _   -> traceError "Expected exactly one script input"                  

        checkAmount :: [Point] -> Bool
        checkAmount ps = case ps of
            []     -> True
            (x:xs) -> pAmount x >= 0 && checkAmount xs
        
        correctInputValue :: Bool
        correctInputValue = checkAmount (psPoints pd)

data Action
instance Scripts.ValidatorTypes Action where
    type instance RedeemerType Action = BalanceAction
    type instance DatumType Action = Points

typedPointValidator :: Scripts.TypedValidator Action
typedPointValidator = Scripts.mkTypedValidator @Action
    $$(PlutusTx.compile [|| mkPointValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @Points @BalanceAction

pointValidator :: Validator
pointValidator = Scripts.validatorScript typedPointValidator

pointHash :: Ledger.ValidatorHash
pointHash = Scripts.validatorHash typedPointValidator

pointAddress :: Ledger.Address
pointAddress = scriptHashAddress pointHash

createBalanceList :: AsContractError e => Points -> Contract w s e ()
createBalanceList ps = do
    logInfo @P.String "---------------- Create balance list ----------------"

    -- let cs  = ppCurrency pp
    --     tn  = ppTokenName pp
    --     amt = ppAmount pp
        -- v   = Value.singleton cs tn amt <> Ada.lovelaceValueOf minLovelace
    let v   = Ada.lovelaceValueOf minLovelace
        tx  = Constraints.mustPayToTheScript ps v

    ledgerTx <- submitTxConstraints typedPointValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                

-- balanceOf :: Address -> Contract w s Text (Integer)
-- balanceOf addr = do
--     logInfo @P.String "---------------- Query balance of a user ----------------"
--     utxos <- utxosAt $ scriptHashAddress pointHash
--     let xs = [ (oref, o)
--              | (oref, o) <- Map.toList utxos
--              ]
--     case xs of
--         []          -> return 0
--         [(oref, o)] -> case _ciTxOutDatum o of
--             Left _          -> return 0
--             Right (Datum e) -> case PlutusTx.fromBuiltinData e of
--                 Nothing -> return 0
--                 Just Points{psPoints = ps, psCreatedTime = _} -> do
--                     let mUser = findUser (\user -> pUser user == addr) ps
--                     case mUser of
--                         Nothing -> return 0
--                         -- Just user -> return (oref, o, pAmount user)
--                         -- Just user -> logInfo @P.String $ printf "balance of %s is %s" (P.show (pUser user)) (P.show (pAmount user))
--                         Just user ->
--                             -- logInfo @P.String $ printf "Balance of user %s is %s: " (P.show addr) (P.show (pAmount user)) 
--                             return (pAmount user)

balanceOf :: Address -> Contract w s Text ()
balanceOf addr = do
    logInfo @P.String "---------------- Query balance of a user ----------------"
    utxos <- utxosAt $ scriptHashAddress pointHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        []          -> logInfo @P.String $ printf "0"
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> logInfo @P.String $ printf "0"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> logInfo @P.String $ printf "0"
                Just Points{psPoints = ps, psCreatedTime = _} -> do
                    let mUser = findUser (\user -> pUser user == addr) ps
                    case mUser of
                        Nothing -> logInfo @P.String $ printf "0"
                        -- Just user -> return (oref, o, pAmount user)
                        -- Just user -> logInfo @P.String $ printf "balance of %s is %s" (P.show (pUser user)) (P.show (pAmount user))
                        Just user ->
                            logInfo @P.String $ printf "Balance of user %s is %s: " (P.show addr) (P.show (pAmount user)) 
                            -- return (pAmount user)                            

updateBalanceList :: forall w s. Points -> Contract w s Text ()
updateBalanceList Points{..} = do
    logInfo @P.String "---------------- Update balance list ----------------"
    (oref, o, d@Points{..}) <- findBalanceList
    logInfo @P.String $ printf "found balance utxo with datum %s" (P.show d)

    pkh <- Contract.ownPaymentPubKeyHash
    let v = Ada.lovelaceValueOf 3_000_000
        r = Redeemer $ PlutusTx.toBuiltinData $ Update d

        lookups = Constraints.typedValidatorLookups typedPointValidator P.<> -- used for the output utxo with the new contract instance
                  Constraints.otherScript pointValidator                P.<> -- used for consuming the input contract instance
                  Constraints.unspentOutputs (Map.singleton oref o)
        -- tx      = case adHighestBid of
        --             Nothing      -> Constraints.mustPayToTheScript d' v                            <>
        --                             Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
        --                             Constraints.mustSpendScriptOutput oref r
        --             Just Bid{..} -> Constraints.mustPayToTheScript d' v                            <>
        --                             Constraints.mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
        --                             Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
        --                             Constraints.mustSpendScriptOutput oref r
        tx = Constraints.mustPayToTheScript d v         <>
             Constraints.mustSpendScriptOutput oref r                                    
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "update new balance list: %s" (P.show d)

findBalanceList :: Contract w s Text (TxOutRef, ChainIndexTxOut, Points)
findBalanceList = do
    utxos <- utxosAt $ scriptHashAddress pointHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> Contract.throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> Contract.throwError "datum has wrong type"
                Just d@Points{..} -> return (oref, o, d)
        _           -> Contract.throwError "auction utxo not found"             

findUser :: (Point -> Bool) -> [Point] -> Maybe Point
findUser _ [] = Nothing
findUser f (x:xs) 
    | f x = Just x
    | otherwise = findUser f xs
