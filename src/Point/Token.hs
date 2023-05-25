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

module Point.Token
    ( tokenPolicy
    , tokenCurSymbol
    , TokenParams (..), BurnParams(..), ListParams(..), TokenPolicyParams(..)
    , adjustAndSubmit, adjustAndSubmitWith
    , mintToken
    , burnToken
    , listTokens
    ) where

import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Ledger.Constraints          as Constraints
import           Ledger.Ada                  as Ada
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Prelude                     (Semigroup (..))
import qualified Prelude                     as P
import           Control.Monad               hiding (fmap)
import           Text.Printf                 (printf)

import           Point.Utils                 (getCredentials)

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx = 
    if mintOnly then 
        traceIfFalse "UTxO is not consumed / Can't mint anymore identical tokens" hasUTxO &&
        traceIfFalse "Wrong amount minted" checkMintedAmount
    else traceIfFalse "Burn quantity more than existing tokens" checkValidAmount
    
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        getFlattenVal :: [(CurrencySymbol, TokenName, Integer)]
        getFlattenVal = flattenValue (txInfoMint info)

        mintOnly :: Bool
        mintOnly = case getFlattenVal of
            [(_, tn', amt')] -> tn' == tn && amt' >= 0 -- positive amt = minting
            _                -> False

        checkMintedAmount :: Bool
        checkMintedAmount = case getFlattenVal of
            [(_, tn', amt')] -> tn' == tn && amt' == amt
            _                -> False
        
        checkValidAmount :: Bool
        checkValidAmount = case getFlattenVal of
            [(_, _, amt')] -> amt >= abs amt' -- burn amount must be less or equal to existing token
            _              -> False

tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt'||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn

data TokenParams = TokenParams
    { tpToken    :: !TokenName
    , tpAmount   :: !Integer
    , tpAddress  :: !Address
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON, ToSchema, P.Show)

data BurnParams = BurnParams
    { btCurrency :: !CurrencySymbol
    , btToken    :: !TokenName
    , btAmount   :: !Integer
    , btAddress  :: !Address
    , btPolicy   :: !TokenPolicyParams
    } deriving (P.Eq, P.Ord, P.Show, Generic, ToJSON, FromJSON, ToSchema)

data TokenPolicyParams = TokenPolicyParams
    { tkpOref   :: !TxOutRef
    , tkpToken  :: !TokenName
    , tkpAmount :: !Integer
    } deriving (P.Eq, P.Ord, P.Show, Generic, FromJSON, ToJSON, ToSchema)

data ListParams = ListParams
    { lpAddress    :: !Address
    , lpWalletName :: !P.String
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
    Contract.logDebug @P.String $ printf "unbalanced: %s" $ P.show unbalanced
    unsigned <- balanceTx unbalanced
    Contract.logDebug @P.String $ printf "balanced: %s" $ P.show unsigned
    signed <- submitBalancedTx unsigned
    Contract.logDebug @P.String $ printf "signed: %s" $ P.show signed
    return signed

adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ Constraints.typedValidatorLookups inst

mintToken :: TokenParams -> Contract w s Text CurrencySymbol
mintToken tp = do
    logInfo @P.String "---------------- Start Minting ----------------"
    -- Contract.logInfo @P.String $ printf "started minting: %s" $ show tp
    let addr = tpAddress tp
    case getCredentials addr of
        Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got %s" $ P.show addr
        Just (x, my) -> do
            oref <- getUnspentOutput
            o    <- fromJust <$> Contract.txOutFromRef oref
            -- o    <- Contract.txOutFromRef oref
            logInfo @P.String $ printf "picked UTxO at %s with value %s" (P.show oref) (P.show $ _ciTxOutValue o)

            let tn          = tpToken tp
                amt         = tpAmount tp
                cs          = tokenCurSymbol oref tn amt
                val         = Value.singleton cs tn amt
                c           = case my of
                    Nothing -> Constraints.mustPayToPubKey x val
                    Just y  -> Constraints.mustPayToPubKeyAddress x y val
                lookups     = Constraints.mintingPolicy (tokenPolicy oref tn amt) <>
                              Constraints.unspentOutputs (Map.singleton oref o)
                constraints = Constraints.mustMintValue val          <>
                              Constraints.mustSpendPubKeyOutput oref <>
                              c

            void $ adjustAndSubmitWith @Void lookups constraints
            Contract.logInfo @P.String $ printf "minted %s" (P.show val)
            return cs

burnToken :: BurnParams -> Contract w s Text ()
burnToken bp = do
    logInfo @P.String "---------------- Burning ----------------"

    utxos <- utxosAt $ btAddress bp

    let xs = [ (oref, o) 
             | (oref, o) <- Map.toList utxos
             , case flattenValue (_ciTxOutValue o) of
                [(_, _, _), (curr', tn', _')] -> curr' /= "" && tn' /= ""
                _                             -> False
             ]
    case xs of
        [] -> logInfo @P.String "Token to be burnt not found"
        ((oref, o) : _) -> do
            let tn = btToken bp
                amt = btAmount bp
                cs = btCurrency bp
                origamt = Value.valueOf (_ciTxOutValue o) cs tn
                tkpparams = btPolicy bp
                tkpolicy = tokenPolicy (tkpOref tkpparams) (tkpToken tkpparams) (tkpAmount tkpparams)
                val = Value.singleton cs tn amt
                lookups = Constraints.mintingPolicy tkpolicy <> Constraints.unspentOutputs utxos
                tx = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref

            logInfo @P.String $ printf "Token Currency: %s " (P.show cs)
            logInfo @P.String $ printf "Token Name: %s " (P.show tn)
            logInfo @P.String $ printf "Current value of token: %s " (P.show origamt)
            logInfo @P.String $ printf "Token to be burnt: %s " (P.show (abs amt))

            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

listTokens :: ListParams -> Contract w s Text ()
listTokens lp = do
    logInfo @P.String $ printf "---------------- Listing for %s ----------------" (lpWalletName lp)
    utxos <- utxosAt (lpAddress lp)
    -- logInfo @P.String $ printf "------- UTXOS for %s -------" (P.show utxos)
    
    let xs = [ flattenValue (_ciTxOutValue o) 
             | (_, o) <- Map.toList utxos        
             , case flattenValue (_ciTxOutValue o) of
                    [(_, _, _), (curr', tn', _)] -> curr' /= "" && tn' /= ""
                    _                            -> False       
             ]
    case xs of
        [] -> logInfo @P.String "No token acquired" 
        _  -> logInfo @P.String $ printf "Current token(s):  %s" (P.show xs)