{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}

module Point.Trace
    ( testWorkFlow
    ) where

import           Control.Monad              hiding (fmap)
import qualified Control.Monad.Freer.Extras as Extras
import qualified Data.Aeson                 as Aeson
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text, pack)
import           Ledger.Ada                 as Ada
import           Ledger                     hiding (mint, singleton) 
import           Ledger.TimeSlot            
import           Ledger.Value               as Value
import           Plutus.Contract            as Contract

import qualified Plutus.Trace               as Trace
import           Plutus.Trace.Emulator      as Emulator
import           Plutus.Trace.Emulator.Types
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..), IO, show)
import qualified Prelude                    as P
import           Wallet.Emulator.Wallet
import           Wallet.Emulator.MultiAgent
import           System.IO

import           Point.Token
import           Point.Schema    
import           Point.Validator
import           Point.Utils


customShowEvent :: EmulatorEvent' -> Maybe P.String
customShowEvent = \case
    UserThreadEvent (UserLog msg)                                            -> Just $ "*** USER LOG: " <> msg <> "\n"
    InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> P.show msg <> "\n"
    InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _)           -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> P.show err <> "\n"
    ev                                                                       -> Nothing

traceConfig :: Trace.TraceConfig
traceConfig = Trace.TraceConfig customShowEvent stdout

emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig (Left $ Map.fromList [(knownWallet 1, v1),  (knownWallet 2, v2), 
                                (knownWallet 3, v3), (knownWallet 4, v4) ]) def def
    where
        v1 :: Value
        v1 = Ada.lovelaceValueOf 100_000_000
        v2 :: Value
        v2 = Ada.lovelaceValueOf 100_000_000
        v3 :: Value
        v3 = Ada.lovelaceValueOf 100_000_000
        v4 :: Value
        v4 = Ada.lovelaceValueOf 100_000_000
        -- v3 :: Value
        -- v3 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 10_000
        -- v4 :: Value
        -- v4 = Ada.lovelaceValueOf 100_000_000 <> assetClassValue token 10_000

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

token :: AssetClass
token = AssetClass (assetSymbol, assetToken)

testWorkFlow :: IO ()
testWorkFlow = Trace.runEmulatorTraceIO' traceConfig emCfg myTrace

myTrace :: Trace.EmulatorTrace ()
myTrace = do
    let wallets = P.show $ P.map P.show knownWallets
    Extras.logInfo $ "Wallets: " ++ wallets   

    let tn   = "Point"
        val1 = 10

    -- hn_m: Wallet n uses endpoints m

    h1_1 <- Trace.activateContractWallet (knownWallet 1) $ endpoints1
    h2_1 <- Trace.activateContractWallet (knownWallet 2) $ endpoints1
    -- h3_1 <- Trace.activateContractWallet (knownWallet 3) $ endpoints1
    -- h4_1 <- Trace.activateContractWallet (knownWallet 4) $ endpoints1

    -- Mint wallet 1: 10 Points
    Trace.callEndpoint @"mint" h1_1 $ TokenParams
        { tpToken  = tn
        , tpAmount = val1                                  
        , tpAddress = mockWalletAddress (knownWallet 1)
        }

    void $ Trace.waitNSlots 1

    -- Mint wallet 2: 20 Points
    -- let val2 = 20

    -- Trace.callEndpoint @"mint" h2_1 $ TokenParams
    --     { tpToken  = tn
    --     , tpAmount = val2                                  
    --     , tpAddress = mockWalletAddress (knownWallet 2)
    --     }

    -- void $ Trace.waitNSlots 1

    -- List token of wallet 1
    h1_2 <- Trace.activateContractWallet (knownWallet 1) $ endpoints2
    
    Trace.callEndpoint @"list" h1_2 $ ListParams
        { lpAddress = mockWalletAddress (knownWallet 1)
        , lpWalletName = P.show (knownWallet 1)
        }

    void $ Trace.waitNSlots 1

    let tkp = TokenPolicyParams 
            { tkpOref   = TxOutRef 
                { txOutRefId = "f6fa7ff5174a36bf606d6de7365d914bccf15f95d0b311e04a36807a8c07b918"
                , txOutRefIdx = 3
                }
            , tkpToken  = tn
            , tkpAmount = val1
            }
    
        cs1 = "cdad0073aae4cd7e6eadb4a9111b3b547c1c05097e9d46469ec74024"  
    
    Trace.callEndpoint @"burn" h1_2 $ BurnParams
        { btCurrency = cs1 
        , btToken  = tn
        , btAmount = -6
        , btAddress = mockWalletAddress (knownWallet 1)
        , btPolicy = tkp
        }

    void $ Trace.waitNSlots 1  

    h1_3 <- Trace.activateContractWallet (knownWallet 2) $ endpoints3        

    Trace.callEndpoint @"create" h1_3 $ Points
        { psPoints = 
            [   Point
                    { pUser = mockWalletAddress (knownWallet 4)
                    , pShop = mockWalletAddress (knownWallet 1)
                    , pAmount = 30
                    , pCreatedTime = 1663322276
                    }
                , Point 
                    { pUser = mockWalletAddress (knownWallet 3)
                    , pShop = mockWalletAddress (knownWallet 1)
                    , pAmount = 40
                    , pCreatedTime = 1663322276
                    }
                , Point
                    { pUser = mockWalletAddress (knownWallet 2)
                    , pShop = mockWalletAddress (knownWallet 1)
                    , pAmount = 10
                    , pCreatedTime = 1663322276
                    }
            ]
        , psOwner = convertToPubKeyHash $ mockWalletAddress (knownWallet 1)
        , psCreatedTime = 1663322276
        }
                
    void $ Trace.waitNSlots 1

    h2_3 <- Trace.activateContractWallet (knownWallet 2) $ endpoints3

    Trace.callEndpoint @"balance" h2_3 $ mockWalletAddress (knownWallet 2)
    
    -- -- void $ Trace.waitNSlots 1 

    -- Trace.callEndpoint @"update" h1_3 $ Points 
    --     { psPoints = 
    --         [   Point
    --                 { pUser = mockWalletAddress (knownWallet 4)
    --                 , pShop = mockWalletAddress (knownWallet 1)
    --                 , pAmount = 30
    --                 , pCreatedTime = 1663322276
    --                 }
    --             , Point 
    --                 { pUser = mockWalletAddress (knownWallet 3)
    --                 , pShop = mockWalletAddress (knownWallet 1)
    --                 , pAmount = 50
    --                 , pCreatedTime = 1663322276
    --                 }
    --             , Point
    --                 { pUser = mockWalletAddress (knownWallet 2)
    --                 , pShop = mockWalletAddress (knownWallet 1)
    --                 , pAmount = 10
    --                 , pCreatedTime = 1663322276
    --                 }
    --         ]
    --     , psOwner = convertToPubKeyHash $ mockWalletAddress (knownWallet 1)
    --     , psCreatedTime = 1663322276
    --     }

    -- void $ Trace.waitNSlots 1        

    -- -- Trace.callEndpoint @"balance" h2_4 $ mockWalletAddress (knownWallet 2)

    -- -- void $ Trace.waitNSlots 1