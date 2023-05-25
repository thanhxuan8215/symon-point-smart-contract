{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Point.Schema where

import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Plutus.Contract             as Contract
import           Data.Text                   (Text) 
import           Ledger                      hiding (mint, singleton)

import           Point.Token                 (TokenParams, BurnParams, ListParams, mintToken, burnToken, listTokens)
import           Point.Validator             

-- Endpoints Schema ---

type MintSchema = 
        Endpoint "mint" TokenParams

endpoints1 :: Contract () ProjectSchema Text ()
endpoints1 = mint' >> endpoints2
    where
        mint' = awaitPromise $ endpoint @"mint" mintToken

type ProjectSchema = 
    Endpoint "mint" TokenParams
    .\/ Endpoint "list" ListParams
    .\/ Endpoint "burn" BurnParams

endpoints2 :: Contract () ProjectSchema Text ()
endpoints2 = awaitPromise (burn' `select` list') >> endpoints2
    where
        burn' = endpoint @"burn" burnToken
        list' = endpoint @"list" listTokens

type PointSchema = 
    Endpoint "create" Points
    .\/ Endpoint "balance" Address
    .\/ Endpoint "update" Points

endpoints3 :: Contract () PointSchema Text ()
endpoints3 = awaitPromise (create' `select` update') >> endpoints3
    where
        create'  = endpoint @"create" createBalanceList
        update' = endpoint @"update" updateBalanceList
        -- balance' = endpoint @"balance" balanceOf

endpoints4 :: Contract () PointSchema Text ()
endpoints4 = awaitPromise balance' >> endpoints4
    where
        balance' = endpoint @"balance" balanceOf      