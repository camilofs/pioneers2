{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet -- Wallet needed
import Data.Text             (unpack)

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

-- Contract w s e a
payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract -- recursively calls the contract (endpoint, runs, endpoint) 

contractHandler :: Contract () PaySchema () ()
contractHandler = do
    Contract.handleError
        (\err -> Contract.logError $ "caught: " ++ unpack err)
        payContract
    contractHandler -- same recursion

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace xs ys = do  -- "do" notation
    h <- activateContractWallet (Wallet 1) contractHandler
    let phk2 = pubKeyHash $ walletPubKey $ Wallet 2
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = phk2
        , ppLovelace  = xs
        }
    void $ Emulator.waitNSlots 1 -- void $ to ignore the result (of waiting)
    callEndpoint @"pay" h $ PayParams
        { ppRecipient = phk2
        , ppLovelace  = ys
        }
    void $ Emulator.waitNSlots 1
    
payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
