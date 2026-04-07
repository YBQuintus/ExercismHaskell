module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where


import Control.Concurrent.MVar

data BankAccount = BankAccount (MVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount mInt) = 
  modifyMVar_ mInt $ \_ -> return Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount mInt) = do 
  readMVar mInt

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount mInt) amount =
  modifyMVar mInt $ \current ->
    case current of
      Nothing -> return (Nothing, Nothing)
      Just bal -> let newBal = bal + amount in return (Just newBal, Just newBal)
    

openAccount :: IO BankAccount
openAccount = do
  value <- newMVar (Just 0)
  return (BankAccount value)
