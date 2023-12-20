-- Data.Transaction.hs
module Data.Transaction
  ( Transaction(..)
  ) where

import Data.Time (UTCTime)
import Data.Member (Member)
import Data.Book (Book)

data Transaction = Transaction
  { transactionType :: Bool  -- True para BorrowTransaction, False para ReturnTransaction
  , transactionMember :: Member
  , transactionBook :: Book
  , transactionDate :: UTCTime
  } deriving (Show, Read)
