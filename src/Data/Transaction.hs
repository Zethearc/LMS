-- Data.Transaction.hs
module Data.Transaction
  ( Transaction(..)
  , borrowTransaction
  , returnTransaction
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

borrowTransaction :: Member -> Book -> UTCTime -> Transaction
borrowTransaction member book date =
  Transaction { transactionType = True, transactionMember = member, transactionBook = book, transactionDate = date }

returnTransaction :: Member -> Book -> UTCTime -> Transaction
returnTransaction member book date =
  Transaction { transactionType = False, transactionMember = member, transactionBook = book, transactionDate = date }
