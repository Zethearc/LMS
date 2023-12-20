-- Data.Transaction.hs
module Data.Transaction
  ( Transaction(..)
  , borrowTransaction
  , returnTransaction
  ) where

import Data.Time (UTCTime)
import Data.Member (Member)
import Data.Book (Book)

data Transaction = BorrowTransaction
  { transactionBorrower :: Member
  , borrowedBook :: Book
  , transactionDate :: UTCTime
  }
  | ReturnTransaction
  { returningMember :: Member
  , returnedBook :: Book
  , transactionDate :: UTCTime
  } deriving (Show, Read)

borrowTransaction :: Member -> Book -> UTCTime -> Transaction
borrowTransaction member book date =
  BorrowTransaction { transactionBorrower = member, borrowedBook = book, transactionDate = date }

returnTransaction :: Member -> Book -> UTCTime -> Transaction
returnTransaction member book date =
  ReturnTransaction { returningMember = member, returnedBook = book, transactionDate = date }
