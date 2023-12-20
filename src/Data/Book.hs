module Data.Book
    ( Book(..)
    ) where

import Data.Time (UTCTime)
import Data.List (find)

data Book = Book
    { bookId :: Int
    , title :: String
    , author :: String
    , available :: Bool
    , stock :: Int
    , borrower :: Maybe String
    , dueDate :: Maybe UTCTime
    , initialValue :: Double
    } deriving (Show, Read)