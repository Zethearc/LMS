module Data.Book
    ( Book(..)
    , decreaseStock
    , decreaseBookStock
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

decreaseStock :: [Book] -> Int -> [Book]
decreaseStock books bookIdToBorrow =
    map (\b -> if bookId b == bookIdToBorrow then decreaseBookStock b else b) books

decreaseBookStock :: Book -> Book
decreaseBookStock book = book { stock = stock book - 1 }
