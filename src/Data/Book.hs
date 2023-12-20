module Data.Book
    ( Book(..)
    , decreaseStock
    , decreaseBookStock
    , increaseStock
    , increaseBookStock
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

increaseStock :: [Book] -> Int -> [Book]
increaseStock books bookIdToReturn =
    map (\b -> if bookId b == bookIdToReturn then increaseBookStock b else b) books

increaseBookStock :: Book -> Book
increaseBookStock book = book { stock = stock book + 1 }