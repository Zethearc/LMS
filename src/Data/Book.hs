module Data.Book
    ( Book(..)
    , addBook
    , createBook
    , readBook
    , updateBook
    , deleteBook
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

-- Función para agregar un nuevo libro
addBook :: [Book] -> Book -> [Book]
addBook books newBook = books ++ [newBook]

-- Función para crear un nuevo libro
createBook :: Int -> String -> String -> Int -> Double -> Book
createBook bookId title author stock initialValue = Book
    { bookId = bookId
    , title = title
    , author = author
    , available = True
    , stock = stock
    , borrower = Nothing
    , dueDate = Nothing
    , initialValue = initialValue
    }

-- Función para leer un libro
readBook :: [Book] -> Int -> Maybe Book
readBook books targetBookId = find (\book -> bookId book == targetBookId) books

-- Función para actualizar un libro
updateBook :: [Book] -> Book -> [Book]
updateBook books updatedBook = map (\book -> if bookId book == bookId updatedBook then updatedBook else book) books

-- Función para eliminar un libro
deleteBook :: [Book] -> Int -> [Book]
deleteBook books bookIdToRemove = filter (\book -> bookId book /= bookIdToRemove) books
