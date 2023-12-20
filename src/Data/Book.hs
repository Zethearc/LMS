module Data.Book
    ( Book (..)
    , addBook
    , createBook
    , readBook
    , updateBook
    , deleteBook
    ) where

import Data.Time (UTCTime)

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

addBook :: Book -> [Book] -> [Book]
addBook book books = book : books

createBook :: Int -> String -> String -> Bool -> Int -> Maybe String -> Maybe UTCTime -> Double -> Book
createBook bookId title author available stock borrower dueDate initialValue =
    Book bookId title author available stock borrower dueDate initialValue

readBook :: Book -> String
readBook book =
    "ID del Libro: " ++ show (bookId book) ++
    "\nTítulo: " ++ title book ++
    "\nAutor: " ++ author book ++
    "\nDisponible: " ++ show (available book) ++
    "\nExistencias: " ++ show (stock book) ++
    (case borrower book of
        Just borrowerName -> "\nTomado por: " ++ borrowerName
        Nothing -> "") ++
    (case dueDate book of
        Just date -> "\nFecha de devolución: " ++ show date
        Nothing -> "") ++
    "\nValor inicial: " ++ show (initialValue book)

updateBook :: Book -> String -> String -> Bool -> Int -> Maybe String -> Maybe UTCTime -> Double -> Book
updateBook book newTitle newAuthor newAvailability newStock newBorrower newDueDate newInitialValue =
    book { title = newTitle
         , author = newAuthor
         , available = newAvailability
         , stock = newStock
         , borrower = newBorrower
         , dueDate = newDueDate
         , initialValue = newInitialValue
         }

deleteBook :: Book -> Book
deleteBook book = book { available = False }