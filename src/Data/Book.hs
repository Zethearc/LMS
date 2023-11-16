module Data.Book
    ( Book (..)
    , addBook
    , createBook
    , readBook
    , updateBook
    , deleteBook
    ) where

data Book = Book
    { bookId :: Int
    , title :: String
    , author :: String
    , available :: Bool
    } deriving (Show, Read)

addBook :: Book -> [Book] -> [Book]
addBook book books = book : books

createBook :: Int -> String -> String -> Bool -> Book
createBook bookId title author available = Book bookId title author available

readBook :: Book -> String
readBook book = "ID del Libro: " ++ show (bookId book) ++ "\nTítulo: " ++ title book ++ "\nAutor: " ++ author book

updateBook :: Book -> String -> String -> Bool -> Book
updateBook book newTitle newAuthor newAvailability =
    book { title = newTitle, author = newAuthor, available = newAvailability }

deleteBook :: Book -> Book
deleteBook book = book { available = False }