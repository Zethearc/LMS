module Data.Book
    ( Book (..)         -- Exporta todos los constructores de datos del tipo Book
    , addBook
    , createBook        -- Función para crear un nuevo libro
    , readBook          -- Función para mostrar la información de un libro
    , updateBook        -- Función para actualizar la información de un libro
    , deleteBook        -- Función para marcar un libro como no disponible
    ) where

data Book = Book
    { bookId :: Int      -- Identificador único del libro
    , title :: String    -- Título del libro
    , author :: String   -- Autor del libro
    , available :: Bool  -- Indica si el libro está disponible
    } deriving (Show)

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