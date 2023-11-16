-- Main.hs
module Main where

import Data.Library
import Data.Member
import Data.Book
import Utils

main :: IO ()
main = do
    putStrLn "¡Bienvenido a la biblioteca!"

    -- Crear la biblioteca
    let library = createLibrary

    -- Añadir libros y miembros
    let book1 = createBook 1 "Libro 1" "Autor 1" True
    let book2 = createBook 2 "Libro 2" "Autor 2" True

    -- Desestructurar la tupla para obtener la lista de libros
    let (books, members) = library

    -- Actualizar la lista de libros
    let updatedLibrary = addBook book1 $ addBook book2 books

    putStrLn "¡Gracias por usar la biblioteca!"
