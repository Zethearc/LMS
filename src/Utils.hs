-- En el archivo Utils.hs

module Utils
  ( displayBook
  , displayMember
  , displayTransaction
  , getExistingLibraries
  , isLibraryDirectory
  , showOptions
  ) where

import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import Data.List (isPrefixOf)
import Data.Book (Book(..))
import Data.Member (Member(..))
import Data.Transaction (Transaction(..))

-- Función auxiliar para mostrar un libro de manera formateada
displayBook :: Book -> String
displayBook book = 
    "ID: " ++ show (bookId book) ++
    ", Título: " ++ title book ++
    ", Autor: " ++ author book ++
    ", Disponible: " ++ show (available book) ++
    ", Stock: " ++ show (stock book) ++
    ", Prestatario: " ++ show (borrower book) ++ 
    ", Fecha de devolución: " ++ show (dueDate book) ++
    ", Valor inicial: " ++ show (initialValue book)

-- Función auxiliar para mostrar un miembro de manera formateada
displayMember :: Member -> String
displayMember member =
    "ID: " ++ show (memberId member) ++
    ", Nombre: " ++ memberName member ++
    ", Correo electrónico: " ++ memberEmail member ++
    ", Libros prestados: " ++ show (length $ borrowedBooks member)

-- Función auxiliar para mostrar una transacción de manera formateada
displayTransaction :: Transaction -> String
displayTransaction transaction =
    case transaction of
        BorrowTransaction { transactionBorrower = member, borrowedBook = book, transactionDate = date } ->
            "Tipo: Préstamo" ++
            ", Miembro: " ++ memberName member ++
            ", Libro: " ++ title book ++
            ", Fecha de transacción: " ++ show date
        ReturnTransaction { returningMember = member, returnedBook = book, transactionDate = date } ->
            "Tipo: Devolución" ++
            ", Miembro: " ++ memberName member ++
            ", Libro: " ++ title book ++
            ", Fecha de transacción: " ++ show date

-- Función para obtener la lista de bibliotecas existentes
getExistingLibraries :: IO [String]
getExistingLibraries = do
    contents <- getDirectoryContents "./"
    let libraryNames = filter (`notElem` [".", ".."]) contents
    return $ filter isLibraryDirectory libraryNames

-- Función para determinar si un directorio es una biblioteca
isLibraryDirectory :: String -> Bool
isLibraryDirectory dir = "Library-" `isPrefixOf` dir

-- Función para mostrar opciones numeradas
showOptions :: Int -> [String] -> IO ()
showOptions _ [] = return ()
showOptions index (opt:opts) = do
    putStrLn $ show index ++ ". " ++ opt
    showOptions (index + 1) opts
