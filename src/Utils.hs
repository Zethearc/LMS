-- En el archivo Utils.hs

module Utils
  ( displayBook
  , displayMember
  , displayTransaction
  , getExistingLibraries
  , isLibraryDirectory
  , showOptions
  , writeFileIfNotExists
  ) where

import Control.Exception (catch, tryJust, IOException)
import Control.Monad (guard, unless)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, getDirectoryContents)
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

displayTransaction :: Transaction -> String
displayTransaction transaction =
    ", Miembro: " ++ displayMember (transactionMember transaction) ++
    ", Libro: " ++ displayBook (transactionBook transaction) ++
    ", Fecha: " ++ show (transactionDate transaction)


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

writeFileIfNotExists :: FilePath -> String -> IO ()
writeFileIfNotExists filePath content = do
    exists <- doesFileExist filePath
    unless exists $ writeFile filePath content
    