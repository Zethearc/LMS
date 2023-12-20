module Data.Library
    ( Library(..)
    , createLibrary
    , createNewLibrary
    , loadLibrary
    , saveLibrary
    , loadBooks
    , loadMembers
    , loadTransactions
    , saveBooks
    , saveMembers
    , saveTransactions
    ) where

import Control.Exception
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Book (Book(..))
import Data.Member (Member(..))
import Data.Transaction (Transaction(..))

data Library = Library
    { libraryName :: String
    , bookDB :: FilePath
    , memberDB :: FilePath
    , transactionDB :: FilePath
    } deriving (Show, Read)

-- Función para crear una nueva biblioteca
createLibrary :: String -> IO Library
createLibrary name = do
    let libraryDir = "./" ++ name ++ "/"
        bookDBFile = libraryDir ++ "BookDB.txt"
        memberDBFile = libraryDir ++ "MemberDB.txt"
        transactionDBFile = libraryDir ++ "TransactionDB.txt"
    createDirectoryIfMissing True libraryDir
    writeFileIfNotExists bookDBFile ""
    writeFileIfNotExists memberDBFile ""
    writeFileIfNotExists transactionDBFile ""
    return $ Library name bookDBFile memberDBFile transactionDBFile

-- Función para crear una nueva biblioteca
createNewLibrary :: IO ()
createNewLibrary = do
    putStrLn "Ingrese el nombre de la nueva biblioteca:"
    newLibraryName <- getLine
    let libraryNameWithPrefix = "Library-" ++ newLibraryName
    createLibrary libraryNameWithPrefix
    putStrLn $ "Se ha creado la biblioteca '" ++ libraryNameWithPrefix ++ "' correctamente."

-- Función para cargar una biblioteca existente
loadLibrary :: String -> IO (Maybe Library)
loadLibrary name = do
    let libraryDir = "./" ++ name ++ "/"
        bookDBFile = libraryDir ++ "BookDB.txt"
        memberDBFile = libraryDir ++ "MemberDB.txt"
        transactionDBFile = libraryDir ++ "TransactionDB.txt"
    exists <- and <$> mapM doesFileExist [bookDBFile, memberDBFile, transactionDBFile]
    if exists
        then return $ Just $ Library name bookDBFile memberDBFile transactionDBFile
        else return Nothing

-- Función para guardar una biblioteca
saveLibrary :: Library -> IO ()
saveLibrary library = do
    -- Puedes agregar aquí la lógica para salvar libros, miembros y transacciones si es necesario
    writeFile (bookDB library) ""
    writeFile (memberDB library) ""
    writeFile (transactionDB library) ""

-- Función para cargar libros desde la biblioteca
loadBooks :: Library -> IO [Book]
loadBooks library = do
    content <- readFile (bookDB library)
    return $ if null content then [] else map read $ lines content

-- Función para cargar miembros desde la biblioteca
loadMembers :: Library -> IO [Member]
loadMembers library = do
    content <- readFile (memberDB library)
    return $ if null content then [] else map read $ lines content

loadTransactions :: Library -> IO [Transaction]
loadTransactions library = do
    content <- readFile (transactionDB library)
    return $ if null content then [] else map read $ lines content
    
-- Función para guardar libros en la biblioteca
saveBooks :: Library -> [Book] -> IO ()
saveBooks library books = do
    let content = show books
    writeFile (bookDB library) content

-- Función para guardar miembros en la biblioteca
saveMembers :: Library -> [Member] -> IO ()
saveMembers library members = do
    let content = show members
    writeFile (memberDB library) content

-- Función para guardar transacciones en la biblioteca
saveTransactions :: Library -> [Transaction] -> IO ()
saveTransactions library transactions = do
    let content = show transactions
    writeFile (transactionDB library) content

-- Función auxiliar para escribir en un archivo solo si no existe
writeFileIfNotExists :: FilePath -> String -> IO ()
writeFileIfNotExists filePath content = do
    exists <- doesFileExist filePath
    if not exists
        then writeFile filePath content
        else return ()
