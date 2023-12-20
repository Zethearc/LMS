module Data.Library
    ( Library(..)
    , createLibrary
    , loadLibrary
    , saveLibrary
    ) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Book (Book)
import Data.Member (Member)
import Data.Transaction (Transaction)

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
    writeFile (bookDB library) ""
    writeFile (memberDB library) ""
    writeFile (transactionDB library) ""

-- Función auxiliar para escribir en un archivo solo si no existe
writeFileIfNotExists :: FilePath -> String -> IO ()
writeFileIfNotExists filePath content = do
    exists <- doesFileExist filePath
    if not exists
        then writeFile filePath content
        else return ()
