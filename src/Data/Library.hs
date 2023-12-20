module Data.Library
    ( Library(..)
    , createLibrary
    , createNewLibrary
    , loadLibrary
    , saveLibrary
    , loadBooks
    , loadMembers
    , loadTransactions
    , addNewBook
    , removeBookFromDatabase
    ) where

import Control.Monad (guard)
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Book (Book(..))
import Data.Member (Member(..))
import Data.Transaction (Transaction(..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.IO.Error (isAlreadyInUseError)
    
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

-- Función para cargar transacciones desde la biblioteca
loadTransactions :: Library -> IO [Transaction]
loadTransactions library = do
    content <- readFile (transactionDB library)
    return $ if null content then [] else map read $ lines content

-- Función auxiliar para escribir en un archivo solo si no existe
writeFileIfNotExists :: FilePath -> String -> IO ()
writeFileIfNotExists filePath content = do
    exists <- doesFileExist filePath
    if not exists
        then writeFile filePath content
        else return ()

-- Lógica para agregar un nuevo libro
addNewBook :: Library -> IO ()
addNewBook library = do
    putStrLn "Ingrese los detalles del nuevo libro:"
    putStrLn "Título:"
    title <- getLine
    putStrLn "Autor:"
    author <- getLine
    putStrLn "Stock:"
    stock <- readLn :: IO Int
    putStrLn "Valor Inicial:"
    initialValue <- readLn :: IO Double

    -- Crea un nuevo libro con el siguiente ID disponible
    let newBook = Book
            { bookId = 0
            , title = title
            , author = author
            , available = True
            , stock = stock
            , borrower = Nothing
            , dueDate = Nothing
            , initialValue = initialValue
            }

    -- Llama a la función para agregar el nuevo libro a la base de datos
    addBookToDatabase library newBook

-- Función para agregar un nuevo libro a la base de datos
addBookToDatabase :: Library -> Book -> IO ()
addBookToDatabase library newBook = do
    result <- tryJust (guard . isAlreadyInUseError) $ withFile (bookDB library) AppendMode $ \handle ->
        hPutStrLn handle (show newBook)
    case result of
        Left _  -> putStrLn "Error: El archivo está en uso. No se pudo agregar el libro."
        Right _ -> putStrLn "Libro agregado con éxito."

removeBookFromDatabase :: Library -> Int -> IO ()
removeBookFromDatabase library bookIdToRemove = do
    content <- readFile (bookDB library)
    putStrLn "Contenido leído del archivo:"
    putStrLn content
    let books = map read $ lines content :: [Book]
        updatedLibrary = filter (\book -> bookId book /= bookIdToRemove) books
    putStrLn "Libro(s) antes de la eliminación:"
    print books
    putStrLn "Libro(s) después de la eliminación:"
    print updatedLibrary
    writeFile (bookDB library) (unlines $ map show updatedLibrary)
    putStrLn "Libro(s) guardado(s) en la base de datos."
