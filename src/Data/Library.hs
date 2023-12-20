module Data.Library
    ( Library(..)
    , createLibrary
    , createNewLibrary
    , loadLibrary
    , saveLibrary
    , loadBooks
    , loadMembers
    , loadTransactions
    , addNewBooktoDatabaseBook
    , removeBookFromDatabaseBook
    , modifyBookFromDatabaseBook
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
import Data.List (find)

data Library = Library
    { libraryName :: String
    , bookDB :: FilePath
    , memberDB :: FilePath
    , transactionDB :: FilePath
    } deriving (Show, Read)

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

createNewLibrary :: IO ()
createNewLibrary = do
    putStrLn "Ingrese el nombre de la nueva biblioteca:"
    newLibraryName <- getLine
    let libraryNameWithPrefix = "Library-" ++ newLibraryName
    createLibrary libraryNameWithPrefix
    putStrLn $ "Se ha creado la biblioteca '" ++ libraryNameWithPrefix ++ "' correctamente."

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

saveLibrary :: Library -> IO ()
saveLibrary library = do
    writeFile (bookDB library) ""
    writeFile (memberDB library) ""
    writeFile (transactionDB library) ""

loadBooks :: Library -> IO [Book]
loadBooks library = do
    content <- readFile (bookDB library)
    return $ if null content then [] else map read $ lines content

loadMembers :: Library -> IO [Member]
loadMembers library = do
    content <- readFile (memberDB library)
    return $ if null content then [] else map read $ lines content

loadTransactions :: Library -> IO [Transaction]
loadTransactions library = do
    content <- readFile (transactionDB library)
    return $ if null content then [] else map read $ lines content

writeFileIfNotExists :: FilePath -> String -> IO ()
writeFileIfNotExists filePath content = do
    exists <- doesFileExist filePath
    if not exists
        then writeFile filePath content
        else return ()

addNewBooktoDatabaseBook :: Library -> IO ()
addNewBooktoDatabaseBook library = do
    putStrLn "Ingrese los detalles del nuevo libro:"
    putStrLn "ISBN:"
    isbn <- readLn :: IO Int
    putStrLn "Título:"
    title <- getLine
    putStrLn "Autor:"
    author <- getLine
    putStrLn "Stock:"
    stock <- readLn :: IO Int
    putStrLn "Valor Inicial:"
    initialValue <- readLn :: IO Double

    let newBook = Book
            { bookId = isbn
            , title = title
            , author = author
            , available = True
            , stock = stock
            , borrower = Nothing
            , dueDate = Nothing
            , initialValue = initialValue
            }

    addBookToDatabaseBook library newBook

addBookToDatabaseBook :: Library -> Book -> IO ()
addBookToDatabaseBook library newBook = do
    result <- tryJust (guard . isAlreadyInUseError) $ withFile (bookDB library) AppendMode $ \handle ->
        hPutStrLn handle (show newBook)
    case result of
        Left _  -> putStrLn "Error: El archivo está en uso. No se pudo agregar el libro."
        Right _ -> putStrLn "Libro agregado con éxito."

removeBookFromDatabaseBook :: Library -> Int -> IO ()
removeBookFromDatabaseBook library bookIdToRemove = do
    content <- readFile (bookDB library)
    putStrLn "Contenido leído del archivo:"
    putStrLn content
    let books = map read $ lines content :: [Book]
        bookToRemove = find (\book -> bookId book == bookIdToRemove) books
    case bookToRemove of
        Just bookToRemove' -> do
            let updatedLibrary = filter (\book -> bookId book /= bookIdToRemove) books
            putStrLn "Libro(s) antes de la eliminación:"
            print books
            putStrLn "Libro(s) después de la eliminación:"
            print updatedLibrary
            writeFile (bookDB library) (unlines $ map show updatedLibrary)
            putStrLn "Libro(s) guardado(s) en la base de datos."
        Nothing -> putStrLn $ "Error: No se encontró ningún libro con el ID " ++ show bookIdToRemove

modifyBookFromDatabaseBook :: Library -> Int -> IO ()
modifyBookFromDatabaseBook library bookIdToModify = do
    content <- readFile (bookDB library)
    putStrLn "Contenido leído del archivo:"
    putStrLn content
    let books = map read $ lines content :: [Book]
        bookToModify = find (\book -> bookId book == bookIdToModify) books
    case bookToModify of
        Just bookToModify' -> do
            putStrLn "Libro a modificar:"
            print bookToModify'
            putStrLn "Ingrese los nuevos detalles del libro:"

            putStrLn "Nuevo ISBN:"
            newISBN <- readLn :: IO Int
            putStrLn "Nuevo Título:"
            newTitle <- getLine
            putStrLn "Nuevo Autor:"
            newAuthor <- getLine
            putStrLn "Nuevo Stock:"
            newStock <- readLn :: IO Int
            putStrLn "Nuevo Valor Inicial:"
            newInitialValue <- readLn :: IO Double

            let modifiedBook = Book
                    { bookId = newISBN
                    , title = newTitle
                    , author = newAuthor
                    , available = available bookToModify'
                    , stock = newStock
                    , borrower = borrower bookToModify'
                    , dueDate = dueDate bookToModify'
                    , initialValue = newInitialValue
                    }

                updatedLibrary = map (\book -> if bookId book == bookIdToModify then modifiedBook else book) books

            putStrLn "Libro modificado:"
            print modifiedBook

            writeFile (bookDB library) (unlines $ map show updatedLibrary)
            putStrLn "Libro guardado en la base de datos."

        Nothing -> putStrLn $ "Error: No se encontró ningún libro con el ID " ++ show bookIdToModify
