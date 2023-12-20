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
    , addNewMembertoDatabaseMember
    , removeMemberFromDatabaseMember
    , modifyMemberFromDatabaseMember
    , deleteLibrary  -- Agregado
    , modifyLibraryName  -- Agregado
    ) where

import Control.Exception (catch, tryJust, IOException)
import Control.Monad (guard, unless)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, renameDirectory)
import System.IO.Error (isAlreadyInUseError)
import Data.Book (Book(..))
import Data.Member (Member(..))
import Data.Transaction (Transaction(..))
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

-- Función para eliminar una biblioteca con todos sus elementos
deleteLibrary :: Library -> IO ()
deleteLibrary library = do
    removeDirectoryRecursive $ libraryName library
    putStrLn $ "La biblioteca '" ++ libraryName library ++ "' ha sido eliminada."

-- Función para modificar el nombre del directorio de la biblioteca
modifyLibraryName :: Library -> String -> IO ()
modifyLibraryName library newLibraryName = do
    let newLibraryDir = "./" ++ newLibraryName ++ "/"
    catch (renameDirectory (libraryName library) newLibraryDir)
        (\e -> putStrLn $ "Error al modificar el nombre de la biblioteca: " ++ show (e :: IOException))
    putStrLn $ "El nombre de la biblioteca ha sido modificado a '" ++ newLibraryName ++ "'."

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
    unless exists $ writeFile filePath content

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
    result <- tryJust (guard . isAlreadyInUseError) $ appendFile (bookDB library) (show newBook ++ "\n")
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

addNewMembertoDatabaseMember :: Library -> IO ()
addNewMembertoDatabaseMember library = do
    putStrLn "Ingrese los detalles del nuevo miembro:"
    putStrLn "ID del Miembro:"
    memberId <- readLn :: IO Int
    putStrLn "Nombre del Miembro:"
    memberName <- getLine
    putStrLn "Correo Electrónico del Miembro:"
    memberEmail <- getLine

    let newMember = Member
            { memberId = memberId
            , memberName = memberName
            , memberEmail = memberEmail
            , borrowedBooks = []
            }

    addMemberToDatabaseMember library newMember

addMemberToDatabaseMember :: Library -> Member -> IO ()
addMemberToDatabaseMember library newMember = do
    result <- tryJust (guard . isAlreadyInUseError) $ appendFile (memberDB library) (show newMember ++ "\n")
    case result of
        Left _  -> putStrLn "Error: El archivo está en uso. No se pudo agregar el miembro."
        Right _ -> putStrLn "Miembro agregado con éxito."

removeMemberFromDatabaseMember :: Library -> Int -> IO ()
removeMemberFromDatabaseMember library memberIdToRemove = do
    content <- readFile (memberDB library)
    putStrLn "Contenido leído del archivo:"
    putStrLn content
    let members = map read $ lines content :: [Member]
        memberToRemove = find (\member -> memberId member == memberIdToRemove) members
    case memberToRemove of
        Just memberToRemove' -> do
            let updatedLibrary = filter (\member -> memberId member /= memberIdToRemove) members
            putStrLn "Miembro(s) antes de la eliminación:"
            print members
            putStrLn "Miembro(s) después de la eliminación:"
            print updatedLibrary
            writeFile (memberDB library) (unlines $ map show updatedLibrary)
            putStrLn "Miembro(s) guardado(s) en la base de datos."
        Nothing -> putStrLn $ "Error: No se encontró ningún miembro con el ID " ++ show memberIdToRemove

modifyMemberFromDatabaseMember :: Library -> Int -> IO ()
modifyMemberFromDatabaseMember library memberIdToModify = do
    content <- readFile (memberDB library)
    putStrLn "Contenido leído del archivo:"
    putStrLn content
    let members = map read $ lines content :: [Member]
        memberToModify = find (\member -> memberId member == memberIdToModify) members
    case memberToModify of
        Just memberToModify' -> do
            putStrLn "Miembro a modificar:"
            print memberToModify'
            putStrLn "Ingrese los nuevos detalles del miembro:"

            putStrLn "Nuevo ID del Miembro:"
            newMemberId <- readLn :: IO Int
            putStrLn "Nuevo Nombre del Miembro:"
            newMemberName <- getLine
            putStrLn "Nuevo Correo Electrónico del Miembro:"
            newMemberEmail <- getLine

            let modifiedMember = Member
                    { memberId = newMemberId
                    , memberName = newMemberName
                    , memberEmail = newMemberEmail
                    , borrowedBooks = borrowedBooks memberToModify'
                    }

                updatedLibrary = map (\member -> if memberId member == memberIdToModify then modifiedMember else member) members

            putStrLn "Miembro modificado:"
            print modifiedMember

            writeFile (memberDB library) (unlines $ map show updatedLibrary)
            putStrLn "Miembro guardado en la base de datos."

        Nothing -> putStrLn $ "Error: No se encontró ningún miembro con el ID " ++ show memberIdToModify