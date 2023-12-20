module Data.Library
    ( Library(..)
    , createLibrary
    , createNewLibrary
    , loadLibrary
    , saveLibrary
    , loadBooks
    , loadMembers
    , loadTransactions
    , addNewBookToDatabase
    , removeBookFromDatabase
    , modifyBookInDatabase
    , addNewMemberToDatabase
    , removeMemberFromDatabase
    , modifyMemberInDatabase
    , deleteLibrary
    , modifyLibraryName
    , borrowBook
    , returnBook
    ) where


import Data.Time.Clock (getCurrentTime)
import Control.Exception (catch, tryJust, IOException)
import Control.Monad (guard, unless)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, renameDirectory)
import System.IO.Error (isAlreadyInUseError)
import Data.Book (decreaseStock, decreaseBookStock, increaseStock, increaseBookStock, Book(..)) 
import Data.Member (addBorrowedBook , updateMembers, returnBorrowedBook, Member(..), MemberId)  
import Data.Transaction (Transaction(..))
import Data.List (find)
import Text.Read (readMaybe)
import Utils

data Library = Library
    { libraryName :: String
    , bookDB :: FilePath
    , memberDB :: FilePath
    , transactionDB :: FilePath
    } deriving (Show, Read)

-- Crear y cargar bibliotecas
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
    putStrLn $ "¡Se ha creado la biblioteca '" ++ libraryNameWithPrefix ++ "' correctamente!"

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

-- Eliminar y modificar bibliotecas
deleteLibrary :: Library -> IO ()
deleteLibrary library = do
    removeDirectoryRecursive $ libraryName library
    putStrLn $ "¡La biblioteca '" ++ libraryName library ++ "' ha sido eliminada correctamente!"

modifyLibraryName :: Library -> String -> IO ()
modifyLibraryName library newLibraryName = do
    let newLibraryDir = "./" ++ newLibraryName ++ "/"
    catch (renameDirectory (libraryName library) newLibraryDir)
        (\e -> putStrLn $ "Error al modificar el nombre de la biblioteca: " ++ show (e :: IOException))
    putStrLn $ "¡El nombre de la biblioteca ha sido modificado a '" ++ newLibraryName ++ "' correctamente!"

-- Cargar libros, miembros y transacciones
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

-- Funciones relacionadas con libros
addNewBookToDatabase :: Library -> IO ()
addNewBookToDatabase library = do
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

    addBookToDatabase library newBook

addBookToDatabase :: Library -> Book -> IO ()
addBookToDatabase library newBook = do
    result <- tryJust (guard . isAlreadyInUseError) $ appendFile (bookDB library) (show newBook ++ "\n")
    case result of
        Left _  -> putStrLn "Error: El archivo está en uso. No se pudo agregar el libro."
        Right _ -> putStrLn "¡Libro agregado correctamente a la base de datos!"

removeBookFromDatabase :: Library -> Int -> IO ()
removeBookFromDatabase library bookIdToRemove = do
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
            putStrLn "¡Libro(s) eliminado(s) correctamente de la base de datos!"
        Nothing -> putStrLn $ "Error: No se encontró ningún libro con el ID " ++ show bookIdToRemove

modifyBookInDatabase :: Library -> Int -> IO ()
modifyBookInDatabase library bookIdToModify = do
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

            putStrLn "¡Libro modificado correctamente!"
            print modifiedBook
            writeFile (bookDB library) (unlines $ map show updatedLibrary)
        Nothing -> putStrLn $ "Error: No se encontró ningún libro con el ID " ++ show bookIdToModify

-- Funciones relacionadas con miembros
addNewMemberToDatabase :: Library -> IO ()
addNewMemberToDatabase library = do
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

    addMemberToDatabase library newMember

addMemberToDatabase :: Library -> Member -> IO ()
addMemberToDatabase library newMember = do
    result <- tryJust (guard . isAlreadyInUseError) $ appendFile (memberDB library) (show newMember ++ "\n")
    case result of
        Left _  -> putStrLn "Error: El archivo está en uso. No se pudo agregar el miembro."
        Right _ -> putStrLn "¡Miembro agregado correctamente a la base de datos!"

removeMemberFromDatabase :: Library -> Int -> IO ()
removeMemberFromDatabase library memberIdToRemove = do
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
            putStrLn "¡Miembro(s) eliminado(s) correctamente de la base de datos!"
        Nothing -> putStrLn $ "Error: No se encontró ningún miembro con el ID " ++ show memberIdToRemove

modifyMemberInDatabase :: Library -> Int -> IO ()
modifyMemberInDatabase library memberIdToModify = do
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

            putStrLn "¡Miembro modificado correctamente!"
            print modifiedMember
            writeFile (memberDB library) (unlines $ map show updatedLibrary)
        Nothing -> putStrLn $ "Error: No se encontró ningún miembro con el ID " ++ show memberIdToModify

borrowBook :: Library -> Int -> Int -> IO ()
borrowBook library memberId bookIdToBorrow = do
    members <- loadMembers library
    books <- loadBooks library

    putStrLn "Miembros cargados:"
    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)

    putStrLn "\nLibros cargados:"
    mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] books)

    -- Obtener el miembro y el libro correspondientes
    let maybeMember = find (\member -> memberId == memberId) members
    let maybeBook = find (\b -> bookIdToBorrow == bookId b) books

    case (maybeMember, maybeBook) of
        (Just member, Just book) -> do
            -- Verificar en la base de datos si hay suficiente stock
            case find (\dbBook -> bookIdToBorrow == bookId dbBook) books of
                Just dbBook ->
                    if stock dbBook > 0
                        then do
                            -- Lógica de préstamo
                            let updatedMember = addBorrowedBook books member bookIdToBorrow
                            let updatedMembers = updateMembers members updatedMember
                            saveMembers library updatedMembers
                            saveBooks library (decreaseStock books bookIdToBorrow)
                            putStrLn "¡Préstamo realizado con éxito!"
                        else putStrLn "Error: No hay unidades disponibles en stock para este libro."
                Nothing -> putStrLn "Error: Libro no encontrado en la base de datos."

        _ -> putStrLn "Error: Miembro o libro no encontrado."

saveMembers :: Library -> [Member] -> IO ()
saveMembers library members =
    writeFile (memberDB library) (unlines $ map show members)

saveBooks :: Library -> [Book] -> IO ()
saveBooks library books =
    writeFile (bookDB library) (unlines $ map show books)

returnBook :: Library -> Int -> IO ()
returnBook library memberId = do
    members <- loadMembers library
    books <- loadBooks library

    putStrLn "Miembros cargados:"
    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)

    -- Obtener el miembro correspondiente
    let maybeMember = find (\member -> memberId == memberId) members

    case maybeMember of
        Just member -> do
            -- Verificar si el miembro tiene libros prestados
            if not (null (borrowedBooks member))
                then do
                    putStrLn "Libros prestados:"
                    mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] (borrowedBooks member))

                    -- Obtener el bookIdToReturn del usuario
                    putStrLn "\nIngrese el bookId del libro a devolver:"
                    bookIdToReturnStr <- getLine
                    let bookIdToReturn = readMaybe bookIdToReturnStr :: Maybe Int

                    case bookIdToReturn of
                        Just bId -> do
                            -- Verificar si el libro está en la lista de libros prestados
                            case find (\b -> bId == bookId b) (borrowedBooks member) of
                                Just selectedBook -> do
                                    let updatedMember = returnBorrowedBook member selectedBook
                                    let updatedMembers = updateMembers members updatedMember
                                    saveMembers library updatedMembers
                                    saveBooks library (increaseStock books bId)
                                    putStrLn "¡Devolución realizada con éxito!"
                                Nothing ->
                                    putStrLn "Error: El libro con el bookId ingresado no está en la lista de libros prestados."
                        Nothing ->
                            putStrLn "Error: Ingrese un número válido como bookId."

                else
                    putStrLn "El miembro no tiene libros prestados."
        Nothing ->
            putStrLn "Error: Miembro no encontrado."
