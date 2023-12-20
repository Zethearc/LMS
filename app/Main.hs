module Main where

import Data.Library
import Utils
import Data.Book (Book(..))
import Data.Member (Member(..))
import Data.Transaction (Transaction(..))

main :: IO ()
main = do
    putStrLn "¡Bienvenido a la aplicación de biblioteca!"
    showLibraryOptions

-- Función para mostrar las opciones de la biblioteca
showLibraryOptions :: IO ()
showLibraryOptions = do
    putStrLn "Librerías existentes:"
    libraryNames <- getExistingLibraries
    showOptions 1 libraryNames
    putStrLn "C. Crear una nueva librería"
    putStrLn "D. Eliminar una biblioteca"
    putStrLn "M. Modificar una biblioteca"
    putStrLn "0. Salir"
    putStrLn "Ingrese el número o la letra correspondiente a la opción deseada:"
    handleLibraryOption

-- Función para manejar la opción seleccionada por el usuario
handleLibraryOption :: IO ()
handleLibraryOption = do
    option <- getLine
    libraries <- getExistingLibraries

    case option of
        "0" -> putStrLn "Gracias por usar la aplicación. ¡Hasta luego!"
        "C" -> do
            putStrLn "Ingrese el nombre de la nueva biblioteca:"
            newLibraryName <- getLine
            let libraryNameWithPrefix = "Library-" ++ newLibraryName
            createLibrary libraryNameWithPrefix
            putStrLn $ "Se ha creado la biblioteca '" ++ libraryNameWithPrefix ++ "' correctamente."
            showLibraryOptions

        "D" -> do
            putStrLn "Ingrese el nombre de la biblioteca que desea eliminar:"
            libraryToDelete <- getLine
            maybeLibrary <- loadLibrary libraryToDelete
            handleLibraryDeletion maybeLibrary
            showLibraryOptions

        "M" -> do
            putStrLn "Ingrese el nombre de la biblioteca que desea modificar:"
            libraryToModify <- getLine
            putStrLn "Ingrese el nuevo nombre para la biblioteca:"
            newLibraryName <- getLine
            maybeLibrary <- loadLibrary libraryToModify
            handleLibraryModification maybeLibrary newLibraryName
            showLibraryOptions

        _   -> handleLibrarySelection option libraries

handleLibraryActions :: Maybe Library -> IO ()
handleLibraryActions maybeLibrary =
    case maybeLibrary of
        Just library -> do
            putStrLn $ "Trabajando en la biblioteca '" ++ libraryName library ++ "'."
            showLibraryOptionsIntern library
        Nothing      -> do
            putStrLn "Error: Biblioteca no encontrada."
            showLibraryOptions
        

-- Funciones auxiliares para manejar la eliminación y modificación de bibliotecas
handleLibraryDeletion :: Maybe Library -> IO ()
handleLibraryDeletion maybeLibrary =
    case maybeLibrary of
        Just library -> deleteLibrary library >> putStrLn "Biblioteca eliminada exitosamente."
        Nothing      -> putStrLn "Error: Biblioteca no encontrada."

handleLibraryModification :: Maybe Library -> String -> IO ()
handleLibraryModification maybeLibrary newLibraryName =
    case maybeLibrary of
        Just library -> modifyLibraryName library newLibraryName >> putStrLn "Nombre de la biblioteca modificado exitosamente."
        Nothing      -> putStrLn "Error: Biblioteca no encontrada."

handleLibrarySelection :: String -> [String] -> IO ()
handleLibrarySelection option libraries = do
    let index = read option :: Int
    if index >= 1 && index <= length libraries
        then do
            let selectedLibrary = libraries !! (index - 1)
            putStrLn $ "Seleccionaste la biblioteca: " ++ selectedLibrary
            maybeLibrary <- loadLibrary selectedLibrary
            handleLibraryActions maybeLibrary >> showLibraryOptions
        else do
            putStrLn "Opción no válida."
            showLibraryOptions

-- Función para mostrar las opciones disponibles y gestionar la entrada del usuario
showLibraryOptionsIntern :: Library -> IO ()
showLibraryOptionsIntern library = do
    putStrLn "Opciones disponibles:"
    putStrLn "1. Libros"
    putStrLn "2. Miembros"
    putStrLn "3. Transacciones"
    putStrLn "4. Salir"
    putStr "Seleccione una opción: "
    option <- getLine
    case option of
        "1" -> handleCategoryOptions library "Libros"
        "2" -> handleCategoryOptions library "Miembros"
        "3" -> handleCategoryOptions library "Transacciones"
        "4" -> putStrLn "Saliendo del programa."
        _   -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            showLibraryOptionsIntern library

handleCategoryOptions :: Library -> String -> IO ()
handleCategoryOptions library category = do
    putStrLn $ "Estás trabajando en la categoría '" ++ category ++ "'."
    putStrLn "Opciones disponibles:"
    putStrLn "1. Ingresar"
    putStrLn "2. Eliminar"
    putStrLn "3. Modificar"
    putStrLn "4. Atras"
    putStr "Seleccione una opción: "
    option <- getLine
    case option of
        "1" -> do
            -- Lógica para Ingresar en la categoría
            case category of
                "Libros" -> do
                    books <- loadBooks library
                    mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] books)
                    addNewBookToDatabase library 
                    handleCategoryOptions library category
                "Miembros" -> do
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    addNewMemberToDatabase library
                    handleCategoryOptions library category
                "Transacciones" -> do
                    books <- loadBooks library
                    mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] books)
                    putStrLn "Ingrese el ID del libro que desea prestar:"
                    bookIdToBorrow <- readLn :: IO Int
                
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    putStrLn "Ingrese el ID del miembro que desea realizar el préstamo:"
                    memberId <- readLn :: IO Int
                
                    borrowBook library memberId bookIdToBorrow
                    handleCategoryOptions library category
                    
                _ -> putStrLn "Categoría no válida."
        
        "2" -> do
            -- Lógica para Eliminar en la categoría
            case category of
                "Libros" -> do
                    books <- loadBooks library
                    mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] books)
                    putStrLn "Ingrese el ID del libro que desea eliminar:"
                    bookIdToRemove <- readLn :: IO Int
                    removeBookFromDatabase library bookIdToRemove
                "Miembros" -> do
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    putStrLn "Ingrese el ID del miembro que desea eliminar:"
                    memberIdToRemove <- readLn :: IO Int
                    removeMemberFromDatabase library memberIdToRemove
                "Transacciones" -> do
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    putStrLn "Ingrese el ID del miembro que desea devolver el libro préstamo:"
                    memberId <- readLn :: IO Int
                    returnBook library memberId 
                _ -> putStrLn "Categoría no válida."
            
        "3" -> do
            -- Lógica para Modificar en la categoría
            case category of
                "Libros" -> do
                    books <- loadBooks library
                    mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] books)
                    putStrLn "Ingrese el ID del libro que desea modificar:"
                    bookIdToModify <- readLn :: IO Int
                    modifyBookInDatabase library bookIdToModify
                    handleCategoryOptions library category
    
                "Miembros" -> do
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    putStrLn "Ingrese el ID del miembro que desea modificar:"
                    memberIdToModify <- readLn :: IO Int
                    modifyMemberInDatabase library memberIdToModify
                    handleCategoryOptions library category
                    
                "Transacciones" -> do

                    handleCategoryOptions library category
                _ -> putStrLn "Categoría no válida."
            
        "4" -> showLibraryOptionsIntern library
        _   -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            handleCategoryOptions library category
