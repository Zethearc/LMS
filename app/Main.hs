{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Library
import Utils
import Data.Book (Book(..))
import Data.Member (Member(..))
import Data.Transaction (Transaction(..))

main :: IO ()
main = do
    putStrLn "Bienvenido a la aplicación de biblioteca."
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
    option <- getChar
    _ <- getChar -- Consumir el carácter de nueva línea
    case option of
        '0' -> putStrLn "Gracias por usar la aplicación. ¡Hasta luego!"
        'C' -> do
            putStrLn "Ingrese el nombre de la nueva biblioteca:"
            newLibraryName <- getLine
            let libraryNameWithPrefix = "Library-" ++ newLibraryName
            createLibrary libraryNameWithPrefix
            putStrLn $ "Se ha creado la biblioteca '" ++ libraryNameWithPrefix ++ "' correctamente."
            showLibraryOptions
        'D' -> do
            putStrLn "Ingrese el nombre de la biblioteca que desea eliminar:"
            libraryToDelete <- getLine
            maybeLibrary <- loadLibrary libraryToDelete
            case maybeLibrary of
                Just library -> deleteLibrary library
                Nothing      -> putStrLn "Error: Biblioteca no encontrada."
            showLibraryOptions
        'M' -> do
            putStrLn "Ingrese el nombre de la biblioteca que desea modificar:"
            libraryToModify <- getLine
            putStrLn "Ingrese el nuevo nombre para la biblioteca:"
            newLibraryName <- getLine
            maybeLibrary <- loadLibrary libraryToModify
            case maybeLibrary of
                Just library -> modifyLibraryName library newLibraryName
                Nothing      -> putStrLn "Error: Biblioteca no encontrada."
            showLibraryOptions
        _   -> do
            putStrLn "Opción no válida."
            showLibraryOptions


-- Función para manejar las acciones de la biblioteca seleccionada
handleLibraryActions :: Library -> IO ()
handleLibraryActions library = do
    putStrLn $ "Estás trabajando en la biblioteca '" ++ libraryName library ++ "'."
    showLibraryOptionsIntern library

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
                    addNewBooktoDatabaseBook library 
                    handleCategoryOptions library category
                "Miembros" -> do
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    addNewMembertoDatabaseMember library
                    handleCategoryOptions library category
                "Transacciones" -> do
                    transactions <- loadTransactions library
                    mapM_ (\(index, transaction) -> putStrLn $ show index ++ ". " ++ displayTransaction transaction) (zip [1..] transactions)
                    -- Agrega aquí la lógica para "Ingresar" en la categoría de "Transacciones" si es necesario
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
                    removeBookFromDatabaseBook library bookIdToRemove
                "Miembros" -> do
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    putStrLn "Ingrese el ID del miembro que desea eliminar:"
                    memberIdToRemove <- readLn :: IO Int
                    removeMemberFromDatabaseMember library memberIdToRemove
                "Transacciones" -> do
                    transactions <- loadTransactions library
                    mapM_ (\(index, transaction) -> putStrLn $ show index ++ ". " ++ displayTransaction transaction) (zip [1..] transactions)
                    -- Agrega aquí la lógica para "Eliminar" en la categoría de "Transacciones" si es necesario
                    handleCategoryOptions library category
                _ -> putStrLn "Categoría no válida."
            
        "3" -> do
            -- Lógica para Modificar en la categoría
            case category of
                "Libros" -> do
                    books <- loadBooks library
                    mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] books)
                    putStrLn "Ingrese el ID del libro que desea modificar:"
                    bookIdToModify <- readLn :: IO Int
                    modifyBookFromDatabaseBook library bookIdToModify
                    handleCategoryOptions library category
    
                "Miembros" -> do
                    members <- loadMembers library
                    mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
                    putStrLn "Ingrese el ID del miembro que desea modificar:"
                    memberIdToModify <- readLn :: IO Int
                    modifyMemberFromDatabaseMember library memberIdToModify
                    handleCategoryOptions library category
                    
                "Transacciones" -> do
                    transactions <- loadTransactions library
                    mapM_ (\(index, transaction) -> putStrLn $ show index ++ ". " ++ displayTransaction transaction) (zip [1..] transactions)
                    -- Agrega aquí la lógica para "Modificar" en la categoría de "Transacciones" si es necesario
                    handleCategoryOptions library category
                _ -> putStrLn "Categoría no válida."
            
        "4" -> showLibraryOptionsIntern library
        _   -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            handleCategoryOptions library category
