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
    putStrLn "0. Salir"
    putStrLn "Ingrese el número o la letra correspondiente a la opción deseada:"
    handleLibraryOption

-- Función para manejar la opción seleccionada por el usuario
handleLibraryOption :: IO ()
handleLibraryOption = do
    option <- getLine
    case option of
        "0" -> putStrLn "Gracias por usar la aplicación. ¡Hasta luego!"
        "C" -> createNewLibrary >> showLibraryOptions
        _   -> do
            let index = read option :: Int
            libraries <- getExistingLibraries
            if index >= 1 && index <= length libraries
                then do
                    let selectedLibrary = libraries !! (index - 1)
                    putStrLn $ "Seleccionaste la biblioteca: " ++ selectedLibrary
                    maybeLibrary <- loadLibrary selectedLibrary
                    case maybeLibrary of
                        Just library -> handleLibraryActions library >> showLibraryOptions
                        Nothing      -> putStrLn "Error al cargar la biblioteca." >> showLibraryOptions
                else do
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
        "1" -> do
            -- Lógica para gestionar libros
            putStrLn "Gestionar Libros"
            books <- loadBooks library
            -- Mostrar los libros en la interfaz gráfica
            mapM_ (\(index, book) -> putStrLn $ show index ++ ". " ++ displayBook book) (zip [1..] books)     
        "2" -> do
            -- Lógica para gestionar miembros
            putStrLn "Gestionar Miembros"
            members <- loadMembers library
            -- Mostrar los miembros en la interfaz gráfica
            mapM_ (\(index, member) -> putStrLn $ show index ++ ". " ++ displayMember member) (zip [1..] members)
    
        "3" -> do
            -- Lógica para gestionar transacciones
            putStrLn "Gestionar Transacciones"
            transactions <- loadTransactions library
            -- Mostrar las transacciones en la interfaz gráfica
            mapM_ (\(index, transaction) -> putStrLn $ show index ++ ". " ++ displayTransaction transaction) (zip [1..] transactions)
            
        "4" -> putStrLn "Saliendo del programa."
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            showLibraryOptionsIntern library
