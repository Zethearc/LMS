{-# LANGUAGE LambdaCase #-}

module Main where

import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import Data.List (isPrefixOf)
import Data.Library

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
    -- Puedes agregar más acciones relacionadas con la biblioteca aquí
    -- Ejemplo: Menú para administrar libros, miembros, transacciones, etc.
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
            -- Puedes llamar a funciones específicas relacionadas con libros aquí
            showLibraryOptionsIntern library
        "2" -> do
            -- Lógica para gestionar miembros
            putStrLn "Gestionar Miembros"
            -- Puedes llamar a funciones específicas relacionadas con miembros aquí
            showLibraryOptionsIntern library
        "3" -> do
            -- Lógica para gestionar transacciones
            putStrLn "Gestionar Transacciones"
            -- Puedes llamar a funciones específicas relacionadas con transacciones aquí
            showLibraryOptionsIntern library
        "4" -> putStrLn "Saliendo del programa."
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            showLibraryOptionsIntern library

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
    
-- Función para crear una nueva biblioteca
createNewLibrary :: IO ()
createNewLibrary = do
    putStrLn "Ingrese el nombre de la nueva biblioteca:"
    newLibraryName <- getLine
    let libraryNameWithPrefix = "Library-" ++ newLibraryName
    createLibrary libraryNameWithPrefix
    putStrLn $ "Se ha creado la biblioteca '" ++ libraryNameWithPrefix ++ "' correctamente."
