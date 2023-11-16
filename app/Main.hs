-- app/Main.hs
module Main where

import System.IO
import System.Directory (doesFileExist)
import Data.Library
import Data.Member
import Data.Book
import Utils

main :: IO ()
main = do
    putStrLn "¡Bienvenido a la biblioteca!"
    library <- loadLibraryFromFile "library.txt"
    interactiveLibraryLoop library

-- Bucle interactivo
interactiveLibraryLoop :: Library -> IO ()
interactiveLibraryLoop library = do
    putStrLn "\nSelecciona una opción:"
    putStrLn "1. Mostrar biblioteca"
    putStrLn "2. Añadir libro"
    putStrLn "3. Añadir miembro"
    putStrLn "4. Guardar y salir"

    choice <- getLine

    case choice of
        "1" -> do
            putStrLn "Biblioteca actual:"
            printLibrary library
            interactiveLibraryLoop library

        "2" -> do
            putStrLn "Ingresa los detalles del nuevo libro:"
            newBook <- getNewBookInfo
            let updatedLibrary = addBook newBook $ fst library
            interactiveLibraryLoop (updatedLibrary, snd library)

        "3" -> do
            putStrLn "Ingresa los detalles del nuevo miembro:"
            newMember <- getNewMemberInfo
            let updatedLibrary = addMember newMember $ snd library
            interactiveLibraryLoop (fst library, updatedLibrary)

        "4" -> do
            saveLibraryToFile "library.txt" library
            putStrLn "¡Gracias por usar la biblioteca!"

        _ -> do
            putStrLn "Opción no válida. Por favor, selecciona una opción válida."
            interactiveLibraryLoop library

-- Función para imprimir la información de la biblioteca
printLibrary :: Library -> IO ()
printLibrary (books, members) = do
    putStrLn "Libros en la biblioteca:"
    mapM_ (putStrLn . readBook) books

    putStrLn "\nMiembros en la biblioteca:"
    mapM_ (putStrLn . show) members

-- Función para obtener información de un nuevo libro
getNewBookInfo :: IO Book
getNewBookInfo = do
    putStrLn "Ingrese el título del libro:"
    title <- getLine
    putStrLn "Ingrese el autor del libro:"
    author <- getLine
    putStrLn "¿El libro está disponible? (True/False):"
    availability <- readLn
    return $ createBook 0 title author availability

-- Función para cargar la biblioteca desde un archivo
loadLibraryFromFile :: FilePath -> IO Library
loadLibraryFromFile fileName = do
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            contents <- readFile fileName
            let library = read contents :: Library
            return library
        else do
            putStrLn "El archivo no existe. Creando una nueva biblioteca."
            return createLibrary

-- Función para guardar la biblioteca en un archivo
saveLibraryToFile :: FilePath -> Library -> IO ()
saveLibraryToFile fileName library = writeFile fileName (show library)
