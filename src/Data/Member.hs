-- Data.Member.hs
module Data.Member
  ( Member(..)
  , MemberId
  , getNewMemberInfo
  , getUpdatedMemberInfo
  , getMemberId
  , addMember
  , getMemberById
  , updateMember
  , deleteMember
  , showMemberInfo
  , borrowBook
  , returnBook
  ) where

import Data.List (find)
import Data.Book (Book, bookId)  -- Asegúrate de importar bookId desde Data.Book

data Member = Member
    { memberId :: MemberId
    , memberName :: String
    , memberEmail :: String
    , borrowedBooks :: [Book]
    } deriving (Show, Read)

type MemberId = Int

getNewMemberInfo :: IO Member
getNewMemberInfo = do
  putStrLn "Ingrese el nombre del miembro:"
  name <- getLine
  putStrLn "Ingrese el correo electrónico del miembro:"
  email <- getLine
  return $ Member 0 name email []

getUpdatedMemberInfo :: Member -> IO Member
getUpdatedMemberInfo member = do
  putStrLn $ "Información actual del miembro:\n" ++ showMemberInfo member
  putStrLn "Ingrese el nuevo nombre del miembro:"
  name <- getLine
  putStrLn "Ingrese el nuevo correo electrónico del miembro:"
  email <- getLine
  return $ member { memberName = name, memberEmail = email }

getMemberId :: IO MemberId
getMemberId = do
  putStrLn "Ingrese el ID del miembro:"
  readLn

addMember :: Member -> [Member] -> [Member]
addMember = (:)

getMemberById :: MemberId -> [Member] -> Maybe Member
getMemberById targetId = find (\member -> memberId member == targetId)

updateMember :: MemberId -> Member -> [Member] -> [Member]
updateMember targetId updatedMember = map (\member -> if memberId member == targetId then updatedMember else member)

deleteMember :: MemberId -> [Member] -> [Member]
deleteMember targetId = filter (\member -> memberId member /= targetId)

showMemberInfo :: Member -> String
showMemberInfo member =
  "ID del Miembro: " ++ show (memberId member) ++ "\n" ++
  "Nombre: " ++ memberName member ++ "\n" ++
  "Correo Electrónico: " ++ memberEmail member ++ "\n" ++
  "Libros Prestados: " ++ show (borrowedBooks member) ++ "\n"

borrowBook :: Member -> Book -> Member
borrowBook member book = member { borrowedBooks = book : borrowedBooks member }

returnBook :: Member -> Book -> Member
returnBook member book = member { borrowedBooks = filter (\b -> Data.Book.bookId b /= Data.Book.bookId book) (borrowedBooks member) }
