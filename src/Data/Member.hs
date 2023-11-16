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
  ) where

import Data.List (find)
import Data.Book (Book)

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

getUpdatedMemberInfo :: Maybe Member -> IO Member
getUpdatedMemberInfo maybeMember = do
  case maybeMember of
    Just member -> do
      putStrLn $ "Información actual del miembro:\n" ++ show member
      putStrLn "Ingrese el nuevo nombre del miembro:"
      name <- getLine
      putStrLn "Ingrese el nuevo correo electrónico del miembro:"
      email <- getLine
      return $ member { memberName = name, memberEmail = email }
    Nothing -> do
      putStrLn "Miembro no encontrado."
      getNewMemberInfo

getMemberId :: IO MemberId
getMemberId = do
  putStrLn "Ingrese el ID del miembro:"
  readLn

addMember :: Member -> [Member] -> [Member]
addMember member members = member : members

getMemberById :: MemberId -> [Member] -> Maybe Member
getMemberById targetId = find (\member -> memberId member == targetId)

updateMember :: MemberId -> Member -> [Member] -> [Member]
updateMember targetId updatedMember = map (\member -> if memberId member == targetId then updatedMember else member)

deleteMember :: MemberId -> [Member] -> [Member]
deleteMember targetId = filter (\member -> memberId member /= targetId)