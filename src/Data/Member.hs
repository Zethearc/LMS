module Data.Member
    ( Member(..)
    , MemberId
    , addBorrowedBook
    , updateMembers
    ) where

import Data.List (find)
import Data.Book (Book, bookId, available, borrower)  -- Asegúrate de importar bookId, available y borrower desde Data.Book

data Member = Member
    { memberId :: MemberId
    , memberName :: String
    , memberEmail :: String
    , borrowedBooks :: [Book]
    } deriving (Show, Read)

type MemberId = Int

addBorrowedBook :: [Book] -> Member -> Int -> Member
addBorrowedBook books member bookIdToBorrow =
    case find (\b -> bookIdToBorrow == bookId b) books of
        Just foundBook ->
            if available foundBook
                then
                    let updatedBook = foundBook { available = False, borrower = Just (show $ memberId member) }
                    in member { borrowedBooks = updatedBook : borrowedBooks member }
                else
                    member  -- El libro ya está prestado, no es necesario agregarlo nuevamente
        Nothing ->
            member  -- El libro no fue encontrado, no es necesario agregarlo

updateMembers :: [Member] -> Member -> [Member]
updateMembers members updatedMember =
    map (\m -> if memberId m == memberId updatedMember then updatedMember else m) members
