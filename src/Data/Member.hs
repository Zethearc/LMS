-- Data.Member.hs
module Data.Member
    ( Member(..)
    , MemberId

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