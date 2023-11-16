-- En Utils.hs

module Utils
    ( findBook
    , findMember
    ) where

import Data.List (find)
import Data.Book
import Data.Member

findBook :: Int -> [Book] -> Maybe Book
findBook targetId = find (\book -> bookId book == targetId)

findMember :: MemberId -> [Member] -> Maybe Member
findMember targetId = find (\member -> memberId member == targetId)