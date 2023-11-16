module Data.Library
    ( Library
    ,createLibrary
    , removeBook
    ) where

import Data.Book
import Data.Member
import Utils

type Library = ([Book], [Member])

createLibrary :: Library
createLibrary = ([], [])

removeBook :: Int -> Library -> Library
removeBook bookIdToRemove (books, members) =
    let updatedBooks = filter (\book -> bookId book /= bookIdToRemove) books
    in (updatedBooks, members)
