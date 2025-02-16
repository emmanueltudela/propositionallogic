module Auxiliary
( strip
) where

strip :: String -> String
strip str =
    reverse $ removeStartingSpaces (reverse $ removeStartingSpaces str)
    where removeStartingSpaces "" = ""
          removeStartingSpaces (c:t)
              | c == ' ' = removeStartingSpaces t
              | otherwise = c:t
