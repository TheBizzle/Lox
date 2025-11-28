module Lox.Scanner.Internal.Classify(isAlphabetic, isAlphanumeric, isDigit) where

isAlphabetic :: Char -> Bool
isAlphabetic c = isLowerCase || isUpperCase || (c == '_')
  where
    isLowerCase = c >= 'a' && c <= 'z'
    isUpperCase = c >= 'A' && c <= 'Z'

isAlphanumeric :: Char -> Bool
isAlphanumeric = (isAlphabetic &&& isDigit) &> (uncurry (||))

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
