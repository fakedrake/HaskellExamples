{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Turn haskell types into start s-expressions.
-- eg. parse 1 => "1"
--     parse "Hello" => "\"hello\""
--     parse False => f
--     parse [1,2,3] => "(1 2 3 )"
--     parse [[1],[2],[3]] => "((1 ) (2 ) (3 ) )"

data EnchValue =
  EnchString [Char]
  | EnchNull
  | EnchBool Bool
  | EnchError String
  | EnchNum Integer
  | EnchChar Char
  | EnchArray [EnchValue]
  | EnchMap [(String, EnchValue)]
  deriving (Show, Read, Eq, Ord)

-- newtype EnchString = StartString { fromStartString::String }
--                    deriving (Show, Read, Eq, Ord)
appendToStartArray :: EnchValue -> EnchValue -> EnchValue
appendToStartArray (EnchChar c) (EnchString s) = EnchString (c:s)
appendToStartArray (EnchChar c) EnchNull = EnchString [c]
appendToStartArray v (EnchArray xs) = EnchArray (v:xs)
appendToStartArray v EnchNull = EnchArray [v]


toStart :: EnchValue -> String
toStart (EnchString s) = "\"" ++ s ++ "\""
toStart (EnchBool True) = "t"
toStart (EnchBool False) = "f"
toStart (EnchArray arr) = "(" ++ (concatMap (\x -> (toStart x) ++ " ") arr) ++ ")"
toStart (EnchChar c) = "'" ++ [c] ++ "'"
toStart (EnchNum i) = show i

showNatives arr = EnchArray $ map fromNative arr
-- Map to real types
class StartType st where
  fromNative :: st -> EnchValue

instance StartType EnchValue where
  fromNative = id

instance StartType Bool where
  fromNative = EnchBool

instance StartType Integer where
  fromNative = EnchNum

instance StartType Char where
  fromNative c = EnchChar c

instance StartType s => StartType [s] where
  fromNative [] = EnchNull
  fromNative (s:xs) = appendToStartArray (fromNative s) (fromNative xs)

parse :: StartType t => t -> String
parse = toStart . fromNative
