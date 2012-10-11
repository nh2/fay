{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Convert a Haskell value to a (JSON representation of a) Fay value.

module Language.Fay.Convert
  (showToFay
  ,readFromFay)
  where

import           Control.Applicative

import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Number


import           Data.Char
import           Data.Data
import           Data.Function
import           Data.Generics.Aliases
import           Data.Generics.Text
import qualified Data.HashMap.Strict    as Map

import           Data.Maybe

import qualified Data.Text              as Text
import qualified Data.Vector            as Vector

import           Numeric
import           Safe
import qualified Text.Show.Pretty       as Show

--------------------------------------------------------------------------------
-- The conversion functions.

-- | Convert a Haskell value to a value representing a Fay value.
showToFay :: Show a => a -> Maybe Value
showToFay = Show.reify >=> convert where
  convert value = case value of
    -- Special cases
    Show.Con "True" _    -> return (Bool True)
    Show.Con "False" _   -> return (Bool False)

    -- Objects/records
    Show.Con name values -> fmap (Object . Map.fromList . (("instance",string name) :))
                                 (slots values)
    Show.Rec name fields -> fmap (Object . Map.fromList . (("instance",string name) :))
                                 (mapM (uncurry keyval) fields)

    -- List types
    Show.Tuple values -> fmap (Array . Vector.fromList) (mapM convert values)
    Show.List values  -> fmap (Array . Vector.fromList) (mapM convert values)

    -- Text types
    Show.String chars -> fmap string (readMay chars)
    Show.Char char    -> fmap (string.return) (readMay char)

    -- Numeric types (everything treated as a double)
    Show.Neg{}     -> double <|> int
    Show.Integer{} -> int
    Show.Float{}   -> double
    Show.Ratio{}   -> double
    where double = convertDouble value
          int = convertInt value

  -- Number converters
  convertDouble = fmap (Number . D) . parseDouble
  convertInt = fmap (Number . I) . parseInt

  -- Number parsers
  parseDouble :: Show.Value -> Maybe Double
  parseDouble value = case value of
    Show.Float str   -> getDouble str
    Show.Ratio x y   -> liftM2 (on (/) fromIntegral) (parseInt x) (parseInt y)
    Show.Neg str     -> fmap (* (-1)) (parseDouble str)
    _ -> Nothing
  parseInt value = case value of
    Show.Integer str -> getInt str
    Show.Neg str     -> fmap (* (-1)) (parseInt str)
    _ -> Nothing

  -- Number readers
  getDouble :: String -> Maybe Double
  getDouble = fmap fst . listToMaybe . readFloat
  getInt :: String -> Maybe Integer
  getInt = fmap fst . listToMaybe . readInt 10 isDigit charToInt
    where charToInt c = fromEnum c - fromEnum '0'

  -- Utilities
  string = String . Text.pack
  slots = zipWithM keyval (map (("slot"++).show) [1::Int ..])
  keyval key val = fmap (Text.pack key,) (convert val)

readFromFay :: Data a => Value -> Maybe a
readFromFay value = do
  skip
  `ext1R` parseArray value
  `extR` parseDouble value
  `extR` parseInt value
  `extR` parseBool value
  `extR` parseString value
  where skip = listToMaybe (map fst (gread ""))

-- | Parse a double.
parseDouble :: Value -> Maybe Double
parseDouble value = do
  number <- parseNumber value
  case number of
    D n -> return n
    _ -> Nothing

-- | Parse an int.
parseInt :: Value -> Maybe Int
parseInt value = do
  number <- parseNumber value
  case number of
    I n -> return (fromIntegral n)
    _ -> Nothing

-- | Parse a number.
parseNumber :: Value -> Maybe Number
parseNumber value =
  case value of
    Number n -> return n
    _ -> Nothing

-- | Parse a bool.
parseBool :: Value -> Maybe Bool
parseBool value =
  case value of
    Bool n -> return n
    _ -> Nothing

-- | Parse a string.
parseString :: Value -> Maybe String
parseString value =
  case value of
    String s -> return (Text.unpack s)
    _ -> Nothing

-- | Parse an array.
parseArray :: Data a => Value -> Maybe [a]
parseArray value =
  case value of
    Array xs -> mapM readFromFay (Vector.toList xs)
    _ -> Nothing

-- | Convert a value representing a Fay value to a Haskell value.
-- readFromFay :: (Data a,Read a) => Value -> Maybe a
-- readFromFay value = result where
--   result = (convert typ >=> readMay) value
-- --  result = (convert) value
--   convert typ v =
--     case v of
--       Object obj -> do
--         name <- Map.lookup "instance" obj >>= getText
--         fmap parens (readRecord typ name obj <|> readData typ name obj)
--       Array array -> do
--         elems <- mapM (convert typ) (Vector.toList array) -- TODO
--         return $ concat ["[",intercalate "," elems,"]"]

--   getText i = case i of
--     String s -> return s
--     _ -> Nothing

--   readData typ name obj = do
--     fields <- forM (zip assocs) $ \(_,v) -> do
--       cvalue <- convert typ v
--       return cvalue
--     return (intercalate " " (Text.unpack name : fields))
--       where assocs = sortBy (comparing fst)
--                             (filter ((/="instance").fst) (Map.toList obj))

--   readRecord typ name (Map.toList -> assocs) = go (dataTypeConstrs typ)
--       where go (cons:conses) = do
--               readConstructor typ name assocs cons <|> go conses
--             go [] = Nothing

--   readConstructor typ name assocs cons = do
--     let getField key =
--           case lookup key (map (first Text.unpack) assocs) of
--             Just v  -> return (key,v)
--             Nothing -> Nothing
--     fields <- forM (constrFields cons) $ \field -> do
--       (key,v) <- getField field
--       cvalue <- convert typ v
--       return (intercalate " " [key,"=",cvalue])
--     guard $ not $ null fields
--     return (Text.unpack name ++
--             if null fields
--                then ""
--                else " {" ++ intercalate ", " fields ++ "}")

--   typ = dataTypeOf $ resType result
--   resType :: Maybe a -> a
--   resType = undefined
--   parens x = "(" ++ x ++ ")"


data Foo = Foo { unFoo :: Integer }
    deriving (Eq, Show, Read, Typeable, Data)
data Bar = Bar Foo
    deriving (Eq, Show, Read, Typeable, Data)
data Baz = Baz Foo'
    deriving (Eq, Show, Read, Typeable, Data)
data Foo' = Foo' Integer
    deriving (Eq, Show, Read, Typeable, Data)


showReadFay :: (Show a, Data a) => a -> Maybe a
showReadFay a =
    case showToFay a of
      Nothing  -> Nothing
      (Just j) -> readFromFay j

-- works
foo_test =
    let foo = Foo 2
    in (showReadFay foo) == Just foo

-- fails, but shouldn't
bar_test =
    let bar = Bar (Foo 2)
    in (showReadFay bar) == Just bar

-- works
foo'_test =
    let foo = Foo' 2
    in (showReadFay foo) == Just foo

-- works
baz_test =
    let baz = Baz (Foo' 2)
    in (showReadFay baz) == Just baz