module AoC where

import Data.Bifunctor       (first)
import Data.List            (unfoldr)
import System.Environment   (getArgs, getProgName)
import Control.Monad        (void)
import Text.Megaparsec      (Parsec, runParser, option, oneOf, some, empty, errorBundlePretty, (<|>))
import Data.Void            (Void)
import Text.Megaparsec.Char (char, digitChar, hspace1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser a = Parsec Void String a


applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)


groupsOf :: Int -> [a] -> [[a]]
groupsOf n = unfoldr (\l -> if null l then Nothing else Just (splitAt n l))


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b


thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c


intP :: Parser Int
intP =
    do
        sign <- option 1 ((char '-' >> return (-1)) <|> (char '+' >> return 1))
        num  <- read <$> some digitChar
        return (num * sign)


digit :: Parser Int
digit = read . (:[]) <$> digitChar


blankP :: Parser ()
blankP = void $ oneOf " \t"


blanksP :: Parser ()
blanksP = void $ some blankP


hspacec :: Parser ()
hspacec = L.space hspace1 empty empty

symbol :: String -> Parser String
symbol = L.symbol hspacec

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspacec

decimal :: Num a => Parser a
decimal = L.decimal

getParsedInput :: Parser a -> String -> IO (Maybe a)
getParsedInput parser file = do
    fileContents <- readFile file
    case runParser parser file fileContents of
        Right res -> return $ Just res
        Left  _   -> return Nothing


parseFromArg :: Parser a -> IO (Either String a)
parseFromArg parser = do
    args <- getArgs
    prog <- getProgName
    case args of
        [inputFile] ->
            do
                fileContent <- readFile inputFile
                return $ first errorBundlePretty $ runParser parser inputFile fileContent
        _ ->
            return $ Left $ "Use: "++prog++" input"


applyInputWith :: Parser a -> (a -> IO ()) -> IO ()
applyInputWith parser f =
    do
        parseRes <- parseFromArg parser
        case parseRes of
            Left err ->
                putStrLn err
            Right parsedRes ->
                f parsedRes


applyInput1 :: (Show b) => Parser a -> (a -> b) -> IO ()
applyInput1 parser solve = applyInputWith parser (print . solve)


applyInput :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> IO ()
applyInput parser solveP1 solveP2 =
    applyInputWith parser solveAndPrint
  where
    solveAndPrint input = do
        print $ solveP1 input
        print $ solveP2 input
