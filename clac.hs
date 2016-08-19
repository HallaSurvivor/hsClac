-- A RPN calculator to be called from the command line,
-- based on 15-122's clac/exp project at CMU.
--
-- Calling with no input will open an interpreter,
-- while calling with a well formed expression
-- will simply print the result of the calculation.
--
-- Christopher Grossack, 2016

import System.Environment
import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Operations

runOperation :: String -> Stack -> Either String Stack
runOperation t s = 
  case safeRead t of
    Just n  -> Right (n:s)
    Nothing -> 
      case lookup t operations of
        Just op -> opToFunction op s
        Nothing -> Left $ "unknown token '" ++ t ++ "'"

opToFunction :: Operation -> Stack -> Either String Stack
opToFunction op s
  | length s < minSize op  = Left stackTooSmall
  | null $ requirements op = Right $ function op s
  | otherwise = 
    case s `failsRequirements` requirements op of
      Just e  -> Left e
      Nothing -> Right $ function op s
  where
    stackTooSmall = token op ++ " requires at least " ++ (show . minSize) op ++ " items on the stack."

failsRequirements :: Stack -> [Stack -> Maybe String] -> Maybe String
failsRequirements s rs = if null listOfFailures then Nothing else head listOfFailures
  where
    listOfFailures = filter isJust $ map ($s) rs

showStack :: Stack -> String
showStack s = "The stack is: " ++ open ++ val2 ++ val1 ++ "  ]  (size: " ++ (show . length) s ++ ")"
  where
    open = if length s <= 2 then "[  " else"[...  "
    val1 = if length s >= 1 then (show . head) s else ""
    val2 = if length s >= 2 then (show . head . tail) s ++ "   " else ""

safeRead :: String -> Maybe Double
safeRead str = 
  case reads str of
    [(n, "")] -> Just n
    _         -> Nothing

solve :: Stack -> String -> Either String Stack
solve s = foldM (flip runOperation) s . words

interpreter :: Stack -> IO ()
interpreter s = do
  putStrLn $ showStack s
  putStr "clac >> "
  hFlush stdout
  input <- getLine
  let stack' = solve s input
  case stack' of
    Right s -> interpreter s
    Left  e -> do
      putStrLn $ "error: " ++ e
      interpreter s

eval :: String -> String
eval exprs = case solve [] exprs of
            Right s -> show s
            Left  e  -> "error: " ++ e

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interpreter []
    exprs -> mapM_ (putStrLn . eval) exprs
