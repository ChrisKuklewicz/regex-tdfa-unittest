module Main where

import Control.Monad
import Control.Monad.Error()
import Data.Array
import Data.List
--import Data.Monoid
import Data.Sequence(Seq)
import Data.String
import Data.Typeable
import Data.Version()
import System.Environment
import Text.Regex.Base
import qualified Data.Foldable as F
import Paths_regex_tdfa_unittest(getDataFileName,version)

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Text.Regex.TDFA.Common as TDFA
import qualified Text.Regex.TDFA as TDFA

default(Int)

type RSource = String
type RType = String -- can be changed to any Extract instance
newtype RegexSource = RegexSource {unSource :: RSource} deriving Show
newtype RegexStringOf a = RegexString {unString :: a} deriving Show
type RegexString = RegexStringOf RType

dictionary :: [Char]
dictionary = ['a'..'c']++['A'..'C']++"_"


class StringIs a where toString :: a -> String
instance StringIs [Char] where toString = id
instance StringIs S.ByteString where toString = S.unpack
instance StringIs L.ByteString where toString = L.unpack
instance StringIs (Seq Char) where toString = F.toList

type A = Array Int (Int,Int)

maxItems :: Int
maxItems=100

testOne :: t -> (t -> t1 -> Array Int (Int, Int)) -> t1 -> String
testOne s op r = 
  let foo ::  String
      foo = concatMap (\(o,l) -> show (o,(o+l))) (take maxItems $ elems (op s r :: Array Int (Int,Int)))
  in if null foo then "NOMATCH" else foo

testOne' :: A -> String
testOne' input = 
  let foo ::  String
      foo = concatMap (\(o,l) -> show (o,(o+l))) (take maxItems $ elems input)
  in if null foo then "NOMATCH" else foo

toTest :: String -> (Int,String,String,String)
toTest line = let [n,regex,input,output] = words line
                  noQ [] = []
                  noQ ('?':xs) = '-':'1':noQ xs
                  noQ (x:xs) = x:noQ xs
                  input' = if input == "NULL" then "" else unN input
              in (read n,regex,input',noQ output)

toTest' :: String -> String -> (String,(Int,String,String,String))
toTest' oldRegex line =
  let [n,regex,input,output] = words line
      noQ [] = []
      noQ ('?':xs) = '-':'1':noQ xs
      noQ (x:xs) = x:noQ xs
      input' = if input == "NULL" then "" else input
      regex' = if regex == "SAME" then oldRegex else regex
  in (regex',(read n,regex',input',noQ output))

load,load' :: FilePath -> IO [(Int, String, String, String)]
load x' = do
  text <- readFile x'
  return $ map toTest (lines text)

load' x' = do
  text <- readFile x'
  return . snd $ mapAccumL toTest' "X_X_X_" (lines text)

checkTest :: PFT A -> (Int,String,String,String) -> IO [Int]
checkTest opM (n,regex,input,output) = do
  let output'e = opM input regex
      p = putStrLn
  p ""
  case output'e of
    Left msg -> do
      p ("############################# Unexpected Error # "++show n ++ " #############################" )
      p ("Searched text: "++show input)
      p ("Regex pattern: "++show regex)
      p ("Expected output: "++show output)
      p ("Error message: "++msg)
      return [n]
    Right output'a -> do
      let output' = testOne' output'a
      case (n<0 , output==output') of
        (False,True) -> p ("Expected Pass #"++show n)
        (False,False) -> p ("############################# Unexpected Fail # "++show n ++ " #############################" )
        (True,True) -> p ("############################# Unexpected Pass # "++show n ++ " #############################" )
        (True,False) ->  p ("Expected Fail #"++show n)
      if (output == output')
        then do p ("text and pattern: "++show input)
                p ("Regex pattern: "++show regex)
                p ("Outputs agree: "++show output)
                return (if n<0 then [n] else [])
        else do p ""
                p ("Searched text: "++show input)
                p ("Regex pattern: "++show regex)
                p ("Expected output: "++show output)
                p ("Actual result  : "++show output')
                return (if n<0 then [] else [n])

checkFile :: (RType -> RSource -> Either String A) -> FilePath -> IO (String,[Int])
checkFile opM dataFile = do
  filepath <- getDataFileName dataFile
  putStrLn $ "\nLoading Tests from: "++show filepath
  vals <- liftM concat (mapM (checkTest opM) =<< load' filepath)
  return (filepath,vals)

checkTests :: (RType -> RSource -> Either String A) -> IO [(String, [Int])]
checkTests opM = do
  dataFileList <- return . lines =<< readFile =<< getDataFileName "test-manifest.txt"
  mapM (checkFile opM) dataFileList

manifest :: String
manifest="test-manifest.txt"

dataFiles :: [String]
dataFiles=["basic3.txt","class.txt","right-assoc.txt","left-assoc.txt","forced-assoc.txt","nullsub3.txt","repetition2.txt","totest.txt"]

type PFT a = RegexContext TDFA.Regex RType a => RType -> RSource -> Either String a

posix :: PFT a
posix x reg =
  let q :: Either String TDFA.Regex
      q = makeRegexOptsM (defaultCompOpt { TDFA.caseSensitive = False}) defaultExecOpt reg
  in q >>= \ s -> return (match s x)

unN :: String -> String
unN ('\\':'n':xs) = '\n':unN xs
unN (x:xs) = x:unN xs
unN [] = []

manual :: [String] -> IO ()
manual [sIn,rIn] = do
  let s :: RType
      r :: String
      s = fromString (unN sIn)
      r = (unN rIn)
  -- first match
  let r1 :: TDFA.Regex
      r1 = makeRegex r
  let b1u@(_,_b1s,_,_)=(match r1 s :: (RType,RType,RType,[RType]))
  putStrLn ("Searched text: "++show s)
  putStrLn ("Regex pattern: "++show r)
  print b1u
  -- multiple matches and counting
  let b1 = (match r1 s :: [MatchArray])
      c1 = (match r1 s :: Int)
  putStrLn $ "Count of matches = "++show c1
  putStrLn $ "Matches found = "++show (length b1)
  mapM_ (putStrLn . testOne') b1
manual _ = error "wrong arguments to regex-posix-unittest's manual function"

main :: IO ()
main = do
  putStr "regex-tdfa-unittest version: "
  print version
  putStr "regex-tdfa-unittest testing Text.Regex.TDFA version: "
  print TDFA.getVersion_Text_Regex_TDFA
  manifestFile <- getDataFileName manifest
  a <- getArgs
  if length a == 2
    then manual a
    else do
      putStrLn $ "Explanation and discussion of these tests on the wiki at http://www.haskell.org/haskellwiki/Regex_Posix including comparing results from different operating systems"
      putStrLn $ "Questions about this package to the author at email <TextRegexLazy@personal.mightyreason.com>"
      putStrLn $ "The type of both the pattern and test is " ++ show (typeOf (undefined :: RType))
      putStrLn $ "Without extactly two arguments:"
      putStrLn $ "    This program runs all test files listed in the manifest at: "++show manifestFile
      putStrLn $ "    Lines with negative number are expected to fail, others are expected to pass."
      putStrLn $ "With exactly two arguments:"
      putStrLn $ "    The first argument is the text to be searched."
      putStrLn $ "    The second argument is the regular expression pattern to search with."
      vals <- checkTests posix
      if null (concatMap snd vals)
        then putStrLn "\nWow, all the tests passed!"
        else putStrLn $ "\nBoo, tests failed!\n"++unlines (map show vals)

{-
-- for TRE
posix x r = let q :: Posix.Regex
                q = makeRegexOpts (defaultCompOpt .|. Posix.compRightAssoc .|. Posix.compIgnoreCase) defaultExecOpt r
            in match q x

tdfa x r = let q :: TDFA.Wrap.Regex
               q = makeRegexOpts (defaultCompOpt { TDFA.Wrap.caseSensitive = False
                                                 , TDFA.Wrap.rightAssoc = True }) defaultExecOpt r
           in match q x

tdfa2 x r = let q :: TDFA2.Wrap.Regex
                q = makeRegexOpts (defaultCompOpt { TDFA2.Wrap.caseSensitive = False
                                                  , TDFA2.Wrap.rightAssoc = True }) defaultExecOpt r
            in match q x
-}
