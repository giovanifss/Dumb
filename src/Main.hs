module Main where

import System.Exit (die)
import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.List (foldl', intercalate)
import System.Environment (getArgs)
import Data.Either.Unwrap (whenRight)
import Network.DNS (makeResolvSeed, defaultResolvConf, withResolver, lookupA, Domain)
import Control.Parallel.Strategies (parMap, rpar)
import qualified Control.Monad.Parallel as MP (mapM_)
import qualified Data.Text as DT (breakOnAll, pack)
import qualified Data.ByteString.Char8 as BS (readFile, lines, null, concat, drop, length, breakSubstring, pack, unpack)

getWordlist :: FilePath -> IO [ByteString]
getWordlist filename = do
  contentOrException <- try $ BS.readFile filename :: IO (Either IOError ByteString)
  case contentOrException of
    Left exception -> die (show exception)
    Right contents -> return (BS.lines contents)

getAllWordlists :: [[ByteString]] -> [FilePath] -> IO [[ByteString]]
getAllWordlists wordlists [] = do
  return wordlists
getAllWordlists wordlists [x] = do
  wordlist <- getWordlist x
  return (wordlist:wordlists)
getAllWordlists wordlists (x:xs) = do
  head <- getAllWordlists wordlists [x]
  tail <- getAllWordlists [] xs
  return (head ++ tail)

replaceFirst :: ByteString -> ByteString -> ByteString -> ByteString
replaceFirst mask substitution string
  | BS.null tail = string
  | otherwise = BS.concat [front, substitution, BS.drop (BS.length mask) tail]
  where (front, tail) = BS.breakSubstring mask string

generateDomain :: ByteString -> [ByteString] -> [ByteString]
generateDomain domain wordlist =
  parMap rpar (\word -> replaceFirst mask word domain) wordlist
    where mask = BS.pack "DUMB"

generateAllDomains :: ByteString -> [[ByteString]] -> [ByteString]
generateAllDomains domain [x] = generateDomain domain x
generateAllDomains domain (x:xs) = concat $ parMap rpar (\dom -> generateAllDomains dom xs) (generateDomain domain x)

countSubStrs :: String -> String -> Int
countSubStrs domain string = length $ DT.breakOnAll (DT.pack domain) (DT.pack string)

validateDomain :: String -> String -> (ByteString, Int)
validateDomain mask domain =
  if maskCount > 0 then (BS.pack domain, maskCount) else (BS.pack ("DUMB." ++ domain), maskCount)
    where maskCount = countSubStrs mask domain

getIPs :: Show a => [a] -> String
getIPs [x] = show x
getIPs (x:xs) = (getIPs xs) ++ "," ++ (getIPs [x])

printAll :: Show a => ByteString -> [a] -> IO ()
printAll sub [] = return ()
printAll sub lst = putStrLn $ "[+] " ++ (BS.unpack sub) ++ " <=> [" ++ (getIPs lst) ++ "]"

--resolve :: Domain -> IO ()
resolve rs domain = do
  result <- withResolver rs $ \resolver -> lookupA resolver domain
  whenRight (result) (printAll domain)

extractFirst :: [String] -> (String, [String])
extractFirst [] = ([],[])
extractFirst [x] = (x,[])
extractFirst (x:xs) = (x,xs)

checkWordlists :: Int -> Int -> IO ()
checkWordlists args mask
  | args > mask = die "[-] Extra wordlist passed, check your domain mask"
  | args < mask = die "[-] Missing wordlist"
  | otherwise = return ()

main :: IO ()
main = do
  args <- getArgs
  let (first, filenames) = extractFirst args
      (domain, wordlistCount) = validateDomain "DUMB" first
  checkWordlists (length filenames) wordlistCount
  putStrLn $ "Reading wordlists: '" ++ (intercalate "', '" filenames) ++ "'"
  wordlists <- getAllWordlists [] filenames
  putStrLn $ "Generating dumains for bruteforce based on mask '" ++ (BS.unpack domain)
  let allDomains = generateAllDomains domain wordlists
  putStrLn $ "Starting bruteforce in " ++ (show (length allDomains)) ++ " dumains..."
  rs <- makeResolvSeed defaultResolvConf
  MP.mapM_ (resolve rs) allDomains
  return ()
