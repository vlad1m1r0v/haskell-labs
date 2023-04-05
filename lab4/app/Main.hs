module Main (main) where

import Lib
import System.Exit (exitSuccess)
import System.Environment
import System.IO
import Text.Read (readMaybe)
import Control.Monad (unless, when)
import Control.Exception (evaluate)
import Data.List (tails, isInfixOf, findIndex, isPrefixOf)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Prelude hiding (log)

-- свій парсер командого рядка
data Command
  = Create Shape
  | Read Int
  | Update Int Shape
  | Delete Int
  | Area Int
  | Type FigureType
  | Box Int
  | Contained Shape
  | Move Int Vector
  deriving (Show)

data Options = Options
  { db :: Maybe String,
    command :: Maybe Command,
    log :: Maybe FilePath,
    silent :: Bool,
    html :: Maybe FilePath,
    help :: Bool
  }
  deriving (Show)

parseShape :: [String] -> Maybe Shape
parseShape ("rectangle" : x1 : y1 : x2 : y2 : _) = Just $ Rectangle (Point (read x1) (read y1)) (Point (read x2) (read y2))
parseShape ("circle" : x : y : r : _) = Just $ Circle (Point (read x) (read y)) (read r)
parseShape ("triangle" : x1 : y1 : x2 : y2 : x3 : y3 : _) = Just $ Triangle (Point (read x1) (read y1)) (Point (read x2) (read y2)) (Point (read x3) (read y3))
parseShape ("label" : x : y : font : text : _) = Just $ Label (Point (read x) (read y)) (read font) text
parseShape _ = Nothing

parseVector :: [String] -> Maybe Vector
parseVector ("vector" : dx : dy : _) = Just $ Vector (Point (read dx) (read dy))

parseCommand :: [String] -> Maybe Command
parseCommand ("create" : xs) = Create <$> parseShape xs
parseCommand ("read" : i : _) = Just $ Read (read i)
parseCommand ("update" : i : xs) = Update <$> Just (read i) <*> parseShape xs
parseCommand ("delete" : i : _) = Just $ Delete (read i)
parseCommand ("area" : i : _) = Just $ Area (read i)
parseCommand ("type" : t : _) = Just $ Type (read t)
parseCommand ("box" : i : _) = Just $ Box (read i)
parseCommand ("contained" : xs) = Contained <$> parseShape xs
parseCommand ("move" : i : xs) = Move <$> Just (read i) <*> parseVector xs
parseCommand _ = Nothing

parseOptions :: [String] -> Options
parseOptions args =
  Options
    { db = getParamValue "-db" args,
      command = parseCommand commandArgs,
      log = getParamValue "-log" args,
      silent = isParamSet "-silent" args,
      html = getParamValue "-html" args,
      help = isParamSet "-help" args
    }
  where
    commandArgs = tail $ dropWhile (/= "-command") args

getParamValue :: String -> [String] -> Maybe String
getParamValue _ [] = Nothing
getParamValue param (x : xs) = if x == param then Just $ head xs else getParamValue param xs

isParamSet :: String -> [String] -> Bool
isParamSet param args = param `elem` args

execCommand :: Maybe Command -> Plane -> IO (Maybe String)
-- CRUD
execCommand (Just (Create s)) p = do
  return $ Just $ show $ createShape (Just s) p
execCommand (Just (Read i)) p = do
  return $ Just $ show $ readShape i p
execCommand (Just (Update i s)) p = do
  return $ Just $ show $ updateShape i (Just s) p
execCommand (Just (Delete i)) p = do
  return $ Just $ show $ deleteShape i p
-- area
execCommand (Just (Area i)) p = do
  return $ Just $ show $ areaShape i p
-- type
execCommand (Just (Type t)) p = do
  return $ Just $ show $ typeShape t p
-- bounding box
execCommand (Just (Box i)) p = do
  return $ Just $ show $ boxShape i p
-- contained
execCommand (Just (Contained r)) p = do
  return $ Just $ show $ containedShape (Just r) p
-- move
execCommand (Just (Move i v)) p = do
  return $ Just $ show $ moveShape i (Just v) p
execCommand Nothing _ = return Nothing

printHelp :: IO ()
printHelp = do
  putStrLn "Usage:"
  putStrLn "  [executable] [options]"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  -db [path]              Set path to database file"
  putStrLn "  -command [COMMAND]      The command to execute"
  putStrLn "  -log [path]             Set path to log file"
  putStrLn "  -silent                 Run in silent mode"
  putStrLn "  -html [path]            Set path to HTML file"
  putStrLn "  -help                   Print this help message"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  create rectangle [x1] [y1] [x2] [y2]"
  putStrLn "  create circle [x] [y] [r]"
  putStrLn "  create triangle [x1] [y1] [x2] [y2] [x3] [y3]"
  putStrLn "  create label [x] [y] [font size] [text]"
  putStrLn "  read [id]"
  putStrLn "  update [id] rectangle [x1] [y1] [x2] [y2]"
  putStrLn "  update [id] circle [x] [y] [r]"
  putStrLn "  update [id] triangle [x1] [y1] [x2] [y2] [x3] [y3]"
  putStrLn "  update [id] label [x] [y] [font] [text]"
  putStrLn "  delete [id]"
  putStrLn "  area [id]"
  putStrLn "  type [type]"
  putStrLn "  box [id]"
  putStrLn "  contained rectangle [x1] [y1] [x2] [y2]"
  putStrLn "  contained circle [x] [y] [r]"
  putStrLn "  contained triangle [x1] [y1] [x2] [y2] [x3] [y3]"
  putStrLn "  contained label [x] [y] [font] [text]"
  putStrLn "  move [id] vector [dx] [dy]"

parseFile :: Maybe FilePath -> IO Plane
parseFile Nothing = putStrLn "File path not provided" >> pure []
parseFile (Just filePath) = do
  contents <- readFile filePath
  let parsedPlane = readMaybe contents :: Maybe Plane
  case parsedPlane of
    Just plane -> return plane
    Nothing -> error "Failed to parse file into Plane"

appendFileWithContent :: String -> FilePath -> IO ()
appendFileWithContent content filePath = appendFile filePath content  

maybeAppendFile :: Maybe FilePath -> String -> IO ()
maybeAppendFile maybeFilePath content = maybe (return ()) (appendFileWithContent content) maybeFilePath

maybeRewriteFile :: Maybe FilePath -> String -> IO ()
maybeRewriteFile (Just file) content = writeFile file content
maybeRewriteFile Nothing _ = return ()

maybeAppendHtml :: Maybe FilePath -> String -> IO ()
maybeAppendHtml (Just file) result = do
  htmlBytes <- BS.readFile file
  let html = BS.unpack htmlBytes
      modifiedHtml = unlines $ take 4 (lines html) ++ ["\t\t\t<li>" ++  result ++ "</li>"] ++ drop 4 (lines html)
  BS.writeFile file $ BS.pack modifiedHtml
maybeAppendHtml Nothing _ = return ()

main :: IO ()
main = do
  args <- getArgs
  let options = parseOptions args
  if help options
    then printHelp >> exitSuccess
    else do
      shapes <- parseFile (db options)
      maybeResult <- execCommand (command options) shapes
      case maybeResult of
        Just result -> do
          unless (silent options) $ putStrLn result
          unless (silent options) $ maybeAppendFile (log options) (result ++ "\n")
          unless (silent options) $ maybeAppendHtml (html options) result
          case command options of
            Just (Create _) -> maybeRewriteFile (db options) result
            Just (Update {}) -> maybeRewriteFile (db options) result
            Just (Delete _) -> maybeRewriteFile (db options) result
            Just (Move {}) -> maybeRewriteFile (db options) result
            _ -> return ()
        Nothing -> return ()