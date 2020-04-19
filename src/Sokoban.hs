-- Simple implementation of the Sokoban game.
-- More details about the game here: http://rubyquiz.com/quiz5.html
-- Inspired from https://github.com/codinguncut/Sokoban

module Sokoban where

import           Control.Monad  (forever)
import           Data.List      (sort)
import           Prelude        hiding (Left, Right)
import           System.IO      (BufferMode (..), hReady, hSetBuffering,
                                 hSetEcho, stdin, stdout)
import           System.Process


data Input = Up | Down | Left | Right

type Coord = (Int, Int)

type Level = String

data Wall = Wall Coord
  deriving (Show, Eq, Ord)

data Crate = Crate Coord
  deriving (Show, Eq, Ord)

data Storage = Storage Coord
  deriving (Show, Eq, Ord)

data Space = Space Coord
  deriving (Show, Eq, Ord)

class WorldEntity a where
  getCoord :: a -> Coord

instance WorldEntity Wall where
  getCoord (Wall c) = c

instance WorldEntity Crate where
  getCoord (Crate c) = c

instance WorldEntity Storage where
  getCoord (Storage c) = c

instance WorldEntity Space where
  getCoord (Space c) = c


data World =
  World
  { wWalls    :: [Wall]
  , wCrates   :: [Crate]
  , wStorages :: [Storage]
  , wSpaces   :: [Space]
  , wPlayer   :: Coord
  , wMax      :: Coord
  } deriving (Show, Eq, Ord)

translateInput :: Input -> Coord -> Coord
translateInput input (x,y) =
  case input of
    Up    -> (x, y - 1)
    Down  -> (x, y + 1)
    Left  -> (x - 1, y)
    Right -> (x + 1, y)

initialWorld :: World
initialWorld = World [] [] [] [] (0,0) (0,0)

parseLevel :: Level -> World
parseLevel level = foldl (flip parseLevelSymbol) initialWorld{wMax = (maxX, maxY)} tokens
    where tokens = concat $ zipWith zip (lines level) coords
          coords = [[(x,y) | x <- [0..]] | y <- [0..]]
          maxX   = maximum $ map (fst . snd) tokens
          maxY   = maximum $ map (snd . snd) tokens

-- parseWall :: (Char, Coord) -> Maybe Wall
-- parseWall ('#', c) = Just $ Wall c
-- parseWall (_, _)   = Nothing

-- parseCrate :: (Char, Coord) -> Maybe Crate
-- parseCrate ('o', c) = Just $ Crate c
-- parseCrate (_, _)   = Nothing

-- parseStorage :: (Char, Coord) -> Maybe Storage
-- parseStorage ('.', c) = Just $ Storage c
-- parseStorage (_, _)   = Nothing

-- parseSpace :: (Char, Coord) -> Maybe Space
-- parseSpace (' ', c) = Just $ Space c
-- parseSpace (_, _)   = Nothing

parseLevelSymbol :: (Char, Coord) -> World -> World
parseLevelSymbol (ch, c) wld =
  case ch of
    '#' -> wld {wWalls    = Wall c:wWalls wld}
    'o' -> wld {wCrates   = Crate c:wCrates wld}
    '.' -> wld {wStorages = Storage c:wStorages wld}
    '@' -> wld {wPlayer   = c}
    ' ' -> wld
    _   -> error ("Unrecognized symbol: " ++ show ch)


displayWorld :: World -> IO ()
displayWorld world = putStrLn board
  where board         = unlines $ map (map showAt) coords
        coords        = [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
        (maxX, maxY)  = wMax world
        showAt  c
          | isStorage c world && isCrate c world  = '*'
          | isStorage c world && isMan c world    = '+'
          | isMan c world                         = '@'
          | isWall c world                        = '#'
          | isCrate c world                       = 'o'
          | isStorage c world                     = '.'
          | otherwise                             = ' '

-- takes a coord, takes the world, and says if that artefact is there in
-- that coord
isMan :: Coord -> World -> Bool
isMan c world = c == wPlayer world

isWall :: Coord -> World -> Bool
isWall c world = Wall c `elem` wWalls world

isCrate :: Coord -> World -> Bool
isCrate c world = Crate c `elem ` wCrates world

isStorage :: Coord -> World -> Bool
isStorage c world = Storage c `elem ` wStorages world

updateWorld :: World -> Input -> World
updateWorld world input
    | isWall newPos world     = world
    | isCrate newPos world    =
        if isCrate newPos' world || isWall newPos' world
          then world
          else moveCrate world newPos
    | otherwise               = world'

    where oldPos              = wPlayer world
          newPos              = translateInput input oldPos
          newPos'             = translateInput input newPos
          world'              = world { wPlayer = newPos }
          moveCrate world pos = world { wPlayer = newPos
                                      , wCrates = Crate newPos':filter (/= Crate newPos) (wCrates world)}

isFinished :: World -> Bool
-- compares by checking if all the coords of the crates match all the
-- coords of all the storages
isFinished world =
  crateCoords == storeCoords
  where
    crateCoords = (map getCoord . sort) $ wCrates world
    storeCoords = (map getCoord . sort) $ wStorages world

----

-- try to get the actual key pressed by the user..
getKey :: IO String
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getInput :: IO Input
getInput = do
    key <- getKey
    case key of
      "w"      -> return Up
      "a"      -> return Left
      "s"      -> return Down
      "d"      -> return Right
      "\ESC[A" -> return Up
      "\ESC[B" -> return Down
      "\ESC[C" -> return Right
      "\ESC[D" -> return Left
      _        -> getInput

loadLevel :: String -> IO World
loadLevel filename = do
  level <- readFile filename
  return $ parseLevel level

gameLoop :: World -> IO ()
gameLoop world = do
    --putStr "\ESC[2J"
    _ <- system "clear"
    displayWorld world
    input <- getInput
    let world' = updateWorld world input
    if isFinished world'
      then system "clear" >> displayWorld world' >> print "Well Done!"
      else gameLoop world'

asciimain :: IO ()
asciimain = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    forever $ do
      levelN <- chooseLevel
      world <- loadLevel ("levels/level" ++ levelN ++ ".txt")
      gameLoop world
  where
    chooseLevel = do
      putStrLn "Choose a level between 1-3:"
      hSetEcho stdin True
      l <- getLine
      hSetEcho stdin False
      return l
