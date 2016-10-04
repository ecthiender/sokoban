-- Simple implementation of the Sokoban game.
-- More details about the game here: http://rubyquiz.com/quiz5.html

module Sokoban where

import Prelude hiding (Left, Right)
import System.IO  (stdin, stdout, hSetEcho, hSetBuffering
                  ,hReady, BufferMode(..))
import System.Process
import Data.List (sort)
import Control.Monad (when)

data Input = Up | Down | Left | Right

type Coord = (Int, Int)

type Level = String

data World = World {wWalls    :: [Coord]
                   ,wCrates   :: [Coord]
                   ,wStorages :: [Coord]
                   ,wMan      :: Coord
                   ,wMax      :: Coord
                   } deriving (Show)

translate :: Input -> Coord -> Coord
translate input (x,y) = case input of
                          Up    -> (x, y - 1)
                          Down  -> (x, y + 1)
                          Left  -> (x - 1, y)
                          Right -> (x + 1, y)

initialWorld :: World
initialWorld = World {wWalls    = []
                     ,wCrates   = []
                     ,wStorages = []
                     ,wMan      = (0,0)
                     ,wMax      = (0,0)
                     }

parseLevel :: Level -> World
parseLevel level = foldl parse initialWorld{wMax = (maxX, maxY)} tokens
    where tokens = concat $ zipWith zip (lines level) coords
          coords = [[(x,y) | x <- [0..]] | y <- [0..]]
          maxX   = maximum $ map (fst . snd) tokens
          maxY   = maximum $ map (snd . snd) tokens
          parse wld (ch, c) = case ch of
                            '#' -> wld {wWalls    = c:wWalls wld}
                            'o' -> wld {wCrates   = c:wCrates wld}
                            '.' -> wld {wStorages = c:wStorages wld}
                            '@' -> wld {wMan      = c}
                            ' ' -> wld
                            otherwise -> error ("Unrecognized symbol: " ++ show ch)

displayWorld :: World -> IO ()
displayWorld world = putStrLn board
  where board         = unlines $ map (map showAt) $ coords
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
isMan c world     = c == (wMan world)
isWall c world    = elem c (wWalls world)
isCrate c world   = elem c (wCrates world)
isStorage c world = elem c (wStorages world)

updateWorld :: World -> Input -> World
updateWorld world input
    | isWall newPos world     = world
    | isCrate newPos world    =
        if isCrate newPos' world || isWall newPos' world
          then world
          else moveCrate world newPos
    | otherwise               = world'

    where oldPos              = wMan world
          newPos              = translate input oldPos
          newPos'             = translate input newPos
          world'              = world {wMan = newPos}
          moveCrate world pos = world {wMan = newPos
                                      ,wCrates = newPos':(filter (\c -> c /= newPos) (wCrates world))}

isFinished :: World -> Bool
-- compares by checking if all the coords of the crates match all the
-- coords of all the storages
isFinished world = sort (wCrates world) == sort (wStorages world)

----

-- try to get the actual key pressed by the user..
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getInput :: IO Input
getInput = do
    key <- getKey
    case key of
      "w"       -> return Up
      "a"       -> return Left
      "s"       -> return Down
      "d"       -> return Right
      "\ESC[A"  -> return Up
      "\ESC[B"  -> return Down
      "\ESC[C"  -> return Right
      "\ESC[D"  -> return Left
      otherwise -> getInput

loadLevel :: String -> IO World
loadLevel filename = do
  level <- readFile filename
  return $ parseLevel level

gameLoop :: World -> IO ()
gameLoop world = do
    --putStr "\ESC[2J"
    system "clear"
    displayWorld world
    input <- getInput
    let world' = updateWorld world input
    if isFinished world'
      then displayWorld world' >> print "Well Done!"
      else gameLoop world'

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    world <- loadLevel "levels/level2.txt"
    gameLoop world
