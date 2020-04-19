{-# LANGUAGE TypeApplications #-}
module SokobanGUI where

import           Data.Maybe                       (fromJust)
import           Debug.Trace
import           Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as E
import           Prelude                          hiding (Left, Right)

import           Sokoban

type Asset = (String, Picture)

displayWorld' :: [Asset] -> World -> Picture
displayWorld' assets world =
  if isFinished world
  then pictures $ [ drawBackground assets
                  , uncurry translate (adjust @Int (-1,2)) $ scale 0.4 0.4 $
                    color red $ text "YOU WON!\n GAME OVER!"
                  ]
  else
    let player = drawPlayer assets (wPlayer world)
        walls = drawWalls assets (wWalls world)
        storages = drawStorages assets (wStorages world)
        crates = drawCrates assets (wCrates world)
        bg = drawBackground assets
    in pictures $
       -- To debug the coords
      -- map showAt coords ++
     [bg] ++ walls ++ storages ++ crates ++ [player]
    -- where
    --   coords       = concat [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]
    --   (maxX, maxY) = wMax world
    --   showAt = drawCell

drawBackground :: [Asset] -> Picture
drawBackground assets =
  let texture = fromJust $ lookup "BG" assets
  in Pictures $ map (\a ->  ((uncurry translate) a) texture) $ translateMatrix (maxWidth) (maxHeight)


drawPlayer :: [Asset] -> Coord -> Picture
drawPlayer assets coord = drawObject "player" assets coord 0.1

drawCrates :: [Asset] -> [Crate] -> [Picture]
drawCrates assets crates =
  map (\(Crate c) -> drawObject "crate" assets c 0.6) crates

drawWalls :: [Asset] -> [Wall] -> [Picture]
drawWalls assets walls =
  map (\(Wall c) -> drawObject "wall" assets c 0.4) walls

drawStorages :: [Asset] -> [Storage] -> [Picture]
drawStorages assets storages =
  map (\(Storage c) -> drawObject "storage" assets c 0.6) storages

drawObject :: String -> [Asset] -> Coord -> Float -> Picture
drawObject name assets coord scaleFactor =
  let pic = scale scaleFactor scaleFactor $ fromJust $ lookup name assets
  in uncurry translate (adjust coord) pic

maxWidth, maxHeight :: Float
maxWidth = 700
maxHeight = 500

blockScale :: Num a => a
blockScale = 50

drawCell :: Coord -> Picture
drawCell coord =
  trace (show coord) $
  uncurry translate (adjust coord) $ rectangleWire blockScale blockScale

adjust :: Integral a => (a, a) -> (Float, Float)
adjust (x, y) =
  let x' = (fromIntegral (x * blockScale) - maxWidth / 2) + 10
      y' = (maxHeight / 2) - fromIntegral (y * blockScale) - 10
  in (x', y')

-- what we want: 640, 480
-- -320--x--(-160)--x--0--x--160--x--320
--      -240      -80    80      240
-- -240--x--(-80)--x--80--x--240
--      -160       0     160
translateMatrix :: Float -> Float -> [(Float, Float)]
translateMatrix w h = concat $ map (zip xTiles)
                             $ map (replicate (length xTiles)) yTiles
                      where xTiles = [lowerbound w, lowerbound w + blockScale..higherbound w]
                            yTiles = [lowerbound h, lowerbound h + blockScale..higherbound h]
                            higherbound size = size/2 - blockScale/2
                            lowerbound size = -(higherbound size)

eventHandler :: E.Event -> World -> World
eventHandler event world =
  case event of
    E.EventResize _          -> world -- TODO: handle it
    E.EventMotion _          -> world
    E.EventKey key keyState _ _ ->
      case mkInput key keyState of
        Nothing    -> world
        Just input -> updateWorld world input
  where
    mkInput key state =
      case (key, state) of
        (E.SpecialKey E.KeyUp, E.Down)    -> Just Up
        (E.SpecialKey E.KeyDown, E.Down)  -> Just Down
        (E.SpecialKey E.KeyLeft, E.Down)  -> Just Left
        (E.SpecialKey E.KeyRight, E.Down) -> Just Right
        _                                 -> Nothing
