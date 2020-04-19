module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy

import           Sokoban
import           SokobanGUI

window :: Display
window = InWindow "Sokoban" (700, 500) (100, 100)

assetNames :: [String]
assetNames = [ "crate"
             , "player"
             , "storage"
             , "wall"
             , "BG"
             ]

loadAssets :: [String] -> IO [Asset]
loadAssets strings = do
  pics <- mapM (\s -> loadJuicyPNG ("assets/" ++ s ++ ".png")) strings
  case sequence pics of
    Nothing -> error "Error loading assets.."
    Just ps -> return $ zip strings ps

main :: IO ()
main = do
  world <- loadLevel "levels/level3.txt"
  assets <- loadAssets assetNames
  play window white 5 world (displayWorld' assets) eventHandler (const id)
