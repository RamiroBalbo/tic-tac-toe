module Main where

import Game
import Prelude
import Graphics.Gloss


import Game
import Logic
import Rendering

window = InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)