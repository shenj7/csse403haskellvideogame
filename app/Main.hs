module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Model
import View
import Control

main :: IO ()
main = play window background fps initialState render handleKeys update
