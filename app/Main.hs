module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Model
import View
import Control

main :: IO ()
main = display View.window View.background View.drawing
