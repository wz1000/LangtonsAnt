module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random
import Data.Monoid
import Debug.Trace
import qualified Data.Map as M


data Dir = N | W | S | E deriving (Eq, Ord, Read, Show)
data RDir = L | R
data Cell = Black | White deriving (Show)
type Board = M.Map (Int, Int) Cell
data World = World [Player] Board Float Float ViewPort
data Player = P { pos :: (Int, Int)
                , dir :: Dir
                , updir :: Cell -> RDir
                }

invert White = Black
invert Black = White

turn :: Dir -> RDir -> Dir
turn N L = W
turn N R = E
turn S L = E
turn S R = W
turn W L = S
turn W R = N
turn E L = N
turn E R = S

defplayer :: Player
defplayer = P (0, 0) N go
  where go Black = L
        go White = R

move :: Player -> Dir -> Player
move p@(P (x, y) _ _) ndir = case ndir of
  N -> p { pos = (x   , y+1), dir = ndir }
  S -> p { pos = (x   , y-1), dir = ndir }
  W -> p { pos = (x-1 , y  ), dir = ndir }
  E -> p { pos = (x+1 , y  ), dir = ndir }

update :: Float -> World -> World
update dt (World [] board t s v) = World [] board (t+dt) s v
update dt (World (player:players) board t s v)
  | floor ((t + dt)*s) > floor (t*s) = let cur = M.findWithDefault White (pos player) board
                                           board' = M.insert (pos player) (invert cur) board
                                           dir' = turn (dir player) (updir player cur)
                                           player' = move player dir'
                                             in case update dt (World players board' t s v) of
                                                  World players' board'' _ _ _ -> World (player':players') board'' (t+dt) s v
  | otherwise = World players board (t+dt) s v

drawBoard :: Board -> Picture
drawBoard = M.foldrWithKey go blank
  where go (x, y) Black pic = (pic <>) $ translate (fromIntegral (x*20)) (fromIntegral (y*20)) $ color black $ rectangleSolid 20 20
        go (x, y) White pic = (pic <>) $ translate (fromIntegral (x*20)) (fromIntegral (y*20)) $ color black $ rectangleWire 20 20

drawWorld :: World -> Picture
drawWorld (World players b _ _ v) =
  applyViewPortToPicture v $ 
       drawBoard b <>   foldMap (\(P (x, y) _ _) -> translate (fromIntegral $ x*20) (fromIntegral $ y*20) $ color red $ rectangleSolid 20 20) players


main :: IO ()
main = simulate  (InWindow "Nice Window" (200, 200) (10, 10))
            white
            60
            (World [defplayer, defplayer {pos = (20,20)}] M.empty 0 100 viewPortInit)
            drawWorld
            (const update)
