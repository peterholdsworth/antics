module Main where

import Graphics.Gloss
import Data.List

type X = Int
type Y = Int
type Square = (X,Y)
type Direction = (X,Y) --  N = (0,1); E = (1,0); S = (0,-1); W = (-1,0);

move :: (Square, [Square], Direction, Int) -> (Square, [Square], Direction, Int)
move  (ant, blacks, dir, max) = (ant', blacks', dir', max')
  where 
    on_black = elem ant blacks
    add (x,y) (x',y') = (x+x',y+y')
    blacks' = if on_black then delete ant blacks else (ant:blacks)
    left = \(x,y) -> (-y,x)
    right = \(x,y) -> (y, -x)
    dir' = (if on_black then right else left) dir
    ant' = add dir' ant
    max' = maximum [ max, fst ant', snd ant', -(fst ant'), -(snd ant')]

grid_pic :: Int -> Picture
grid_pic extent = pictures $
  (map (\y -> line [(((-ext) - 0.5),y),(ext + 0.5,y)]) [((-ext)-0.5)..(ext+0.5)])
  ++ (map (\x -> line [((x, (-ext) - 0.5)),(x, ext + 0.5)]) [((-ext)-0.5)..(ext+0.5)])
    where ext = fromIntegral extent

blacks_pic :: [Square] -> Picture
blacks_pic blacks = pictures $
  map (\(x,y) -> translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 1 1) blacks

ant_pic :: Square -> Direction -> Picture
ant_pic (x,y) dir = translate (fromIntegral x) (fromIntegral y) $ color red $ scale 0.3 0.3 $ 
  case dir of 
    (1,0)  -> Rotate 0   ant
    (0,-1) -> Rotate 90  ant
    (-1,0) -> Rotate 180 ant
    (0,1)  -> Rotate 270 ant
  where ant = polygon [(1,0),((-1), 0.7), ((-1), (-0.7))]

drawing :: Float -> Picture
drawing time = scale 40 40 $ pictures $ [grid_pic extent, blacks_pic blacks, ant_pic ant dir]
  where
    (ant, blacks, dir, extent) = head $ drop (floor $ 2 * time) $ iterate move ((0,0), [], (0,1), 0)

main :: IO ()
main = animate (InWindow "Langton's Ant" (1000, 1000) (0,0)) white drawing
