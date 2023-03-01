module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.List

type X = Int
type Y = Int
type Square = (X,Y)
type Direction = (X,Y) --  N = (0,1); E = (1,0); S = (0,-1); W = (-1,0);
type State = (Square, [Square], Direction, Int) 

move :: ViewPort -> Float -> State -> State
move  _ _ (ant, blacks, dir, max) = (ant', blacks', dir', max')
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
  (map (\y -> line [(-ext,y),(ext,y)]) [-ext..ext])
  ++ (map (\x -> line [(x,-ext),(x,ext)]) [-ext..ext])
    where ext = (fromIntegral extent) + 0.5

blacks_pic :: [Square] -> Picture
blacks_pic = pictures . map (\(x,y) -> translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 1 1)

ant_pic :: Square -> Direction -> Picture
ant_pic (x,y) dir = translate (fromIntegral x) (fromIntegral y) $ color red $ scale 0.3 0.3 $ 
  Rotate (case dir of {(1,0)->0; (0,-1)->90; (-1,0)->180; (0,1)->270}) $ polygon [(1,0),((-1), 0.7), ((-1), (-0.7))]

drawing :: State -> Picture
drawing (ant, blacks, dir, extent) = scale 20 20 $ pictures $ [grid_pic extent, blacks_pic blacks, ant_pic ant dir]

main :: IO ()
main = simulate (InWindow "Langton's Ant" (1500, 1500) (0,0)) white 100 ((0,0), [], (0,1), 0) drawing move
-- main = simulate window background fps initialState render update
