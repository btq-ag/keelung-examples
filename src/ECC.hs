-- This is a incomplete and buggy ECC implemetation, this only serves as
-- a demostration of using tuples in Keelung.
{-# LANGUAGE DataKinds #-}

module ECC where

import Keelung

type EC = (Field, Field)

newtype Point = Point (EC, Field, Field)
  deriving (Eq, Show)

instance Reusable Point where
  reuse (Point ((a, b), x, y)) = do
    a' <- reuse a
    b' <- reuse b
    x' <- reuse x
    y' <- reuse y
    return $ Point ((a', b'), x', y')

-- instance Encode Point where
--   encode ((a, b), x, y) = do
--     xs <- mapM encode [a,b,x,y]
--     let xs' = map (fromMaybe (error "") . decode) xs
--     return $ Misc (xs', [], [])

genPoint' :: Int -> Comp (Field, Field)
genPoint' n = do
  a <- inputField Private
  b <- inputField Private
  x <- inputField Private
  y <- inputField Private
  genPoint n (Point ((a, b), x, y))

genPoint :: Int -> Point -> Comp (Field, Field)
genPoint n (Point ((a, b), x, y)) = do
  assert ((y * y) `eq` ((x * x * x) + (x * a) + b))
  Point (_, x', y') <- Point ((a, b), x, y) `times` n
  return (x', y')
  where
    times :: Point -> Int -> Comp Point
    times point 1 = return point
    times point number = do
      if even number
        then (point `times` (number `div` 2)) >>= reuse >>= \p -> return $ double p
        else (point `times` pred number) >>= reuse >>= \p -> return $ p `addP` point
    addP :: Point -> Point -> Point
    addP p0@(Point (ec, x0, y0)) p1@(Point (_, x1, y1))
      | p0 == p1 = double p0
      | x0 == x1 = Point (ec, 0, 0)
      | otherwise = Point (ec, x2, y2)
      where
        slope = (y1 - y0) / (x1 - x0)
        x2 = slope * slope - (x0 + x1)
        y2 = (x0 - x2) * slope - y1
    double :: Point -> Point
    double (Point ((a', b'), x', y'))
      | y == 0 = Point ((a', b'), 0, 0)
      | otherwise = Point ((a', b'), x0, y0)
      where
        slope = (x * x * 3 + a') / (y' + y')
        x0 = slope * slope - x' - x'
        y0 = (x' - x0) * slope - y'
