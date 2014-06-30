{-|
Module      : Alpaca.MaMa.Types
Description : Alpaca Machine Type-Representation
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of Alpaca.

Alpaca is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Alpaca is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Alpaca. If not, see <http://www.gnu.org/licenses/>.
-}

module Alpaca.MaMa.Types 
(
    -- * Basic Types
    Basic (Boolean, Integer, Double, Pointer, Label),

    -- * Heap Objects
    HeapObject (Basic, Closure, FunVal, Vector, Cons)
)
where

    import Data.List (intercalate)
    import Data.Map (Map, toList)

    data Basic = 
          Boolean Bool
        | Integer Int 
        | Double Double
        | Pointer Int
        | Label Int
        deriving (Eq) 

    instance Show Basic where
        show (Boolean b) = show b
        show (Integer i) = show i 
        show (Pointer p) = show p ++ "*"
        show (Label l)   = show l ++ "§"



    data HeapObject = 
          Basic Basic
        | Closure Int Int
        | FunVal Int Int Int
        | Vector Int (Map Int Int)
        | Cons String Int (Map Int Int)
        deriving (Eq)

    instance Show HeapObject where
        show (Basic b) = "basic(" ++ show b ++ ")"
        show (Closure c p) = "closure(" ++ show c ++ ", " ++ show p ++ ")"
        show (FunVal f l g) = "funval(" ++ show f ++ ", " ++ show l ++ ", " ++ show g ++ ")"
        show (Vector n ps) = "vector[" ++ show n ++ "](" ++ intercalate ", " (map show . toList $ ps) ++ ")"
        show (Cons c n ps) = c ++ "[" ++ show n ++ "](" ++ intercalate ", " (map show . toList $ ps) ++ ")"