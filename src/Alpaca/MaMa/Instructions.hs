{-|
Module      : Alpaca.MaMa.Instructions
Description : Alpaca Machine Instructions
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

module Alpaca.MaMa.Instructions 
(
)
where

    import Alpaca.MaMa.Machine (Pointer, MaMa, stmt, haul, runtimeError)
    import Alpaca.MaMa.Types (Basic (..), HeapObject (..))


    loop :: Int -> (Int -> MaMa ()) -> MaMa ()
    loop 0 todo     = todo 0
    loop times todo = todo times >> loop (times - 1) todo


    getbasic :: MaMa ()
    getbasic = stmt [] "$st[$sp] := $hp[$st[$sp]].v"

    mkbasic :: MaMa ()
    mkbasic = stmt [] "$st[$sp] := new B ($st[$sp])"

    pushloc :: Int -> MaMa ()
    pushloc n = do 
        stmt [("n", Pointer n)] "$st[$sp+1] := $st[($sp - n)]"
        stmt [] "$sp := $sp + 1"

    pushglob :: Int -> MaMa ()
    pushglob j = do 
        stmt [("j", Pointer j)] "$st[($sp + 1)] := $hp[$gp].v[j]"
        stmt [] "$sp := ($sp + 1)"

    slide :: Int -> MaMa ()
    slide k = do 
        stmt [("k", Pointer k)] "$st[($sp - k)] := $st[$sp]"
        stmt [("k", Pointer k)] "$sp := ($sp - k)"

    mkvec :: Int -> MaMa ()
    mkvec g = do
        h <- haul [("g", Pointer g)] "new V (g)"
        stmt [("g", Pointer g)] "$sp := (($sp - g) + 1)"
        loop (g-1) $ \i -> stmt [("i", Pointer i), ("h", h)] "$hp[h].v[i] := $st[($sp + 1)]"
        stmt [("h", h)] "$st[$sp] := h"

    mkfunval :: Int -> MaMa ()
    mkfunval a = do
        vec <- haul [] "new V (0)"
        stmt [("A", Pointer a)] "$st[$sp] := new F (A, vec, $st[$sp])"

    mark :: Int -> MaMa ()
    mark a = do
        stmt [] "$st[($sp + 1)] := $gp"
        stmt [] "$st[($sp + 2)] := $fp"
        stmt [("A", Pointer a)] "$st[($sp + 3)] := A"
        stmt [] "$fp := $sp"
        stmt [] "$sp := ($sp + 3)"

    apply :: MaMa ()
    apply = do
        h <- haul [] "$st[$sp]"
        a <- haul [("h", h)] "$hp[h].ap"
        Pointer n <- haul [("a", a)] "$hp[a].n"
        loop (n-1) $ \i -> stmt [("i", Pointer i)] "$st[($sp + i)] := $hp[a].v[i]"
        stmt [("n", Pointer n)] "$sp := (($sp + n) -1)"
        stmt [("h", h)] "$gp := $hp[h].gp"
        stmt [("h", h)] "$pc := $hp[h].cp"

    mkvec0 :: MaMa ()
    mkvec0 = do
        Pointer n <- haul [] "($sp - $fp)"
        a <- haul [("n", Pointer n)] "new V (n)"
        stmt [] "$sp := ($fp + 1)"
        loop (n-1) $ \i -> stmt [("a", a), ("i", Pointer i)] "$hp[a].v[i] := $st[($sp + i)]"
        stmt [("a", a)] "$st[$sp] := a"



