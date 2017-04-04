--Author: Chongxian Chen
--Date: February 27, 2017

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)
import Data.Maybe

import KarelSyntax
import KarelState
import KarelExamples

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing c) _ r = if (c == (getFacing r)) then True else False
-- neighbor (cardTurn d (getFacing r)) (getPos r) 
-- will get the new position
test (Clear d) w r = isClear (neighbor (cardTurn d (getFacing r)) (getPos r) ) w
test Beeper w r = hasBeeper (getPos r) w
test Empty _ r = if (getBag r) > 0 then False else True

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w r = if ((isClear (neighbor (getFacing r) (getPos r)) w ) == True)
                     then OK w (setPos (neighbor (getFacing r) (getPos r)) r) 
                     else Error ("Blocked at: " ++ show (neighbor (getFacing r) (getPos r)))
stmt PutBeeper _ w r = let p = getPos r
                       in if isEmpty r 
                             then Error "No beeper to put."
                             else OK (incBeeper p w) (decBag r)
stmt (Turn d) _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Block []) _ w r = OK w r
stmt (Block (x:xs)) m w r = onOK (stmt (Block xs) m) (stmt x m w r)
stmt (If t s1 s2) m w r =  if test t w r 
                              then stmt s1 m w r
                              else stmt s2 m w r
stmt (Call macro) m w r = if isJust (lookup macro m)
                             then stmt (fromJust (lookup macro m)) m w r
                             else Error ("Undefined macro: " ++ macro)
stmt (Iterate num s) m w r = if (num > 1) then onOK (stmt (Iterate (-1 + num) s) m) (stmt s m w r) else stmt s m w r
stmt (While t s) m w r = if test t w r then onOK (stmt (While t s) m) (stmt s m w r) else OK w r 

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
