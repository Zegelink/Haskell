module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

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
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
