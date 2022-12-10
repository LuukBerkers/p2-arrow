module Algebra where

import           Data.Char                      ( toLower )
import           Model

-- Exercise 5
type Algebra program rule cmd dir alt pat
    = ( -- Program
        [rule] -> program
      , -- Rule
        String -> [cmd] -> rule
      , -- Go
        cmd
      , -- Take
        cmd
      , -- Mark
        cmd
      , -- Not
        cmd
      , -- Turn
        dir -> cmd
      , -- Case
        dir -> [alt] -> cmd
      , -- Ident
        String -> cmd
      , -- Le
        dir
      , -- Ri
        dir
      , -- Fr
        dir
      , -- Alt
        pat -> [cmd] -> alt
      , -- Empty
        pat
      , -- Lambda
        pat
      , -- Debris
        pat
      , -- Asteroid
        pat
      , -- Boundary
        pat
      , -- Underscore
        pat
      )

fold :: Algebra prog rule cmd dir alt pat -> Program -> prog
fold (tProgram, tRule, tGo, tTake, tMark, tNot, tTurn, tCase, tIdent, tLe, tRi, tFr, tAlt, tEmpty, tLambda, tDebris, tAsteroid, tBoundary, tUnderscore)
    = fProg
  where
    fProg (Program rules) = tProgram (map fRule rules)
    fRule (Rule name cmds) = tRule name (map fCmd cmds)
    fCmd Go            = tGo
    fCmd Take          = tTake
    fCmd Mark          = tMark
    fCmd Not           = tNot
    fCmd (Turn d     ) = tTurn (fDir d)
    fCmd (Case d alts) = tCase (fDir d) (map fAlt alts)
    fCmd (Ident name ) = tIdent name
    fDir Le = tLe
    fDir Ri = tRi
    fDir Fr = tFr
    fAlt (Alt pat cmds) = tAlt (fPat pat) (map fCmd cmds)
    fPat Empty      = tEmpty
    fPat Lambda     = tLambda
    fPat Debris     = tDebris
    fPat Asteroid   = tAsteroid
    fPat Boundary   = tBoundary
    fPat Underscore = tUnderscore

-- Exercise 6
noUndefinedRulesAlg
    :: Algebra
           Bool
           ([String] -> ([String], Bool))
           ([String] -> Bool)
           ()
           ([String] -> Bool)
           ()
noUndefinedRulesAlg =
    ( tProgram
    , tRule
    , tGo
    , tTake
    , tMark
    , tNot
    , tTurn
    , tCase
    , tIdent
    , tLe
    , tRi
    , tFr
    , tAlt
    , tEmpty
    , tLambda
    , tDebris
    , tAsteroid
    , tBoundary
    , tUnderscore
    )
  where
    tProgram    = undefined
    tRule       = undefined
    tGo         = undefined
    tTake       = undefined
    tMark       = undefined
    tNot        = undefined
    tTurn       = undefined
    tCase       = undefined
    tIdent      = undefined
    tLe         = undefined
    tRi         = undefined
    tFr         = undefined
    tAlt        = undefined
    tEmpty      = undefined
    tLambda     = undefined
    tDebris     = undefined
    tAsteroid   = undefined
    tBoundary   = undefined
    tUnderscore = undefined

startRuleExistsAlg :: Algebra Bool Bool () () () ()
startRuleExistsAlg =
    ( tProgram
    , tRule
    , ()
    , ()
    , ()
    , ()
    , const ()
    , const . const ()
    , const ()
    , ()
    , ()
    , ()
    , const . const ()
    , ()
    , ()
    , ()
    , ()
    , ()
    , ()
    )
  where
    tProgram = or
    tRule name _ | map toLower name == "start" = True
                 | otherwise                   = False

checkProgram :: Program -> Bool
checkProgram = undefined
