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
noUndefinedRulesAlg :: Algebra Bool (String, [String]) [String] () [String] ()
noUndefinedRulesAlg =
    ( tProgram
    , tRule
    , []
    , []
    , []
    , []
    , const []
    , tCase
    , tIdent
    , ()
    , ()
    , ()
    , tAlt
    , ()
    , ()
    , ()
    , ()
    , ()
    , ()
    )
  where
    tProgram rs = and [ used `elem` rules | used <- useds ]
      where
        (rules, usedss) = unzip rs
        useds           = concat usedss
    tRule name useds = (name, concat useds)
    tCase _ useds = concat useds
    tIdent name = [name]
    tAlt _ useds = concat useds

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

noDoubleRulesAlg :: Algebra Bool String () () () ()
noDoubleRulesAlg =
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
    tProgram = noDups
      where
        noDups []       = True
        noDups (x : xs) = (x `notElem` xs) && noDups xs
    tRule = const

completePatternsAlg :: Algebra Bool Bool Bool () (Pat, Bool) Pat
completePatternsAlg =
    ( tProgram
    , tRule
    , True
    , True
    , True
    , True
    , const True
    , tCase
    , const True
    , ()
    , ()
    , ()
    , tAlt
    , Empty
    , Lambda
    , Debris
    , Asteroid
    , Boundary
    , Underscore
    )
  where
    tProgram = and
    tRule _ = and
    tCase _ alts =
        (  (Underscore `elem` pats)
            || all (`elem` pats) [Empty, Lambda, Debris, Asteroid, Boundary]
            )
            && and bs
        where (pats, bs) = unzip alts
    tAlt pat cmds = (pat, and cmds)

checkProgram :: Program -> Bool
checkProgram pr =
    noUndefinedRules && startRuleExists && noDoubleRules && completePatterns
  where
    noUndefinedRules = fold noUndefinedRulesAlg pr
    startRuleExists  = fold startRuleExistsAlg pr
    noDoubleRules    = fold noDoubleRulesAlg pr
    completePatterns = fold completePatternsAlg pr

checkWithFeedback :: Program -> IO ()
checkWithFeedback pr = putStrLn $ maybe passed feedback $ findProblem pr
  where
    passed = "No problems!"
    feedback UndefinedRules = "A rule is used but never defined"
    feedback NoStartRule    = "There is no rule named `start`"
    feedback DoubleRules    = "Some rules are defined multiple times"
    feedback IncompletePatterns =
        "Some case expressions have incomplete patterns"

data Problem = UndefinedRules | NoStartRule | DoubleRules | IncompletePatterns

findProblem :: Program -> Maybe Problem
findProblem pr | undefinedRules     = Just UndefinedRules
               | noStartRule        = Just NoStartRule
               | doubleRules        = Just DoubleRules
               | incompletePatterns = Just IncompletePatterns
               | otherwise          = Nothing
  where
    undefinedRules     = not $ fold noUndefinedRulesAlg pr
    noStartRule        = not $ fold startRuleExistsAlg pr
    doubleRules        = not $ fold noDoubleRulesAlg pr
    incompletePatterns = not $ fold completePatternsAlg pr
