module Algebra where

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
      )

fold = undefined

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined
