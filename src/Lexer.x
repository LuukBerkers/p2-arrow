{
module Lexer
    ( alexScanTokens
    ) where

import Model
}

%wrapper "basic"

tokens :-
  _ { const Token }

