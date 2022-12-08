{
module Lexer
    ( alexScanTokens
    ) where

import Model
}

%wrapper "basic"

$identSymbol = [a-z A-Z 0-9 \+ \-]

tokens :-

  $white+           ;
  "--".*            ;
  "->"              { const TArrow }
  "."               { const TDot }
  ","               { const TComma }
  go                { const TGo }
  take              { const TTake }
  mark              { const TMark }
  nothing           { const TNothing }
  turn              { const TTurn }
  case              { const TCase }
  of                { const TOf }
  end               { const TEnd }
  left              { const TLeft }
  right             { const TRight }
  front             { const TFront }
  ";"               { const TSemiColon }
  Empty             { const TEmpty }
  Lambda            { const TLambda }
  Debris            { const TDebris }
  Asteroid          { const TAsteroid }
  Boundary          { const TBoundary }
  "_"               { const TUnderscore }
  $identSymbol+     { TIdent }
