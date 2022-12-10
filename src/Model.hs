module Model
    ( Token(..)
    , Program(..)
    , Rule(..)
    , Cmd(..)
    , Dir(..)
    , Alt(..)
    , Pat(..)
    ) where

-- Exercise 1
data Token
    = TArrow
    | TDot
    | TComma
    | TGo
    | TTake
    | TMark
    | TNothing
    | TTurn
    | TCase
    | TOf
    | TEnd
    | TLeft
    | TRight
    | TFront
    | TSemiColon
    | TEmpty
    | TLambda
    | TDebris
    | TAsteroid
    | TBoundary
    | TUnderscore
    | TIdent String
    deriving (Show)

-- Exercise 2
newtype Program = Program [Rule]
    deriving (Show)

data Rule = Rule String [Cmd]
    deriving Show

data Cmd
    = Go
    | Take
    | Mark
    | Not
    | Turn Dir
    | Case Dir [Alt]
    | Ident String
    deriving (Show)

data Dir
    = Le
    | Ri
    | Fr
    deriving (Show)

data Alt = Alt Pat [Cmd]
    deriving Show

data Pat
    = Empty
    | Lambda
    | Debris
    | Asteroid
    | Boundary
    | Underscore
    deriving (Show)
