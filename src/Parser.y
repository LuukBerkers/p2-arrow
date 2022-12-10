{
module Parser
    ( parser
    ) where

import Model
}

%name parser
%tokentype { Token }

%token
    "->"        { TArrow }
    '.'         { TDot }
    ','         { TComma }
    go          { TGo }
    take        { TTake }
    mark        { TMark }
    nothing     { TNothing }
    turn        { TTurn }
    case        { TCase }
    of          { TOf }
    end         { TEnd }
    left        { TLeft }
    right       { TRight }
    front       { TFront }
    ';'         { TSemiColon }
    empty       { TEmpty }
    lambda      { TLambda }
    debris      { TDebris }
    asteroid    { TAsteroid }
    boundary    { TBoundary }
    '_'         { TUnderscore }
    ident       { TIdent $$ }

%%

Program : Rules  { Program $1 }

Rules : {- empty -}     { [] }
      | Rules Rule      { $2 : $1 }

Rule : ident "->" Cmds '.'  { Rule $1 $3 }

Cmds : {- empty -}      { [] }
     | Cmd              { [$1] }
     | Cmds ',' Cmd     { $3 : $1 }

Cmd : go                    { Go }
    | take                  { Take }
    | mark                  { Mark }
    | nothing               { Not }
    | turn Dir              { Turn $2 }
    | case Dir of Alts end  { Case $2 $4 }
    | ident                 { Ident $1 }

Dir : left      { Le }
    | right     { Ri }
    | front     { Fr }

Alts : {- empty -}      { [] }
     | Alt              { [$1] }
     | Alts ';' Alt     { $3 : $1 }

Alt : Pat "->" Cmds     { Alt $1 $3 }

Pat : empty     { Empty }
    | lambda    { Lambda }
    | debris    { Debris }
    | asteroid  { Asteroid }
    | boundary  { Boundary }
    | '_'       { Underscore }

{

happyError _ = error "parse error"

}