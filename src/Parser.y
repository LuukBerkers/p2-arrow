{
module Parser
    ( parser
    ) where

import Model
}

%name parser
%tokentype { Token }

%token
  "->"      { TArrow }
  '.'       { TDot }
  ','       { TComma }
  go        { TGo }
  ident     { TIdent $$ }

%%

Program : Rules  { Program $1 }

Rules : {- empty -}     { [] }
      | Rules Rule      { $2 : $1 }

Rule : ident "->" Cmds '.'  { Rule $1 $3 }

Cmds : {- empty -}      { [] }
     | Cmd              { [$1] }
     | Cmds ',' Cmd     { $3 : $1 }

Cmd : go    { Go }

{

happyError _ = error "parse error"

}