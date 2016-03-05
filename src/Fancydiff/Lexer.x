{

{-# LANGUAGE OverloadedStrings			#-}
{-# LANGUAGE NoMonomorphismRestriction	        #-}
{-# LANGUAGE CPP				#-}
{-# OPTIONS_GHC -fno-warn-unused-binds		#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures	#-}
{-# OPTIONS_GHC -fno-warn-unused-matches	#-}
{-# OPTIONS_GHC -fno-warn-unused-imports	#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing	#-}
{-# OPTIONS_GHC -fno-warn-tabs			#-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Fancydiff.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexMonadScan
  , alexSetStartCode
  , alexStructError
  , runAlex
  , clang
  , haskell
  , js
  )
where

import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Fancydiff.Data (Element(..))
}

%wrapper "monadUserState-bytestring"

$space = [ \ \t ]
$eol   = \n

$letter		= [a-zA-Z]
$digit		= 0-9
$digitzero    	= 0-9
$digit  	= 1-9
$digitoct	= 0-7
$hexdigit	= [0-9a-fA-F]

$cidentletter	= [a-zA-Z_\$]

$validstr       = [^ \\ \" \n ]
$validchar      = [^ \\ \' \n ]
$nonwhitspace   = . # $white

@sp             = $white+
@punct          = [ \^ \~
   		\[ \] \( \) \- \+
		\{ \} \\ \: \?
		\! \@ \# \$ \% \;
		\&    \, \/ \| \.
                \` \" \< \> \* \= ]
@cId            = [ a-z A-Z 0-9 \_ \$ ]+
@haskTypeCtr    = [A-Z_][a-z A-Z 0-9 \_ ']*
@haskBind       = [a-z_][a-z A-Z 0-9 \_ ']*

@clangRsv1 = _Pragma|__attribute__|asm|auto|break|case|char|const
@clangRsv2 = continue|default|define|do|double|else|endif|enum
@clangRsv3 = extern|float|for|goto|if|ifdef|ifndef|include|include_once
@clangRsv4 = inline|int|int|long|pragma|register|return|short
@clangRsv5 = signed|sizeof|static|struct|switch|typedef|undef
@clangRsv6 = union|unsigned|void|volatile|while
@clangRsvA = @clangRsv1|@clangRsv2|@clangRsv3
@clangRsvB = @clangRsv4|@clangRsv5|@clangRsv6
@clangRsv  = @clangRsvA|@clangRsvB

@haskellRsv1 = as|case|class|data|data|default|deriving|do|else|family
@haskellRsv2 = forall|foreign|hiding|if|import|import|import|in|infix
@haskellRsv3 = infixl|infixr|instance|let|let|mdo|module|newtype|of
@haskellRsv4 = proc|qualified|rec|then|type|where
@haskellRsv  = @haskellRsv1|@haskellRsv2|@haskellRsv3|@haskellRsv4

@jsRsv1 = abstract|arguments|boolean|break|byte|case|catch|char
@jsRsv2 = class|const|continue|debugger|default|delete|do|double
@jsRsv3 = else|enum|eval|export|extends|false|final|finally
@jsRsv4 = float|for|function|goto|if|implements|import|in
@jsRsv5 = instanceof|int|interface|let|long|native|new|null
@jsRsv6 = package|private|protected|public|return|short|static|super
@jsRsv7 = switch|synchronized|this|throw|throws|transient|true|try
@jsRsv8 = typeof|var|void|volatile|while|with|yield

@jsRsv = @jsRsv1|@jsRsv2|@jsRsv3|@jsRsv4|@jsRsv5|@jsRsv6|@jsRsv8

state:-

  <0>         @sp                     { tok       Ignore  	    }

  <str>       [\\] .                  { tok       String            }
  <str>       [\\] [ \n ]             { tok       String            }
  <str>       [\"]                    { tokPop    String            }
  <str>       [^ \\ \"]+              { tok       String            }

  <strsq>     [\\] .                  { tok       String            }
  <strsq>     [\']                    { tokPop    String            }
  <strsq>     [^ \\ \']+              { tok       String            }

  <charx>     [\\] .                  { tok       Char              }
  <charx>     [\']                    { tokPop    Char              }
  <charx>     [\n]                    { tokPop    Char              }
  <charx>     [^ \\ \']+              { tok       Char              }

  <ccomm>      "*/"                   { tokPop    Comment           }
  <ccomm>      [ [^ \*] \n]+          { tok       Comment           }
  <ccomm>      [\*]                   { tok       Comment           }
  <comm2>     [ ^ \n ]*\n?            { tokPop    Comment           }
  <doccomm2>  [ ^ \n ]*\n?            { tokPop    DocComment        }

  <clang>     @sp                     { tok       Ignore  	    }
  <clang>     [\"]                    { tokPush   String    str     }
  <clang>     [\']                    { tokPush   Char      charx   }
  <clang>     "/*"                    { tokPush   Comment   ccomm   }
  <clang>     "//"                    { tokPush   Comment   comm2   }
  <clang>     @clangRsv               { tok       Keyword           }
  <clang>     "0x" [ 0-9 a-f      ]+  { tok       Number            }
  <clang>     [0-9]+ [\.] [ 0-9 ]+    { tok       Number            }
  <clang>     [\.] [ 0-9 ]+           { tok       Number            }
  <clang>     [ 0-9               ]+  { tok       Number            }
  <clang>     @cId                    { tok       Identifier        }
  <clang>     @punct                  { tok       Ignore            }
  <clang>     .                       { tok       Ignore            }

  <js>        @sp                     { tok       Ignore  	    }
  <js>        [\/] [\[] [\/] [^ \/]+ [\/]
  	                              { tok       String            }
  <js>        [\/] [^ \/]+ [\/]       { tok       String            }
  <js>        [\"]                    { tokPush   String    str     }
  <js>        [\']                    { tokPush   String    strsq   }
  <js>        "/*"                    { tokPush   Comment   ccomm   }
  <js>        "//"                    { tokPush   Comment   comm2   }
  <js>        @jsRsv                  { tok       Keyword           }
  <js>        "0x" [ 0-9 a-f      ]+  { tok       Number            }
  <js>        [0-9]+ [\.] [ 0-9 ]+    { tok       Number            }
  <js>        [\.] [ 0-9 ]+           { tok       Number            }
  <js>        [ 0-9               ]+  { tok       Number            }
  <js>        @cId                    { tok       Identifier        }
  <js>        @punct                  { tok       Ignore            }
  <js>        .                       { tok       Ignore            }

  <haskell>   @sp+                    { tok       Ignore  	    }
  <haskell>   [\"]                    { tokPush   String   haskstr  }
  <haskell>   [\']                    { tokPush   Char     charx    }
  <haskell>   "-- |"                  { tokPush   DocComment  doccomm2 }
  <haskell>   "--"                    { tokPush   Comment  comm2    }
  <haskell>   [\{] [\-] [\#]          { tokPush   Special2 hpragma  }
  <haskell>   [\{] [\-] " " "|"       { tokPush   DocComment  hmlcomm2 }
  <haskell>   [\{] [\-]               { tokPush   Comment  hmlcomm  }
  <haskell>   [\[] @haskBind [\|]     { tokPush   String   haskqq   }
  <haskell>   @haskellRsv             { tok       Keyword           }

  <hpragma>   [\#] [\-] [\}]          { tokPop    Special2          }
  <hpragma>   [\n]                    { tok       Special2          }
  <hpragma>   [^ \#]+                 { tok       Special2          }
  <hpragma>   .                       { tok       Special2          }

  <hmlcomm>   [\-] [\}]               { tokPop    Comment           }
  <hmlcomm>   [\n]                    { tok       Comment           }
  <hmlcomm>   [^ \n \-]+              { tok       Comment           }
  <hmlcomm>   .                       { tok       Comment           }

  <hmlcomm2>  [\-] [\}]               { tokPop    DocComment        }
  <hmlcomm2>  [^ \n \-]+              { tok       DocComment        }
  <hmlcomm2>  [\n]                    { tok       DocComment        }
  <hmlcomm2>  .                       { tok       DocComment        }

  <haskell>   \- \>                   { tok       Special           }
  <haskell>   \< \-                   { tok       Special           }
  <haskell>   \= \>                   { tok       Special2          }
  <haskell>   \: \:                   { tok       Special3          }
  <haskell>   [ \[ \] ]               { tok       Brackets          }
  <haskell>   [ \( \) ]               { tok       Parentheses       }
  <haskell>   [ \{ \} ]               { tok       Curly             }
  <haskell>   \$                      { tok       Special           }
  <haskell>   \=                      { tok       Special           }

  <haskell>   @punct                  { tok       Ignore            }

  <haskell>   "0x" [ 0-9 a-f     ]+   { tok       Number            }
  <haskell>   [0-9]+ [\.] [ 0-9  ]+   { tok       Number            }
  <haskell>   [\.] [ 0-9         ]+   { tok       Number            }
  <haskell>   [ 0-9              ]+   { tok       Number            }

  <haskstr>   [\\] .                  { tok       String            }
  <haskstr>   [\\] [\n]               { tok       String            }
  <haskstr>   [\"]                    { tokPop    String            }
  <haskstr>   [\n]+                   { tok       String            }
  <haskstr>   [^ \\ \"]+              { tok       String            }

  <haskqq>    [\|] [\]]               { tokPop    String            }
  <haskqq>    [^ \|]+                 { tok       String            }
  <haskqq>    [\n]+                   { tok       String            }
  <haskqq>    .                       { tok       String            }

  <haskell>   @haskTypeCtr            { tok       Type              }
  <haskell>   @haskBind               { tok       Identifier        }
  <haskell>   .                       { tok       Ignore            }
{

-- Some action helpers:

tok x ((AlexPn _ line column), _, input, _) len = do
    return $ Token (TokenDemark line column x) (B.take (fromIntegral len) input)

tokPush cls code inp len = do
    alexGetStartCode >>= pushStateStack
    alexSetStartCode code >> tok cls inp len
    where pushStateStack :: Int -> Alex ()
          pushStateStack state = Alex $ \s@AlexState{alex_ust=ust} -> Right (s {alex_ust = state:ust}, ())

tokPop  cls inp len = do
    popStateStack >>= alexSetStartCode >> tok cls inp len
    where popStateStack :: Alex Int
          popStateStack = Alex $ \s@AlexState{alex_ust=ust} -> Right (s {alex_ust = tail ust}, head ust)

alexStructError (line, column, e) = alexError $ "show-error: " ++ (show (line, column, e))
token_fail e ((AlexPn _ line column), _, input) len = alexStructError (line, column, e :: String)

-- The token type:
data Token = Token TokenClass B.ByteString
  deriving (Show)

data TokenClass
 = TokenDemark !Int !Int !Element
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token TokenEOF ""

type AlexUserState = [Int]
alexInitUserState = []
}
