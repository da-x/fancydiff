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

module Fancydiff.Lexer where

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

@hexnumber      = "0x" [0-9a-fA-F]+
@fracpart       = [ 0-9 ]+ ([e E] [\-]? [0-9]+ )?
@fracnumber1    = [0-9]+ [\.] @fracpart
@fracnumber2    = [\.] @fracpart
@fracnumber     = @fracnumber1 | @fracnumber2
@decnumber      = [0-9]+
@number         = ([\-])? (@hexnumber | @decnumber | @fracnumber)

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

@pythonRsv1 = and|as|assert|break|class|continue|def|del
@pythonRsv2 = elif|else|except|exec|False|finally|for|from
@pythonRsv3 = global|if|import|in|is|lambda|None|nonlocal
@pythonRsv4 = not|or|pass|print|raise|return|True|try|while|with|yield

@pythonRsv = @pythonRsv1 | @pythonRsv2 | @pythonRsv3 | @pythonRsv4

@golangRsv1 = break|default|func|interfaceselect
@golangRsv2 = case|defer|go|map|struct|continue|for|import|return|var
@golangRsv3 = chan|else|goto|package|switch
@golangRsv4 = const|fallthroughif|range|type

@golangRsv = @golangRsv1 | @golangRsv2 | @golangRsv3 | @golangRsv4

@rustRsv1 = abstract|alignof|as|become|box|break|const|continue|crate|do
@rustRsv2 = else|enum|extern|false|final|fn|for|if|impl|in|let|loop|macro
@rustRsv3 = match|mod|move|mut|offsetof|override|priv|proc|pub|pure|ref|return
@rustRsv4 = Self|self|sizeof|static|struct|super|trait|true|type|typeof
@rustRsv5 = unsafe|unsized|use|virtual|where|while|yield

@rustRsv = @rustRsv1 | @rustRsv2 | @rustRsv3 | @rustRsv4 | @rustRsv5

@javaRsv1 = abstract|assert|boolean|break|byte|case|catch|char|class|const
@javaRsv2 = continue|default|do|double|else|enum|extends|final|finally|float
@javaRsv3 = for|goto|if|implements|import|instanceof|int|interface|long|native
@javaRsv4 = new|package|private|protected|public|return|short|static|strictfp|super
@javaRsv5 = switch|synchronized|this|throw|throws|transient|try
@javaRsv6 = void|volatile|while

@javaRsv = @javaRsv1 | @javaRsv2 | @javaRsv3 | @javaRsv4 | @javaRsv5 | @javaRsv6

state:-

  <0>         @sp                     { tok       Ignore  	    }

  <str>       [\\] .                  { tok       String            }
  <str>       [\\] [ \n ]             { tok       String            }
  <str>       [\"]                    { tokPop    String            }
  <str>       [^ \\ \"]+              { tok       String            }

  <strbq>     [\\] .                  { tok       String            }
  <strbq>     [\\] [ \n ]             { tok       String            }
  <strbq>     [\`]                    { tokPop    String            }
  <strbq>     [^ \\ \`]+              { tok       String            }

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
  <clang>     @number                 { tok       Number            }
  <clang>     @cId                    { tok       Identifier        }
  <clang>     @punct                  { tok       Ignore            }
  <clang>     .                       { tok       Ignore            }

  <java>      @sp                     { tok       Ignore  	    }
  <java>      [\"]                    { tokPush   String    str     }
  <java>      [\']                    { tokPush   Char      charx   }
  <java>      "/*"                    { tokPush   Comment   ccomm   }
  <java>      "//"                    { tokPush   Comment   comm2   }
  <java>      @javaRsv                { tok       Keyword           }
  <java>      @number                 { tok       Number            }
  <java>      @cId                    { tok       Identifier        }
  <java>      @punct                  { tok       Ignore            }
  <java>      .                       { tok       Ignore            }

  <js>        @sp                     { tok       Ignore  	    }
  <js>        [\/] ([\[] ([^\\ \]]|[\\].) * [\]] | [^\/ \\] | [\\] .) + [\/]
  	                              { tok       String            }
  <js>        [\"]                    { tokPush   String    str     }
  <js>        [\']                    { tokPush   String    strsq   }
  <js>        "/*"                    { tokPush   Comment   ccomm   }
  <js>        "//"                    { tokPush   Comment   comm2   }
  <js>        @jsRsv                  { tok       Keyword           }
  <js>        @number                 { tok       Number            }
  <js>        @cId                    { tok       Identifier        }
  <js>        @punct                  { tok       Ignore            }
  <js>        .                       { tok       Ignore            }

  <python>    @sp                     { tok       Ignore  	    }
  <python>    [\"][\"][\"]            { tokPush   String    pmstr1  }
  <python>    [\'][\'][\']            { tokPush   String    pmstr2  }
  <python>    [\"]                    { tokPush   String    str     }
  <python>    [\']                    { tokPush   String    strsq   }
  <python>    "#"                     { tokPush   Comment   comm2   }
  <python>    @pythonRsv              { tok       Keyword           }
  <python>    @number                 { tok       Number            }
  <python>    @cId                    { tok       Identifier        }
  <python>    "@"                     { tok       Special2          }
  <python>    @punct                  { tok       Ignore            }
  <python>    .                       { tok       Ignore            }

  <golang>    @sp                     { tok       Ignore  	    }
  <golang>    [\"]                    { tokPush   String    str     }
  <golang>    [\`]                    { tokPush   String    strbq   }
  <golang>    [\']                    { tokPush   String    strsq   }
  <golang>    "/*"                    { tokPush   Comment   ccomm   }
  <golang>    "//"                    { tokPush   Comment   comm2   }
  <golang>    @golangRsv              { tok       Keyword           }
  <golang>    @number                 { tok       Number            }
  <golang>    @cId                    { tok       Identifier        }
  <golang>    @punct                  { tok       Ignore            }
  <golang>    .                       { tok       Ignore            }

  <rustlang>  @sp                     { tok       Ignore  	    }
  <rustlang>  [\"]                    { tokPush   String    str     }
  <rustlang>  [\']                    { tokPush   Char      charx   }
  <rustlang>  [\'] [a-z]+             { tok       Identifier        }
  <rustlang>  "/*"                    { tokPush   Comment   ccomm   }
  <rustlang>  "//"                    { tokPush   Comment   comm2   }
  <rustlang>  @rustRsv                { tok       Keyword           }
  <rustlang>  @number                 { tok       Number            }
  <rustlang>  @cId                    { tok       Identifier        }
  <rustlang>  @punct                  { tok       Ignore            }
  <rustlang>  .                       { tok       Ignore            }

  <pmstr1>    [\"][\"][\"]            { tokPop    String            }
  <pmstr1>    [\"][\"]                { tok       String            }
  <pmstr1>    [\"]                    { tok       String            }
  <pmstr1>    [ \n ]                  { tok       String            }
  <pmstr1>    [ [^ \"] \n ]+          { tok       String            }
  <pmstr2>    [\'][\'][\']            { tokPop    String            }
  <pmstr2>    [\'][\']                { tok       String            }
  <pmstr2>    [\']                    { tok       String            }
  <pmstr2>    [ [^ \'] \n ]+          { tok       String            }

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
  <haskell>   @number                 { tok       Number            }

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
