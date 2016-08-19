module Operations 
( Stack
, Operation(..)
, operations
) where

type Stack = [Double]

data Operation =
  Operation { token        :: String
            , function     :: Stack -> Stack
            , minSize      :: Int
            , requirements :: [Stack -> Maybe String]
            }

operations :: [(String, Operation)]
operations = map (\op -> (token op, op))
  [ Operation 
    { token        = "+"
    , function     = \(x1:x2:xs) -> x2+x1:xs
    , minSize      = 2
    , requirements = []
    }
  , Operation
    { token        = "-"
    , function     = \(x1:x2:xs) -> x2-x1:xs
    , minSize      = 2
    , requirements = []
    }
  , Operation
    { token        = "*"
    , function     = \(x1:x2:xs) -> x2*x1:xs
    , minSize      = 2
    , requirements = []
    }
  , Operation
    { token        = "/"
    , function     = \(x1:x2:xs) -> x2/x1:xs
    , minSize      = 2
    , requirements = [\(x1:xs) -> requires (x1 /= 0) "Cannot divide by 0."]
    }
  , Operation
    { token        = "**"
    , function     = \(x1:x2:xs) -> x2**x1:xs
    , minSize      = 2
    , requirements = []
    }
  , Operation
    { token        = "ln"
    , function     = \(x1:xs) -> log x1:xs
    , minSize      = 1
    , requirements = [\(x1:xs) -> requires (x1 > 0) "ln undefined for values < = 0"]
    }
  , Operation
    { token        = "exp"
    , function     = \(x1:xs) -> exp x1:xs
    , minSize      = 1
    , requirements = []
    }
  , Operation
    { token        = "sin"
    , function     = \(x1:xs) -> sin x1:xs
    , minSize      = 1
    , requirements = []
    }
  , Operation
    { token        = "cos"
    , function     = \(x1:xs) -> cos x1:xs
    , minSize      = 1
    , requirements = []
    }
  , Operation
    { token        = "e"
    , function     = \xs -> exp 1:xs
    , minSize      = 0
    , requirements = []
    }
  , Operation
    { token        = "pi"
    , function     = \xs -> pi:xs
    , minSize      = 0
    , requirements = []
    }
  , Operation
    { token        = "swap"
    , function     = \(x1:x2:xs) -> x2:x1:xs
    , minSize      = 2
    , requirements = []
    }
  , Operation
    { token        = "drop"
    , function     = \(x1:xs) -> drop (floor x1) xs
    , minSize      = 1
    , requirements = []
    }
  ]

requires :: Bool -> String -> Maybe String
requires p e = if p then Nothing else Just e
