{ name = "AOC-2016 day 12"
, dependencies = [ "prelude"
                 , "console"
                 , "effect"
                 , "node-process"
                 , "node-fs"
                 , "node-buffer"
                 , "arrays"
                 , "tuples"
                 , "exceptions"
                 , "maybe"
                 , "integers"
                 , "strings"
                 , "foldable-traversable"
                 ]
, packages = ./packages.dhall
, sources = [ "src/Main.purs" ]
}
