module Main where

import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Effect.Console (log)
import Data.Traversable (for_)
import Data.String (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Array (drop, fromFoldable, filter, (!!))
import Data.Int (fromString)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Node.Process (argv)
import Node.FS.Sync (readFile)
import Node.Encoding (Encoding(UTF8))

import Node.Buffer as Buf

type Registers = { a :: Int , b :: Int , c :: Int , d :: Int }

zeroRegisters :: Registers
zeroRegisters = { a: 0, b: 0, c: 0, d: 0 }

data Register = A | B | D | C

apReg :: Register -> Registers -> (Int -> Int) -> Registers
apReg A rs f = rs { a = (f rs.a) }
apReg B rs f = rs { b = (f rs.b) }
apReg C rs f = rs { c = (f rs.c) }
apReg D rs f = rs { d = (f rs.d) }

regAt :: Register -> Registers -> Int
regAt A rs = rs.a
regAt B rs = rs.b
regAt C rs = rs.c
regAt D rs = rs.d

instance showRegister :: Show Register where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"

data Value = Int Int | Reg Register

instance showValue :: Show Value where
  show (Int i) = "Int" <> " " <> show i
  show (Reg r) = "Reg" <> " " <> show r

data Instr
  = Cpy Value Register
  | Inc Register
  | Dec Register
  | Jnz Value Int

instance showInstr :: Show Instr where
  show (Cpy v r) = "Cpy" <> " " <> show v <> " " <> show r
  show (Inc r) = "Inc" <> " " <> show r
  show (Dec r) = "Dec" <> " " <> show r
  show (Jnz v i) = "Jnz" <> " " <> show v <> " " <> show i

type Program = Array Instr

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

parseInstructions :: Array String -> Program
parseInstructions lines = map parseLine (map (split (Pattern " ")) lines)
  where
    parseLine ["cpy", val, reg] = Cpy (pVal val) (pReg reg)
    parseLine ["inc", reg] = Inc (pReg reg)
    parseLine ["jnz", val, i] = Jnz (pVal val) (pInt i)
    parseLine ["dec", reg] = Dec (pReg reg)
    parseLine _ = error "bad input"

    pInt s = case fromString s of
      Nothing -> error "bad input"
      Just i -> i

    pVal s = case fromString s of
      Nothing -> Reg (pReg s)
      Just i -> Int i

    pReg "a" = A
    pReg "b" = B
    pReg "c" = C
    pReg "d" = D
    pReg _ = error "bad input"

exec :: Program -> Int -> Int
exec program c = (entry zeroRegisters).a
  where
    entry registers = go (registers { c = c }) 0

    go :: Registers -> Int -> Registers
    go rs pc = case program !! pc of
                 Nothing -> rs -- out of bounds means HALT
                 Just i -> let (Tuple r' pc') = execInstr i in go r' pc'

      where execInstr :: Instr -> (Tuple Registers Int)
            execInstr (Cpy v r) = Tuple (apReg r rs (\_ -> (val v rs))) (pc + 1)
            execInstr (Inc r) = Tuple (apReg r rs (\x -> x + 1)) (pc + 1)
            execInstr (Dec r) = Tuple (apReg r rs (\x -> x - 1)) (pc + 1)
            execInstr (Jnz v i) = Tuple rs (pc + (if (val v rs) == 0 then 1 else i))

    val :: Value -> Registers -> Int
    val v rs = case v of
      Int i -> i
      Reg r -> regAt r rs

main :: Effect Unit
main = do
  fps <- (drop 2 <<< fromFoldable) <$> argv
  for_ fps \fp -> do
    log ("Solving for file : " <> fp)
    input <- readFile fp >>= Buf.toString UTF8
    let parse = parseInstructions <<< filter (\x -> x /= "") <<< fromFoldable <<< split (Pattern "\n")
    let prog = parse input
    log (show (exec prog 0))
    log (show (exec prog 1))
