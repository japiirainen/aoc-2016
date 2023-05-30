{-# LANGUAGE StrictData #-}
module Main where

import Control.Lens
import Control.Monad.State
import Data.Vector (Vector)
import Data.Set (Set)
import Data.List (find)
import Data.Foldable (for_)
import Text.Read (readMaybe)
import System.Environment (getArgs)

import qualified Data.Set as Set
import qualified Data.Vector as Vector

data Register = A | B | C | D
  deriving (Show)

data Registers = Registers { _a, _b, _c, _d :: Int }
  deriving (Show, Eq, Ord)

makeLenses ''Registers

class HasRegisters a where
  reg :: Functor f => Register -> LensLike' f a Int

instance HasRegisters Registers where
  reg A = a
  reg B = b
  reg C = c
  reg D = d

data Value = Int Int | Reg Register
  deriving (Show)

data Instr
  = Copy Value Register
  | Inc Register
  | Dec Register
  | Jnz Value Value
  | Out Value
  deriving (Show)

parseRegister :: String -> Register
parseRegister "a" = A
parseRegister "b" = B
parseRegister "c" = C
parseRegister "d" = D
parseRegister r = error ("Unknown register : " <> r)

parseValue :: String -> Value
parseValue s = maybe (Reg (parseRegister s)) Int (readMaybe s)

parseInstruction :: [String] -> Instr
parseInstruction ["cpy", v, r]  = Copy (parseValue v) (parseRegister r)
parseInstruction ["inc", r]  = Inc (parseRegister r)
parseInstruction ["dec", r]  = Dec (parseRegister r)
parseInstruction ["jnz", v, v']  = Jnz (parseValue v) (parseValue v')
parseInstruction ["out", v]  = Out (parseValue v)
parseInstruction i = error ("Unknown instruction : " <> unwords i)

data Next = One | Zero

data Machine = Machine
  { _registers :: Registers
  , _progress :: Next
  , _targets :: (Set (Int, Registers))
  }

makeLenses ''Machine

instance HasRegisters Machine where
  reg r = registers . reg r

initMachine :: Machine
initMachine = Machine (Registers 0 0 0 0) Zero mempty

rval :: (MonadState r m, HasRegisters r) => Value -> m Int
rval = \case
  Int i -> pure i
  Reg r -> use (reg r)

exec :: Vector Instr -> Int -> Bool
exec prog init = evalState entrypoint initMachine
  where
    entrypoint :: State Machine Bool
    entrypoint = reg A .= init >> goto 0

    step ip = \case
      Inc r -> reg r += 1 >> goto (ip + 1)
      Dec r -> reg r -= 1 >> goto (ip + 1)
      Copy i o -> reg o <~ rval i >> goto (ip + 1)
      Jnz i o -> do
        v <- rval i
        o' <- rval o
        goto (ip + (if v == 0 then 1 else o'))
      Out o -> do
        v <- rval o
        p <- use progress
        case (p, v) of
          (One, 1) -> progress .= Zero >> goto (ip + 1)
          (Zero, 0) -> do
            rs <- use registers
            ts <- use targets
            let now = (ip, rs)
            if Set.member now ts
            then pure True
            else do
                targets . contains now .= True
                progress .= One
                goto (ip + 1)
          _ -> pure False

    goto ip = case prog Vector.!? ip of
      Nothing -> pure False
      Just o -> step ip o

main :: IO ()
main = do
  args <- getArgs
  for_ args $ \fp -> do
    prog <- Vector.fromList . map (parseInstruction . words) . lines <$> readFile fp
    print (find (exec prog) [1..])
