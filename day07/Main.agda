module Main where

import IO
import IO.Base
import Data.Nat.Show as ℕ
import Data.List.Effectful

open import Data.Nat.Base using (ℕ)
open import Data.Unit.Polymorphic.Base using (⊤)
open import Data.String as String using (String; _++_; wordsByᵇ; toList; lines)
open import Data.List.Base as List using (List; []; _∷_; any; filterᵇ; length; tails; map; null)
open import Data.Char using (Char; _≈ᵇ_)
open import Data.Maybe.Base using (Maybe; from-just; is-just; nothing; just)
open import Data.Bool using (Bool; _∨_; _∧_; not; false; true; if_then_else_)
open import Data.Product as Prod using (_×_; _,_)

open import Effect.Monad using (RawMonad)
open import Effect.Applicative using (RawApplicativeZero)

open import Agda.Builtin.IO using () renaming (IO to PrimIO)
open import Function.Base using (_$_; _∘_)
open import IO.Finite using (putStrLn; readFile)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Seems like we don't have a builtin way to get command-line arguments.
-- Fortunitely we can use Haskell FFI :-).

postulate
  getArgs : PrimIO (List String)

{-# FOREIGN GHC
  import qualified System.Environment as Env
  import qualified Data.Text as T

  getArgsText :: IO [T.Text]
  getArgsText = Env.getArgs >>= return . map T.pack

#-}
{-# COMPILE GHC getArgs = getArgsText #-}

Input : Set
Input = List (String)

module Parse where

  parseInput : String → Input
  parseInput = wordsByᵇ $ λ c → (c ≈ᵇ '[') ∨ (c ≈ᵇ ']')

  example : Input
  example = parseInput "abba[mnop]qrst"

  example¹ : Input
  example¹ = parseInput "aba[bab]xyz"

  _ : example ≡ "abba" ∷ "mnop" ∷ "qrst" ∷ []
  _ = refl

split : Input → Input × Input
split [] = [] , []
split (x ∷ []) = x ∷ [] , []
split (x ∷ y ∷ zs) with split zs
split (x ∷ y ∷ _) | a , b = (x ∷ a) , y ∷ b

_ : split Parse.example ≡ ("abba" ∷ "qrst" ∷ [] , "mnop" ∷ [])
_ = refl

module TLS where

  supportsTLS : Input → Bool
  supportsTLS xs with split xs
  supportsTLS xs | ss , hs = any hasABBA ss ∧ (not $ any hasABBA hs)
    where
      isABBA : (List Char) → Bool
      isABBA (a ∷ b ∷ c ∷ d ∷ _) = (a ≈ᵇ d) ∧ (b ≈ᵇ c) ∧ (not $ a ≈ᵇ b)
      isABBA _ = false
      hasABBA : String → Bool
      hasABBA ys = any isABBA (tails (toList ys))

  _ : supportsTLS Parse.example ≡ true
  _ = refl

module SSL where

  open Data.List.Effectful using (monad; applicativeZero)

  isPrefixOf : (List Char) → (List Char) → Bool
  isPrefixOf [] _ = true
  isPrefixOf _ [] = false
  isPrefixOf (x ∷ xs) (y ∷ ys) = (x ≈ᵇ y) ∧ isPrefixOf xs ys

  _isInfixOf_ : (List Char) → (List Char) → Bool
  needle isInfixOf haystack = any (isPrefixOf needle) (tails haystack)

  supportsSSL : Input → Bool
  supportsSSL xs with split xs
  supportsSSL xs | ss , hs =
    not $ null $ do
      s ← ss
      ts ← tails (toList s)
      (x , y , _) ← aba ts
      h ← hs
      guard $ (y ∷ x ∷ y ∷ []) isInfixOf (toList h)

    where
      open RawMonad monad
      open RawApplicativeZero applicativeZero

      aba : (List Char) → List (Char × Char × Char)
      aba (x ∷ y ∷ z ∷ _) with (x ≈ᵇ z) ∧ (not $ x ≈ᵇ y)
      aba (x ∷ y ∷ z ∷ _) | true  = (x , y , z) ∷ []
      aba (x ∷ y ∷ z ∷ _) | false = []
      aba _ = []

  _ : supportsSSL Parse.example¹ ≡ true
  _ = refl

module Solve where

  solve : (Input → Bool) → (List Input) → ℕ
  solve pred = length ∘ filterᵇ pred

module Entrypoint where
  open IO.Base using (IO; Main; _>>=_; _>>_; _<$>_; lift)
  open IO.List using (mapM′)

  solveFile : String → IO ⊤
  solveFile fp = do
    input ← (map Parse.parseInput ∘ lines) <$> readFile fp
    putStrLn $ "Solving for file : " ++ fp
    putStrLn $ "Part 1 : " ++ ℕ.show (Solve.solve TLS.supportsTLS input)
    putStrLn $ "Part 2 : " ++ ℕ.show (Solve.solve SSL.supportsSSL input)

  main : Main
  main = IO.run $ lift getArgs >>= mapM′ solveFile

