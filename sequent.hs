import Data.List
import Data.Maybe


-- Add every and exist
data Formula = Var String
             | And  Formula  Formula
             | Or   Formula  Formula
             | Impl Formula  Formula
             deriving(Eq)

type Formulae = [Formula]

data Sequent = Sequent Formulae Formulae

data ProofTree = ProofTree Sequent Rule [ProofTree]

-- Todo : rules
rules :: Sequent -> [([Sequent], Rule)]

