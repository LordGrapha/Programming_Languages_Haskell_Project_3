module Sintax where

-- ------------------------------------------------------------------
data Proposicion = 
    Constante Bool 
  | Variable String
  | Negacion Proposicion
  | Conjuncion Proposicion Proposicion
  | Disyuncion Proposicion Proposicion
  | Implicacion Proposicion Proposicion
  | Equivalencia Proposicion Proposicion
  deriving Show

-- ------------------------------------------------------------------
p = Variable "p" 
q = Variable "q" 
r = Variable "r" 
prop1 = Conjuncion p q 
prop2 = Disyuncion p q
prop3 = Disyuncion p (Negacion p)
prop4 = Implicacion p q