-- ------------------------------------------------------------------
data Proposicion = 
    Constante Bool 
  | Variable String
  | Negacion Proposicion
  | Conjuncion Proposicion Proposicion
  | Disyuncion Proposicion Proposicion
  | Implicacion Proposicion Proposicion
  | Equivalencia Proposicion Proposicion
  deriving (Show, Eq)

(¬) :: Proposicion -> Proposicion
(¬) prop = Negacion prop
infix ¬

(/\) :: Proposicion -> Proposicion -> Proposicion
prop1 /\ prop2 = Conjuncion prop1 prop2
infixl 7 /\

(\/) :: Proposicion -> Proposicion -> Proposicion
prop1 \/ prop2 = Disyuncion prop1 prop2
infixl 6 \/

(>=>) :: Proposicion -> Proposicion -> Proposicion
prop1 >=> prop2 = Implicacion prop1 prop2
infixr 5 >=>

(<=>) :: Proposicion -> Proposicion -> Proposicion
prop1 <=> prop2 = Equivalencia prop1 prop2
infixl 4 <=>

-- ------------------------------------------------------------------
p = Variable "p" 
q = Variable "q" 
r = Variable "r" 
prop1 = Conjuncion p q 
prop2 = Disyuncion p q
prop3 = Disyuncion p (Negacion p)
prop4 = Implicacion p q