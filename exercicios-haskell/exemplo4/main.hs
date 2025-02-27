data N = Zero | Suc N deriving(Eq,Show)

soma :: N-> N-> N
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2)

mult :: N -> N-> N
mult Zero n = Zero
mult (Suc n1) n2 = soma(mult n1 n2) n2

leq :: N -> N-> Bool
leq Zero = True
leq n != Zero = False
leq (Suc n1) (Suc n2) = leq(n1 n2)