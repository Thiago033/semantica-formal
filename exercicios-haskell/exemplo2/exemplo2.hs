data Forma = Circulo Float |Retangulo Float Float
    deriving(Eq,Show)

redondo :: Forma-> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma-> Float
area (Circulo r) = pi * r *r
area (Retangulo b a) = b * a