data Temperatura = Frio | Calor
    deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
    deriving(Eq,Show)

tempo :: Estacao-> Temperatura
tempo Verao = Calor
tempo _ = Frio