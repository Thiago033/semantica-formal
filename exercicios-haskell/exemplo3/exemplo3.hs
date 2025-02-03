data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq,Show)

arv1 :: Arvore
arv1 = Nodo 10 (Folha 9) (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6))

somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2*n)
multDoisArvore (Nodo n a1 a2) = Nodo (2*n) (multDoisArvore a1) (multDoisArvore a2)

multArvore :: Int-> Arvore-> Arvore
multArvore x (Folha n) = Folha (x * n)
multArvore x (Nodo n a1 a2) = Nodo (x * n) (multArvore x a1) (multArvore x a2)

contaFolhas :: Arvore-> Int
contaFolhas ( Folha _ ) = 1
contaFolhas (Nodo _ a1 a2) = contaFolhas a1 + contaFolhas a2

contaNodos :: Arvore-> Int
contaNodos ( Folha _ ) = 0
contaNodos (Nodo _ a1 a2) = 1 + contaNodos a1 + contaNodos a2

quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha n) = if x == n then 1 else 0
quantasVezes x (Nodo n a1 a2) = quantasVezes x a1 + quantasVezes x a2