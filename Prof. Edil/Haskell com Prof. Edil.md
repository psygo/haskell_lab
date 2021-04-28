# Haskell com Prof. Edil

Tags: Haskell, Functional Programming

["Programando" em cálculo lambda - YouTube](https://www.youtube.com/watch?v=cEsWWAnZhGM&list=PLfdR3_dt2rbeTo6kFqKww_AXpjbn-VioQ&index=5)

[Programação Funcional com Haskell](https://www.notion.so/Programa-o-Funcional-com-Haskell-3a250210f2f745959e8d674f638e17b6)

[Alan Turing on Computability and Intelligence | University of Oxford Podcasts - Audio and Video Lectures](http://podcasts.ox.ac.uk/series/alan-turing-2018)

- Não há diferença entre dado e função, especialmente no que diz respeito à tipagem.
- Há duas funções `curry` e `uncurry`.
- Tipo produto: `data TipoLouco = Tag Int Char`
- Tipo soma (união): `data TipoLouco = L1 | L2`
- Os construtores são a projeção do `TipoLouco` em diferentes planos/subconjuntos.
- Existe um tipo dos tipos: `Kind`
- Abstrair é jogar informação fora, simplificar.
- No vídeo de prova por indução:
    - (1:42:00) É possível utilizar provas para talvez sintetizar algo mais difícil e não trivial somente com o seu programa.
        - Se você quiser sintetizar uma nova função, utilize ferramentas já existentes na prova, para reaproveitar o que você já fez, e possivelmente facilitar o processo.
- Listas
    - Adicionar elementos é muito fácil e rápido, mas procurar é muito mais custoso.
    - `map f (map g xs) = map f . map g = map (f . g)`
    - `map (+) [0,1,2] = [(+0), (+1), (+2)]`
        - Use `$` para finalizar as operações: `map ($ 10) (map (+) [0,1,2])`
    ```hs
    -- `b` will be the value of the empty list.
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr f acc []     = acc
    foldr f acc (x:xs) = f x (foldr f acc xs)
    -- foldr (*) 1 [1,2,3]
    -- (*)1 ((*)2 ((*3) (foldr (*) 1 [])))
    ```
    - For performance, prefer `foldr` instead of `foldl`.
    - Map-Reduce -> `fold` + `map`.
- Álgebra
    - Operações binárias para navegar dentro de um conjunto.
        - Operações fechadas dentro do conjunto.
            - Magma ou grupóide.
        - Semigrupo: operações associativas.
            - `(a <> b) <> c = a <> (b <> c)`
            - Excelente para paralelismo.
            - `mappend = <> = redução = fusão`
        - Monóide: semigrupo + identidade
        - Functor: álgebra onde se pode mapear funções sobre estruturas, sem alterar a estrutura.
            - Os dados podem mudar (tipos também), mas a estrutura permanece a mesma (não é possível deletar itens por exemplo)
            - Sinônimo de `fmap`: `lift`.
            - É possível utilizar funções comuns, em itens individuais, para funcionar sobre estruturas.
            - `fmap, <$> :: (a -> b) -> f a -> f b`
                - `<$ :: a -> f b -> f a`
            - Leis:
                1. Identidade: `fmap id = id`
                1. Composição: `fmap f . fmap g = fmap (f . g)`
            ```hs
            instance functor Maybe where
              fmap :: (a -> b) -> Maybe a -> Maybe b
              fmap f Nothing  = Nothing
              fmap f (Just x) = Just (f x)

            instance functor [] where
              fmap = map
            ```
        - Aplicativo
            - Functor -> Aplicativo -> Mônada
            - Leis dos Aplicativos:
                1. `pure id <*> x = x`
                1. `pure g  <*> pure x = pure (g $ x)`
                1. `mf <*> pure y = pure ($ y) <*> mf`
                1. `x <*> (y <*> z) = ((.) <$> x <*> y) <*> z = (pure (.) <*> x <*> y) <*> z`
            - Como criar funções `fmap` com diferentes números de argumentos? Basta:
                ```hs
                pure :: a -> f a
                <*>  :: f (a -> b) -> f a -> f b
                <$>  ::   (a -> b) -> f a -> f b

                -- Ex.:

                1.
                pure (+1)  <*> x
                f (a -> b)     f a -> f b

                2.
                pure f        <*> x <*> y
                (a -> b -> c)     f a   fb -> fc
                ```
            - Exemplos concretos
                ```hs
                -- O Aplicativo cobre o caso em que os dados já estão estruturados, o que é uma restrição além do functor (`f a`).

                instance Functor Maybe where
                  -- fmap :: (a -> b) -> Maybe a -> Maybe b
                  fmap f Nothing  = Nothing
                  fmap f (Just x) = Just (f x)

                instance Applicative Maybe where
                  pure :: a -> Maybe a
                  pure x           = Just x

                  <*> :: Maybe (a -> b) -> Maybe a -> Maybe b
                  Nothing  <*> mx = Nothing
                  (Just f) <*> mx = fmap f mx

                instance Applicative [] where
                  pure x = [x]
                  -- (<*>) :: [a -> b] -> [a] -> [b]
                  gs <*> xs = [ g x | g <- gs, x <- xs ]
                ```
        - Mônadas
            - O inuito é *composição* de funções, dentro de estruturas.
                - Composição demanda que a ordem importe.
            - Leis:
                1. Identidade do *bind* à esquerda: `return x >>= f = f x`
                1. Identidade do *bind* à direita: `m x >>= return = m x`
                1. `(m x >>= f) >>= g = m x >>= (\a -> (f a >>= g))`
            - Assinatura:
                - `\a -> f a >>= \a' -> g a'`
                - `(>>=) :: M a -> (a -> M b) -> M b`
            ```hs
            data Expr = Val Int | Div Expr Expr

            eval :: Expr -> Maybe Int
            eval (Val n)     = Just n
            eval (Div e1 e2) = case eval e1 of
                                 Nothing  -> Nothing
                                 (Just x) -> case eval e2 of
                                   Nothing  -> Nothing
                                   (Just y) -> safediv x y

            -- Outra maneira, mas os tipos não batem:
            eval (Val n)     = pure n
            eval (Div e1 e2) = pure safediv <*>
                               eval e1 <*>
                               eval e2

            -- Abstrair:
            bind = mx >>= f = \mx f -> case mx of
              Nothing -> Nothing
              Just x  -> f x

            -- Com mônadas:
            eval (Val n)     = Just n
            eval (Div e1 e2) = eval e1 >>= \x ->
                               eval e2 >>= \y ->
                               safediv x y

            -- Com notação `do`
            eval (Val n) = return n
            eval (Div e1 e2) = do
                                  x <- eval e1
                                  y <- eval e2
                                  safediv x y

            safediv :: Int -> Int -> Maybe Int
            safediv _ 0 = Nothing
            safediv n d = Just (n `div` d)
            ```

## Simulando Estado em Haskell

```hs
-- ST = State Transformer
newtype ST s a = ST { app :: s -> (a,s) }
-- f :: Char -> ST s a = Char -> s -> (a,s)

instance Functor (ST s) where
  -- fmap :: (a -> b) -> ST s a -> ST s b
           = (a -> b) -> (s -> (a,s)) -> (s -> (b,s))

  fmap f st = ST $ \s ->
                let (a,s') = app st s
                in (f a, s')
  -- Ex.:
  -- app (ord <$> (counter 'x')) S1

instance Applicative (ST s) where
  -- pure :: a -> ST s a
  pure x = ST $ \s -> (x,s)

  -- (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  (ST f) <*> (ST x) = ST $ \s ->
                        let (fa, s')  = f s
                            (xa, s'') = x s'
                        in  (fa xa, s'')

instance Monad (ST s) where
  -- (>>=) :: ST s a -> (a -> ST s b) -> ST s b
  st >>= f = ST $ \s ->
               let (xa, s')   = app st s
               in  app (f xa) s'
```

## Lazy Evaluation

Lazy Evaluation = outermost + sharing

> Use `:sprint` with `:set -XMonomorphismRestriction` on GHCi to examine evaluation.

`length` for example evaluates the length but not each item, it only counts boxes.

Memoização = caching.

`seq` avalia até a *Weak Head Normal Form* &mdash; até o primeiro construtor que você encontrar. Se você quiser realmente ir até o final, utilize `deepSeq`.

`!(MyList' a)` é a notação de avaliação estrita.
