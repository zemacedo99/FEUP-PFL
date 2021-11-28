# FEUP-PFL

- a descrição de vários casos de teste para todas as funções;

- uma explicação sucinta do funcionamento de cada função;

- as estratégias utilizadas na implementação das funções da alínea 2;

- a resposta à alínea 4


- Funções e Casos-Teste
  - BigNumber.hs
    - FibRecBN: 
    - divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
    - mulBN :: BigNumber -> BigNumber -> BigNumber
    - subBN :: BigNumber -> BigNumber -> BigNumber
    - somaBN :: BigNumber -> BigNumber -> BigNumber
    - 
  - Fib.hs
    - fibRec :: (Integral a) => a -> a
    - fibLista2 :: Num a => Int -> a
    - fibLista :: Num b => Int -> b
    - fibListaInfinita :: Num a => Int -> a
    - fibRecBN :: String -> String
      - fibRecBN' :: BigNumber -> BigNumber
        - [fibRecBn (show n) | n <- [0..30]]
    - fibListaInfinitaBN :: Int -> String
      - fibListaInfinitaBN' :: Int -> BigNumber

- 