# Functional programming. Assignment # 3, Interpolation.

## Описание задания, цели и требования

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.
В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую аппроксимации (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма аппроксимирования и выводимых данных должны задаваться через аргументы командной строки:
    - какие алгоритмы использовать (в том числе два сразу);
    - частота дискретизации результирующих данных;
    - и т.п.;
- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

## Реализация

### Интерполяция по Лагранжу

```haskell
lagrange :: Double -> [(Double, Double)] -> (Double, Double)
lagrange x xys =
  let len = length xys
      numerator index = product $ map (\i -> x - fst (xys !! i)) $ filter (/= index) [0 .. len - 1]
      denominator index = product $ map (\i -> fst (xys !! index) - fst (xys !! i)) $ filter (/= index) [0 .. len - 1]
   in (x, sum [snd (xys !! i) * numerator i / denominator i | i <- [0 .. len - 1]])
```

### Интерполяция по Ньютону

```haskell
newton :: Double -> [(Double, Double)] -> (Double, Double)
newton x xys = helper 0 0 1 xys
  where
    helper _ res _ [] = (x, res)
    helper ind res prod (xy' : xys') = helper (ind + 1) (res + prod * dividedDifference ind 0 xys) (prod * (x - fst xy')) xys'

dividedDifference :: Int -> Int -> [(Double, Double)] -> Double
dividedDifference 0 ind xys = snd $ xys !! ind
dividedDifference degree ind xys =
  (dividedDifference (degree - 1) (ind + 1) xys - dividedDifference (degree - 1) ind xys)
    / (fst (xys !! (ind + degree)) - fst (xys !! ind))
```

### "Движок" вычислений

```haskell
cliRoutine :: Double -> Int -> [(Double, Double)] -> Bool -> IO ()
cliRoutine step' window' vals instart = do
  maybePoint <- maybeReadPoint
  case maybePoint of
    Just pt -> start $ vals ++ [pt]
    Nothing -> do
      interpolateInInterval lagrangeList vals (fst $ head vals) (fst $ last vals) step'
  where
    start :: [(Double, Double)] -> IO ()
    start vals'
      | length vals' < window' = cliRoutine step' window' vals True
      | otherwise = do
          let middleX = (fst (head vals') + fst (last vals')) / 2
          if instart
            then do
              interpolateInInterval lagrangeList vals' (fst $ head vals') (fst $ last vals') step'
            else do
              interpolateInInterval lagrangeList vals' middleX middleX 1
          cliRoutine step' window' (drop 1 vals') False
```

Парсинг аргументов/параметров командной строки можно найти в app/Main.hs
Тривиальные тесты на "правильность" реализации алгоритмов интерполяции можно найти в test/Test.hs

## Пример работы программы

Числа вводятся по одной в строку, абсцисса и ордината разделены знаком ';'.
Формат вывода: (x, interpolated y)

```sh
$ ./fp3-exe --step=1 --window=3 --method=lagrange
Step: 1.0
Window: 3
Method: ["lagrange"]
2;4
4;8
6;12
(2.0,4.0)
(3.0,6.0)
(4.0,8.0)
10;20
(7.0,14.0)
15;30
(10.5,21.0)
18;36
(14.0,28.0)
# --- Ctrl + D ---
(15.0,30.0)
(16.0,32.0)
(17.0,34.0)
(18.0,36.0)
```

Также допускается ввод нескольких методов одновременно, для этого нужно повторить параметр `--method`, например:
```
./fp3-exe --step=1 --window=3 --method=lagrange --method=newton
```

Программа выдаст сообщение об ошибке если:
- не указан параметр `--step`
- не указан параметр `--window`
- не указан хотя бы единожды параметр `--method``
- в параметре `--method` указан несуществующий метод

## Выводы

В ходе л.р. были изучены основы работы с IO в Haskell, пререквизитом к чему стало знакомство с понятием "монада" и основами работы с монадами в Haskell. Мой тривиальный императивный мозг пребывает в легком шоке.