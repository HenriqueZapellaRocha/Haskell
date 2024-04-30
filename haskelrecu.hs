--primeira questao, converter binario para decimal
bin2dec :: [Int] -> Int
bin2dec n | null n = 0
                     | otherwise = 2 ^ (length n - 1) * (head n) + bin2dec (tail n)


--segunda questao, converter decimal para bianrio recebdno quantidade de bits para a representacao
dec2bin :: Int -> Int -> [Int]
dec2bin _ 0 = []
dec2bin n bits
  | n < 0 || bits <= 0 = error "Número de bits ou valor decimal inválido."
  | otherwise = adicionaZeros (toBinary n) bits

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary 1 = [1]
toBinary n = toBinary (n `div` 2) ++ [n `mod` 2]

adicionaZeros :: [Int] -> Int -> [Int]
adicionaZeros binary bits
--caso o numero de binary seja maior que a quantidade de bits especificado na chamada
  | length binary > bits = error "Número de bits insuficiente para representar o valor."
--caso o numero de binary seja menor que a quantidade de bits especificado na chamada
  | length binary <= bits = replicate (bits - length binary) 0 ++ binary


--terceira questao
-- Função auxiliar para inverter os bits de um inteiro
bincompl2dec :: [Int] -> Int
bincompl2dec [] = 0
bincompl2dec (x:xs) 
                                | x == 0 = bin2dec (x:xs) -- caso seja positivo e necessario apenas chamar a funcao da primeira questao
                                | otherwise = -1 * (bin2dec(flipBitsWrapper(x:xs))) -- -1 serve para deixar o valor negativo e o resto e uma composicao de chamadas de funcao

invertBits :: Int -> Int
invertBits n = 
    if n == 1 then
        0
    else 
        1

--inverte o numeor binario e envia para a funcao que faz o complemento de dois
flipBitsWrapper :: [Int] -> [Int]
flipBitsWrapper xs = reverse(flipBits (reverse xs) False)

--gera o comeplemento de dois invertendo todos os bits depois do primeiro bit sinal 1
flipBits :: [Int] -> Bool -> [Int]
flipBits [] _ = []
flipBits (x:xs) bool 
    | bool = invertBits x : flipBits xs bool
    | x == 1 = 1 : flipBits xs True 
    | x == 0 = 0 : flipBits xs bool


--quarta questao


dec2bincompl :: Int -> Int -> [Int]
dec2bincompl 0 _ = []
dec2bincompl n l | n > 0 = verificaIncioPositivo(dec2bin n l)
                              | otherwise = adicionaUms (flipBitsWrapper (toBinary(abs n))) l
  
                         

verificaIncioPositivo :: [Int] -> [Int]
verificaIncioPositivo [] = []
verificaIncioPositivo (x:xs) 
                            | x > 0 = error "Número de bits insuficiente para representar o valor."
                            | otherwise = (x:xs)

adicionaUms :: [Int] -> Int -> [Int]
adicionaUms binary bits
--caso o numero de binary seja maior que a quantidade de bits especificado na chamada
  | length binary > bits = error "Número de bits insuficiente para representar o valor."
--caso o numero de binary seja menor que a quantidade de bits especificado na chamada
  | length binary <= bits = replicate (bits - length binary) 1 ++ binary

--quinta questao
somarbin :: [Int] -> [Int] -> Int -> [Int] 
somarbin [] [] _ = []
somarbin xs ys n 
                  | length xs /= n = error "Número de bits insuficiente para representar o valor."
                  | length ys /= n = error "Número de bits insuficiente para representar o valor."
                  | otherwise = (reverse (take n (somaBianrios xs  ys 0)))

somaBianrios :: [Int] -> [Int] -> Int -> [Int] 
somaBianrios [] [] carry = [carry]
somaBianrios (x:xs) (e:es) carry = xor(xor x e) carry : somaBianrios xs es (calculaCarry x e carry)

calculaCarry :: Int -> Int -> Int -> Int
calculaCarry x y carry 
                      | x + y + carry == 3 = 1
                      | x + y + carry == 2 = 1
                      | otherwise = 0 

xor :: Int -> Int -> Int
xor x y | x == 1 && y == 1 = 0
        | x == 0 && y == 1 = 1 
        | x == 1 && y == 0 = 1
        | x == 0 && y == 0 = 0
