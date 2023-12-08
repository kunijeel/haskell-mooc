module Examples.QuickCheck_222230 where

import Data.Char
import Test.QuickCheck

{-
<작성한 명세>

하스켈 프로그래밍을 통해 카이사르 암호 프로그램을 만들거야 소문자만을 암호화하고 그 외의 문자는 그대로 둘거야 그리고 이 프로그램은 총 5개의 함수로 구성될 예정이야 

먼저 'a'에서 'z' 사이의 소문자를 그에 대응하는 0에서 25사이의 정수로 바꾸는 "let2int" 함수를 만들어줘 함수의 인자로 소문자만 전달될거고 type annotation은 let2int :: Char -> Int 야
-}

let2int :: Char -> Int
let2int c 
  | c >= 'a' && c <= 'z' = ord c - ord 'a'
  | otherwise = -1

-- ASCII 소문자만을 생성하는 제너레이터
asciiLowerChar :: Gen Char
asciiLowerChar = elements ['a'..'z']

-- 소문자에 대한 테스트 케이스
prop_Let2IntLowercase :: Property
prop_Let2IntLowercase = forAll asciiLowerChar $ \c ->
  let2int c == ord c - ord 'a'

-- 비소문자에 대한 테스트 케이스
prop_Let2IntNonLowercase :: Char -> Property
prop_Let2IntNonLowercase c = not (isLower c) ==> let2int c == -1

{-
ChatGPT가 카이사르 암호 프로그램을 이해할 수 있도록 자세히 설명했기에 한번의 질문으로 "let2int" 함수를 정확히 만들었다.

작성한 테스트 케이스는 소문자와 비소문자에 대해 정상적으로 작동하는지를 확인하기 위한 목적으로 만들었고 적절하다고 생각한다. '\42866'과 같은 값들이 이상하게도 소문자로
인식되는 경우가 있어서 추가적으로 "asciiLowerChar"를 작성해 소문자에 대해서만 검사가능하도록 만들었다.

ChatGPT 활용 코딩의 장점은 빠른속도로 코드를 작성해준다는 점이 있고 단점은 질문안에서 하스켈 프로그래밍이라고 명시했지만 다른 프로그래밍 언어로 설명하는 경우가 있다.
따라서 ChatGPT가 정확히 인식할 수 있도록 한 질문안에 여러번 언급해주는 부분이 번거로웠다.

ghci> quickCheck prop_Let2IntLowercase 
+++ OK, passed 100 tests.
ghci> quickCheck prop_Let2IntNonLowercase 
+++ OK, passed 100 tests; 14 discarded.
-}

{-
<작성한 명세>

다음으로는 "int2let" 함수를 만들어줘 이 함수는 0에서 25 사이의 정수를 그에 대응하는 'a'에서 'z' 사이의 소문자로 바꾸는 함수야 "let2int"와 반대 역할을 한다고 생각하면 될 것 같아 type annotation은 int2let :: Int -> Char이야
-}

int2let :: Int -> Char
int2let n
  | n >= 0 && n <= 25 = chr (n + ord 'a')
  | otherwise = '?'

-- 0에서 25 사이의 정수에 대한 테스트 케이스
prop_Int2LetValid :: Int -> Property
prop_Int2LetValid n = (n >= 0 && n <= 25) ==> int2let n == chr (n + ord 'a')

-- 범위 밖의 정수에 대한 테스트 케이스
prop_Int2LetInvalid :: Int -> Property
prop_Int2LetInvalid n = (n < 0 || n > 25) ==> int2let n == '?'

{-
이번에도 한 번의 질문으로 "int2let" 함수를 완성했다.

작성한 테스트 케이스는 해당 범위 사이일때와 범위 밖일때 각각 정상적으로 작동하는지 확인하는 용도로 작성했고 적절하다고 생각한다.

ChatGPT 활용 코딩의 장점은 이전의 질문을 모두 기억하고 있다는 것이다. 바로 직전 질문에서 "let2int"를 구현해 달라고 요청했기에 비교적 짧고 간결하게 "int2let" 함수
구현을 요청했다. 단점은 이전 질문에서 ChatGPT가 만약 잘못 이해한 부분이 있다면 사용자가 직접 이를 지적하고 수정요구를 해야한다는 점이다.

ghci> quickCheck prop_Int2LetValid 
+++ OK, passed 100 tests; 350 discarded.
ghci> quickCheck prop_Int2LetInvalid 
+++ OK, passed 100 tests; 71 discarded.
-}

{-
<작성한 명세>

이제 방금 만든 두 함수를 활용해서 "shift" 함수를 만들어줘 이 함수는 전달받은 문자를 숫자로 변환하고 그 숫자에 치우침 인자를 더해 26으로 나누어 나머지를 구해 그리고 그 나머지 정수값을 다시 소문자로 바꿔 반환하는 역할을 할거야 

type annotation은 shift :: Int -> Char -> Char 이고 이 함수는 양과 음의 치우침 인자를 모두 받을 수 있어 또한 소문자가 아닌 문자가 들어오면 그대로 반환해야해 

예를 들어 shift 3 'a' 는 'd', shift 3 'z' 는 'c', shift (-3) 'c' 는 'z', shift 3 ' ' 는 ' ' 이야
-}

shift :: Int -> Char -> Char
shift n c
  | c >= 'a' && c <= 'z' = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

-- 소문자에 대한 테스트 케이스
prop_ShiftLowercase :: Int -> Property
prop_ShiftLowercase n = forAll asciiLowerChar $ \c -> 
  shift n c == int2let ((let2int c + n) `mod` 26)

-- 비소문자에 대한 테스트 케이스
prop_ShiftNonLowercase :: Int -> Char -> Property
prop_ShiftNonLowercase n c = not (c >= 'a' && c <= 'z') ==> shift n c == c

-- 알파벳 순환 테스트 케이스
prop_ShiftWrapAround :: Int -> Property
prop_ShiftWrapAround n = forAll asciiLowerChar $ \c ->
  shift (n + 26) c == shift n c

-- 치우침 인자가 음수일 때 테스트 케이스
prop_ShiftNegative :: Int -> Property
prop_ShiftNegative n = forAll asciiLowerChar $ \c -> 
  shift (-n) c == int2let ((let2int c - n) `mod` 26)

-- 치우침 인자의 반복적 적용 테스트
prop_ShiftRepeated :: Int -> Property
prop_ShiftRepeated n = forAll asciiLowerChar $ \c ->
  shift n (shift n c) == shift (2 * n) c

-- 치우침 인자의 취소 테스트
prop_ShiftCancel :: Int -> Property
prop_ShiftCancel n = forAll asciiLowerChar $ \c ->
  shift (-n) (shift n c) == c

-- 치우침 인자가 0일 때 테스트 케이스
prop_ShiftZero :: Property
prop_ShiftZero = forAll asciiLowerChar $ \c ->
  shift 0 c == c

-- 극단적인 치우침 인자 테스트
prop_ShiftExtreme :: Int -> Property
prop_ShiftExtreme n = forAll asciiLowerChar $ \c ->
  let largeShift = n * 1000
  in shift largeShift c == int2let ((let2int c + largeShift) `mod` 26)

{-
"shift" 함수는 한 번의 질문을 통해 완성했다.

"shift" 함수는 비교적 여러 부분을 통해 검증이 가능해 여러가지 테스트 케이스를 작성해보았다. 각각의 테스트 케이스들은 함수의 다양한 측면을 
검토하고 완성도 높은 구현을 달성할 수 있도록 도움을 준다고 생각한다.

ChatGPT 활용 코딩의 장점은 이전에 만든 "let2int" 와 "int2let" 함수를 사용해 사용자가 요청하는 함수를 작성해 준다는 점이고 단점은 크게 느끼지 못했다.

ghci> quickCheck prop_ShiftLowercase 
+++ OK, passed 100 tests.
ghci> quickCheck prop_ShiftNonLowercase 
+++ OK, passed 100 tests; 17 discarded.
ghci> quickCheck prop_ShiftWrapAround 
+++ OK, passed 100 tests.
ghci> quickCheck prop_ShiftNegative 
+++ OK, passed 100 tests.
ghci> quickCheck prop_ShiftRepeated 
+++ OK, passed 100 tests.
ghci> quickCheck prop_ShiftCancel 
+++ OK, passed 100 tests.
ghci> quickCheck prop_ShiftZero 
+++ OK, passed 100 tests.
ghci> quickCheck prop_ShiftExtreme 
+++ OK, passed 100 tests.
-}

{-
<작성한 명세>

이제 "encode" 함수를 만들어야해 이 함수는 주어진 문자열을 치우침 인자에 맞게 암호화하는 함수야 방금 만든 "shift" 함수를 활용하면 만들 수 있을거야 문자열은 소문자만 암호화하는걸 잊지말고 type annotation은 encode :: Int -> String -> String 이야 

encode 3 "haskell is fun" 은 "kdvnhoo lv ixq" 이고 encode (-3) "kdvnhoo lv ixq"는 "haskell is fun" 이야
-}

encode :: Int -> String -> String
encode n s = [shift n c | c <- s]

-- 문자열 암호화 및 복호화 테스트
prop_EncodeDecode :: Int -> String -> Property
prop_EncodeDecode n s = 
  let encoded = encode n s
      decoded = encode (-n) encoded
  in decoded === s

-- 소문자가 아닌 문자 처리 테스트
prop_EncodeNonLowercase :: Int -> String -> Property
prop_EncodeNonLowercase n s = 
  all (not . isLower) s ==> encode n s === s

{-
"encode" 함수는 한 번의 질문을 통해 완성했다.

문자열 암호화가 잘 작동하는지 또 소문자가 아닌 문자에 대해서는 암호화가 이루어지지 않는지를 확인하는 테스트 케이스로 "encode" 함수의 기능을
확인하기에 적절하다고 생각한다.

ChatGPT 활용 코딩의 장점은 "let2int", "int2let", "shift" 함수를 만들면서 쌓인 정보로 사용자에게 더 높은 품질의 답안을 제공한다는 점이고 단점은 느끼지 못했다.

ghci> quickCheck prop_EncodeDecode 
+++ OK, passed 100 tests.
ghci> quickCheck prop_EncodeNonLowercase 
+++ OK, passed 100 tests; 537 discarded.
-}

{-
<작성한 명세>

마지막으로 "decode" 함수를 작성해줘 이 함수는 치우침 인자와 문자열을 주면 해당 암호를 해독하는 역할을 하는 함수야 
-}

decode :: Int -> String -> String
decode n s = encode (-n) s

-- 암호화 후 복호화 테스트
prop_DecodeEncode :: Int -> String -> Property
prop_DecodeEncode n s = 
  let encoded = encode n s
      decoded = decode n encoded
  in decoded === s

-- 소문자가 아닌 문자 처리 테스트
prop_DecodeNonLowercase :: Int -> String -> Property
prop_DecodeNonLowercase n s = 
  all (not . isLower) s ==> decode n s === s

{-
"decode" 함수 역시 한번의 질문을 통해 완성했다.

"encode" 함수와 비슷한 방식으로 복호화가 정상적으로 작동하는지 확인하는 테스트 케이스와 소문자가 아닌 문자에 대해 복호화가 진행되지 않는 것을 확인하는
테스트 케이스를 작성했고 적절하다고 생각한다.

"encode" 함수를 만들어 달라고 요청했을때 ChatGPT에게 요청하지 않았지만 알아서 "decode" 함수를 만들어 주었다. 따라서 ChatGPT 활용 코딩의 장점은 사용자가 
무엇을 원하는지 알아서 판단하고 필요할 것으로 예상되는 부분을 요청하지 않아도 제공해준다는 것이다. 단점은 느끼지 못했다.

ghci> quickCheck prop_DecodeEncode 
+++ OK, passed 100 tests.
ghci> quickCheck prop_DecodeNonLowercase 
+++ OK, passed 100 tests; 654 discarded.
-}