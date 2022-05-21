
``` {.haskell .invisible}
{-# LANGUAGE TypeOperators, StandaloneDeriving #-}
module Chapters.Basics where

import Data.Char (ord, chr)

import Prelude ()
import Common.MiniPrelude hiding (until, gcd)
```

# 值、函數、與定義 {#ch:basics}

語言是概念的載體。
如第\@ref{ch:intro}章所述，程式語言不僅用來表達概念，也用於演算，分擔我們思考的負擔。
在本書中，為便於精確說明，我們也不得不選一個語言。

本書中使用的語言大致上基於 Haskell, 但為適合本書使用而經過簡化、修改。
我們將在本章初步介紹 Haskell 語言的一小部分，包括在 Haskell 中何謂「計算」、值與函數的定義、常見的資料結構，以及串列上的常用函數。
目的是讓讀者具備足夠的基本概念，以便進入之後的章節。
也因此，本書中所介紹的語言並非完整的 Haskell, 本書也不應視作 Haskell 語言的教材。
對於有此需求的讀者，我將在本章結尾推薦一些適合的教科書。

## 值與求值 {#sec:evaluation}

Haskell 是個可被編譯(compile)、也可被直譯(interpret)的語言。Haskell 直譯器延續了 LISP 系列語言的傳統，是個「讀、算、印 (read, evaluate, pring)」的迴圈 --- 電腦從使用者端讀取一個算式，算出結果，把結果印出，然後再等使用者輸入下一個算式。一段與 Haskell 直譯器的對話可能是這樣：
```spec
Main> 3+4
7
Main> sum [1..100]
5050
Main>
```
{.nobreak}在此例中，|Main>| 是 Haskell 直譯器的提示符號。
^[此處的人機互動紀錄擷取自 GHCi (Glasgow Haskell Compiler Interactive). GHC 為目前最被廣泛使用的 Haskell 實作。]
使用者輸入 |3+4|, Haskell 算出其值 |7| 並印出.
接著，使用者想得知 |1| 到 |100| 的和，Haskell 算出 |5050|.

上例中的算式只用到 Haskell 已知的函數（如|(+)|, |sum|等）。
使用者若要自己定義新函數，通常得寫在另一個檔案中，命令 Haskell 直譯器去讀取。
例如，我們可把如下的定義寫在一個檔案中：
```spec
double :: Int -> Int
double x = x + x {-"~~."-}
```
{.nobreak}上述定義的第一行是個型別宣告。當我們寫 |e :: t|, 代表 |e| 具有型別 |t|.
\index{::@@{|(::)|}}
|Int| 是整數的型別，而箭號 |(->)| 為函數型別的建構元。第一行 |double :: Int -> Int| 便是告知電腦我們將定義一個新識別字 |double|, 其型別為「從整數(|Int|)到整數的函數」。
^[Haskell 標準中有多種整數，其中 |Int| 為有限大小（通常為該電腦中一個*字組*(word)）的整數，|Integer| 則是所謂的*大整數*或*任意精度整數*，無大小限制。本書中只使用 |Int|.]
至於該函數的定義本體則在第二行 |double x = x + x|, 告知電腦「凡看到 |double x|, 均可代換成 |x + x|.」

{title="求值"}第\@ref{ch:intro}章曾提及：一套程式語言是設計者看待世界的抽象觀點。程式通常用來表達計算，因此程式語言也得告訴我們：在其假想的世界中，「計算」是怎麼一回事。
指令式語言的世界由許多容器般的變數組成，計算是將值在變數之間搬來搬去。
邏輯編程\index{logic programming 邏輯編程}中，描述一個問題便是寫下事物間的邏輯關係，計算是邏輯規則「歸結(resolution)」\index{logic programming 邏輯編程!resolution 歸結}的附帶效果。
共時(concurrent)\index{concurrency 共時}程式語言著眼於描述多個同時執行的程式如何透過通道傳遞訊息，計算也靠此達成。
::: {.infobox title="演算格式"}
::: {.texonly}
```
%format expr0
%format expr1
%format expr2
%format exprn = "\Varid{expr}_{n}"
%format reason0
%format reason1
```
:::
本書中將使用如下的格式表達（不）等式演算或推論：
``` spec
   expr0
 =   { reason0 }
   expr1
 >=  { reason1 }
   expr2
   :
 = exprn {-"~~."-}
```
這是一個 |expr0 >= exprn| 的證明。式子 |expr0 .. exprn| 用具有遞移律的運算子(如|(=)|, |(>=)|等等)串起。放在大括號中的是註解，例如 |reason0| 是 |expr0 = expr1| 的原因，|reason1| 是 |expr1 >= expr2| 的原因。

根據 @Snepscheut:93:What[p19], 此格式最早由 W.H.J. Feijen 所建議。
:::

函數語言中，一個程式便是一個數學式，而「計算」便是依照該式子中各個符號的定義，反覆地展開、歸約，直到算出一個「值」為止。
這個過程又稱作「求值(evaluation)」\index{evaluation 求值}。
在 Haskell 直譯器中，若 |double| 的定義已被讀取，輸入 |double (9 * 3)|, 電腦會算出 |54|:
```spec
Main> double (9 * 3)
54
```
但 |54| 這個值是怎麼被算出來的？以下是其中一種可能：
```spec
  double (9 * 3)
=   {- |(*)| 的定義 -}
  double 27
=   {- |double| 的定義 -}
  27 + 27
=   {- |(+)| 的定義 -}
  54 {-"~~."-}
```
{.nobreak}上述演算的第一步將 |9*3| 歸約成 |27| --- 我們尚未定義 |(*)| 與 |(+)|, 但目前只需知道它們與大家所熟悉的整數乘法、加法相同。
第二步將 |double 27| 變成 |27 + 27|, 根據的是 |double| 的定義：|double x = x + x|.
最後，|27 + 27| 理所當然地歸約成 |54|.
「歸約」\index{reduction 歸約}一詞由 $\beta$-reduction 而來，在此處指將函數本體中的形式參數代換為實際參數。%
^[Reduction 的另一個常見譯名是「化簡」，然而，許多情況下，一個式子被 reduce 後變得更長而不「簡」，因此本書譯為「歸約」。]
在上述例子中，我們遇到如 |double (9*3)| 的函數呼叫，先將參數 (|9*3|)化簡，再展開函數定義，可說是*由內到外*的歸約方式。
大部分程式語言都依這樣的順序求值，讀者可能也對這種順序較熟悉。

但這並不是唯一的求值順序。我們能否由外到內，先把 |double| 展開呢？
```spec
  double (9 * 3)
=   {- |double| 的定義 -}
  (9 * 3) + (9 * 3)
=   {- |(*)| 的定義 -}
  27 + (9 * 3)
=   {- |(*)| 的定義 -}
  27 + 27
=   {- |(+)| 的定義 -}
  54 {-"~~."-}
```
{.nobreak}以這個順序求值，同樣得到 |54|.

一般說來，一個式子有許多種可能的求值順序：可能是由內往外、由外往內、或其他更複雜的順序。
我們自然想到一個問題：這些不同的順序都會把該式子化簡成同一個值嗎？
有沒有可能做出一個式子，使用一個順序會被化簡成 |54|, 另一個順序化簡成 |53|?

我們看看如下的例子。
函數 |three| 顧名思義，不論得到什麼參數，都傳回 |3|；|inf| 則是一個整數：
```spec
three :: Int -> Int
three x = 3 {-"~~,"-}

inf :: Int
inf = 1 + inf {-"~~."-}
```
{.nobreak}在指令式語言中，|inf| 的定義可能會被讀解為「將變數 |inf| 的值加一」。
但函數語言中「變數」的值是不能更改的。
此處的意義應是：|inf| 是一個和 |1 + inf| 相等的數值。
我們來看看 |three inf| 會被為甚麼？
如果我們由內往外求值：
```spec
  three inf
=   {- |inf| 的定義 -}
  three (1 + inf)
=   {- |inf| 的定義 -}
  three (1 + (1 + inf))
=   {- |inf| 的定義 -}
  ...
```
{.nobreak}看來永遠停不下來！但如果我們由外往內，|three inf| 第一步就可變成 |3|：
```spec
  three inf
=   {- |three| 的定義 -}
  3 {-"~~."-}
```
{.nobreak}我們該如何理解、討論這樣的現象呢？

{title="範式與求值順序"}
為描述、討論相關的現象，我們得非正式地介紹一些術語。
用較直觀、不形式化的方式理解，一個式子中「接下來可歸約之處」稱作其*歸約點(redex)*\index{redex 歸約點}。例如|(9*3) + (4*6)|中，|9*3| 與 |4*6| 都是歸約點。
如果一個式子已沒有歸約點、無法再歸約了，我們說該式已是個*範式 (normal form)*\index{normal form 範式}。

回頭看，經由之前的例子我們已得知：

  * 有些式子有範式(如 |double (9*3)| 有個範式 |54|)，有些沒有（如 |inf|）。
  * 同一個式子可用許多順序求值。有些求值順序會碰到範式，有些不會。

{.nobreak}給一個式子，我們很自然地希望知道它有沒有值，並算出其值。如果一個式子有很多個範式，我們便難說哪一個才是該式子的「值」。如此一來，立刻衍生出幾個問題。給定一個式子，我們是否能得知它有沒有範式呢？如果有，用哪個求值順序才能得到那個範式？以及，有沒有可能用一個求值順序會得到某範式，換另一個求值順序卻得到另一個範式？

很不幸地，第一個問題的答案是否定的：沒有一套固定的演算法可判定任意一個式子是否有範式。
這相當於計算理論中常說到的*停機問題(halting problem)* --- 沒有一個演算法能準確判斷任意一個演算法（對某個輸入）是否能正常終止。

但對於另兩個問題，計算科學中有個重要的*Church-Rosser 定理*\index{Church-Rosser Theorem}。
非常粗略地說，該定理告訴我們：在我們目前討論的這類語言中%
^[此處討論的可粗略說是以 $\lambda$-calculus 為基礎的函數語言。基於其他概念設計的程式語言當然可能不遵守 Church-Rosser 定理。]

  * 一個式子*最多只有一個*範式。
  * 如果一個式子有範式，使用由外到內的求值順序可得到該範式。

{.nobreak}如此一來，給定任一個式子，我們都可用由外到內的方式算算看。
假設一算之下得到（例如）|54|。
用其他的求值順序可能得不到範式，但若有了範式，必定也是 |54|.
如果由外到內的順序得不到範式，用其他任何順序也得不到。

由於「由外到內」的求值順序有「最能保證得到範式」的性質，又被稱作「*範式順序* (*normal order evaluation*)」\index {evaluation 求值!normal order 範式順序}。
「由內到外」的則被稱作「*應用順序*(*applicative order evaluation*)」\index {evaluation 求值!applicative order 應用順序}。
以本書的目的而言，我們可大致把 Haskell 使用的求值方式視為範式順序。
但請讀者記得這是個粗略、不盡然正確的說法 --- Haskell 實作上使用的求值方式經過了更多最佳化。
正式的 $\lambda$-calculus 教科書中對於歸約點、求值順序、Church-Rosser 定理等概念會有更精確的定義。

{title="被迫求值"}
型別 |Bool| 表示真假，有兩個值 |False| 和 |True|。
常用的函數 |not| 可定義如下：
```spec
not :: Bool -> Bool
not False  = True
not True   = False {-"~~."-}
```
{.nobreak}此處 |not| 的定義依輸入值不同而寫成兩個條款。
這種定義方式在 Haskell 中稱作*樣式配對*(*pattern matching*)：
|False| 與 |True| 在此處都是樣式 (patterns)。\index{pattern matching 樣式配對}
遇到這樣的定義時，Haskell 將輸入*依照順序*與樣式們一個個比對。
如果對得上，便傳回相對應的右手邊。
本例中，若輸入為 |False|，傳回值為 |True|，否則傳回值為 |False|。

我們來看看 |not (5 <= 3)| 該怎麼求值？若依照範式順序，照理來說應先將 |not| 的定義展開。
但若不知 |5 <= 3| 的值究竟是 |False| 還是 |True|, 我們不知該展開哪行定義！
因此，要計算 |not (5 <= 3)|，也只好先算出 |5 <=3| 了：
```spec
  not (5 <= 3)
=   {- |(<=)| 之定義 -}
  not False
=   {- |not| 之定義 -}
  True {-"~~."-}
```

求值過程中若必須得知某子算式的值才能決定如何進行，只好先算那個子算式。
在 Haskell 中有不少（有清楚定義的）場合得如此，
包括遇上|(<=)|、|(>=)| 等運算子、樣式配對、|case| 算式（將於第\@ref{sec:boolean}節中介紹）...等等。

::: {.exlist}
::: {.exer}
定義一個函數 |myeven :: Int -> Bool|，判斷其輸入是否為偶數。
你可能用得到以下函數：^[此處所給的並非這些函數最一般的型別。]
```spec
mod   :: Int -> Int -> Int {-"~~,"-}
(==)  :: Int -> Int -> Bool {-"~~."-}
```
其中 |mod x y| 為 |x| 除以 |y| 之餘數，|(==)| 則用於判斷兩數是否相等。
:::
::: {.exans .compact}
```spec
myeven   :: Int -> Bool
myeven x = x `mod` 2 == 0  {-"~~."-}
```
:::
::: {.exer}
定義一個函數 |area :: Float -> Float|, 給定一個圓的半徑，
計算其面積。（可粗略地將 |22/7| 當作圓周率。）
:::
::: {.exans .compact}
```spec
area    :: Float -> Float
area r  = (22/7) * r * r {-"~~."-}
```
:::
:::

{title="惰性求值" #para:lazy-evaluation}
\index{evaluation 求值!lazy evaluation 惰性求值}
實作上，Haskell 求值的方式還經過了更多的最佳化：
例如將歸約過的算式改寫為它的值，避免重複計算。
這套求值方式稱為*惰性求值*(*lazy evaluation*).

技術上說來，惰性求值和範式順序求值並不一樣。
但前者可視為後者的最佳化實作 --- 惰性求值的結果必須和範式順序求值相同。
因此，在本書之中大部分地方可忽略他們的差異。
和惰性求值對偶的是*及早求值*(*eager evaluation*)
\index{evaluation 求值!eager evaluation 及早求值}，
可視為應用順序求值的實作 --- 在呼叫一個函數之前，總是把其參數先算成範式。
這也是一般程式語言較常見的計算方法。

本書中談到偏向實作面的議題時會用「惰性求值/及早求值」，
在談不牽涉到特定實作的理論時則使用「範式順序求值/應用順序求值」。

## 函數定義 {#sec:function-defns}

考慮如下的函數定義：
```spec
smaller :: Int -> Int -> Int
smaller x y = if x <= y then x else y {-"~~."-}
```
我們可粗略地理解為：|smaller| 是一個函數，拿兩個參數 |x| 與 |y|，傳回其中較小的那個。
如 |smaller (double 6) (3+4)| 的值為 |7|.

::: {.exlist}
::: {.exer}
用前一節介紹的求值順序，將|smaller (double 6) (3+4)|化簡為範式。
:::
:::

{title="守衛"} 如果函數本體做的第一件事就是條件判斷，Haskell 提供另一種語法：
```spec
smaller :: Int -> Int -> Int
smaller x y  | x <= y  = x
             | x > y   = y {-"~~."-}
```
{.nobreak}這麼寫出的 |smaller| 的行為仍相同：如果 |x <= y| 成立，傳回 |x|；如果 |x > y|, 傳回 |y|。
但這種語法較接近一些數學教科書中定義函數的寫法。
其中，放在 |x <=y| 和 |x > y| 等位置的必須是型別為 |Bool| 的算式。
它們擋在那兒，只在值為 |True| 的時候才讓執行「通過」，因此被稱為*守衛*(*guard*)\index{guard 守衛}。
如果有三個以上的選項，如下述例子，使用守衛比 |if .. then.. else| 更清晰可讀：
```spec
sign :: Int -> Int
sign x  | x > 0   = 1
        | x == 0  = 0
        | x < 0   = -1 {-"~~."-}
```

遇到數個守衛時，Haskell 將*依照順序*嘗試每個選項，直到碰到第一個為真的守衛，然後只執行該項定義。
也就是說， 即使我們把 |sign| 定義中的 |x == 0| 改為 |x >= 0|, |sign 10| 的值仍會是 |1|。
若每個守衛都是 |False|, 程式則將中斷執行（並傳回一個錯誤訊息）。
在許多程式中，我們會希望最後一個守衛能捕捉所有的漏網之魚：如果其他條款都不適用，就執行最後一個。一種做法是讓一個守衛是 |True|。或著，在 Haskell 中有個 |otherwise| 關鍵字可讓定義讀來更口語化些：
```spec
sign :: Int -> Int
sign x  | x > 0      = 1
        | x == 0     = 0
        | otherwise  = -1 {-"~~."-}
```
事實上，|otherwise| 的定義就是 |otherwise = True|.

{title="區域識別字"} Haskell 提供兩種宣告區域識別字的語法：|let| 與 |where|.
也許最容易理解的方式是用例子說明。
::: {.example #ex:payment}
工讀生每小時的時薪為新台幣 130 元。
假設一週有五個工作天，每天有八小時上班時間。
定義一個函數 |payment :: Int -> Int|，輸入某學生工作的週數，計算其薪資。
:::
::: {.answer}
我們當然可直接用一個式子算出薪資。但為清楚起見，我們可用兩個區域識別字 |days| 和 |hours|,
分別計算該學生工作的日數和時數。如果用 |let|, 可這麼做。
```spec
payment :: Int -> Int
payment weeks =  let  days   = 5 * weeks
                      hours  = 8 * days
                 in 130 * hours {-"~~."-}
```
{.nobreak}|let| 算式的語法為
```spec
  let  x1 = e1
       x2 = e2 ....
  in e {-"~~."-}
```
{.nobreak}其中 |e| 為整個算式的值，而 |x1|, |x2| 等等為區域識別字。兩個區域識別字的有效範圍包括 |e|, 以及 |e1| 與 |e2|。

另一種語法是 |where| 子句。若用它定義 |payment|, 可寫成這樣：
```spec
payment :: Int -> Int
payment weeks = 130 * hours {-"~~,"-}
  where  hours  = 8 * days
         days   = 5 * weeks {-"~~."-}
```
:::

該用 |let| 或是 |where|?
大部份時候這可依個人習慣，看寫程式的人覺得怎麼說一件事情比較順。
使用 |let| 時，敘事的順序是由小到大，先給「工作日數」、「工作時數」等小元件的定義，再用他們組出最後的式子 |130 * hours|。
使用 |where| 時則是由大到小，先說我們要算「工作時數乘以 130」，然後補充「其中，工作時數的定義是...」。

但，Haskell 之所以保留了兩種語法，是為了因應不同的用途。語法上，|let| 是一個算式，可出現在算式中。如下的算式是合法的，其值為 |24|:
```spec
   (1 + 1) * (let y = double 5 in y + 2) {-"~~."-}
```
{.nobreak}|where| 子句的一般語法則如下例所示：
```spec
f x0 = d0
   where y0 = e0
f x1 = d1
   where y1 = e1 {-"~~."-}
```
{.nobreak}由語法設計上即可看出，子句 |where y0 = e0| 只能放在 |f x0 = ...| 的一旁當作補述，不能出現在 |d0| 之中。
這個例子中， |y0| 的有效範圍含括 |d0| 與 |e0|。另，|e0| 可以使用 |x0|.

算式中只能用 |let|. 相對地，也有些只能使用 |where| 的場合。我們來看
我們來看一個只能使用 |where| 的例子：
::: {.example}
延續例 \@ref{ex:payment}。
今年起，新勞動法規規定工作超過 19 週的工讀生必須視為正式雇員，
學校除了薪資外，也必須付給勞保、健保費用。
學校需負擔的勞健保金額為雇員薪資的百分之六。請更新函數 |payment|,
輸入某工讀生工作週數，計算在新規定之下，學校需為工讀生付出的總額。
:::
::: {.answer}
一種可能寫法是先使用守衛，判斷工作週數是否大於 19：
```spec
payment :: Int -> Int
payment weeks  | weeks > 19  = round (fromIntegral baseSalary * 1.06)
               | otherwise   = baseSalary {-"~~,"-}
 where  baseSalary = 130 * hours
        hours  = 8 * days
        days   = 5 * weeks {-"~~."-}
```
{.nobreak}在 |where| 子句中，我們先算出不含勞健保費用的薪資，用識別字 |baseSalary| 表達。
如果 |weeks| 大於 |19|, 我們得將 |baseSalary| 乘以 |1.06|；否則即傳回 |baseSalary|.
函數 |fromIntegral| 把整數轉為浮點數，|round| 則把浮點數四捨五入為整數。
請注意：兩個被守衛的算式都用到了 |baseSalary| ---
|where| 子句中定義的識別字是可以跨越守衛的。
相較之下，|let| 算式只能出現在等號的右邊，而在守衛 |weeks > 19 = ...| 之後出現的
|let| 所定義出的區域識別字，顯然無法在 |otherwise = ...| 之中被使用，反之亦然。
:::

{title="巢狀定義"} |let| 算式之中還可有 |let| 算式，
|where| 子句中定義的識別字也可有自己的 |where| 子句。
我們看看兩個關於 |let| 的例子：
::: {.example}
猜猜看 |nested| 和 |recursive| 的值分別是什麼。
將他們載入 Haskell 直譯器，看看和你的猜測是否相同。
::: {.multicols}
::: {.mcol width="0.4\\textwidth"}
```spec
nested :: Int
nested =  let x = 3
          in (  let  x = 5
                in x + x) + x {-"~~,"-}
```
:::
::: {.mcol width="0.4\\textwidth"}
```spec
recursive :: Int
recursive =  let x = 3
             in  let x = x + 1
                 in x {-"~~."-}
```
:::
:::
::: {.answer}
|nested| 的值是 |13|，因為 |x+x| 之中的 |x| 在 |let x = 5| 的範圍中,
而 |.. + x| 中的 |x| 則在 |let x = 3| 的範圍中。^[在各種語言中，範圍的設計都是為了給程式員方便：在寫子算式時，可不用擔心是否與外層的識別字撞名。在教學時，我們難免舉各種撞名的例子作為說明。若把這些刁鑽例子當作考題，就是違反設計者本意的發展了。]
至於 |recursive| 的值，關鍵在於 |x = x + 1| 中右手邊的 |x| 指的是哪個。
若是 |x = 3| 的那個 |x|, 整個算式的值將是 |4|. 若 |x = x + 1| 中，
等號右手邊的 |x| 也是左手邊的 |x|, |recursive| 就是 |((..)+1)+1| , 沒有範式。
這兩種設計都有其道理。
Haskell 選了後者：在 |let x = e in ...| 之中，|x| 的有效範圍包括 |e|.
因此 |recursive| 在 Haskell 中會無窮地求值下去。
但也有些函數語言中的 |let| 採用前者的設計。通常這類語言中會另有一個 $\Keyword{letrec}$
結構，和 Haskell 的 |let| 功能相同。
:::
:::

## 高階函數

目前為止，我們看過由整數到整數的函數、由整數到真假值的函數...
那麼，可以有由函數到函數的函數嗎？
函數語言將函數視為重要的構成元件，因此函數也被視為*一級公民*。
如果整數、真假值... 可以當作參數、可以被函數傳回，函數當然也可以。
一個「輸入或輸出也是函數」的函數被稱為*高階函數*(*higher order function*)\index{higher-order function 高階函數}。
Haskell 甚至設計了許多鼓勵我們使用高階函數的機制。
本書中我們將見到許多高階函數。其實，我們已經看過一個例子了。

::: {.infobox title="一級公民"}
在程式語言中，若說某物/某概念是*一級公民*(*first-class citizen*)\index{first-class citizen 一級公民}或「一級的」，
通常指它和其他事物被同等對待：
如果其他事物可被當作參數、可被當作傳回值、可被覆寫...那麼它也可以。
這是一個沒有嚴格形式定義的說法，由 Christopher Strachey 在 1960 年代提出，
可用在型別、值、物件、模組... 等等之上。

例如：OCaml 是個有「一級模組」的語言，因為 OCaml 模組也可當作參數，可定義從模組到模組的函數（OCaml 中稱之為 functor）。
在 C 語言之中函數是次級的，因為函數不能當參數傳（能傳的是函數的指標，而非函數本身）。
Strachey指出，在 Algol 中實數是一級的，而程序是次級的。
:::

{title="Currying"} 回顧|smaller|的定義：
```
smaller :: Int -> Int -> Int
smaller x y = if x <= y then x else y {-"~~."-}
```
\@ref{sec:function-defns} 節中說「|smaller| 是一個函數，拿兩個參數 |x| 與 |y|」。
但這僅是口語上方便的說法。
事實上，在 Haskell 中（如同在 $\lambda$-calculus 中），所有函數都只有一個參數。
函數 |smaller| 其實是一個\emph{傳回函數的函數}：

  *  |smaller| 的型別 |Int -> Int -> Int| 其實應看成 |Int -> (Int -> Int)|：
這個函數拿到一個 |Int| 後，會傳回一個型別為 |Int -> Int| 的函數。
  * |smaller 3| 的型別是 |Int -> Int|。這個函數還可拿一個 |Int| 參數，將之和 |3| 比大小，傳回較小的那個。
  * |smaller 3 4| 是一個 |Int|。它其實是將函數 |smaller 3| 作用在 |4| 之上。也就是說，|smaller 3 4| 其實應看成 |(smaller 3) 4|. 根據定義，它可展開為 |if 3 <= 4 then 3 else 4|，然後化簡為 |3|.

::: {.exlist}
::: {.exer}
將 |smaller| 的定義鍵入一個檔案，載入 Haskell 直譯器中。

  1. |smaller 3 4| 的型別是什麼？在 GHCi 中可用 {\tt :t e} 指令得到算式 |e| 的型別。
  2. |smaller 3| 的型別是什麼？
  3. 在檔案中定義 |st3 = smaller 3|. 函數 |st3| 的型別是什麼？
  4. 給 |st3| 一些參數，觀察其行為。


:::
:::

「用『傳回函數的函數』模擬『接收多個參數的函數』」這種做法稱作 *currying*.
\index{currying}^[Currying 為動名詞，形容詞則為 *curried*。
此詞來自於邏輯學家 Haskell B. Curry 的姓氏。
詳見第 \@ref{sec:refs-basics} 節。]
Haskell 鼓勵大家使用 currying --- 它的內建函數大多依此配合設計，
語法設計上也很給 currying 方便。
當型別中出現連續的 |(->)| 時，預設為往右邊結合，例如
|Int -> Int -> Int| 應看成 |Int -> (Int -> Int)|.
這使得「傳回函數的函數」容易寫。
而在值的層次，連續的函數應用往左結合。
例如，|(smaller 3) 4| 可寫成 |smaller 3 4|。
這讓我們能很方便地將參數一個個餵給 curried 函數。

另一方面，如果我們想使用 |double| 兩次，計算 |x| 的四倍，應寫成 |double (double x)|.
若寫 |double double x| ，會被視為 |(double double) x| --- |double| 作用在自身之上，而這顯然是個型別錯誤。
::: {.texonly}
```
%format poly1
%format poly2
```
:::


我們再看一個使用 currying 的練習：
::: {.example}
給定 |a|, |b|, |c|, |x|, 下述函數 |poly| 計算 $ax^2 + bx + c$:
```spec
poly :: Int -> Int -> Int -> Int -> Int
poly a b c x = a * x * x + b * x + c {-"~~."-}
```
請定義一個函數 |poly1|, 使得 $poly_1~x = x^2 + 2x + 1$.
函數 |poly1| 需使用 |poly|.
:::
::: {.answer}
一種作法是：
```spec
poly1 :: Int -> Int
poly1 x = poly 1 2 1 x {-"~~."-}
```
但這相當於 |poly1 x = (poly 1 2 1) x| ---
|poly| 拿到 |x| 後，立刻把 |x| 傳給 |poly 1 2 1| 這個函數。
因此 |poly1| 可更精簡地寫成：
```spec
poly1 :: Int -> Int
poly1 = poly 1 2 1 {-"~~."-}
```
{.nobreak}兩種寫法都有人使用。
有提及|x|的寫法著重於描述拿到參數|x|之後要對它進行什麼操作。
而省略|x|的寫法則是在函數的層次上思考：
我們要定義一個函數，稱作|poly1|。這個函數是什麼呢？
*就是 |poly| 拿到 |1|, |2|, |1| 之後傳回的那個函數。*

如果我們想用 |poly| 定義出另一個函數
$poly_2~a~b~c = a\times 2^2 + b \times 2 + 2$ 呢？
最好理解的可能是不怎麼意外的寫法：
```spec
poly :: Int -> Int -> Int -> Int
poly2 a b c = poly a b c 2 {-"~~."-}
```
我們可以用一些技巧使 |a|, |b|, 和|c|不出現在定義中，
但如此得到的程式並不會更好懂。
:::

{title="二元運算子"}
在進入其他主題前，我們交代一些語法細節。
Haskell 鼓勵 currying 的使用，也把二元運算子都設計成 curried 的。
例如加法的型別是 |Int -> Int -> Int|.
Haskell 也配套設計了種種關於二元運算子的特殊語法，希望讓它們更好用。
但這些語法規則的存在都僅是為了方便我們寫出（主觀上）語法漂亮的程式，
而不是非有不可、非學不可的規定。

{#par:binary-operator-sectioning}
假設某二元運算子 |oplus| 的型別是 |a -> b -> c|,
|(x `oplus`)| 是給定了 |oplus| 的第一個參數後得到的函數；
|(`oplus` y)| 則是給定了 |oplus| 的第二個參數後得到的函數：%
^[根據@Hudak:07:Being，此種「*切片*」(*sectioning*)語法最早見於 David Wile 的博士論文。
後來被包括 Richard Bird 在內的 IFIP WG 2.1 成員使用，並由 David A. Turner 實作在他的語言 Miranda 中。]
```spec
(x `oplus`) y  = x `oplus` y  {-"\quad"-}  {- |(x `oplus`)| 的型別為 |b -> c|; -}
(`oplus` y) x  = x `oplus` y               {- |(`oplus` y)| 的型別為 |a -> c|.-}
```
{.nobreak}例如：

  * |(2 *)| 和 |(* 2)| 都是把一個數字乘以二的函數；
  * |(/ 2)| 則把輸入數字除以二；
  * |(1 /)| 計算輸入數字的倒數。

名字以英文字母開頭的函數預設為前序的。例如，
計算餘數的函數 |mod| 使用時得寫成 |mod 6 4|。
若把它放在「倒引號(backquote)」中，表示將其轉為中序 ---
如果我們比較喜歡把 |mod| 放到中間，可以寫成 |6 `mod` 4|.
首字元非英文字母的函數（如 |(+)|, |(/)| 等）則會被預設為中序的二元運算子。
若把一個中序二元運算子放在括號中，表示將其轉為前序運算子。
例如，|(+) 1 2| 和 |1 + 2| 的意思相同。

在 Haskell 的設計中，函數應用的優先順序比中序運算子高。
因此 |double 3 + 4| 會被視作 |(double 3) + 4|, 而不是 |double (3+4)|.
將中序運算子放在括號中也有「讓它不再是個中序運算子，只是個一般識別字」的意思。
例如算式 |f + x| 中，|f| 和 |x| 是中序運算子 |(+)| 的參數。
但在 |f (+) x| 中，|(+)| 和 |x| 都是 |f| 的參數
（這個式子可以讀解為 |(f (+)) x|）。

{title="以函數為參數"} 下述函數 |square| 計算輸入的平方：
```haskell
square :: Int -> Int
square x = x * x {-"~~."-}
```
{.nobreak}我們可另定義一個函數 |quad :: Int -> Int|，把 |square| 用兩次，
使得 |quad x| 算出 $x^4$.
```haskell
quad :: Int -> Int
quad x = square (square x)   {-"~~."-}
```

但，「把某函數用兩次」是個常見的編程模式。
我們能不能把 |quad| 與 |square| 抽象掉，單獨談「用兩次」這件事呢？
下面的函數 |twice| 把參數 |f| 在 |x| 之上用兩次：
```haskell
twice      :: (a -> a) -> (a -> a)
twice f x  = f (f x) {-"~~."-}
```
{.nobreak}有了 |twice|, 我們可以這麼定義 |quad|:
```spec
quad :: Int -> Int
quad = twice square {-"~~."-}
```

函數 |twice| 是本書中第一個「以函數為參數」的函數。
我們可看到「讓函數可作為參數」對於抽象化是有益的：
我們可以把「做兩次」這件事單獨拿出來說，把「做什麼」抽象掉。

「函數可以當作參數」意味著我們可以定義*作用在函數上的運算子*。
|twice| 就是這麼一個運算子：
它拿一個函數 |f|，把它加工一下，做出另一個函數（後者的定義是把 |f| 用兩次）。

{title="參數式多型"}
函數 |twice| 也是本書中第一個*多型*函數。
在 Haskell 的型別中，小寫開頭的識別字（如其中的 |a|）是型別層次的參數。
讀者可想像成在 |twice| 的型別最外層有一個省略掉的 |forall a|.
也就是說, |twice| 的完整型別是 |forall a . (a -> a) -> (a -> a)|  ---
對所有的型別 |a|, |twice| 都可拿一個型別為 |a -> a| 的函數，
然後傳回一個型別為 |a -> a| 的函數。

在 |twice| 的型別 |(a -> a) -> (a -> a)| 中，

  * 第一個 |a -> a| 是參數 |f| 的型別，
  * 在第二個 |a -> a| 中，第一個 |a| 是 參數 |x| 的型別，
  * 第二個 |a| 則是整個計算結果的型別。

{.nobreak}參數 |f| 的型別必須是 |a -> a|: 輸出入型別必須一樣，因為 |f| 的結果必須可當作 |f| 自己的輸入。

在 |twice| 被使用時，型別參數 |a| 會依照上下文被*特化*(*instantiate*)成別的型別。
例如 |twice square| 中， 因為 |square| 的型別是 |Int -> Int|,
*這一個* |twice| 的型別變成了 |(Int -> Int) -> (Int -> Int)| --- |a| 被特化成 |Int|.
若某函數 |k| 的型別是 |Float -> Float|, 在 |twice k| 中，|twice| 的型別是 |(Float -> Float) -> (Float -> Float)|。
同一個函數 |twice| 可能依其上下文而有許多不同的型別，但都是 |(a -> a) -> (a -> a)| 的特例。
「一段程式可能有許多不同型別」的現象稱作*多型*(*polymorphism*)\index{polymorphism 多型}。
多型又有許多種類，此處為其中一種。詳情見... \todo{}


::: {.exlist}
::: {.exer}
為何 |twice| 的型別不可以是 |(a -> b) -> (a -> b)|?
:::
:::

::: {.example}
```haskell
forktimes f g x = f x * g x {-"~~."-}
```
算式 |forktimes f g x| 把 |f x| 和 |g x| 的結果乘起來。

  1. 請想想 |forktimes| 的型別該是什麼？
  2. 試定義函數 |compute :: Int -> Int|, 使用 |forktimes| 計算 $x^2 + 3\times x  + 2$。 **提示**：$x^2 + 3\times x  + 2 = (x+1) \times (x+2)$.


:::
::: {.answer}
如同 |twice|, |forktimes| 可以有很多型別，但都應該是
|(a -> Int) -> (a -> Int) -> a -> Int| 的特例：
在 |forktimes f g x| 中， |f| 和 |g| 的型別可以是 |a -> Int|,
其中 |a| 可以是任何型別 |a|，而 |x| 的型別必須也是同一個 |a|.
函數 |compute| 可定義如下：
```haskell
compute :: Int -> Int
compute = forktimes (+1) (+2) {-"~~."-}
```
其中 |forktimes| 型別中的 |a| 被特化為 |Int|.
:::

::: {.texonly}
```
%format lift2
```
:::

如前所述，|forktimes f g x| 把 |f x| 和 |g x| 的結果乘起來。
但，一定得是乘法嗎？我們當然可以再多做一點點抽象化。
::: {.example}
考慮函數 |lift2 h f g x = h (f x) (g x)|.

  1. |lift2| 的型別是什麼？
  2. 用 |lift2| 定義 |forktimes|.
  3. 用 |lift2| 計算 $x^2 + 3\times x  + 2$.


:::
::: {.answer}
我們把 |lift2| 最泛用的型別和其定義重複如下：
```haskell
lift2 :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c {-"~~."-}
lift2 h f g x = h (f x) (g x) {-"~~."-}
```
{.nobreak}有了 |lift2|, |forktimes| 可定義為：
```spec
forktimes :: (a -> Int) -> (a -> Int) -> a -> Int
forktimes = lift2 (*) {-"~~,"-}
```
{.nobreak}請讀者觀察：|lift| 型別中的 |a|, |b|, |c| 都特化成 |Int|,
|d| 則改名為 |a|.

我們也可用 |lift2| 定義 |compute|:
```spec
compute :: Int -> Int
compute = lift2 (*) (+1) (+2) {-"~~."-}
```

函數 |lift2| 可以看作一個作用在二元運算子上的運算子，功用是把
二元運算子「提升」到函數層次。
例如，原本 |(*)| 只能拿兩個 |Int| 當作參數，
（例：|1 * 2| 是「把 |1| 和 |2| 乘起來」），
但現在 |lift2 (*)| 可將函數 |(+1)| 和 |(+2)| 當參數了，
意思為「把 |(+1)| 和 |(+2)| 的結果乘起來」。
:::


## 函數合成

拿到一個函數 |f|，我們能做的基本操作包括把 |f| 作用在某個參數上、
把 |f| 傳給別的函數...
此外，另一個常用的基本操作是將 |f| 和別的函數*合成*(*compose*)
\index{function composition 函數合成}\index{.@@{|(.)|}}
為一個新函數。
^[Robert Gl\"{u}ck 認為函數上應有三個基本操作：
函數應用、函數合成、以及求一個函數的反函數。
前兩者已經提到，第三者則是大部分語言欠缺的。]
「合成」運算子在 Haskell 中寫成|(.)|.
這個運算子的形式定義如下（我們先看定義本體，待會兒再看型別）：
```spec
(f . g) x  = f (g x) {-"~~."-}
```
若用口語說，|f . g| 是將 |f| 和 |g|兩個函數「串起來」得到的新函數：
輸入 |x| 先丟給 |g|, 後者算出的結果再傳給 |f|.

::: {.example}
|square . double| 與 |double . square| 都是由 |Int| 到 |Int| 的函數.
直覺上，前者把輸入先給 |double|, 其結果再給 |square|。
後者則反過來。
如何了解它們的行為？既然它們是函數，我們便餵給它們一個參數，看看會展開成什麼！
兩者分別展開如下：
::: {.multicols}
::: {.mcol width="0.4\\textwidth"}
```spec
   (square . double) x
=    {- |(.)| 的定義 -}
   square (double x)
=  (x + x) * (x + x) {-"~~,"-}
```
:::
::: {.mcol width="0.4\\textwidth"}
```
   (double . square) x
=    {- |(.)| 的定義 -}
   double (square x)
=  (x * x) + (x * x) {-"~~."-}
```
:::
:::
所以，如果輸入為 |x|, |(square . double) x| 計算 $(2x)^2$;
|(double . square) x| 則是 $2x^2$.
:::

但，並不是所有函數都可以串在一起：
|f . g| 之中，|g| 的輸出型別和 |f| 的輸入型別必須一致才行。
運算子 |(.)| 包括型別的完整定義為：
```spec
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x  = f (g x) {-"~~."-}
```
{.nobreak}如果 |g| 的型別是 |a -> b|, |f| 的型別是 |b -> c|, 將他們串接起來後，
便得到一個型別為 |a -> c| 的函數。

有了|(.)|, 函數 |twice| 可以定義如下：
```spec
twice    :: (a -> a) -> (a -> a)
twice f  = f . f  {-"~~."-}
```
{.nobreak}確實，根據 |(.)| 的定義，|twice f x = (f . f) x = f (f x)|，和 |twice| 原來的定義相同。

為了討論函數合成的性質，我們先介紹一個函數 |id|:
```
id :: a -> a
id x = x {-"~~."-}
```
{.nobreak}它又稱作*單位函數*或*恆等函數*。\index{identity function 單位函數 恆等函數}
這是一個似乎沒有在做什麼的函數：給任何輸入，|id| 都原封不動地把它送到輸出 ---
這也反映在他的型別 |a -> a| 上。
這個函數有什麼重要性呢？
原來，|(.)| 滿足結合律，並且以 |id| 為單位元素（這也是「單位函數」這名字的由來）：
```texonly
\begin{align*}
  |id . f| ~=&~ |f| ~=~ |f . id {-"~~,"-}|\\
  |(f . g) . h| ~&=~ |f . (g . h) {-"~~."-}|
\end{align*}
```
用數學術語來說的話，|id| 與 |(.)| 形成一個*幺半群* (*monoid*)。
\index{monoid 幺半群}
函數 |id| 的重要性就如同 |0| 在代數中的重要性（|0| 與 |(+)| 也是一個幺半群）。我們在許多計算、證明中都會見到它。

以下我們試著證明 |(.)| 的結合律。我們想論證
|(f . g) . h = f . (g . h)|，但該如何下手？
該等式的等號左右兩邊都是函數。
當我們說兩個整數相等，意思很清楚：如果等號左邊是 |0|, 右邊也是 |0|;
如果左邊是 |1|, 右邊也是 |1|... 但說兩個函數「相等」，是什麼意思呢？

::: {.definition title="外延相等(extensional equality)" #def:extensional-eq}
給定兩個型別相同的函數 |f| 和 |g|, 當我們說它們*外延相等*(*extensionally equal*)\index{extensional equality 外延相等}，
意思是給任何一個輸入，|f| 和 |g| 都算出相等的輸出。也就是：
|(forall x. f x = g x)|.

本書中，當我們寫兩個函數相等(|f = g|)時，指的便是外延相等，除非例外註明。
:::

在外延相等的假設下，證明 |(f . g) . h = f . (g . h)| 也就是證明對任何一個 |x|, |((f . g) . h) x = (f . (g . h)) x| 均成立. 我們推論如下：
```spec
  ((f . g) . h) x
=  {- |(.)| 的定義 -}
  (f . g) (h x)
=  {- |(.)| 的定義 -}
  f (g (h x))
=  {- |(.)| 的定義 -}
  f ((g . h) x) {-"~~."-}
=  {- |(.)| 的定義 -}
  (f . (g . h)) x {-"~~."-}
```
既然 |(f . g) . h = f . (g . h)|，我們便可統一寫成 |f . g . h|, 不用加括號了。

::: {.exlist}
::: {.exer}
證明 |id . f = f = f . id|.
:::
::: {.exans}
根據外延相等，我們需證明 |(forall x . id . f = f = f . id)|.
推論如下：
```spec
   (id . f) x
=    {- |(.)| 之定義 -}
   id (f x)
=    {- |id| 之定義 -}
   f x
=    {- |id| 之定義 -}
   f (id x)
=    {- |(.)| 之定義 -}
   (f . id) x {-"~~."-}
```
:::
:::

合成 |(.)| 也是一個中序運算子。和其他中序運算子一樣，其優先性低於函數應用。
因此，當我們寫 |f . g x|, 指的是 |f . (g x)| --- |g x| 為一個函數，
和 |f| 合成，而不是 |(f . g) x| （後者根據 |(.)| 的定義，是 |f (g x)|）。

::: {.example}
下列程式中，有些是合法的 Haskell 式子、有些則有型別錯誤。
對每個程式，如果它是合法的，請找出它的型別，並說說看該程式做什麼。
如果有型別錯誤，請簡述為什麼。

  1. |square . smaller 3|;
  2. |smaller 3 . square|;
  3. |smaller (square 3)|;
  4. |smaller . square 3|.


:::
::: {.answer}
前三者都是 |Int -> Int|。

  1. 根據 |(.)| 的定義，|(square . smaller 3) x = square (smaller 3 x)|.
因此 |square . smaller 3| 是一個函數，將其輸入和 |3| 比較，取較小者的平方。
  2. |(smaller 3 . square) x = smaller 3 (square x)|. 因此它讀入 |x|, 並在 |3| 或 |x^2| 之中選較小的那個。
  3. |smaller (square 3)| 是一個函數，讀入 |x| 之後，選擇 |x| 與 |3^2| 之中較小的那個。
  4. |smaller . square 3| 有型別錯誤： |square 3| 不是一個函數（而是一個整數），
無法和 |smaller| 合成。


:::



{title="函數應用運算子" #para:fun-apply}
本書中有些時候會將許多函數組合成一串，例如 |square . double . (+1) . smaller 3|
。由於函數應用的優先順序比一般二元運算元高，把上述式子應用在參數 |5| 之上時得寫成
```spec
  (square . double . (+1) . smaller 7) 5 {-"~~,"-}
```
{.nobreak}（這個式子的值為 $(2\times(5+1))^2$。）
每次都得加一對括號似乎有些累贅。Haskell 另有一個運算子|($)|, 唸作 ``apply'', 代表函數應用：
\index{\$@@{|($)|}}
```spec
($) :: (a -> b) -> a -> b
f $ x = f x {-"~~."-}
```
{.nobreak}|f $ x| 和 |f x| 意思一樣。那麼我們為何需要這個運算子呢？原因之一是
|($)| 的優先度比 |(.)| 低，因此上式可省去括號改寫如下：
```spec
  square . double . (+1) . smaller 7 $ 5 {-"~~."-}
```

運算子 |($)| 的另一個重要意義是：「函數應用」這個動作有了符號，
成為可以獨立討論的事物。例如，|($)| 可以當作參數。
一個這麼做的例子是習題 \@ref{ex:uncurry-apply}。

{title="常量函數" #para:const}
既然介紹了 |id|, 本節也順便介紹一個以後將使用到的基本組件。
給定 |x| 之後，函數 |const x| 是一個 *不論拿到什麼函數，都傳回 |x|*的函數。
函數 |const| 的定義如下：
```spec
const :: a -> b -> a
const x y = x {-"~~."-}
```
第\@ref{sec:evaluation}節開頭的範例 |three| 可定義為 |three = const 3|.

「無論如何都傳回 |x|」聽來好像是個沒用的函數，
但和 |id| 一樣，我們日後會看到它在演算、證明中時常用上。
事實上，*組件邏輯*理論告訴我們：所有函數都可以由 |id|, |const|, 和下述的 |subst| 三個函數組合出來。
```spec
subst :: (a -> b -> c) -> (a -> b) -> (a -> c)
subst f g x = f x (g x) {-"~~."-}
```

## $\lambda$ 算式

雖說函數是一級市民，在本書之中，目前為止，仍有一項功能是其他型別擁有、函數卻還沒有的：寫出一個*未命名*的值的能力。
整數、真假值都能不經命名、直接寫在算式中，例如，
我們可寫 |smaller (square 3) 4|, 而不需要先定義好
::: {.texonly}
```
%format num1
%format num2
%format e1
%format e2
```
:::
```spec
num1, num2 :: Int
num1  = 3
num2  = 4 {-"~~,"-}
```
{.nobreak}才能說 |smaller (square num1) num2|. 但使用函數時，似乎非得
先給個名字，才能使用它：
```spec
square :: Int -> Int
square x = ... {-"~~,"-}
quad = twice square {-"~~."-}
```
{.nobreak}如果為了某些原因（例如，如果在我們的程式中 |square| 只會被用到一次），我們不想給 |square| 一個名字，我們能不能直接把它寫出來呢？

$\lambda$ 算式便是允許我們這麼做的語法。
以直觀的方式解釋，|\x -> e| 便是一個函數，其中 |x| 是參數，
|e| 是函數本體。
例如，|(\x -> x * x)| 是一個函數，計算其輸入(|x|)的平方。
如果我們不想給 |square| 一個名字, 我們可將 |quad| 定義為：
```spec
quad = twice (\x -> x * x) {-"~~."-}
```

寫成 $\lambda$ 算式的函數也可直接作用在參數上，例如 |(\x -> e1) e2|。
這個式子歸約的結果通常表示為|e1[e2/x]|,
意思是「將 |e1| 之中的 |x| 代換為 |e2|」。
例如，算式 |(\x -> x * x) (3+4)| 可歸約為 |(3+4) * (3+4)|.
::: {.example}
以下是一些 $\lambda$ 算式的例子：

  * 函數 |(\x -> 1 + x)| 把輸入遞增 --- 和 |(1+)| 相同。
    其實，把 |(1+)| 的語法糖去掉後，得到的就是這個 $\lambda$ 算式。
  * |(\x -> \y -> x + 2 * x * y)| 是一個傳回 $\lambda$ 算式的函數。
    *  |(\x -> \y -> x + 2 * x * y) (3+4)| 可歸約為
       |(\y -> (3+4) + 2 * (3+4) * y)|。注意 |\x| 不見了，
       函數本體中的 |x| 被代換成 |3+4|，|\y -> ..| 則仍留著。
    * |(\x -> \y -> x + 2 * x * y) (3+4) 5| 可歸約為
      |(3+4) + 2 * (3+4) * 5|.
  * 由於傳回函數的函數是常見的，Haskell (如同 $\lambda$-calculus)
    提供較短的語法。上述例子中的函數也可簡寫成：
    |(\x y -> x + 2 * x * y)|.
  * 函數也可以當參數。例如，|(\x -> x 3 3) (+)| 可歸約為 |(+) 3 3|, 或 |3 + 3|.
  * 以下是 |(\f x -> f x x) (\y -> 2 * y) 3| 的求值過程：
```spec
   (\f x -> f x x) (\y z -> 2 * y + z) 3
=  (\x -> (\y -> 2 * y + z) x x) 3
=  (\y -> 2 * y + z) 3 3
=  2 * 3 + 3
=  9 {-"~~."-}
```
  * 在 |\x -> e| 之中，|x| 是範圍限於 |e| 的區域識別字。
    因此：
```spec
   (\f x -> x + f x) (\x -> x + x) 3
=  (\x -> x + (\x -> x + x) x) 3
=  3 + (\x -> x + x) 3
=  3 + 3 + 3
=  9 {-"~~."-}
```


:::

有了 $\lambda$ 算式後，函數 |smaller| 又有另一種寫法：
```spec
smaller :: Int -> Int -> Int
smaller = \x y -> if x <= y then x else y {-"~~."-}
```
事實上，$\lambda$ 算式可視為更基礎的機制 ---
目前為止我們所介紹的種種語法結構都僅是 $\lambda$ 算式的語法糖，都可展開、轉譯為 $\lambda$ 算式。
Haskell 的 $\lambda$ 算式源於一套稱為 *$\lambda$ 演算* (*$\lambda$ calculus*)的形式語言 --- 這是一個為了研究計算本質而發展出的理論，也是函數語言的理論核心。我們將在爾後的章節中做更詳盡的介紹。\todo{which?}

## 簡單資料型態

藉由一些例子，我們已經看過 Haskell 的一些數值型別：|Int|, |Float| 等等。
在本節中我們將簡短介紹我們將用到的一些其他型別。

### 布林值 {#sec:boolean}

布林值 (Boolean)\index{Boolean 布林值}常用於程式中表達真和假。
在 Haskell 中，我們可假想有這樣的一個型別定義：
```spec
data Bool = False | True {-"~~."-}
```
{.nobreak}其中，|data| 是 Haskell 宣告新資料型別的保留字。
上述定義可用口語描述成「定義一個稱作 |Bool| 的新資料型別，
有兩個可能的值，分別為 |False| 和 |True|.」
|False| 和 |True| 是型別 |Bool| 的*唯二*兩個*建構元* ---
任何型別為 |Bool| 的值，如果有正規式，必定是它們兩者之一。
在 Haskell 之中，建構元必須以大寫英文字母或冒號(|:|)開頭。

{title="樣式配對"} \index{pattern matching 樣式配對} 有了資料，我們來看看怎麼定義該型別上的函數。以布林值為輸入的函數中，
最簡單又常用的可能是 |not|:
```spec
not :: Bool -> Bool
not False  = True
not True   = False {-"~~."-}
```
這和我們的的直覺理解一致：|not False| 是 |True|, |not True| 是
|False|. 我們看到這個定義寫成兩行（正式說來是兩個「子句」），\emph{每一個子句分別對應到 |Bool| 的一個可能的值}。
以下則是邏輯上的「且」和「或」（分別寫作|(&&)|與|(||||)|）的定義：%
^[邏輯「且」又稱作合取(conjunction)\index{conjunction 合取、且}；邏輯「或」又稱作析取(disjunction).\index{disjunction 析取、或}
在 Haskell 中，「且」與「或」需分別寫成 $(\mathtt{\&\&})$ 和 $(\mathtt{||||})$。本書中採用數學與邏輯領域較常使用的 |(&&)|與|(||||)|.]
```spec
(&&), (||) :: Bool -> Bool -> Bool
False  && y  = False
True   && y  = y {-"~~,"-}

False  || y  = y
True   || y  = True {-"~~."-}
```
運算子|(&&)|與|(||||)|的定義同樣是各兩個子句，每個子句分別考慮其第一個參數的值。
以 |x && y| 為例：如果 |x| 是 |False|, 不論 |y| 的值為何，|x && y| 都是 |False|；
如果 |x| 是 |True|, |x & y| 的值和 |y| 相同。|(||||)| 的情況類似。

::: {.example}
以下函數判斷給定年份 |y| 是否為閏年。
```haskell
leapyear :: Int -> Bool
leapyear y =  (y `mod` 4 == 0) &&
               (y `mod` 100 /= 0 || y `mod` 400 == 0) {-"~~."-}
```
:::
我們來算算看 |leapyear 2016|。依照定義展開為
```spec
  (2016 `mod` 4 == 0) && (2016 `mod` 100 /= 0 || 2016 `mod` 400 == 0) {-"~~."-}
```
接下來該怎麼做呢？函數 |(&&)| 的定義有兩個子句，我們得知道 |2016 `mod` 4 == 0| 的值才能得知該歸約成哪個。因此只好先算 |2016 `mod` 4 == 0|，得到 |True|:
```spec
  True && (2016 `mod` 100 /= 0 || 2016 `mod` 400 == 0) {-"~~,"-}
```
然後依照|(&&)| 的定義歸約為 |2016 `mod` 100 /= 0 |||| 2016 `mod` 400 == 0|.
接下來也依此類推。

我們發現這是第\@ref{sec:evaluation}節中所提及的*被迫求值*的例子：我們得先把參數算出，才知道接下來如何走。
函數 |not|, |(&&)|, |(||||)| 定義成許多個子句，每個都分析其參數的可能外觀，據此決定該怎麼走。這種定義方式稱作*樣式配對 (pattern matching)*：等號左手邊的 |False|, |True| 等等在此是樣式(pattern)。使用這些函數時，例如 |x && y| 中， |x| 得先被算到可以和這些樣式配對上的程度，才能決定接下來的計算如何進行。

樣式配對也可用在不止一個參數上。例如，以下的運算元 |(==)| 判斷兩個布林值是否相等。
```spec
(==) :: Bool -> Bool -> Bool
False  == False  = True
False  == True   = False
True   == False  = False
True   == True   = True {-"~~."-}
```
讀者可能注意到我們用了同一個符號 |(==)| 來表示整數與布林值的相等測試。
請讀者暫且接受，相信 Haskell 有某些方式可得知任一個算式中的 |(==)| 到底是什麼型別的相等。詳情 \todo{where?}

Haskell 中另有一個專用來做樣式配對的 |case| 算式。例如，|(&&)| 也可寫成如下的形式：
```spec
(&&) :: Bool -> Bool
x && y =  case x of
            False  -> False
            True   -> y {-"~~."-}
```
由於 |case| 是算式，如同 |let| 一樣可出現在其他算式中，也可巢狀出現。

:::{.exlist}
:::{.exer}
以 |case| 算式定義 |not|, |(||||)|, 和 |(==)|.
:::
:::{.exer}
另一個定義 |(==) :: Bool -> Bool -> Bool| 的方式是
```spec
x == y = (x && y) || (not x && not y) {-"~~."-}
```
請將 |(x,y) := (False, False)|, |(x,y) := (False, True)|
等四種可能分別代入化簡，看看是否和本節之前的 |(==)| 定義相同。
:::
:::

### 字元

我們可把「字元」這個型別想成一個很長的 |data| 宣告：
```spec
data Char = 'a' | 'b' | ... | 'z' | 'A' | 'B' ....
```
{.nobreak}其中包括所有字母、符號、空白... 目前的 Haskell 甚至有處理 Unicode 字元的能力。
但無論如何，|Char| 之中的字元數目是有限的。我們可用樣式配對定義字元上的函數。
注意：字元以單引號括起來。

我們也可假設字元是有順序的，每個字元對應到一個內碼。
關於 |Char| 的常用函數中，|ord| 將字元的內碼找出，|chr| 則將內碼轉為字元：
```spec
ord  :: Char -> Int {-"~~,"-}
chr  :: Int -> Char {-"~~."-}
```
:::{.example}
下列函數 |isUpper| 判斷一個字元是否為大寫英文字母；|toLower| 則將
大寫字母轉成小寫字母，若輸入並非大寫字母則不予以變動。
```haskell
isUpper :: Char -> Bool
isUpper c = let x = ord c in ord 'A' <= x && x <= ord 'Z' {-"~~,"-}

toLower :: Char -> Char
toLower c  | isUpper c  = chr (ord c - ord 'A' + ord 'a')
           | otherwise  = c {-"~~."-}
```
:::


### 序對 {#sec:pairs}

數學上，將兩個值（如 |3| 和 |'a'|) 放在一起，就成了一個*有序對*(*ordered pair*)，可寫成|(3,'a')|。\index{pair 序對}
之所以稱作「有序」對，因為其中兩個元素的順序是不可忽略的 --- |(3,'a')| 與 |('a',3)| 是不同的有序對。
另一個常見譯名是「數對」。由於我們處理的不只是數字，本書將之簡稱為「序對」。

給兩個集合 |A| 和 |B|, 從 |A| 之中任取一元素 |x|，從 |B| 之中也任取一元素 |y|，
兩者的序對 |(x,y)| 形成的集合稱作 |A| 和 |B| 的*笛卡兒積*(*Cartesian product*)，寫成 |A :* B|:\index{Cartesian product 笛卡兒積}
```spec
   A :* B = {(x,y) | x `mem` A, y `mem` B} {-"~~."-}
```
Haskell 之中也有類似的構造。給定型別 |a| 與 |b|, 它們的序對的型別是 |(a :* b)|.
^[然而，由於「程式可能不終止」這個因素作怪，|a :* b| 的元素並不僅是 |a| 與 |b| （如果視做集合）的笛卡兒積。詳見 \todo{where?}]
我們可以想像 Haskell 有這麼一個型別定義：
```spec
data (a :* b) = (a,b) {-"~~."-}
```
{.nobreak}以口語說的話，|(a :* b)| 是一個新型別，而具有此型別的值若有範式，必定是 |(x,y)| 的形式，其中 |x| 的型別是 |a|, |y| 的型別是 |b|.
^[其實這個定義並不符合 Haskell 的語法，因此只是方便理解的想像。另，型別 |(a :* b)| 在 Haskell 中寫成 |(a,b)|. 我的經驗中，讓型別與值的語法太接近，反易造成困惑。]
序對的建構元寫成|(,)|，型別為 |a -> b -> (a :* b)|.
例如 |(,) 4 'b' = (4,'b')|.

兩個常用的函數 |fst| 與 |snd| 分別取出序對中的第一個和第二個元素：
::: {.multicols}
::: {.mcol width="0.4\\textwidth"}
```spec
fst :: (a :* b) -> a
fst (x,y) = x {-"~~,"-}
```
:::
::: {.mcol width="0.4\\textwidth"}
```spec
snd :: (a :* b) -> b
snd (x,y) = y {-"~~."-}
```
:::
:::
函數 |fst| 與 |snd| 的定義方式也是樣式配對：輸入值必須先計算成 |(x,y)| 的形式。

::: {.example}
以下是一些序對與其相關函數的例子。

  * |(3,'a')| 是一個型別為 |(Int :* Char)| 的序對。
  * |fst (3,'a') = 3|, |snd (3,'a') = 'a'|
  * 函數 |swap| 將序對中的元素調換：
```spec
swap :: (a :* b) -> (b :* a)
swap (x,y) = (y,x) {-"~~."-}
```
另一個定義方式是 |swap p = (snd p, fst p)|.
但這兩個定義並不盡然相同。詳見第\@ref{sec:weak-head-normal-form}節。
:::

序對也可以巢狀構成。例如 |((True,3), 'c')| 是一個型別為 |((Bool :* Int) :* Char)| 的序對，而 |snd (fst ((True,3), 'c')) = 3|. 在 Haskell 之中，|((a :* b) :* c)| 與 |(a :* (b :* c))| 被視為不同的型別，但他們是*同構*的 --- 我們可定義一對函數在這兩個型別之間作轉換：
```haskell
assocr :: ((a :* b) :* c) -> (a :* (b :* c))
assocr ((x,y),z) = (x,(y,z)) {-"~~,"-}

assocl :: (a :* (b :* c)) -> ((a :* b) :* c)
assocl (x,(y,z)) = ((x,y),z) {-"~~,"-}
```
{.nobreak}並且滿足 |assocr . assocl = id|,  和 |assocl . assocr = id|.

:::{.exlist}
:::{.exer}
試試看不用樣式配對，而以 |fst| 和 |snd| 定義 |assocl| 和 |assocr|:
```spec
assocl  p = ...
assocr  p = ...
```
:::
:::



:::{.infobox title="同構"}
\index{isomorphism 同構}
兩個集合|A|與|B| *同構*(*isomorphic*)，意思是
|A| 之中的 *每個*元素都*唯一地*對應到 |B| 之中的*一個*元素，反之亦然。

一個形式定義是：|A|與|B|同構意謂我們能找到兩個全(total)函數
\index{function 函數!total 全函數} |to :: A -> B| 和 |from :: B -> A|, 滿足
```spec
from . to = id {-"~~,"-}
to . from = id {-"~~."-}
```
此處的兩個 |id| 型別依序分別為 |A -> A| 和 |B -> B|。
將定義展開，也就是說，對所有 |x :: A|，|from (to x) = x|; 對所有 |y :: B|, |to (from y) = y|. 這個定義迫使對每個 |x| 都存在一個唯一的 |to x|, 反之亦然。

我們已有兩個例子：|((a :* b) :* c)| 與 |(a :* (b :* c))| 同構，此外，|(a :* b)| 與 |(b :* a)| 也同構，因為 |swap . swap = id|.

如果集合|A|與|B|同構，不僅 |A| 之中的每個元素都有個在 |B| 之中相對的元素，給任一個定義在 |A| 之上的函數 |f|, 我們必可構造出一個 |B| 之上的函數，具有和 |f| 相同的性質。即使|A|與|B|並不真正相等，我們也可把它們視為*基本上沒有差別*的。在許多無法談「相等」的領域中，同構是和「相等」地位一樣的觀念。
:::

另外可一提的是，Haskell 允許我們在 $\lambda$ 算式中做樣式配對。例如 |fst| 的另一種寫法是：
```spec
fst = \(x,y) -> x {-"~~."-}
```

Haskell 另有提供更多個元素形成的有序組，例如 |(True, 3, 'c')| 是一個型別為 |(Bool :* Int :* Char)| 的值。但本書暫時不使用他們。
