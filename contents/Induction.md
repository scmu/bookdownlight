``` {.haskell .invisible}
{-# LANGUAGE TypeOperators #-}
module Chapters.Induction where

import Prelude ()
import Control.Arrow ((***))
import Common.MiniPrelude hiding (exp, length, take, drop, gcd)
import Chapters.Basics (ETree(..), ITree(..))
```

# 歸納定義與證明 {#ch:induction}

「全麥編程」的觀念鼓勵我們以小組件組織出大程式。
但這些個別組件該如何實作呢？或著，沒有合用的組件時該怎麼辦？
我們可以回到更基礎的層次，用*遞迴*(recursion)\index{recursion 遞迴}定義它們。
「遞迴」意指一個值的定義又用到它本身，是數學中常見的定義方式。
在早期的編程教材中，遞迴常被視為艱澀、難懂、進階的主題。
但在函數程設中，遞迴是唯一可使程式不定次數地重複一項計算的方法。
一旦跨過了門檻，遞迴其實是個很清晰、簡潔地描述事情的方式。

對於遞迴，許多初學者一方面不習慣、覺得如此構思程式很違反「直覺」，另一方面也納悶：以自己定義自己，到底是什麼意思？
這兩個難處其實都談到了好問題。
對於前者，我們希望發展一些引導我們構思遞迴程式的思路；希望有了這些依據，能使寫遞迴程式變得直覺而自然。
關於後者，其實並非所有遞迴定義都有「意思」 --- 有些「不好」的遞迴並沒有定義出東西。我們討論遞迴的意義時必須限定在「好」的、有意義的程式上。最好有些方式確保我們寫出的遞迴定義是好的。

在本章我們將討論一種型式較單純的遞迴：*歸納*(induction)\index{induction 歸納}。
對上述兩個問題，本章的回應是：先有歸納定義出的資料結構，再依附著該資料結構撰寫歸納定義的程式，是一種思考、解決程式問題的好方法，也是一種理解遞迴程式的方式。
此外，依循這種方法也能確保該定義是「好」的。
我們將從數學歸納法出發，發現歸納程式與數學歸納法的相似性 --- 寫程式和證明其實是很相似的活動。

本書以 Haskell 為學習工具，但在之後的幾章，我們僅使用 Haskell 的一小部分。
Haskell 支援無限大的資料結構，也允許我們寫出不會終止的程式。
但我們將假設所有資料結構都是有限的（除非特別指明），所有函數皆會好好終止（也就是說函數都是「*全函數*(total function)」\index{total function 全函數}--- 對每一個值，都會好好地算出一個結果，不會永遠算下去，也不會丟回一個錯誤）。這麼做的理由將在本章解釋。

## 數學歸納法 {#sec:math-induction}

在討論怎麼寫程式之前，我們得先複習一下*數學歸納法* --- 晚點我們就會明白理由。
回顧：*自然數*\index{natural number 自然數}在此指的是|0, 1, 2...| 等所有非負整數。
^[有些數學派別的「自然數」是從 |1| 算起的。計算科學中則通常習慣以 |0| 起算。]
自然數有無限多個，但每個自然數都是有限大的。
自然數的型別記為 |Nat|.
若|a|是一個型別, |a|之上的*述語*(predicate)\index{predicate 述語}可想成型別為|a -> Bool|的函數，常用來表示某性質對某特定的|a|是否成立。
自然數上的述語便是|Nat -> Bool|。
數學歸納法可用來證明某性質對所有自然數都成立：

> 給定述語|P :: Nat -> Bool|. 若
> 1. |P| 對 |0| 成立，並且
> 2. 若 |P| 對 |n| 成立，|P| 對 |1+n| 亦成立，
> 我們可得知 |P| 對所有自然數皆成立。

為何上述的論證是對的？我們在\@ref{sec:induction-set-theory}節將提供一個解釋。但此處我們可以提供一個和程式設計較接近的理解方式。自然數可被想成如下的一個資料結構：
```spec
  data Nat = Zero | Suc Nat {-"~~."-}
```
這行定義有幾種讀解法，目前我們考慮其中一種 --- 該定義告訴我們：

1. |Zero| 的型別是 |Nat|;
2. 如果 |n| 的型別是 |Nat|, |Suc n| 的型別也是 |Nat|;
3. 此外，沒有其他型別是 |Nat| 的東西。

{.noindent}這種定義方式稱作*歸納定義*(inductive definition)。
其中「沒有其他型別是 |Nat| 的東西」一句話很重要 ---
這意味著任一個自然數只可能是 |Zero|，或是另一個自然數加一，沒有別的可能。
任一個自然數都是這麼做出來的：由 |Zero| 開始，套上*有限*個 |Suc|。
反過來說，給任意一個自然數，我們將包覆其外的 |Suc| 一層層拆掉，在有限時間內一定會碰到 |Zero|.
有人主張將 inductive definition 翻譯為*迭構定義*，著重在從基底（此處為 |Zero|）開始，一層層堆積上去（此處為套上 |Suc|）的概念。

本書中，我們把自然數的 |Zero| 寫成粗體，表明它是資料建構元；把 |Suc| 的加號寫得小些並和 $\mathbf{1}$ 放得很近，以強調「加一」是資料建構元、是一個不可分割的動作（和我們之後將介紹的一般自然數加法|(+)|不同）。
數字 |2| 其實是 |Suc (Suc Zero)| 的簡寫, |3| 其實是 |Suc (Suc (Suc Zero))| 的簡寫。

歸納定義出的資料型別允許我們做*歸納證明*。
由於 |P| 是 |Nat| 到 |Bool| 的函數，「|P| 對 |0| 成立」可記為 |P Zero|，「若 |P| 對 |n| 成立，|P| 對 |1+n| 亦成立」可記為 |P (Suc n) <== P n|。
^[|P <== Q| 意思是「若 |Q| 則 |P|」。依此順序寫，有「為證明 |P|，我們想辦法讓 |Q| 成立」的感覺。許多人不習慣由右到左的箭頭，但不論數學上或日常生活中，這都是常使用的論證思考方向。]
我們假設兩者都已被證明，用它們證明 |P 3|：
```spec
     P (Suc (Suc (Suc Zero)))
<==    {- 因 |P (Suc n) <== P n| -}
     P (Suc (Suc Zero))
<==    {- 因 |P (Suc n) <== P n| -}
     P (Suc Zero)
<==    {- 因 |P (Suc n) <== P n| -}
     P Zero {-"~~."-}
```
{.noindent}第一步中，我們希望 |P (Suc (Suc (Suc Zero)))| 成立，根據 |P (Suc n) <== P n|, 只要 |P (Suc (Suc Zero))| 即可。第二步中，我們希望 |P (Suc (Suc Zero))| 成立，同樣根據|P (Suc n) <== P n|，只要 |P (Suc Zero)| 成立即可... 最後，只要 |P Zero| 成立，|P (Suc Zero)| 即成立，但 |P Zero| 是已知的。因此我們已論證出 |P 3| 成立！

由上述推演中，我們發現：數學歸納法的兩個前提 |P Zero| 與 |P (Suc n) <== P n| 給了我們一個*對任一個自然數|m|, 生成一個 |P m| 之證明*的方法。
這是由於自然數本就是一個歸納定義出的資料型別：任一個自然數 |m| 都是有限個 |Suc| 套在 |Zero| 之上的結果，因此，只要反覆用 |P (Suc n) <== P n| 拆，總有碰到 |P Zero| 的一天。
既然對任何 |m|, 都做得出一個 |P m| 的證明，我們就可安心相信 |P| 對任何自然數都成立了。

為了之後討論方便，我們將前述的數學歸納法寫得更形式化些：
```spec
  {-"\mbox{\bf 自然數上之歸納法}:~"-} (forall n . P n) {-"~"-}<== {-"~"-} P Zero && (forall n . P (Suc n) <== P n) {-"~~."-}
```
{.noindent}這只是把之前的文字描述改寫成二階邏輯，但可清楚看出：給定 |P|, 我們希望證明它對所有自然數都成立，只需要提供 |P Zero| 和 |P (Suc n) <== P n| 兩個證明。
其中 |P Zero| 是確定 |P| 對 |0| 成立的*基底* (base case)，\index{induction 歸納!base case 基底}
|P (Suc n) <== P n| 則被稱作*歸納步驟*(inductive step)：\index{induction 歸納!inductive step 歸納步驟}
在*假設 |P n| 成立的前提下，想辦法「多做一步」，論證 |P (Suc n)| 也成立*。餘下的就可交給數學歸納法這個架構了。

## 自然數上之歸納定義 {#sec:induction-on-Nat}

數學歸納法和編程有什麼關係呢？考慮一個例子：給定 |b, n :: Nat|, 我們希望寫個函數 |exp| 計算乘冪，使得 |exp b n = {-"\Varid{b}^{\Varid{n}}"-}|. 我們先把型別寫下：
```spec
exp :: Nat -> Nat -> Nat
exp b n = ?
```
{.noindent}問號部分該怎麼寫？沒有其他線索很難進行，因此我們回想：|n| 是自然數，而任何自然數只可能是 |Zero| 或 |Suc| 做出的。我們便分成這兩個狀況考慮吧：
```spec
exp :: Nat -> Nat -> Nat
exp b Zero     = ?
exp b (Suc n)  = ?
```
{.noindent}其中，|exp b Zero| 較簡單：顯然應該是 $\Varid{b}^0 = 1$. 至於 |exp b (Suc n)| 的右手邊該怎麼寫？似乎很難一步定出來。
但*假設 |exp b n| 已經順利算出了$\Varid{b}^{\Varid{n}}$*, 由於 $\Varid{b}^{1+\Varid{n}} = \Varid{b} \times \Varid{b}^{\Varid{n}}$, |exp b (Suc n)| 與之的關係可寫成：
```spec
exp b (Suc n) = b * exp b n {-"~~."-}
```
{.noindent}如此一來我們便完成了一個計算乘冪的程式：
```haskell
exp :: Nat -> Nat -> Nat
exp b Zero     = 1
exp b (Suc n)  = b *: exp b n {-"~~."-}
```

::: {.infobox title="Haskell v.s Math"}
很不幸地，Haskell 並不接受\@ref{sec:induction-on-Nat}節中|exp|的定義。

首先，Haskell 並沒有獨立的自然數型別。我們可自己定（並將其宣告為 |Num| 類別的一員），或著直接使用 Haskell 內建的 |Int| 型別。
其次，Haskell 原本允許我們在定義的左手邊寫 |exp b (n+1)| ，但這套稱作``|n+k| pattern'' 的語法已在 Haskell 2010 中被移除。目前我們得將 |exp| 寫成：
```spec
exp :: Int -> Int -> Int
exp b 0  = 1
exp b n  = b * exp b (n-1) {-"~~."-}
```
{.noindent}|n+k| pattern 曾引起激烈討論。支持者主要著眼於它在教學上的方便：這方便我們討論數學歸納法、做證明、並讓我們更明顯地看出自然數與串列的相似性。
反對者則批評它與 type class 的衝突。後來由反方勝出。

有些 Haskell 教科書堅持書中出現的程式碼須是能在一個字一個字地鍵入電腦後即可執行的。
本書的定位並非 Haskell 教材，而是函數編程概念的入門書。
為此目的，我們希望選擇適合清晰表達概念、易於操作、演算、證明的符號。
而一個實用目的的語言得在許多設計上妥協尋求平衡，基於種種考量，往往得犧牲符號的簡潔與便利性（這點我們完全能理解）。
因此本書中的程式語法偶爾會和 Haskell 語法有所不同。
我們會盡量指明這些不同處，使讀者知道如何將本書中的程式轉換成現下的 Haskell 語法。
:::

回顧一下剛剛的思路：我們難以一步登天地對任何 |n| 寫出 |exp b n|, 但我們提供 |exp b Zero| 該有的值，並在假設 |exp b n| 已算出該有的值的前提下，試著做一點加工、多算那一步，想法做出 |exp b (Suc n)| 該有的值。
這和前述的數學歸納法是一樣的！
*寫歸納程式和做歸納證明是很類似的行為。*
使用數學歸納法證明 |P| 需要提供一個基底 |P 0| 和歸納步驟 |P (Suc n) <== P n|.
歸納定義程式也一樣。
在 |exp b n| 的定義中，基底是 |exp b Zero|，歸納步驟則是由 |exp b n| 想法變出 |exp b (Suc n)|.
有這兩個元件，我們便有了一個*對任何自然數 |n|, 保證算出 |exp b n| 的方法*。作為例子，我們看看 |exp 2 3| 是怎麼被算出來的：
```spec
   exp 2 (Suc (Suc (Suc Zero)))
=    {- |exp| 之歸納步驟 -}
   2 * exp (Suc (Suc Zero))
=    {- |exp| 之歸納步驟 -}
   2 * 2 * exp (Suc Zero)
=    {- |exp| 之歸納步驟 -}
   2 * 2 * 2 * exp Zero
=    {- |exp| 之基底 -}
   2 * 2 * 2 * 1 {-"~~."-}
```
{.noindent}第一步中，要算出 |exp 2 (Suc (Suc (Suc Zero)))|, 我們得先算出 |exp (Suc (Suc Zero))|. 要算出後者，在第二步中我們得先算出 |exp (Suc Zero)|... 直到我們碰到 |exp b Zero|.

{title="自然數上的歸納定義"}
我們將 |b| 固定，稍微抽象一點地看 |exp b :: Nat -> Nat| 這個函數。該定義符合這樣的模式：
```spec
f :: Nat -> a
f Zero     = e
f (Suc n)  = ... f n ... {-"~~."-}
```
{.noindent}這類函數的輸入是 |Nat|，其定義中 |f (Suc n)| 的狀況以 |f n| 定出，此外沒有其他對 |f| 的呼叫。若一個函數符合這樣的模式，我們說它是*在自然數上歸納定義*出的，其中 |f Zero| 那條稱作其基底，|f (Suc n)| 那條稱作其歸納步驟。我們日後將看到的許多程式都符合這個模式。

數學上，若一個函數能為其值域內的每個值都找到一個輸出，我們說它是個全函數(total function)，否則是部分函數(partial function).
計算上，當我們說 |f| 是一個全函數，意謂只要 |x| 型別正確並可算出值，|f x| 便能終止並算出一個值，不會永久跑下去，也不會丟出錯誤。

如前所述的、在自然數上歸納定義的 |f| 會是全函數嗎？首先，任何自然數都可拆成 |Zero| 或是 |Suc n|，而這兩個情況已被 |f| 的兩行定義涵括，不會出現漏失的錯誤。其次，|f| 每次呼叫自己，其參數都少了一層 |Suc|. 長此以往，不論輸入多大，總有一天會遇到基底 |f Zero| ---
因為任何自然數都是從 |Zero| 開始，套上有限個 |Suc|.
只要基底狀況的 |e| 以及在歸納步驟中 |f n| 前後的計算都正常終止，對任何輸入，|f| 都會正常終止。因此 |f| 是個全函數。

「程式會終止」是很重要的性質，我們之後會常談到。
在本書目前為止示範的編程方法中，「一個函數若呼叫自己，只能給它更小的參數」是個單純但重要的規範
（例如 |f (Suc n)| 的右手邊可以有 |f n|, 不能有 |f (Suc n)| 或 |f (Suc (Suc n))|）。
操作上這確保程式會終止，而在第 \todo{where?} 章之中，我們將提到這也確保該遞迴定義是「好」的、有意義的。

順便一提：在 |f (Suc n)| 的右手邊中，|f n| 可以出現不只一次 ---
因為 |... f n ... f n ...| 可看成
|(\x -> ... x ...x ...) (f n)|.
在 |f n| 的前後 |...| 的部分可以出現 |n| --- 將在第\@pageref{ex:factorial}頁中介紹的階層函數就是一個這樣的例子。
有些情況下我們不允許 |n| 出現在 |...| 中，此時會額外說明。

{title="乘法、加法"} 我們多看一些歸納定義的例子。在 |exp| 中我們用到乘法，但假若我們的程式語言中只有加法、沒有乘法呢？我們可自己定定看：
```spec
(*) :: Nat -> Nat -> Nat
m * n = ?
```
{.noindent}若不用組件，我們目前會的寫程式方法只有歸納法，也只有這招可試試看了。
但，|(*)| 有兩個參數，我們該把 |(m *) :: Nat -> Nat| 視為一個函數，分別考慮 |n| 是 |Zero| 或 |Suc ...| 的情況，還是把
|(* n) :: Nat -> Nat| 視為一個函數，考慮 |m| 是 |Zero| 或 |Suc ...| 的情況？答案是兩者皆可，並無根本性的差異。只是現在我們做的選擇會影響到之後與 |(*)| 相關的證明怎麼寫(見第\@ref{sec:inductive-proof-on-Nat}節)。本書中的習慣是拆左手邊的參數，因此我們考慮以下兩種情況。
```spec
(*) :: Nat -> Nat -> Nat
Zero     * n  = ?
(Suc m)  * n  = ... m * n ....
```
{.noindent}基底狀況中，|Zero * n| 的合理結果應是 |Zero|.
歸納步驟中，我們得想法算出 |(Suc m) * n|, 但我們可假設 |m * n| 已經算出了。
稍作思考後，讀者應可同意以下的做法：
```spec
(*) :: Nat -> Nat -> Nat
Zero     * n  = Zero
(Suc m)  * n  = n + (m * n) {-"~~,"-}
```
{.noindent}如果已有 |m * n|，多做一個 |(n+)|, 就可得到 |(Suc m) * n|了。

如果我們的程式語言中連加法都沒有呢？加法可看成連續地做 |Suc|:
```spec
(+) :: Nat -> Nat -> Nat
Zero     + n  = n
(Suc m)  + n  = Suc (m + n) {-"~~."-}
```
{.noindent}此處 |(+)| 是我們定義的、可將任意兩個自然數相加的加法，而 |Suc| 只做「加一」，是基本的資料建構元。
為求一致，我們同樣在左邊的參數上做歸納。
基底狀況中，|Zero + n| 只應是 |n|. 想計算 |(Suc m) + n|, 先假設 |m+n| 已經算出，再多套一個 |Suc|. 不難看出 |m + n| 是把 |n| 當做基底，在外面套上 |m| 個 |Suc| 的結果。
^[「這樣做不是很慢嗎？」是的。本章的自然數表示法，以及其引申出的運算元都不應看作有效率的實作，而是理論工具。了解加法與乘法可這樣看待後，許多其相關性質都可依此推導出來。]

## 自然數上之歸納證明 {#sec:inductive-proof-on-Nat}

上一節中我們定出了函數 |exp|。如果定義正確，|exp b n| 算的應是 $\Varid{b}^\Varid{n}$.
例如，我們知道 $\Varid{b}^\Varid{m+n} = \Varid{b}^\Varid{m} \times \Varid{b}^\Varid{n}$. 我們定出的函數 |exp| 是否真有此性質呢？

::: {.theorem #thm:exp-plus-times}
  對任何 |b, m, n :: Nat|, |exp b (m + n) = exp b m * exp b n|.
:::
{.noindent}我們試著證明定理\@ref{thm:exp-plus-times}。數學歸納法是我們目前唯一的工具，
而要使用它，第一個問題是：該用 |b|, |m|, 或 |n| 的哪一個來做歸納呢（意即把哪一個拆解）？

觀察定理\@ref{thm:exp-plus-times}中待證明式的等號兩邊，並參照 |exp|, |(+)|, 與 |(*)|的定義。
等號左手邊的 |exp b (m + n)| 之中，化簡 |exp b| 前得知道 |m + n| 究竟是 |Zero| 還是 |Suc k|。
而根據 |(+)| 的定義，化簡 |m + n| 前需知道 |m| 究竟是 |Zero| 還是 |Suc k|.
再看右手邊，根據 |(*)| 的定義，要化簡 |exp b m * exp b n| 得先化簡 |exp b m|, 而後者也得知道 |m| 是什麼。對兩邊的分析都指向：我們應針對 |m| 做歸納！

策略擬定後，我們便試試看吧！

::: {.proof title="證明定理 thm:exp-plus-times"}
欲證明 |exp b (m + n) = exp b m * exp b n|, 我們在 |m| 之上做歸納。
|m| 要不就是 |Zero|, 要不就是 |Suc k|.

{.noindent}**情況** |m := Zero|. 此時需證明 |exp b (Zero + n) = exp b Zero * exp b n|. 推論如下：
```{.haskell .invisible}
expPlusTimesP0 b n =
```
```haskell
   exp b (Zero +: n)
 ===    {- |(+)| 之定義 -}
   exp b n
 ===    {- 因 |1 * k = k| -}
   1 *: exp b n
 ===    {- |exp| 之定義 -}
   exp b Zero *: exp b n {-"~~."-}
```

{.noindent}**情況** |m := Suc m|. 此時需證明 |exp b ((Suc m) + n) = exp b (Suc m) * exp b n|, 但可假設 |exp b (m + n) = exp b m * exp b n| 已成立。推論如下：
```{.haskell .invisible}
expPlusTimesP1 b m n =
```
```haskell
   exp b ((Suc m) +: n)
 ===    {- |(+)| 之定義 -}
   exp b (Suc (m +: n))
 ===    {- |exp| 之定義 -}
   b *: exp b (m +: n)
 ===    {- 歸納假設 -}
   b *: (exp b m *: exp b n)
 ===    {- |(*)| 之遞移律 -}
   (b *: exp b m) *: exp b n
 ===    {- |exp| 之定義 -}
   exp b (Suc m) *: exp b n {-"~~."-}
```
:::

對這個證明，讀者是否有所懷疑？最大的疑問可能在「假設 |exp b (m + n) = exp b m * exp b n| 成立」這句上。這不就是我們要證明的性質嗎？在證明中假設它成立，似乎是用該性質自己在證明自己。這是可以的嗎？

為清楚說明，我們回顧一下第\@ref{sec:math-induction}節中的數學歸納法（並把區域識別字改為 |k| 以避免混淆）：
```spec
  {-"\mbox{\bf 自然數上之歸納法}:~"-} (forall k . P k) {-"~"-}<== {-"~"-} P Zero && (forall k . P (Suc k) <== P k) {-"~~."-}
```
{.noindent}證明\@ref{thm:exp-plus-times}欲證的是 |exp b (m + n) = exp b m * exp b n|，並在 |m| 上做歸納。
更精確地說，就是選用了下述的 |P|:%
^[在程式推導圈子中，|(<=>)| 常用來代表「只用在真假值上、且滿足遞移律的等號」。本書中使用 |(<=>)| 以和 |(=)| 做區分。]
```spec
P m <=> (exp b (m + n) = exp b m * exp b n) {-"~~,"-}
```
{.noindent}在證明中改變的是 |m|，而 |b| 與 |n| 是固定的。數學歸納法可證明 |(forall m . P m)|, 展開後正是 |(forall m . exp b (m + n) = exp b m * exp b n)|. 而根據數學歸納法，我們需提供 |P Zero| 與 |(forall m . P (Suc m) <== P m)| 的證明。

證明\@ref{thm:exp-plus-times}中「{\bf 情況} |m := Zero|」的部分，就是 |P Zero| 的證明。
而「{\bf 情況} |m := Suc m|」則是 |(forall m . P (Suc m) <== P m)| 的證明。
「假設 |exp b (m + n) = exp b m * exp b n| 成立」指的是假設 |P m| 成立，我們在此前提之下試圖證明 |P (Suc m)|.
因此，證明 \@ref{thm:exp-plus-times} 並沒有「用該性質自己證明自己」。
我們是以一個比較小的結果（|P m|）證明稍大一點的結果（|P (Suc m)|）。
就如同我們寫歸納程式時，假設 |f n| 已經算出，試著用它定出 |f (Suc n)|.

在證明之中，如 |P m| 這種在歸納步驟假設成立的性質被稱作*歸納假設*(induction hypothesis)。\index{induction 歸納!induction hypothesis 歸納假設}

{title="程式與證明"} 證明\@ref{thm:exp-plus-times}還有一些能啟發我們之處。方才，我們看到 |exp b (m + n) = exp b m * exp b n|， 決定以數學歸納法證明，但接下來怎麼著手？怎麼選定在哪個變數上做歸納？

答案是：分析該式中程式的行為。程式怎麼拆其參數，我們在證明中就怎麼拆。
我們試圖證明某些程式的性質，但程式本身便提供了證明可怎麼進行的提示。
「使證明的結構符合程式的結構」是許多證明的秘訣。
並非所有證明都可以如此完成，但本原則在許多情況下適用。

再看看 |exp|, |(+)|, |(*)| 等函數的定義。
為何他們都分出了兩個情況：|Zero| 與 |Suc n|, 並且 |Suc n| 的情況使用到該函數對於 |n| 的值？
因為自然數的資料型別是這麼定的！
自然數只可能是|Zero| 或 |Suc n|，而後者是由 |n| 做出來的。
因此程式也如此寫。程式的結構依循與其處理的資料型別之結構。

資料、程式、與證明原來有著這樣的關係：*證明的結構依循著程式的結構，而程式的結構又依循著資料型別的結構*。歸納定義出了一個型別後，自然知道怎麼在上面寫歸納程式；歸納程式有了，自然知道如何做關於這些程式的歸納證明。一切由定義資料結構開始。掌握這個原則，大部分的證明就不是難事。

{title="讓符號為你工作"} 再考慮證明\@ref{thm:exp-plus-times}中的狀況|m := Suc m|. 假想由你做這個證明，由第一行 |exp b ((Suc m) + n)| 開始。接下來該怎麼進行？

既然我們已經打定主意用數學歸納法，在證明的某處必定會使用|exp b (m + n) = exp b m * exp b n|這個歸納假設。
因此，證明前幾行的目的便是想辦法將 |exp b ((Suc m) + n)| 中的 |Suc| 往外提，將 |exp b| 往內側推，使得式子中出現 |exp b (m + n)|。一旦成功，就可運用歸納假設，將其改寫成 |exp b m * exp b n|！
接下的就是機械化地收尾、將式子整理成 |exp b (Suc m) * exp b n| 了。

這呼應到第\@ref{sec:let-symbols-work}節所說的「讓符號為你工作」。
光從語意上想，我們不易理解為何 |exp b ((Suc m) + n)| 能夠等於 |exp b (Suc m) * exp b n|. 但符號給我們線索。
我們可觀察式子的結構，暫時不去想語意；
我們的目標是操作、移動這些符號，將它們轉換成可使用歸納假設的形式。
因此，接下來的演算推導便有所依據而非盲目進行：已知目標是把某符號往外提或往內推，
我們就可尋找、使用可達到此目的的規則。
這些規則包括已知函數的定義、或諸如分配律、遞移律、結合律等等數學性質。
若很明顯地缺了一個想要的性質，也許可以把它當作引理另外證證看。
符號幫助我們，使我們的思考清晰而有方向。

:::{.exlist}
:::{.exer}
證明 |1 * k = k|. 這個證明並不需要歸納。
:::
:::{.exer #ex:add-associative}
證明 |(+)| 之遞移律: |m + (n + k) = (m + n) + k|. 此證明中你使用的述語是什麼？
:::
:::{.exer #ex:add-right-id}
證明 |k + 1 = k|. 你需要使用歸納法嗎？用什麼述語？
:::
:::

最後，說到自然數上的歸納定義，似乎不得不提*階層*(factorial)。用非正式的寫法，|fact n = n * (n-1) * (n-2) * ... * 1|.
\index{factorial 階層}
\label{ex:factorial}
形式化的定義如下：
```haskell
fact :: Nat -> Nat
fact Zero     = 1
fact (Suc n)  = (Suc n) *: fact n {-"~~."-}
```
{.noindent}我們在定理 \@ref{thm:length-perms} 中將會談到階層與排列的關係。


## 串列與其歸納定義 {#sec:induction-lists}

如同第\@ref{sec:lists}所述，「元素型別為|a|的串列」可定義成如下的資料型別：^[Haskell 中「元素型別為|a|的串列」寫成|[a]|. 由於這樣的符號在教學中遇到許多困難，本書中寫成|List a|.]
```spec
data List a = [] | a : List a {-"~~."-}
```
{.noindent}這個定義可以理解為

  1. |[]| 是一個串列，
  2. 若 |xs| 是一個元素型別為|a|的串列，|x| 型別為 |a|, 則 |x:xs| 也是一個元素型別為|a|的串列，
  3. 此外沒有其他元素型別為|a|的串列。

我們不難發現 |List a| 和 |Nat| 是相當類似的資料結構：|[]| 相當於 |Zero|, |(:)| 則類似 |Suc|, 只是此處我們不只「加一」，添加的那個東西多了一些資訊，是一個型別為|a|的元素。
或著我們可反過來說，串列「只是」在每個|Suc|上都添了一些資訊的自然數！
既然自然數與串列有類似的結構，不難想像許多自然上的函數、自然數的性質，都有串列上的類似版本，

### 串列上之歸納定義 {#sec:induction-lists-defn}

和自然數類似，許多串列上的函數可歸納地定義出來。
由於串列只可能由|[]|或|(:)|做出，定義串列上的函數時也分別處理這兩個情況。
基底情況為|[]|, 而欲定義 |f (x:xs)| 的值時，可假設 |f xs| 已算出來了：
```spec
f :: List a -> b
f []      = e
f (x:xs)  = ... f xs ...
```
{.noindent}來看些例子吧！「算一個陣列的和」可能是許多人學到陣列後得寫的頭幾個練習程式。串列版的和可以這麼寫：
```spec
sum :: List Int -> Int
sum []      = 0
sum (x:xs)  = x + sum xs {-"~~."-}
```
{.noindent}基底狀況中，空串列的和應是|0|。歸納步驟中，我們要算|x:xs| 的和，可假設我們已算出|xs| 的和，再加上|x| 即可。計算串列長度的 |length| 有很類似的定義 ：
```haskell
length :: List a -> Nat
length []      = Zero
length (x:xs)  = Suc (length xs) {-"~~."-}
```
{.noindent}在歸納步驟中，我們想計算|x:xs| 的長度，只需假設我們已知|xs| 的長度，然後加一。
事實上，|length| 剛好體現了前述「|List a| 只是在每個|Suc|上添了資訊的自然數」一事：
|length| 把串列走過一遍，將 |[]| 代換成 |Zero|，並將每個 |(_:)| 中附加的資訊拋棄，代換成 |Suc|。

函數 |map f :: List a -> List b|，也就是 |map| 給定函數 |f| 的結果，也可在串列上歸納定義：
```spec
map :: (a -> b) -> List a -> List b
map f []      = []
map f (x:xs)  = f x : map f xs {-"~~."-}
```
{.noindent}基底狀況的合理結果是|[]|. 歸納步驟中，要對 |x:xs| 中的每個元素都做 |f|,
我們可假設已經知道如何對 |xs| 中的每個元素都做 |f|, 把其結果接上 |f x| 即可。

函數 |(++)| 把兩個串列接起來。如果我們在其左邊的參數上做歸納定義，可得到：%
^[依照 Haskell 的運算元優先順序，|x : (xs ++ ys)| 其實可寫成 |x : xs ++ ys|, 一般也常如此寫。此處為了清楚而加上括號。]
```spec
(++) :: List a -> List a -> List a
[]      ++ ys  = ys
(x:xs)  ++ ys  = x : (xs ++ ys) {-"~~."-}
```
{.noindent}空串列接上 |ys| 仍是 |ys|. 歸納步驟中，要把 |x:xs| 接上 |ys|, 我們可假設已有辦法把 |xs| 接上 |ys|, 然後只需添上 |x| 即可。

請讀者比較一下|(++)|與自然數加法|(+)|的定義，會發現兩者的結構一模一樣！
如果串列是在每個|Suc|中加上資料的自然數，|(++)|就是串列上的加法了。
若要形式化地把 |List a|, |Nat|, |(++)|, 與 |(+)| 牽上關係，連接他們的橋樑就是 |length| --- |xs ++ ys| 的長度，應是 |xs| 與 |ys| 的長度之和！意即：
```texonly
\begin{align}
  |length (xs ++ ys) = length xs + length ys| \mbox{~~.} \label{eq:length-append}
\end{align}
```
習題 \@ref{ex:length-append} 中將證明此性質。

最後，|(++)| 是反覆使用 |(:)|, 函數 |concat| 則是反覆使用 |(++)|:
```spec
concat :: List (List a) -> List a
concat [] = []
concat (xs:xss) = xs ++ concat xss {-"~~."-}
```

### 串列上之歸納證明

如果 |List a| 是一個歸納定義出的資料結構，我們應可以在 |List a| 之上做歸納證明。確實，串列上的歸納法可寫成：
```spec
  {-"\mbox{\bf 串列上之歸納法}:~"-} (forall xs . P xs) {-"~"-}<== {-"~"-} P [] && (forall x xs . P (x:xs) <== P xs) {-"~~."-}
```
{.noindent}以文字敘述的話：給定一個述語 |P :: List a -> Bool|, 若要證明 |P xs| 對所有 |xs| 都成立，只需證明 |P []| 和「對所有 |x| 和 |xs|, 若 |P xs| 則 |P (x:xs)|」。

下述的 *|map| 融合定理*(*map-fusion theorem*)
\index{map-fusion map 融合定理@@{|map|-fusion |map| 融合定理}}
是關於 |map| 極常用的定理之一。所謂「融合」在此處是把兩個 |map| 融合為一。
我們日後會見到更多的融合定理。
::: {.theorem title="|map| 融合定理" #thm:map-fusion}
對任何 |f| 與 |g|,
|map f . map g = map (f.g)|.
:::
{.noindent}作為一個例子，我們試著證明定理\@ref{thm:map-fusion}。
我們目前只會用歸納證明，但是 |map f . map g = map (f.g)| 的左右邊都是函數，
沒有出現串列也沒有出現自然數。該拿什麼東西來歸納呢？

回顧*外延相等*（定義\@ref{def:extensional-eq}）：
當|h|, |k| 均是函數，|h = k| 的意思是對任何參數 |x|, |h x = k x|.
因此，將待證式左右邊各補上參數，並將 |(.)| 展開，可得知其意義為對任何 |xs|,
```texonly
\begin{align*}
    |map f (map g xs) = map (f.g) xs| \mbox{~ ~.}
\end{align*}
```
我們便可以在|xs|上做歸納了！
:::{.proof}
當 |xs := []|，等式兩邊皆歸約為 |[]|.
考慮 |xs := x:xs| 的情況：
```spec
  map f (map g (x:xs))
=   {- |map| 之定義 -}
  map f (g x : map g xs)
=   {- |map| 之定義 -}
  f (g x) : map f (map g xs)
=   {- |(.)| 之定義 -}
  (f.g) x : map f (map g xs)
=   {- 歸納假設 -}
  (f.g) x : map (f.g) xs
=   {- |map| 之定義 -}
  map (f.g) (x:xs) {-"~~."-}
```
:::

:::{.texonly}
% 前面說到 |(++)| 與 |(+)| 的相似性。若要形式化地把 |List a|, |Nat|, |(++)|, 與 |(+)|
% 牽上關係，連接他們的橋樑就是 |length| --- |xs ++ ys| 的長度，應是 |xs| 與 |ys| 的長度之和！我們試著證明看看。
% \begin{theorem} \label{thm:length-append}
% 對所有 |xs| 與 |ys|, |length (xs ++ ys) = length xs + length ys|.
% \end{theorem}
% \begin{proof} 檢視 |length|, |(++)|, 與 |(+)| 的定義，會發現等號兩邊都須對 |xs| 做分析才能化簡。因此我們對 |xs| 做歸納。
%
% \noindent {\bf 狀況} |xs := []|.
% %if False
% ```haskell
% lengthAppendPf0 ys =
% ```
% %endif
% ```haskell
%       length ([] ++ ys)
%  ===    {- |(++)| 之定義 -}
%       length ys
%  ===   {- |(+)| 之定義 -}
%       Zero +: length ys
%  ===   {- |length| 之定義 -}
%       length [] +: length ys {-"~~."-}
% ```
%
% \noindent {\bf 狀況} |xs := x : xs|.
% %if False
% ```haskell
% lengthAppendPf1 :: a -> List a -> List a -> Nat
% lengthAppendPf1 x xs ys =
% ```
% %endif
% ```haskell
%    length ((x:xs) ++ ys)
%  ===    {- |(++)| 之定義  -}
%    length (x : (xs ++ ys))
%  ===    {- |length| 之定義 -}
%    Suc (length (xs ++ ys))
%  ===    {- 歸納假設 -}
%    Suc (length xs +: length ys)
%  ===    {- |(+)| 之定義 -}
%    (Suc (length xs)) +: length ys
%  ===    {- |length| 之定義 -}
%    length (x:xs) +: length ys {-"~~."-}
% ```
% \end{proof}
% 讀者可觀察到類似的技巧：在|xs := x : xs|的狀況中，頭幾步的目的是將|length|往裡推，製造出|length (xs++ys)|, 以便使用歸納假設。
:::

:::{.infobox title="等式證明的步驟該多詳細？"}
本書中目前為止的等式證明相當細：每一個定義展開都成為獨立的步驟。
這是為了教學目的，實務上不一定得如此。
以我而言，自己的研究手稿中可能會將步驟寫得極詳細，
為確保每個細節正確，並讓他人（或幾年後已經忘記細節的自己）在不需知道上下文的情況下也能機械化地檢查每個步驟。
但在論文中，因篇幅有限，及考量讀者一次能處理的資訊量有限，
發表出的證明可能會省略許多步驟。

實務上，被認為簡單、不寫出也不妨礙理解的步驟或說明都可被省略。
但何謂簡單則很依靠作者的判斷與習慣。
一般說來，僅展開定義的步驟用電腦便可自動做到，通常是可精簡掉的。
最好寫出的步驟則可能是決定整個證明之結構的、不易以電腦決定而得靠人類智慧與經驗的，等等。
這可能包括使用歸納假設的那步，或使用較特別的引理時。
例如，性質 \@ref{eq:length-append}的歸納步驟證明可能被精簡如下：
```spec
   length ((x:xs) ++ ys)
=  Suc (length (xs ++ ys))
=    {- 歸納假設 -}
   Suc (length xs +: length ys)
=  length (x:xs) +: length ys {-"~~."-}
```
:::

:::{.exlist}
:::{.exer}
證明對所有 |xs|, |xs ++ [] = xs|. 比較本題與習題\@ref{ex:add-right-id}的證明。
:::
:::{.exer #ex:reverse}
定義函數 |reverse :: List a -> List a|, 將輸入的串列反轉。例如 |reverse [1,2,3,4,5] = [5,4,3,2,1]|.
:::
:::{.exans .compact}
```spec
reverse :: List a -> List a
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x] {-"~~."-}
```
{.noindent}另，關於 |reverse| 效率的討論詳見第 \@ref{sec:efficiency-basics} 節。
:::
:::{.exer #ex:length-map}
證明對所有 |f|, |length . map f = length|.
:::
:::{.exer #ex:sum-map-times}
證明對所有 |x|, |sum . map (x*) = (x*) . sum|.
:::
:::{.exans}
欲證明|sum (map (x*) ys) = x * sum ys|, 在 |ys| 上歸納。

{.noindent}**情況** |ys := []|, 兩邊都歸約為 |0|.

{.noindent}**情況** |ys := y:ys|:
```spec
   sum (map (x*) (y:ys))
=    {- |map| 之定義 -}
   sum (x*y : map (x*) ys)
=    {- |sum| 之定義 -}
   x * y + sum (map (x*) ys)
=    {- 歸納假設 -}
   x * y + x * sum ys
=    {- 乘法與加法之分配律 -}
   x * (y+sum ys)
=    {- |sum| 之定義 -}
   x * sum (y:ys) {-"~~."-}
```
:::
:::{.exer #ex:sum-map-suc}
證明對所有 |xs|, |sum (map (Suc) xs) = length xs + sum xs|.
:::
:::{.exer #ex:sum-map-const}
證明對所有 |xs| 與 |y|,
|sum (map (const y) xs) = y * length xs|.
:::
:::

討論自然數時，習題\@ref{ex:add-associative}曾請讀者證明加法都滿足結合律。此處示範證明類似定理的串列版：
::: {.theorem}
|(++)| 滿足結合律。意即，對任何 |xs|, |ys|, 和|zs|,
|(xs ++ ys) ++ zs = xs ++ (ys ++ zs)|.
:::
::: {.proof}
上述式子中有三個變數，我們怎麼得知該在哪一個變數上做歸納呢？
此時絕對別急著把三個變數都拆開，變成多達八種狀況。
觀察：如果要歸約等號左邊的 |(xs ++ ys) ++ zs|，根據 |(++)| 的定義，得對 |xs ++ ys| 做狀況分析；要歸約 |xs ++ ys|，又得對 |xs| 做狀況分析。
同樣地，根據 |(++)| 的定義，要歸約等號右邊的 |xs ++ (ys ++ zs)| 得對 |xs| 做狀況分析。
不論左右邊，最關鍵的值都是 |xs|.
因此我們在 |xs| 之上做歸納。

{.noindent}**狀況** |xs:=[]|:
```spec
   ([] ++ ys) ++ zs
=   {- |(++)| 之定義 -}
   ys ++ zs
=   {- |(++)| 之定義 -}
   [] ++ (ys ++ zs) {-"~~."-}
```

{.noindent}**狀況** |xs:= x:xs|:
```spec
   ((x:xs) ++ yz) ++ zs
=    {- |(++)| 之定義 -}
   (x : (xs ++ ys)) ++ zs
=    {- |(++)| 之定義 -}
   x : ((xs ++ ys) ++ zs)
=    {- 歸納假設 -}
   x : (xs ++ (ys ++ zs))
=    {- |(++)| 之定義 -}
   (x : xs) ++ (ys ++ zs) {-"~~."-}
```
:::
基底狀況的證明很簡單。至於歸納步驟，同樣地，前兩步都是為了湊出 |(xs ++ ys) ++ zs|, 以便使用歸納假設。既然 |(++)| 滿足結合律，日後我們寫 |xs ++ ys ++ zs| 就可不加括號了。

:::{.exlist}
:::{.exer #ex:length-append}
證明性質\@eqref{eq:length-append}：對所有 |xs| 與 |ys|, |length (xs ++ ys) = length xs + length ys|.
:::
:::{.exans}
檢視 |length|, |(++)|, 與 |(+)| 的定義，會發現等號兩邊都須對 |xs| 做分析才能化簡。因此我們對 |xs| 做歸納。

{.noindent}**狀況** |xs := []|.
```{.haskell .invisible}
lengthAppendPf0 ys =
```
```haskell
      length ([] ++ ys)
 ===    {- |(++)| 之定義 -}
      length ys
 ===   {- |(+)| 之定義 -}
      Zero +: length ys
 ===   {- |length| 之定義 -}
      length [] +: length ys {-"~~."-}
```

{.noindent}**狀況**  |xs := x : xs|.
```{.haskell .invisible}
lengthAppendPf1 :: a -> List a -> List a -> Nat
lengthAppendPf1 x xs ys =
```
```haskell
   length ((x:xs) ++ ys)
 ===    {- |(++)| 之定義  -}
   length (x : (xs ++ ys))
 ===    {- |length| 之定義 -}
   Suc (length (xs ++ ys))
 ===    {- 歸納假設 -}
   Suc (length xs +: length ys)
 ===    {- |(+)| 之定義 -}
   (Suc (length xs)) +: length ys
 ===    {- |length| 之定義 -}
   length (x:xs) +: length ys {-"~~."-}
```
:::
:::{.exer #ex:map-append}
證明對所有 |f|, |xs|, 與 |ys|, |map f (xs ++ ys) = map f xs ++ map f ys|.
:::
```texonly
%\Exercise 完成定理\@ref{thm:map-fusion}的證明。
% \Answer 欲證明|map f (map g xs) = map (f.g) xs|, 在 |xs| 上歸納。
%
% \noindent{\bf 情況} |xs := []|, 兩邊都歸約為 |[]|.
%
% \noindent{\bf 情況} |xs := x:xs|:
% ```spec
%   map f (map g (x:xs))
% =   {- |map| 之定義 -}
%   map f (g x : map g xs)
% =   {- |map| 之定義 -}
%   f (g x) : map f (map g xs)
% =   {- |(.)| 之定義 -}
%   (f.g) x : map f (map g xs)
% =   {- 歸納假設 -}
%   (f.g) x : map (f.g) xs
% =   {- |map| 之定義 -}
%   map (f.g) (x:xs) {-"~~."-}
% ```
```
:::


## 從資料、程式、到證明 {#sec:data-prog-proof}

本節再用一個例子談談「讓符號為我們工作」。考慮證明下述性質：
```texonly
\begin{align*}
 |sum . concat = sum . map sum|\mbox{~ ~.} \label{eq:sum-concat}
\end{align*}
```
根據外延相等，這相當於證明對所有|xss|,
|sum (concat xss) = sum (map sum xss)|.
當 |xss := []|, 等號兩邊都歸約成 |0|. 考慮 |xss := xs :xss| 的情況：
```{.haskell .invisible}
sumConcatInd :: List Int -> List (List Int) -> Int
sumConcatInd xs xss =
```
```haskell
      sum (concat (xs:xss))
 ===    {- |concat| 之定義 -}
      sum (xs ++ concat xss)
 ===    {- 因 |sum (xs ++ ys) = sum xs + sum ys| -}
      sum xs + sum (concat xss)
 ===    {- 歸納假設 -}
      sum xs + sum (map sum xss)
 ===    {- |sum| 之定義 -}
      sum (sum xs : map sum xss)
 ===    {- |map| 之定義 -}
      sum (map sum (xs:xss)) {-"~~."-}
```
{.noindent}讀者對這個證明最大的疑問可能是：我們怎麼知道該用 |sum (xs ++ ys) = sum xs + sum ys| 呢？
為什麼當我們遇到 |sum (xs ++ concat xss)|, 我們不是把 |xs| 再拆成首和尾，甚至把 |xss| 拆開？
這是許多綜合考量的結果。
首先，我們預期會使用歸納假設，因此證明前幾步的目的均為把 |sum| 推到 |concat xss| 左側，試圖做出 |sum (concat xss)|.
此處我們再強調一個觀念：*證明中的步驟不是瞎猜的，而是有目的的*。
符號給我們提示：我們需要把 |sum| 往右推，因此我們試圖尋找能完成這個目標的性質。

其次，*證明的結構跟隨著程式的結構*。由於 |concat| 是由 |(++)| 定義的，每一個關於 |concat| 的定理，均很有可能奠基在一個關於 |(++)| 的相對定理上。為了證明一個描述 |sum| 與 |concat| 的關係的定理，我們可能會需要一個關於 |sum| 與 |(++)| 的性質。

有了符號給我們的這兩個線索，我們便可以運用我們對語意的了解：|sum| 與 |(++)| 應該會滿足什麼性質？
我們可猜測應該是 |sum (xs ++ ys) = sum xs + sum ys| --- 根據我們對 |sum| 與 |(++)| 的語意上的理解，這性質成立的機會很大。
而且，這個性質允許我們把 |sum| 推到右邊。

符號仍舊幫助了我們。
我們不可能期待所有定理都只靠符號推導出，在關鍵時刻我們仍會需要對於特定領域的、語意上的知識。但符號給了我們許多引導，
縮小我們需要猜測的範圍。

至於 |sum (xs ++ ys) = sum xs + sum ys| 該怎麼證明呢？
不要急著把 |xs| 與 |ys| 都拆開。
觀察等號左右邊的式子，根據 |(++)|, |(+)| 與 |sum| 的定義，兩個式子的歸約都是先對 |xs| 做情況分析。
因此我們可試著在 |xs| 上做歸納。

*證明的結構跟隨著程式的結構* --- 這是做證明時相當好用的指引。
程式對哪個參數做歸納，我們做證明時就在那個參數上做歸納。
函數 |f| 用函數 |g| 定義，我們證明 |f| 的性質時就可以預期會需要一個 |g| 的相關性質。
並非所有證明都可以如此做出，但這個模式在許多情況下都適用。

而如我們所知，*程式的結構又跟隨著資料的結構*：
例如，串列有 |[]| 和 |x:xs| 兩個情況，我們定義串列上的程式時也分出這兩個情況，
定義 |f (x:xs)| 時可以使用 |f xs| 的值。
資料結構、程式、與證明於是有著這樣的連續關係：
歸納定義出一個資料結構，我們就有了一個在上面歸納定義程式的方法；
有了歸納定義的程式，我們便知道怎麼做關於它的歸納證明。
一切由資料結構開始。

:::{.exlist}
:::{.exer #ex:length-concat}
證明 |length . concat = sum . map length|.
我們會需要什麼關於 |(++)| 的相關性質？
:::
:::{.exans}
考慮 |xs:xss| 的情況：
```spec
     length (concat (xs:xss))
===  length (xs ++ concat xss)
===    {- 因 |length (xs ++ ys) = length xs + length ys| -}
     length xs + length (concat xss)
===    {- 歸納假設 -}
     length xs + sum (map length xss)
===  sum (length xs : map length xss)
===  sum (map length (xs:xss)) {-"~~."-}
```
{.noindent}我們需要的性質是 |length (xs ++ ys) = length xs + length ys| .
:::
:::{.exer #ex:map-concat}
證明對所有 |f|, |map f . concat = concat . map (map f)|.
我們會需要什麼關於 |(++)| 的相關性質？
:::
:::{.exans}
考慮 |xs:xss| 的情況：
```spec
     map f (concat (xs:xss))
===  map f (xs ++ concat xss)
===   {- 因 |map f (xs ++ ys) = map f xs ++ map f ys| -}
     map f xs ++ map f (concat xss)
===   {- 歸納假設 -}
     map f xs ++ concat (map (map f xss))
===  concat (map f xs : map (map f xss))
===  concat (map (map f (xs : xss))) {-"~~."-}
```
{.noindent}我們需要的性質是 |map f (xs ++ ys) = map f xs ++ map f ys| .
:::
:::

## 更多歸納定義與證明 {#sec:more-inductive-defns}

為讓讀者熟悉，本節中我們多看一些自然數或串列上的歸納定義。

### |filter|, |takeWhile|, 與 |dropWhile|

{title="filter"} 我們曾見過的函數 |filter| 可寫成如下的歸納定義：
```spec
filter :: (a -> Bool) -> List a -> List a
filter p []      = []
filter p (x:xs)  = if p x then x : filter p xs else filter p xs {-"~~."-}
```
{.noindent}在 |filter| 的許多性質中，我們試著證明下述性質作為例子：
:::{.theorem #thm:filter-map}
|filter p . map f = map f . filter (p . f)|.
:::
:::{.proof}
和定理\@ref{thm:map-fusion}一樣，待證式的左右邊都是函數。
根據外延相等，我們將左右邊各補上參數 |xs|，並在 |xs| 上做歸納：
```texonly
\begin{align*}
  |filter p (map f xs) = map f (filter (p . f) xs)| \mbox{~~.}
\end{align*}
```
情況 |x := []| 中左右邊都可化簡成|[]|.
我們看看 |xs := x:xs| 的情況：
```{.haskell .invisible}
filterMapPf1 p f x xs =
```
```haskell
   filter p (map f (x:xs))
 ===    {- |map| 之定義 -}
   filter p (f x : map f xs)
 ===    {- |filter| 之定義 -}
   if p (f x)  then f x : filter p (map f xs) else filter p (map f xs)
 ===    {- 歸納假設 -}
   if p (f x)  then f x : map f (filter (p . f) xs) else map f (filter (p . f) xs)
 ===    {- |map| 之定義 -}
   if p (f x)  then map f (x : filter (p . f) xs) else map f (filter (p . f) xs)
 ===    {- 因 |f (if p then e1 else e2) = if p then f e1 else f e2|, 如後述 -}
   map f (if p (f x) then x : filter (p . f) xs else filter (p . f) xs)
 ===    {- |filter| 之定義 -}
   map f (filter (p . f) (x:xs)) {-"~~."-}
```
:::

{title="終止與證明"}
上述證明的倒數第二步為將 |map f| 提到外面，用了一個關於 |if| 的性質：
```texonly
\begin{align}
  |f (if p then e1 else e2) {-"~"-}={-"~"-} if p then f e1 else f e2| \mbox{~~.} \label{eq:fn-if-distribute}
\end{align}
```
這性質對嗎？若 |p| 成立，左右手邊都化簡為|f e1|, 若 |p| 不成立，左右手邊都化簡為 |f e2|. 因此 \@eqref{eq:fn-if-distribute} 應該成立，是嗎？

答案是：如果我們假設的世界中有不終止的程式，\@eqref{eq:fn-if-distribute}便不正確了。
例如，當 |f| 是 |three x = 3|，而 |p| 是個永遠執行、不終止的算式（例：|let b = not b in b|)：
```texonly
\begin{align*}
  |three (if p then e1 else e2) {-"~\stackrel{?}{=}~"-} if p then three e1 else three e2|
\end{align*}
```
上述式子的左手邊直接化簡成|3|, 但右手邊卻不會終止，因為 |if| 得知道 |p| 的值。
我們找到了 \@eqref{eq:fn-if-distribute} 的反例！

在允許可能不終止的程式存在的世界中，\@eqref{eq:fn-if-distribute}得多些附加條件。
通常的做法是限定 |f| 須是個*嚴格函數*, 意即 |f| 的輸入若不終止，|f|也不會終止。
但\@eqref{eq:fn-if-distribute}並不是唯一帶著附加條件的性質 --- 許多常用性質都得加上類似的附加條件。
所有狀況分析也都得將不終止考慮進去，例如，自然數除了 |Zero| 與 |Suc n| 之外，還多了第三種情況「不終止」。^[\cite{Bird:87:Introduction} 就採用這種作法。]
推論與證明變得更複雜。
有些人因此較喜歡另一條路：藉由種種方法確保我們只寫出會終止的程式，便可假設我們確實活在所有程式都正常終止的世界中。

{title="保護式 v.s. 條件分支"} 有些人喜歡用保護式語法定義 |filter|：
```spec
filter p []      = []
filter p (x:xs)  | p x        = x : filter p xs
                 | otherwise  = filter p xs {-"~~."-}
```
{.noindent}若在此定義下證明定理\@ref{thm:filter-map}，依「證明的結構與程式的結構相同」的原則，順理成章地，我們可在 |xs:=x:xs| 中再分出 |p (f x)| 成立與不成立的兩個子狀況：
```spec
 {-"\mbox{\bf 狀況}~"-} xs:=[]:  ...
 {-"\mbox{\bf 狀況}~"-} xs:=x:xs: ...
    {-"\mbox{\bf 狀況}~"-} p (f x):
         filter p (map f (x:xs))
      =    {- |p (f x)| 成立 -}
         f x : filter p (map f xs)
      =  ... {-"~~."-}
    {-"\mbox{\bf 狀況}~"-} not (p (f x)):
         filter p (map f (x:xs))
      =   {- |(not p (f x))| -}
         filter p (map f xs)
      =  ... {-"~~."-}
```
{.noindent}這個定義中不用 |if|, 因此證明中也用不上 \@eqref{eq:fn-if-distribute}, 但該證明要成立仍須假設所有程式都正常終止 --- 我們少證了一個 「|p (f x)| 不終止」的情況（而確實，在此情況下\@eqref{eq:fn-if-distribute}並不成立）。喜歡用哪個方式純屬個人偏好。

前幾章提過的 |takeWhile| 與 |dropWhile| 兩函數型別與 |filter| 相同。他們可寫成如下的歸納定義：
```spec
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p []      = []
takeWhile p (x:xs)  = if p x then x : takeWhile p xs else [] {-"~~,"-}

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p []      = []
dropWhile p (x:xs)  = if p x then dropWhile p xs else x:xs {-"~~."-}
```
{.noindent}兩者都是在輸入串列上做歸納。兩者也都可用保護式語法定義。

:::{.exlist}
:::{.exer #ex:take-cat-drop}
證明 |takeWhile p xs ++ dropWhile p xs = xs|.
:::
:::{.exer}
以保護式語法定義 |takeWhile| 與 |dropWhile|, 以此定義做做看習題 \@ref{ex:take-cat-drop}.
:::
:::



### |elem| 與不等式證明

{title="不等式證明"} 給定如下的定義，|elem x xs| 判斷 |x| 是否出現在串列 |xs| 中：
```spec
elem x []      = False
elem x (y:xs)  = (x == y) || elem x xs {-"~~."-}
```
% 在 Haskell 中 |elem| 的型別是 |Eq a => a -> List a -> Bool|.
% 我們將在第???章詳細解釋 |Eq a =>| 的部分。
% 目前可粗略地理解為：|elem| 檢查某型別為 |a| 的元素是否出現在型別為 |List a| 的串列中，但有個附加條件：屬於型別 |a| 的值必須能判斷是否相等。
目前為止，我們所練習的都是以|(=)|將式子串起的等式證明。
以下以|elem|為例，我們嘗試證明一個「不等式」：
```spec
  elem z xs {-"~"-}==>{-"~"-} elem z (xs ++ ys) {-"~~."-}
```
{.noindent}以口語說出的話：「若|z| 出現在|xs| 中，|z| 也出現在|xs ++ ys| 中」。
欲證明上式，該從哪一側推到哪一側呢？
一般認為從式子較長、或結構較複雜的那側開始，化簡成較短、較簡單的那側，是較容易的。
因此我們嘗試由右側推到左側：由 |elem z (xs ++ ys)| 開始，尋找使之成立的條件，
並希望 |elem z xs| 是足夠的。
:::{.proof}
在 |xs| 上做歸納。基底 |xs := []| 的狀況在此省略，
看 |xs:=x:xs| 的狀況：
```spec
     elem z ((x:xs) ++ ys)
<=>   {- |(++)| 之定義 -}
     elem z (x : (xs ++ ys))
<=>   {- |elem| 之定義 -}
     (z==x) || elem z (xs ++ ys)
<==   {- 歸納假設 -}
     (z==x) || elem z xs
<=>   {- |elem| 之定義 -}
     elem z (x:xs) {-"~~."-}
```
:::
{.noindent}讀者可注意：第1, 2, 4 步使用的邏輯關係都是 |(<=>)| ，第 3 步卻是 |(<==)|，因此整個證明建立了「若|elem z (x:xs)|，則|elem z ((x:xs) ++ ys)|」。

:::{.exlist}
:::{.exer}
證明 |not (elem z (xs ++ ys)) ==> not (elem z xs)|.
:::
:::{.exer #ex:elem-catleft}
證明 |elem z xs ==> elem z (ys ++ xs)|.
:::
:::{.exer #ex:all-monotonic}
證明|(forall x . p x ==> q x) ==> all p xs ==> all q xs|.
其中 |all| 的定義為：
```spec
all :: (a -> Bool) -> List a -> Bool
all p []      = True
all p (x:xs)  = p x && all p xs {-"~~."-}
```
:::
:::{.exer}
證明 |all (`elem` xs) (filter p xs)|. 其中 |x `elem` xs| 是 |elem x xs| 的中序寫法。 我們可能需要習題 \@ref{ex:elem-catleft} 和 \@ref{ex:all-monotonic} 的結果，以及下述性質：
```texonly
\begin{equation}
    |if p then x else x| = |x| \mbox{~ ~.} \label{eq:ifpxx}
\end{equation}
```
:::
:::{.exans}
在 xs 上做歸納。
```{.haskell .invisible}
allElemFilterPf1 p x xs =
```
```haskell
      all (`elem` (x:xs)) (filter p (x:xs))
 <=>  all (`elem` (x:xs)) (if p x then x:filter p xs else filter p xs)
 <=>  if p x  then all (`elem` (x:xs)) (x:filter p xs)
              else all (`elem` (x:xs)) (filter p xs)
 <=>  if p x  then x `elem` (x:xs) && all (`elem` (x:xs)) (filter p xs)
              else all (`elem` (x:xs)) (filter p xs)
 <==  if p x  then all (`elem` (x:xs)) (filter p xs)
              else all (`elem` (x:xs)) (filter p xs)
 <=>     {- \@eqref{eq:ifpxx} -}
      all (`elem` (x:xs)) (filter p xs)
 <==     {- 因習題 \@ref{ex:elem-catleft}, |z `elem` (x:xs) <== z `elem` xs| -}
      all (`elem` xs) (filter p xs)
 <==  True {-"~~."-}
```
:::
:::

### 串列區段 {#sec:list-segments}

{title="前段與後段"}
本章目前為止討論的歸納定義都依循著這樣的模式：欲定義 |f :: List a -> b|, 只要為 |f []| 與 |f (x:xs)| 找到定義。在定義後者時，只需定義出由 |f xs| 做出 |f (x:xs)| 的關鍵一步。
目前為止，這關鍵一步都是加一、加上一個元素等簡單的動作。現在我們來看些更複雜的例子。

例\@ref{ex:inits}中曾提及：如果一個串列 |xs| 可分解為 |ys ++ zs|, 我們說 |ys| 是 |xs| 的一個*前段(prefix)*,\index{list 串列!prefix 前段}
|zs| 是 |xs| 的一個*後段(suffix)*.\index{list 串列!suffix 後段}
例如，串列 |[1,2,3]| 的前段包括 |[]|, |[1]|, |[1,2]|, 與|[1,2,3]| （注意：|[]|是一個前段，串列 |[1,2,3]| 本身也是）, 後段則包括 |[1,2,3]|, |[2,3]|, |[3]|, 與 |[]|。我們是否能定義一個函數 |inits :: List a -> List (List a)|, 計算給定串列的所有前段呢？
例\@ref{ex:inits}給的答案是：
```spec
inits xs = map (\n -> take n xs) [0 .. length xs] {-"~~."-}
```
{.noindent}如果不用組件，改用歸納定義呢？我們試試看：
```spec
inits :: List a -> List (List a)
inits []      = ?
inits (x:xs)  = ?
```
{.noindent}基底狀況 |inits []| 的可能選擇是 |[[]]| （見後述）。至於歸納步驟該怎麼寫？
我們用例子來思考。比較 |inits [2,3]| 與 |inits [1,2,3]|:
```spec
  inits [2,3]    = [[],[2],[2,3]] {-"~~,"-}
  inits [1,2,3]  = [[],[1],[1,2],[1,2,3]] {-"~~."-}
```
{.noindent}假設我們已算出 |inits [2,3]|, 如何把它加工變成 |inits [1,2,3]|? 請讀者暫停一下，思考看看！

一個思路是：如果在 |[[],[2],[2,3]]| 中的每個串列前面都補一個 |1|, 我們就有了 |[[1],[1,2],[1,2,3]]|. 再和 |inits [1,2,3]| 比較，就只差一個空串列了！
因此 |inits| 的一種定義方式是：
```haskell
inits :: List a -> List (List a)
inits []      = [[]]
inits (x:xs)  = [] : map (x:) (inits xs) {-"~~."-}
```

在此得提醒：有些讀者認為基底狀況 |inits []| 的值選為 |[[]]|，是因為結果的型別是 |List (List a)|
（直覺地把每個 |List| 都對應到一組中括號，或認為 |[[]]| 是型別為 |List (List a)| 的最簡單的值）。
但事實並非如此：畢竟，|[]| 的型別也可以是 |List (List a)|！
我們讓 |inits [] = [[]]| 的原因是空串列 |[]| 的「所有前段」只有一個，恰巧也是 |[]|。
就如同在自然數上的歸納函數定義中，有些基底狀況是 |0|, 有些是 |1|, 有些是別的值，
此處我們也依我們的意圖，選定最合適的基底值。

::: {.texonly}
```
%format initsp = "\Varid{inits}^{+}"
```
:::
:::{.exlist}
:::{.exer}
試定義 |initsp :: List a -> List (List a)|, 計算一個串列的所有*非空前段*。例如 |initsp [1,2,3]| 是 |[[1],[1,2],[1,2,3]]|。當然，其中一個定義方式是 |initsp = tail . inits|. 你能以歸納方式定義出 |initsp| 嗎？
```spec
initsp []      = ?
initsp (x:xs)  = ?
```
:::
:::{.exans .compact}
```haskell
initsp :: List a -> List (List a)
initsp []      = []
initsp (x:xs)  = [x] : map (x:) (initsp xs) {-"~~."-}
```
:::
:::{.exer}
我們驗證一下 |inits| 在例\@ref{ex:inits} 中的組件定義與本章的歸納定義是相等的。定義 |upto :: Nat -> List Nat|:
```haskell
upto Zero     = [Zero]
upto (Suc n)  = 0 : map (Suc) (upto n) {-"~~."-}
```
{.noindent}使得 |upto n = [0.. n]|.
假設 |inits| 已如本節一般地歸納定義，證明對所有 |xs|,
|inits xs = map (\n -> take n xs) (upto (length xs))|.
您可能會需要 |map| 融合定理（定理\@ref{thm:map-fusion}），
:::
:::{.exans}
在 |xs| 上做歸納。

\noindent **情況** |xs := []|. 此時等號兩邊都是 |[[]]|.

\noindent **情況** |xs := x:xs|.
```{.haskell .invisible}
initsEquival x xs =
```
```haskell
    map (\n -> take n (x:xs)) (upto (length (x:xs)))
 ===    {- |length| 之定義 -}
    map (\n -> take n (x:xs)) (upto (Suc (length xs)))
 ===    {- |upto| 之定義 -}
    map (\n -> take n (x:xs)) (0 : map (Suc) (upto (length xs)))
 ===    {- |length| 之定義 -}
    map (\n -> take n (x:xs)) (0 : map (Suc) (upto (length xs)))
 ===    {- |map|之定義，|take 0 (x:xs) = []| -}
    [] : map (\n -> take n (x:xs)) (map (Suc) (upto (length xs)))
 ===    {- 定理\@ref{thm:map-fusion}：|map f . map g = map (f.g)| -}
    [] : map (\n -> take (Suc n) (x:xs)) (upto (length xs))
 ===    {- |take| 之定義 -}
    [] : map (\n -> x : take n xs) (upto (length xs))
 ===    {- 定理\@ref{thm:map-fusion}：|map f . map g = map (f.g)| -}
    [] : map (x:) (map (\n -> take n xs) (upto (length xs)))
 ===    {- 歸納假設 -}
    [] : map (x:) (inits xs)
 ===    {- |inits| 之定義 -}
    inits (x:xs) {-"~~."-}
```
\end{exlist}

定義傳回後段的函數 |tails| 時可依循類似的想法：
如何把 |tails [2,3] = [[2,3],[3],[]]|加工，得到 |tails [1,2,3] = [[1,2,3],[2,3],[3],[]]|?
這次較簡單：加上 |[1,2,3]| 即可。
的確，串列 |x:xs| 的後段包括 |x:xs| 自己，以及 |xs| 的後段：
```haskell
tails :: List a -> List (List a)
tails []      = [[]]
tails (x:xs)  = (x:xs) : tails xs {-"~~."-}
```
{.noindent}在習題 \@ref{ex:zip-inits-tails} 中我們將證明一個將 |inits| 與 |tails| 牽上關係的定理：將 |inits| 傳回的前段與 |tails| 傳回的後段照其順序對應，每對接起來都是原來的串列。

{title="連續區段"}
給定一個串列，許多傳統最佳化問題的目標是計算符合某條件的*連續區段*（簡稱「區段」）。%
\index{list 串列!segment 區段}
例如，|[1,2,3]| 的區段包括|[]|,|[1]|,|[2]|,|[3]|,|[1,2]|,|[2,3]|, 以及
|[1,2,3]|本身。
我們可用 |inits| 與 |tails| 得到一個串列的所有區段。
```haskell
segments :: List a -> List (List a)
segments = concat . map inits . tails {-"~~."-}
```
{.noindent}但 |segments| 無法寫成本章目前這種形式的歸納定義。我們將在以後的章節再討論到 |segments|.

\begin{exlist}
\Exercise 試著把 |segments| 寫成如下的歸納定義：
```spec
segments :: List a -> List (List a)
segments []      = ?
segments (x:xs)  = ... segments xs ... {-"~~,"-}
```
{.noindent}在歸納步驟中希望由 |segments xs| 湊出 |segments (x:xs)|。
這是能在不對輸入串列（型別為|List a|）做任何限制之下做得到的嗎？
如果做不到，為什麼？
\Answer 考慮如何從 |segments [1,2,3]| 湊出 |segments [0,1,2,3]|.
前者應當有的結果可能是：
```spec
[[],[1],[1,2],[1,2,3],[2],[2,3],[3]] {-"~~."-}
```
{.noindent}而 |segments [0,1,2,3]| 的結果可能是：
```spec [[],[0],[0,1],[0,1,2],[0,1,2,3],[1],[1,2],[1,2,3],[2],[2,3],[3]] {-"~~."-}
```
{.noindent}乍看之下，要多做的一步是：在空串列或所有|1|開頭的串列前面補上 |0|.
之所以只擴充|1|開頭的串列，因為只有這些是原本便靠在左邊，和 |0| 相鄰的
（也就是說它們是|[1,2,3]|的前段）。

然而，當輸入串列型別為 |List a|（而不是特定的 |List Int|, |List Char| 時），我們無法用比較的方式找出這些前段。函數 |segments| 傳回的資訊不夠多。如果要歸納定義，我們必須把「前段」們和其他的區段分開。
\end{exlist}

### 插入、排列、子串列、與劃分 {#sec:fan-perm}

{title="排列"}
函數 |fan x xs| 把 |x| 插入 |xs| 的每一個可能空隙。例如，|fan 1 [2,3,4]| 可得到
|[[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]|. 讀者不妨想想它該怎麼定義？一種可能方式如下：
```haskell
fan :: a -> List a -> List (List a)
fan x []      = [[x]]
fan x (y:xs)  = (x:y:xs) : map (y:) (fan x xs) {-"~~."-}
```
{.noindent}有了 |fan|, 我們不難定義 |perms :: List a -> List (List a)|, 計算一個串列所有可能
的*排列*。例如，|perms [1,2,3] = [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]]|：
```haskell
perms :: List a -> List (List a)
perms []      = [[]]
perms (x:xs)  = concat (map (fan x) (perms xs)) {-"~~."-}
```
{.noindent}讀者可思考為何我們需要 |concat|? 如果沒有，會出現什麼錯誤？

基於 |perm| 的這個定義，我們證明一個定理：*長度為 |n| 的串列有 |fact n| 種排列*。
這個證明將使用到許多輔助性質與引理，有些已經是我們之前證明過的習題，有些則可作為接下來的習題。
在本證明之中我們也練習將連續的函數應用 |f (g (h x))| 寫成函數合成
|f . g . h $ x| 以方便計算。
:::{.theorem #thm:length-perms}
對任何 |xs|, |length (perms xs) = fact (length xs)|.
:::
:::{.proof}
在 |xs| 上做歸納。

\noindent{\bf 基底狀況} |xs:=[]|:
```{.haskell .invisible}
lengthPermsPf0 =
```
```haskell
      length (perms [])
 ===  length [[]]
 ===  1
 ===  fact (length []) {-"~~."-}
```
\noindent **歸納步驟}** |xs := x:xs|:
```{.haskell .invisible}
lengthPermsPf1 x xs =
```
```haskell
      length (perms (x:xs))
 ===    {- |perms|, |(.)|, 與 |($)| 之定義 -}
      length . concat . map (fan x) . perms $ xs
 ===    {- 因 |length . concat = sum . map length| (習題\@ref{ex:length-concat}), |map| 融合 -}
      sum . map (length . fan x) . perms $ xs
 ===    {- 因 |length . fan x = (Suc) . length| (習題 \@ref{ex:length-fan}) -}
      sum . map ((Suc) . length) . perms $ xs
 ===    {- 因 |map length (perms xs) =|
 |map (const (length xs)) (perms xs)| (習題\@ref{ex:map-length-perms})  -}
      sum . map ((Suc) . const (length xs)) . perms $ xs
 ===    {- 因 |sum (map (Suc) xs) = length xs + sum xs| (習題\@ref{ex:sum-map-suc})-}
      length (perms xs) +: sum (map (const (length xs)) (perms xs))
 ===    {- 因 |sum (map (const y) xs) = y * length xs| (習題\@ref{ex:sum-map-const}) -}
      length (perms xs) +: length xs *: length (perms xs)
 ===    {- 四則運算: |x + y * x = (1+y) * x| -}
      (Suc (length xs)) *: length (perms xs)
 ===    {- 歸納假設 -}
      (Suc (length xs)) *: fact (length xs)
 ===    {- |fact| 之定義 -}
      fact (Suc (length xs))
 ===    {- |length| 之定義 -}
      fact (length (x:xs)) {-"~~."-}
```
:::

:::{.exlist}
:::{.exer}
證明 |map f . fan x = fan (f x) . map f|.
:::
:::{.exer}
證明 |perm . map f = map (map f) . perm|.
:::
:::{.exer #ex:length-fan}
證明 |length (fan x xs) = Suc (length xs)|.
:::
:::{.exans}
在 |xs| 上做歸納。基底狀況很容易成立。考慮 |xs := y:xs|:
```{.haskell .invisible}
lengthFan x y xs =
```
```haskell
      length (fan x (y:xs))
 ===  length ((x:y:xs) : map (y:) (fan x xs))
 ===  Suc (length (map (y:) (fan x xs)))
 ===   {- 因 |length . map f = length| (練習 \@ref{ex:length-map}) -}
      Suc (length (fan x xs))
 ===   {- 歸納假設 -}
      Suc (Suc (length xs))
 ===  Suc (length (y:xs)) {-"~~."-}
```
:::
:::{.exer #ex:map-length-perms}
證明 |perms xs| 傳回的每個串列都和 |xs| 一樣長，也就是 |map length (perms xs) = map (const (length xs)) (perms xs)|.
其中 |const| 定義於第 \@pageref{para:const} 頁 --- |const y| 是一個永遠傳回 |y| 的函數。
:::
:::{.exans}
欲證明 |map length (perms xs) = map (const (length xs)) (perms xs)|, 在 |xs| 之上做歸納。基底狀況 |xs := []| 中，等號兩邊都化約為 |[0]|. 考慮歸納步驟 |xs := x:xs|:
```{.haskell .invisible}
repeatN :: a -> Nat -> List a
repeatN x n = take n (repeat x)

mapLengthPermsInd x xs =
```
```haskell
  map length (perms (x:xs))
 ===   {- |perms| 之定義 -}
  map length . concat . map (fan x) . perms $ xs
 ===   {- |map f . concat = concat . map (map f)| (習題\@ref{ex:map-concat}), |map| 融合 -}
  concat . map (map length . fan x) . perms $ xs
 ===   {- |map length . fan x = (\n -> repeatN n n) . length| -}
  concat . map ((\n -> repeatN n n) . (Suc) . length) . perms $ xs
 ===   {- |map| 融合 -}
  concat . map ((\n -> repeatN n n) . (Suc)) . map length . perms $ xs
 ===   {- 歸納假設 -}
  concat . map ((\n -> repeatN n n) . (Suc)) . map (const (length xs)) . perms $ xs
 ===   {- |map| 融合 -}
  concat . map ((\n -> repeatN n n) . (Suc) . const (length xs)) . perms $ xs
 ===   {- 歸約 $\lambda$ 算式 -}
  concat . map (repeatN (Suc (length xs)) . (Suc) . length) . perms $ xs
 ===   {- |length . fan x = (Suc) . length| (習題\@ref{ex:length-fan})-}
  concat . map (repeatN (Suc (length xs)) . length . fan x) . perms $ xs
 ===   {- \todo{???} |map (const y) = repeatN y . length| -}
  concat . map (map (const (Suc (length xs))) . fan x) . perms $ xs
 ===   {- |map f . concat = concat . map (map f)| (習題\@ref{ex:map-concat}) -}
  map (const (length (x:xs))) . concat . map (fan x) . perms $ xs
 ===   {- |perms| 之定義 -}
  map (const (length (x:xs))) (perms (x:xs)) {-"~~."-}
```
:::
:::

{title="子串列"}
函數 |sublists :: List a -> List (List a)| 計算一個串列的所有*子串列*。
後者是類似子集合的概念，只是把順序也考慮進去： |ys| 是 |xs| 的子串列，
如果將 |xs| 中的零個或數個元素移除後可得到 |ys|:
例如 |sublists [1,2,3]| 的結果可能是： |[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]|。
怎麼定義 |sublists| 呢？在基底狀況中，空串列仍有一個子串列 |[]|.
歸納步驟中，|x:xs| 的子串列可分為兩種：不含 |x| 的，以及含 |x| 的。
不含 |x| 的子串列就是 |xs| 的所有子串列（以下稱作 |yss|），
而含 |x| 的子串列就是 |yss| 中的每個串列接上 |x|. 因此我們可定義：
```haskell
sublists :: List a -> List (List a)
sublists []      = [[]]
sublists (x:xs)  = yss ++ map (x:) yss {-"~~,"-}
    where yss = sublists xs {-"~~."-}
```

:::{.exlist}
:::{.exer #ex:splits}
定義 |splits :: List a -> List (List a :* List a)|，
使 |splits xs| 傳回所有滿足 |ys ++ zs| 的 |(ys,zs)|.
例：
```texonly
\begin{align*}
   &|splits [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3,[]])]|\mbox{~~.}
\end{align*}
```
另一種說法是 |splits xs = zip (inits xs) (tails xs)|.
:::
:::{.exans .compact}
```haskell
splits :: List a -> List (List a :* List a)
splits []      = [([],[])]
splits (x:xs)  = ([],x:xs) : map ((x:)***id) (splits xs) {-"~~."-}
```
{.noindent}其中 |(f *** g) (x,y) = (f x, g y)|.
:::
:::{.exer #ex:length-sublists}
證明 |length . sublists = exp 2 . length|. 也就是說，長度為 |n| 的串列的子串列數目為 $2^n$. 你會需要的性質可能包括 \@eqref{eq:length-append} (|length (xs++ys) = length xs + length ys|), 以及 |length . map f = length|.
:::
:::{.exans}
將式子改寫為 |length (sublists xs) = exp 2 (length xs)|, 在 |xs| 上做歸納。
歸納步驟 |xs := x:xs| 為：
```{.haskell .invisible}
lengthSublists x xs =
```
```haskell
      length (sublists (x:xs))
 ===  length (sublists xs ++ map (x:) (sublists xs))
 ===    {- 因 \@eqref{eq:length-append} |length (xs++ys) = length xs + length ys|  -}
      length (sublists xs) +: length (map (x:) (sublists xs))
 ===    {- 因 |length . map = length| (練習 \@ref{ex:length-map})-}
      length (sublists xs) +: length (sublists xs)
 ===    {- 因 |x + x = 2 * x| -}
      2 *: length (sublists xs)
 ===    {- 歸納步驟 -}
      2 *: exp 2 (length xs)
 ===  exp 2 (Suc (length xs))
 ===  exp 2 (length (x:xs)) {-"~~."-}
```
:::
:::

```texonly
% ```spec
%   elem z xs ==> elem z (ys ++ xs)
% ```
% ```spec
%     elem z ((y:ys) ++ xs)
% <=>
%     elem z (y : (ys ++ xs))
% <=>
%     (z==y) || elem z (ys ++ xs)
% <==
%     (z==y) || elem z xs
% ```

% exercise
%   not (elem z (xs ++ ys)) ==> not (elem z xs)
%
%      not (elem z ((x:xs) ++ ys))
% <=>
%      not (elem z (x : (xs ++ ys)))
% <=>
%      not ((z==x) || elem z (xs ++ ys))
% <=>
%      z /= x && not (elem z (xs ++ ys))
% ==>
%      z /= x && not (elem z xs)
% <=>  not (elem z (x:xs))

% ```spec
%   all (`elem` xs) (filter p xs)
% ```
%
% ```spec
%    all (`elem` (x:xs)) (filter p (x:xs))
% =  all (`elem` (x:xs))
%      (if p x then x: filter p xs else filter p xs)
% =  if p x  then all (`elem` (x:xs)) (x : filter p xs)
%            else all (`elem` (x:xs)) (filter p xs)
% =  if p x  then x `elem` (x:xs) && all (`elem` (x:xs)) (filter p xs)
%            else all (`elem` (x:xs)) (filter p xs)
% ```
```
