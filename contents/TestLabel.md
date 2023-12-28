# 1 {#ch:1}

::: {.definition title="外延相等(extensional equality)" #def:extensional-eq}
給定兩個型別相同的函數 |f| 和 |g|, 當我們說它們*外延相等*(*extensionally equal*)\index{extensional equality 外延相等}，
意思是給任何一個輸入，|f| 和 |g| 都算出相等的輸出。也就是：
|(forall x. f x = g x)|.

本書中，當我們寫兩個函數相等(|f = g|)時，指的便是外延相等，除非例外註明。
:::

## 章節 {#sec:sec0}

::: {.example #ex:ex1}
工讀生每小時的時薪為新台幣 130 元。
假設一週有五個工作天，每天有八小時上班時間。
定義一個函數 |payment :: Int -> Int|，輸入某學生工作的週數，計算其薪資。
:::
::: {.answer}
我們當然可直接用一個式子算出薪資。但為清楚起見，我們可用兩個區域識別字 |days| 和 |hours|,
分別計算該學生工作的日數和時數。如果用 |let|, 可這麼做。
:::

## 章節

::: {.example #ex:ex2}
工讀生每小時的時薪為新台幣 130 元。
假設一週有五個工作天，每天有八小時上班時間。
定義一個函數 |payment :: Int -> Int|，輸入某學生工作的週數，計算其薪資。
:::
::: {.answer}
我們當然可直接用一個式子算出薪資。但為清楚起見，我們可用兩個區域識別字 |days| 和 |hours|,
分別計算該學生工作的日數和時數。如果用 |let|, 可這麼做。
:::

::: {.example}
工讀生每小時的時薪為新台幣 130 元。
假設一週有五個工作天，每天有八小時上班時間。
定義一個函數 |payment :: Int -> Int|，輸入某學生工作的週數，計算其薪資。
:::
::: {.answer}
我們當然可直接用一個式子算出薪資。但為清楚起見，我們可用兩個區域識別字 |days| 和 |hours|,
分別計算該學生工作的日數和時數。如果用 |let|, 可這麼做。
:::

## 章節 {#sec:sec2}

### 子章節 {#sec:subsec1}

::: {.example #ex:ex4}
工讀生每小時的時薪為新台幣 130 元。
假設一週有五個工作天，每天有八小時上班時間。
定義一個函數 |payment :: Int -> Int|，輸入某學生工作的週數，計算其薪資。
:::
::: {.answer}
我們當然可直接用一個式子算出薪資。但為清楚起見，我們可用兩個區域識別字 |days| 和 |hours|,
分別計算該學生工作的日數和時數。如果用 |let|, 可這麼做。
:::

## 章節 {#sec:sec3}

::: {.example #ex:ex5}
工讀生每小時的時薪為新台幣 130 元。
假設一週有五個工作天，每天有八小時上班時間。
定義一個函數 |payment :: Int -> Int|，輸入某學生工作的週數，計算其薪資。
:::
::: {.answer}
我們當然可直接用一個式子算出薪資。但為清楚起見，我們可用兩個區域識別字 |days| 和 |hours|,
分別計算該學生工作的日數和時數。如果用 |let|, 可這麼做。
:::

:::{.exlist}
:::{.exer}
事實上，|curry| 與 |uncurry| 的存在證明了 |(a :* b) -> c|
與 |a -> b -> c| 是同構的。試證明
|curry . uncurry = id|, 以及 |uncurry . curry = id|.
:::
:::{.exans}
欲證明 |curry . uncurry = id|：
```spec
     curry . uncurry = id
<=>    {- 外延相等及 |id| 之定義 -}
     (forall f : curry (uncurry f) = f)
<=>    {- 外延相等 -}
     (forall f x y : curry (uncurry f) x y = f x y) {-"~~."-}
```
{.nobreak}因此我們證明 |curry (uncurry f) x y = f x y| 如下：
```spec
   curry (uncurry f) x y
=   {- |curry| 之定義 -}
   uncurry f (x,y)
=   {- |uncurry| 之定義 -}
   f x y  {-"~~."-}
```

與此相似，欲證明 |uncurry . curry = id| 我們須證明
|uncurry (curry f) (x,y) = f (x,y)|，其證明也與上面的證明類似。
:::
:::{.exer #ex:uncurry-apply}
請說明 |map (uncurry ($))| 的型別與功能。
關於 |($)| 請參考第\pageref{para:fun-apply}頁。
:::
:::{.exans}
|map (uncurry ($)) :: List ((a -> b) :* a) -> List b|.
它拿一個內容均為「(函數 $\times$ 參數)」序對的串列，將每個函數作用在其參數上。
例如：
```spec
map (uncurry ($)) [((1+), 3), (square, 4), (smaller 5, 3)]
```
{.nobreak}會得到 |[1+3,4*4,3]|.
:::
:::
