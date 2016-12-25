# 最初の一歩
## 目的
私は今回、 stack を用いて Haskell で scheme を実装するチュートリアルである scheme 48 を読みました。この記事の第一目的は、自分が行った思考などを記録することが目的ですが、その副産物として
- stack を実際に利用しているときに遭遇する問題と解決方法
- Haskell の実践的なチュートリアルとして scheme 48 を読み始めたが理解ができなかった人向けの補足
が含まれています。二番目の補足に関しては、scheme 48 を読んで、私が理解するために使ったがこの記事には明示されていない知識、または読んでいて理解が容易ではないと感じた部分などを補足しています。またこの元記事は license が Creative Commons (CC) のため全文がこの記事に含まれています。よってこの記事だけで全てのチュートリアルを読み進めることができます。またこの記事は github で全文が公開されているため修正案・指摘などを Pull Request することができます (もちろん Mail や Twitter・issue での報告も受け賜わります)。

## 事前知識

チュートリアルであると言っており

> このチュートリアルの対象読者は主に以下の2種類です。
>   1. LispかSchemeを知っていて、Haskellを学びたい人
>   2. プログラミング言語を何も知らないけれども、一定の背景知識を持っていてコンピュータに詳しい人

というようにしていますが、残念ながらこの文章は本当に Haskell について何も知らない人へのチュートリアルとしては不十分です。『すごい Haskell たのしく学ぼう』のできれば 8 章あたりまで、または『プログラミングHaskell』の 7 章まで読んでいると Haskell の基本としての準備は整うかと思います。完全に理解をしている必要はなく、このチュートリアルを進める中で、 Haskell とセットで語られることが多いモナドなどと同時に理解をすすめることができるでしょう。実際に Haskell の環境を作った上で一度目を通して簡単に自分の手で動かしていれば、このチュートリアルを読む準備は整うと考えられます。もちろん、上述の書籍には可能な限り目を通しておく方がこの記事の理解は容易になる。

## 下準備

前記事の stack で作成したプロジェクトを使います。始めは `app/` 内の `Main.hs` を変更していきます。

```haskell:Main.hs
module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0)
```

```sh
$ stack exec scheme48-exe Nobunaga
Hello, Nobunaga
```

## 内容

> モナドは「私たちはある決まったやり方でいくらかの追加情報とともに値を持ち回り組合せますが、殆どの関数はそれについて気にしなくていいですよ」と言っています。

現段階では例が無いので追加情報云々がよくわかりませんが今回の main 関数を見てみると `getLine :: IO String` という関数が `IO String` の値を返しまています。この `IO String` の値を謎の矢印 `<-` を通して `args` が受けとり `IO String` の `String` 部分だけとなります。そして、それを後続の式 `putStrLn ("Hello, " ++ args !! 0)` に渡すことになりますが、`"Hello, " ++ ` というのは残りの部分に `String` を要求する関数と見ることができるので、`IO` という部分を気にせずにプログラミングが行えているのがわかります。そして最後にその `String` を受けとった `putStrLn` が `IO String` に戻す役割をしています。

> name <- action1
> action2
>
> 最初の形はaction1の結果を後続のアクションで使えるようnameに束縛します。例えば、もしaction1の型がIO [String] (getArgsのように文字列のリストを返すIOアクション)なら、nameは"bind"演算子>>=によって後続の全てのアクションで持ち回される文字列のリストに束縛されます。二つ目の形はただaction2を実行し、もしあれば次の行に>>演算子を使ってそれを渡します。

いきなり `do` 構文が `>>=` や `>>` の式に変換可能であることの例なども無しに話をし始めました。この辺りですでにこの文章を読むのが不安でしょうがなくなります。(逆に言えばこのあたりでこのまとめ記事を書こうという決心をしました) この前提知識は明らかに

> このチュートリアルの対象読者は主に以下の2種類です。
>   1. LispかSchemeを知っていて、Haskellを学びたい人
>   2. プログラミング言語を何も知らないけれども、一定の背景知識を持っていてコンピュータに詳しい人

に当てはまらない気がしますね。今回の `main` 関数は

```haskell:Main.hs
main :: IO ()
-- main = do
--   args <- getArgs
--   putStrLn ("Hello, " ++ args !! 0)
main = getArgs >>= \args -> putStrLn ("Hello, " ++ args !! 0)
```

このように書きかえることができます。 `\args -> putStrLn ("Hello, " ++ args !! 0)` はラムダ式です。また `(>>)` に関しては、雑な例ですが

```haskell:Main.hs
main :: IO ()
-- main = do
--   getLine
--   args <- getArgs
--   putStrLn ("Hello, " ++ args !! 0)
main = getLine >> getArgs >>= \args -> putStrLn ("Hello, " ++ args !! 0)
```

この例は完全に無意味なことをしていますが `getLine :: IO String` で入力を受けとり、それを使わずに後続のアクションを実行しています。このようにを何かアクションをするけどその返り値は使わないとい (IO モナドであれば副作用のみに目的がある) ときに使えます。

> bind演算子の意味付けは特定のモナドに依存するので、一つのdoブロックの中に異なるモナド型の動作を混在させることはできません。この場合なら、IOモナドのみ使用可能です。

自分は競技プログラミングで Haskell を使っていたので、 IO モナド以外にほとんど触ったことがなかったのでこれを意識することは殆どありませんでした。逆に言えば IO モナドだけなんとなくわかっていれば、実際に CLI アプリなどの Haskell のコードは書けるとも言えます。

### 練習問題 1
```haskell:main.hs
module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0 ++ args !! 1)
```

```sh
$ stack build
$ stack exec scheme48-exe Nobunaga Oda
Hello, NobunagaOda
```

### 練習問題 2
```haskell:main.hs
module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn . show $ (read (args !! 0) + read (args !! 1))
```

```sh
$ stack build
$ stack exec scheme48-exe 10 20
30
```

### 練習問題 3
```haskell
module Main where

main :: IO ()
main = do
  putStrLn "Please input your name"
  line <- getLine
  putStrLn $ "Hello, " ++ line
```

```sh
$ stack build
$ stack exec scheme48-exe
Please input your name
Nobunaga Oda
Hello, Nobunaga Oda
```

# 構文解析
## 簡単なパーサ

`Text.ParserCombinators.Parsec` を main で使うには .cabal を編集する必要がある。

```cabal
executable scheme48-exe
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:       base
                     , scheme48
                     , parsec
  default-language:    Haskell2010
```

ただここからは Parser という別なモジュールにコードを書いて行くのが良いと思うのでファイルを分ける。 `new-template` を使っている場合は Lib.hs があると思うのでこれを Parser.hs にして、 .cabal も書き変える。

```sh
$ pwd
~/Haskell/scheme48
$ ls src/
Parser.hs
```

```cabal
library
  hs-source-dirs:      src
  exposed-modules:     Parser
  build-depends:       base >= 4.7 && < 5
                     , parsec
  default-language:    Haskell2010
```

```haskell:Parser.hs
module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
```

これは〈symbol というものは "!#$%&|*+-/:<=>?@^\_~" のなかのひとつである〉という定義と読める。この段階では `Parser` という型を持つ関数が何をするのかイメージしずらいかもしれないが、 Bool 値を返す関数である述語に近いだろう。例えば `symbol` であれば、この条件 `oneOf "..."` でが成立すれば、これは真でありそれにマッチしたものを返す、というような。

```haskell:Parser.hs
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
                   Left err -> "No match" ++ show err
                   Right val -> "Found value"
```
ぱっと見わかりづらいかもしれないので補足しておくと

```haskell
case exp of
  v1 -> e1
  v2 -> e2
  ...
  vn -> en
```

これが Haskell の `case` 式の一般形であるので、今回の場合一般形の `exp` とはすなわち `parse symbol "lisp" input` である。 `parse` というのはここでは定義していないので Parsec の中で定義されている、引数を3つ取る関数だろうということがわかる。実際にこのように定義されている http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Prim.html#v:parse
返り値は `Either ParseError a` という型であり、
> Haskellの一般的な慣習に従って、ParsecはEitherデータ型を返します。Left構築子でエラーを、Rightで通常の値を表します。
の通り Either というのは Haskell では失敗か成功を表すときに用いる型である。 Right というモナド(?)で包まれている方が成功を表し、Left で包まれているのは失敗を表す (型によって分岐させてエラーの場合の処理を書ける)。

```haskell:app/Main.hs
module Main where
import System.Environment
import Parser

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
```

ここから、Wiki の方ではコマンドラインを使って動作確認をするが毎回 `stack build` して `stack exec` とやるのは微妙に面倒なので `stack ghci` で対話環境を起動して確認を行うようにする。scheme48 フォルダ直下で

```sh
$ stack ghci
```

とやると

```sh
*Main Lib Parser>
```

という、依存モジュールなどをロードした状態のプロンプトが立ち上がる。Emacs を使っている人は haskell-program-name を

```elisp
(setq haskell-program-name "/path/to/stack ghci")
```

というように設定すると、`stack ghci` を使用してロードしたファイルの上に存在する .cabal を探してプロンプトを起動・再読み込みしてくれるのでとても便利なのでこちらをおすすめする。(やってみたらできた :sushi:)
　ghci を使用する時の注意として現在の `main` は `getArgs` によって実行時の引数を受け取るので

```sh
*Main Parser> main $
```

のようにしてもエラーになってしまう。コマンドラインのようにふるまわせるには ghci の機能 `:main` が使える。

```sh
*Main Parser> :main $
Found value
*Main Parser> :main a
No match: "lisp" (line 1, column 1):
unexpected "a"
```

## 空白文字

特になし？ `(>>)` がこちら側からは見えない情報がくっついているということをなんとなくイメージしておくといいと思われる。

## 戻り値

> 現段階では、パーサは与えられた文字列が認識できるか否かを表示するだけで、特には何もしません。普通、パーサには与えられた入力を扱いやすいデータ構造に変換して欲しいものです。

インタプリタとかを実装したことがないとよくわからない気がするが、パーサで文字列を読む → 読んだものをデータに変換する (例えば、"3" は Int 3 に、関数は関数を表すデータに) → 読み込んだデータに応じてその先の操作を決める (関数を読んだら、それを適応する値を探す、など)。

またファイルを分けておきましょう。<!-- 型 はいろいろな場所から参照したくなる場面が多い気がする --> stack を使ってるとファイルを分けても .cabal などを編集する必要が無いので楽です。

```haskell:Datatype.hs
module Datatype where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
```

これはつまり、LispVal という型を持つデータがあり、それには Atom, List, DottedList などがあり、 Atom は String を持ち、List は [ListVal] (ListVal のリスト) を持ち……というような意味です。

> コンストラクタと型は別々の名前空間を持つ

なるほど。

Parser.hs では DataType.hs で定義したデータ型 LispVal を使うので `import` する必要がありますが、その時は単にモジュールを `import` するだけではなく以下のようにします。
```haskell:Parser.hs
import Datatype (LispVal(..))
```

```haskell:Parser.hs
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x
```

新しいコードを見て行きましょう。まず `char`, `many`, `noneOf` は全て module Parser で定義されているものです。これを言葉として読んでいるのが以下の文です。

> 文字列は、二重引用符で始まり、それに引用符以外の文字が0個以上続き、二重引用符で閉じられます。

つまり

> 文字列は (parseString) 、二重引用符で始まり (char '"')、それに引用符以外の文字が0個以上続き (many (noneOf "\"")))、二重引用符で閉じられます (char '"')。

という風に対応していることになります。これを全て満した場合に限り、x が文字列として認められて Parser というモナドに包まれた LispVal の中の String 型の値として返されるわけです。

> また>>演算子の代わりにdo記法を使っています。これは私たちがパース結果の値(many (noneOf "\"")によって返される)を取り出し、他のパース関数を間に挟みながら操作することになるからです。一般に、アクションが値を返さないときに>>を、値をすぐに次のアクションに渡すときに>>=を、その他の場合にdo記法を使います。

理解を深めるために do 記法を展開してみましょう。

```haskell:Parser.hs
parseString :: Parser LispVal
-- parseString = do char '"'
--                  x <- many (noneOf "\"")
--                  char '"'
--                  return $ String x
parseString = char '"' >> many (noneOf "\"") >>= (\x -> char '"' >> (return $ String x))
```

do 記法での `x <- many (noneOf "\"")` 以降の全てが引数に `x` を取るラムダ式で包みこまれているのがキモです。これによって `(>>=)` 以降の式の中で `x` を使うことができるようになっています。
また説明はしっかりありますが、Haskell における return は C などとのものとは全く違う挙動をする (ただし見方によっては全く同じような振舞いに見える) ので注意が必要でしょう。

> $演算子は中置関数適用です。
> $は演算子なので、引数として他の関数に渡したり部分適用するなど、関数と同様に扱うことができます。この点に於て、$はLispの関数applyのように働きます。

これに関しては例を見るのが早いでしょう。

```
-- ex1
*> zipWith ($) [\x -> x * 2, \x -> x ^ 2] [1, 2]
[1,4]
-- ex2
*> map ($ 3) [\x -> x * 2, \x -> x ^ 2]
[6,9]
```

ex1 では `($) :: (a -> b) -> a -> b` を高階関数に渡し、`(a -> b)` 型の関数のリストと `a` 型の値のリストを渡すことにより、その適用結果のリストを得ることができます。ex2 では `($)` とに値を部分適用したものを関数リストに map することで適用結果のリストを得ています。
　この関数のどのように振る舞うのかを `ghci` で確かめたいと思う人も多いだろう。実装している `parser` 関数 `LispVal` を返すので `Show` のインスタンスにする必要があるので加える。

```haskell:Datatype.hs
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)
```

これで

```
$ stack ghci
<略>
Ok, modules loaded: Parser, Datatype, Main.
Loaded GHCi configuration from /private/var/folders/k3/tny640fj2j1f7c7111_pgmv80000gn/T/ghci31315/ghci-script

*Main Datatype Parser> :load "./src/Parser.hs"
<略>
Ok, modules loaded: Parser, Datatype.
*Parser> parse parseString "Lisp" "\"hoge\""
Right (String "hoge")
```

このように確かめることができるようになる。今回私達のプログラムでは Perser を import しているのは module Parser であるのでそこに ghci で入る必要がある。((Emacs では `Parser.hs` で `C-c C-l` をすれば load してくれる。Terminal ではパスを手打ちするしかないだろうか。))

> 次はSchemeの変数です。atomは一つの文字か記号のあとに0個以上の文字、数字、または記号が連なったものです。

これは逆に言えば `atom` としては最初に数字が来ることは認めないということを示している。

```haskell:Parser.hs
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom
```

Parser が述語に近い振舞いをしているというイメージを持っていると `(<|>)` がある程度自然に受けとれるのではないか。拡張された OR 演算子 `(|)` と考えられる。
　慣れが無いと混乱しやすいと思われるが、返り値が `Parse LispVal` であることから `parseAtom` の最後のパターンマッチ式での `Bool` や `Atom` というのは `LispVal` のコンストラクタであることに注意。

```haskell:Parser.hs
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
```

ここでは型の変化の流れをひとつずつ追っていく。ここでほんの少しだけ Parser に踏みこんでみる。 `many1 digit` の型を見てみると

```haskell
*Parser> :t many1 digit
many1 digit
  :: Text.Parsec.Prim.Stream s m Char =>
     Text.Parsec.Prim.ParsecT s u m [Char]
```

かなり複雑であるが `getLine` と比較してみよう

```
*Parser> :t getLine
getLine :: IO String
```
更に最初に出てきたモナドについての説明を振り替えると

> モナドは「私たちはある決まったやり方でいくらかの追加情報とともに値を持ち回り組合せますが、殆どの関数はそれについて気にしなくていいですよ」と言っています。

IO モナド (今回は `getLine`) についての「気にしなくてよいいくらかの追加情報」というものは明らかに IO という部分で、「値」というものは `String` である。 `many1 digit` における 「気にしなくて良いいくらかの追加情報」というのは `Text.Parsec.Prim.ParsecT s u m` という部分であり、「値」というのは `[Char]` である。 <!-- TODO: Text.Parsec.Prim.ParsecT s u m について --> なんともスッキリしないと思われるが、実際のところ IO というものについても私達はなんとなくわかった気になっているだけである (`getLine` を使ったときに具体的にどのようにして PC が入力を受けとって Haskell がそれをプログラム内で処理できる形にしているかを説明できる人は殆どいないだろう。もちろんそれを知ることは様々なことに役立つので、詳細を知ることの必要性を否定するわけではない)。これで、少なくとも `many1 digit` というものが結果としてあるモナドに包まれた `[Char]` を返す関数であることはとにかくわかったわけである。
　プログラムに戻ろう。

```haskell:Parser.hs
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
```

`many1 digit` が何をするのかはわかったので、 `($)` の型から何をしているかを探ろう。

```Haskell
*Parser> :t ($)
($) :: (a -> b) -> a -> b
```

これは `(a -> b)` という関数と型 `a` の値をを受けとり、その値に関数を適用して型 `b` を返すものであった。つまり `parseNumber` における `(a -> b)` の関数というのは `liftM (Number . read)` という部分であり、型 `a` の値というのが `many1 digit` の返す値であるといのがわかる。 ((Haskell のプログラムが何をしているのかわからない、という状態は、そもそもそれぞれの型が何であるかを理解していない場合が殆どである (と思う)。)) では `liftM (Number . read)` とは何だろうか。 `Number` は `Integer` を受けとって `LispVal` 型にするコンストラクタであり、 `read` は `String` を値に変換してくれるものである。では `liftM` とは何か。ここで理解しやすいようにこの後の練習問題に含まれるのだが、`parseNumber` を全く同じふるまいをする `do` を用いた関数に書き変えてみる。

```haskell:Parser.hs
parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
  x <- many1 digit
  return . Number . read $ x
```

`liftM` が無くなり `->` と `return` が出現した。`liftM` が行っているのはまさにこの二つのことである。型を見てみよう。

```haskell
*Parser> :t liftM
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
```

とても見覚えがある形である。

```Haskell
*Parser> :t ($)
($) :: (a -> b) -> a -> b
```

型変数を置き変えて極端にわかりやすくしてみよう。

```haskell
liftM :: Monad m => (a -> b) -> m a -> m b
```

これが示すことは、`liftM` は `(a -> b)` である関数を受けとり、モナドに包まれた型 `a` の値を受けとり、そのモナドに包まれた型 `a` の値に関数を適用して、モナドで包みなおして `m b` という型を持つ値として返すということをする。モナドがからんだ関数適用である。実際前のプログラムは中置記法を使うことにより `($)` と全く同じように書くことができる。((正し結合力の関係で `(Number . read)` の括弧を外すことはできないのでこのように書くことは Haskell 的では無いだろう。`($)` が括弧をはずすことができるのは `($)` の結合力が Haskell の関数 (オペレーターの中で最も低いことによる))

```haskell:Parser.hs
parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = (Number . read) `liftM` many1 digit
```

> これはモナドの値を取り扱うさらにもう一つのやり方を示しています。
というのは、まさにこのモナドを取り扱うことができる高階関数 (今回は `liftM`) によって、「追加情報」を気にせずにプログラミングができる手段を提供しています。別なやりかたと言うのは、`do` 記法や `(>>=)` を使ったやりかたと並べているということである。

```haskell:Parser.hs
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"
```

readExpr をこのように修正し、

```
main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
```

がこのようになっている (`Main` が `Datatype` に依存している) 場合で、`stack build` 等を行う前に .cabal の `exposed-modules` に `Datatype` を追加する必要があるので気をつけること。もし exposed されているライブラリが不十分な場合は `stack build` 時に

```sh
Linking .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/scheme48-exe/scheme48-exe ...
Undefined symbols for architecture x86_64:
  "_scheme48zm0zi1zi0zi0zmDFX5GRCmp5W7oZZ0UyusQh2_Datatype_Atom_con_info", referenced from:
      _c5Iv_info in libHSscheme48-0.1.0.0-DFX5GRCmp5W7oZ0UyusQh2.a(Parser.o)
      _c5Kf_info in libHSscheme48-0.1.0.0-DFX5GRCmp5W7oZ0UyusQh2.a(Parser.o)
  "_scheme48zm0zi1zi0zi0zmDFX5GRCmp5W7oZZ0UyusQh2_Datatype_Bool_static_info", referenced from:
      _r5iw_closure in libHSscheme48-0.1.0.0-DFX5GRCmp5W7oZ0UyusQh2.a(Parser.o)
      _r5iy_closure in libHSscheme48-0.1.0.0-DFX5GRCmp5W7oZ0UyusQh2.a(Parser.o)
  "_scheme48zm0zi1zi0zi0zmDFX5GRCmp5W7oZZ0UyusQh2_Datatype_Number_con_info", referenced from:
      _s5nF_info in libHSscheme48-0.1.0.0-DFX5GRCmp5W7oZ0UyusQh2.a(Parser.o)
      _s5pn_info in libHSscheme48-0.1.0.0-DFX5GRCmp5W7oZ0UyusQh2.a(Parser.o)
  "_scheme48zm0zi1zi0zi0zmDFX5GRCmp5W7oZZ0UyusQh2_Datatype_String_con_info", referenced from:
      _r5iL_info in libHSscheme48-0.1.0.0-DFX5GRCmp5W7oZ0UyusQh2.a(Parser.o)
ld: symbol(s) not found for architecture x86_64
collect2: error: ld returned 1 exit status
`gcc' failed in phase `Linker'. (Exit code: 1)
```

のようになるので .cabal を確認するようにする。

```cabal
library
  hs-source-dirs:      src
  exposed-modules:     Parser
                     , Datatype
  build-depends:       base >= 4.7 && < 5
                     , parsec
  default-language:    Haskell2010
```

実行してみよう

```sh
$ stack build
$ stack exec scheme48-exe "\"this is a string\""
Found value
$ stack exec scheme48-exe 25
Found value
$ stack exec scheme48-exe symbol
Found value
$ stack exec scheme48-exe (symbol)
zsh: unknown file attribute: y
$ stack exec scheme48-exe "(symbol)"
No match: "lisp" (line 1, column 1):
unexpected "("
expecting letter, "\"" or digit
```

### 練習問題 2
練習問題は解答を見る前に必ず自分で考えること

#### 練習問題 2-1

- do 記法
```haskell:Parser.hs
parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return . Number . read $ x
```

- bind 記法

```haskell:Parser.hs
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \x -> return . Number . read $ x
```

#### 練習問題 2-2

問題が少し分かりずらいが、現在のパーサーではエスケープが実装されていないので以下のように動作してしまう。

```
*Parser> parse parseString "lisp" "\"ho\\\"ge\""
Right (String "ho\\")
```

したがって

```
*Parser> parse parseString "lisp" "\"ho\\\"ge\""
Right (String "ho\"ge")
```

このように動作するパーサーを実装する。

```haskell:Parser.hs
parseEscape :: Parser Char
parseEscape = char '\\' >> char '"'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ parseEscape <|> noneOf "\""
                 char '"'
                 return $ String x
```

#### 練習問題 2-3

```
parseEscape :: Parser Char
parseEscape = do
  char '\\'
  x <- oneOf "\"nrt\\"
  return $ case x of
             'n' -> '\n'
             'r' -> '\r'
             't' -> '\t'
             _   -> x
```

##### 確認

```haskell
putStrLn "\\hoge\tfuga\nfuga\thoge\\"
\hoge	fuga
fuga	hoge\
```
これと全く同じ動作をする文字列を試すと

```
*Parser> parse parseString "lisp" "\"\\\\hoge\\tfuga\\nfuga\\thoge\\\\\""
Right (String "\\hoge\tfuga\nfuga\thoge\\")
```

パースに成功しているが、正しく動作しているのかわかりずらいので確認するために `parse` の結果を受け取って表示する `printString` を実装しておく。`parse` の型は

```
*Parser> :t parse parseString "lisp" "\"ho\\\"\n\r\t\\\\ge\""
parse parseString "lisp" "\"ho\\\"\n\r\t\\\\ge\""
  :: Either ParseError LispVal
```

であるので

```haskell:printString
printString :: Either ParseError LispVal -> IO ()
printString (Right (String s)) = putStrLn s
printString _ = putStrLn "Print error"
```

これで確認してみると

```haskell:printString
*Parser> printString $ parse parseString "lisp" "\"\\\\hoge\\tfuga\\nfuga\\thoge\\\\\""
\hoge	fuga
fuga	hoge\
```

正しくパースできているのがわかる。



#### 練習問題 2-4
この問題は難しい (4時間ほど要した)。これはまず scheme の方でどのように振る舞うかを確認しておく

```scheme
gosh> #b1101
13
gosh> #o777
511
gosh> #xabc
2748
gosh> #d10
10
```

このように `#b`・`#o`・`#x`・`#d`の後に来た数字をそれぞれ2進数・8 進数・16 進数・10 進数として値を読み込みこんで10進数表示するように、`parseNumber` を変更していく。まず Haskell の `Numeric` module には `redOct` と `readHex` はあるが2進数用の関数は無いので同じく `Numeric` module にある `readInt` という関数を用いて実装する必要がある。まず `readHex` のふるまいを確認しておくと (:m で ghci にモジュールをインポートできる)

```
*Parser Numeric> :t readHex
readHex :: (Num a, Eq a) => ReadS a
*Parser> :m Numeric
Parser Numeric> readHex "abhogeab"
[(171,"hogeab")]
```

今回のパーサーでは `#b` などの後に続く数字だけを `many1 digit` で取りだすので、タプルの第2要素の変換でき
なかった文字列を気にする必要はない。`readInt` を使用した `readBin` は以下のようになる。 `digitToInt` は module `Data.Char` で定義されている。

```
readBin :: (Num a, Eq a) => ReadS a
readBin = readInt 2 (\c -> elem c "01") digitToInt 
```

`readInt` の第一引数は基数を、第二引数は文字がその基数に対してふさわしい値であるかどうかの述語 (今回の2進数では `0` か `1` のどちらかを表わすために `elme c "01"` としている)。 最後はその述語を満たした文字を対応する `Int` に変換する関数を取る。

```
*Parser> readBin "1111"
[(15,"")]
```

ここで私は `parseNumber` を以下のように実装した

```haskell:parseNumber
parseNumber :: Parser LispVal
parseNumber = do
  top <- char '#' <|> digit
  case top of
    '#' -> do
      unit <- oneOf "bodx"
      rest <- many1 digit
      return $ Number . fst . head . (case unit of
                                        'b' -> readBin
                                        'o' -> readOct
                                        'd' -> readDec
                                        'x' -> readHex) $ rest
    _   -> liftM (Number . read . (:) top) $ many digit
```

そして `parseExpr` の順番を以下のように交換することにより実装できると考えたが、

```haskell:parseExpr
parseExpr :: Parser LispVal
parseExpr = parseNumber
            <|> parseAtom
            <|> parseString
```

```
*Parser> parse parseExpr "lisp" "#b110hoge"
Right (Number 6)
*Parser> parse parseExpr "lisp" "#b110"
Right (Number 6)
*Parser> parse parseExpr "lisp" "110"
Right (Number 110)
*Parser> parse parseExpr "lisp" "#o110"
Right (Number 72)
*Parser> parse parseExpr "lisp" "#x110"
Right (Number 272)
```

値自体のパースは上手くいっている((実際には"#xabdf"のような16進数を受けつけないバグがある。後ほど出てくる実装ではそれに対応している)。しかし

```
*Parser> parse parseExpr "lisp" "#t"
Left "lisp" (line 1, column 2):
unexpected "t"
```

このように `parseAtom` で定義されている `#t` がうまく扱われなくなる。これは通常パーサーが戻れるのは一文字のみであり、`parseNumber` の `top <- char '#' <|> digit` でどちらかにマッチした時点でその後の `parseNumber` が失敗しても、私が順番を変更した `parseExpr` における、`parseNumber` の次の `parseAtom` に行くということはしてくれないからのようだ。これではどう実現すればいいのかわからなくなったが、 http://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Prim.html#v:try にある `try` と `string` を用いれば実現できそうだとういうことを見つけることができた((諦めかけていた))。以下の `parseNumber` がそれを使ったものである ((Haskell で `if` を使うことはあまり無いらしいがこれが一番綺麗だと思ったので使った))。

```haskell:parseNumber
parseNumber :: Parser LispVal
parseNumber = do
  s <- many1 digit
       <|> try (string "#b") <|> try (string "#o")
       <|> try (string "#d") <|> try (string "#x")
  if isDigit . head $ s
    then return . Number . read $ s
    else case s of
           "#b" -> many1 (oneOf "01") >>= toNumber . readBin
           "#o" -> many1 (oneOf "01234567") >>= toNumber . readOct
           "#d" -> many1 (oneOf "0123456789") >>= toNumber . readDec
           "#x" -> many1 (oneOf "0123456789abcdef") >>= toNumber . readHex
  where toNumber = return . Number . fst . head
```

```
*Parser> parse parseExpr "lisp" "#t"
Right (Bool True)
*Parser> parse parseExpr "lisp" "#f"
Right (Bool False)
*Parser> parse parseExpr "lisp" "#b1010111"
Right (Number 87)
*Parser> parse parseExpr "lisp" "#o777"
Right (Number 511)
*Parser> parse parseExpr "lisp" "#xabcdef"
Right (Number 11259375)
*Parser> parse parseExpr "lisp" "3093"
Right (Number 3093)
*Parser> parse parseExpr "lisp" "#hoge"
Right (Atom "#hoge")
```

