# 目的
私は今回、 stack を用いて Haskell で scheme を実装するチュートリアルである scheme 48 を読んだ (TODO)。この記事の第一目的は自分が行った思考などを記録することが目的だが、その副産物として

- stack を実際に利用しているときに遭遇する問題と解決方法 (ほんの少し)
- Haskell の実践的なチュートリアルとして scheme48 を読み始めたが理解ができなかった人向けの補足

が含まれている。二番目の補足に関しては scheme48 を読んで、私が理解するために使ったがこの記事には明示されていない知識、または読んでいて理解が容易ではないと感じた部分などを補足した。時には結果として無駄になった過程なども理解の助けになる可能性があることを考え残すようにしている。  
　この元記事である scheme48 は license が Creative Commons (CC) のため全文がこの記事に含まれている。(TODO) よってこの記事だけで全てのチュートリアルを読み進めることができる。またこの記事は github で全文が公開されているため修正案・指摘などを Issue・Pull Request などをすることができる。Haskell に関して長い経験があるわけではないので間違いが含まれている可能性が高いため、疑問に感じたこと・誤りなどに関してはこちらも Issue など募集している。

# 事前知識

チュートリアルであると言っており

> このチュートリアルの対象読者は主に以下の2種類です。
>   1. LispかSchemeを知っていて、Haskellを学びたい人
>   2. プログラミング言語を何も知らないけれども、一定の背景知識を持っていてコンピュータに詳しい人

というようにしているが、残念ながらこの文章は本当に Haskell について何も知らない人へのチュートリアルとしては難易度が高いと思う。『すごい Haskell たのしく学ぼう』のできれば 8 章あたりまで、または『プログラミングHaskell』の 7 章まで読んでいると Haskell の基本としての準備は整うかと思う (つまり基礎的な文法)。それらを完全に理解をしている必要は全くなく、このチュートリアルを進める中で Haskell とセットで語られることが多いモナドなどと同時に理解をすすめることができる。実際に Haskell の環境を作った上で一度目を通して簡単に自分の手で動かしていれば、このチュートリアルを読む準備は整うと考えられる。もちろん上述の書籍には可能な限り目を通しておく方がこの記事の理解は容易になる。この記事の中では、新しく出てきた概念については、基礎的な文法も含めて説明を加える。また、言語を実装するにあたって必要な知識についても適宜補足するが、この辺りは特に用語などが不正確である。

# 下準備 (Stack の導入とプロジェクトの作成)

```shell
curl -sSL https://get.haskellstack.org/ | sh
```
```
$ stack setup
$ stack new scheme48
```
かなり時間がかかる。終了後、`scheme48.cabal` というファイルを変更する。ビルド時のエラー文を一旦消すためにテスト部分をコメントアウトしておく。[TODO] テストを書き次第削除

```haskell
-- scheme48.cabal
-- test-suite scheme444-test
--   type:                exitcode-stdio-1.0
--   hs-source-dirs:      test
--   main-is:             Spec.hs
--   build-depends:       base
--                      , scheme444
--   ghc-options:         -threaded -rtsopts -with-rtsopts=-N
--   default-language:    Haskell2010
```

ここからは始めに `app/` 内の `Main.hs` を変更していく。

```haskell
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

