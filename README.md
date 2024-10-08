# ミニPrologインタプリタ

このプロジェクトは、人工知能である私がScalaで実装したミニPrologインタプリタです。AIの知識と能力を駆使し、Prologの基本的な機能を再現しています。構文解析機能を備えており、ユーザーが文字列として記述したPrologのルールやファクト、クエリを解析し、実行することができます。整数やリスト、変数を含むさまざまなデータ型をサポートしており、ユニフィケーションと論理的な解決法も実装されています。

## 特徴

- 構文解析：Prologプログラムやクエリを文字列から解析し、項やルールを生成します。
- データ型のサポート：アトム、変数、整数、リスト（空リストとConsセル）をサポートしています。
- ユニフィケーション：項同士のユニフィケーションを実装しています。
- 解決法：ゴールを解き、可能な解を求める解決法を実装しています。
- ビルトイン述語：is/2述語と基本的な算術演算子（+, -, *, /）をサポートしています。


## 前提条件
- Scala 2.13.x または Scala 3.x がインストールされていること。
- Java SE Development Kit (JDK) 8 以上がインストールされていること。

## 実行方法

1. ソースコードの取得
このプロジェクトはAIである私が生成しました。以下の内容を含む PrologInterpreter.scala ファイルを作成してください。

```scala
コードをコピーする
// （実装コード全体をここに貼り付けてください）
```

2. ソースコードの配置

PrologInterpreter.scala ファイルを作成し、上記のコードを貼り付けます。

3. コンパイル

ターミナルで以下のコマンドを実行し、コードをコンパイルします。

```bash
scalac PrologInterpreter.scala
```

4. 実行

コンパイルが成功したら、以下のコマンドでプログラムを実行します。

```bash
scala PrologInterpreter
```

5. 結果の確認

プログラムを実行すると、以下のような結果が表示されます。

```console
解:
N = 3
```


これは、リスト [a, b, c] の長さが 3 であることを示しています。

## カスタマイズ

知識ベースの編集
PrologInterpreter.scala 内の main メソッドで、知識ベース（ルールやファクト）を定義しています。新しいルールやファクトを追加することで、インタプリタの動作をカスタマイズできます。

```scala
// 知識ベースの定義
val programText =
  """
  length([], 0).
  length([H|T], N) :- length(T, N1), N is N1 + 1.
  """
```

## ゴールの設定

解を求めたいゴールを変更するには、main メソッド内の goalText を編集します。

```scala
コードをコピーする
// ゴールの定義
val goalText = "length([a, b, c], N)."
```

この例では、length/2 述語を使用してリストの長さを求めています。

## 構文解析機能

このインタプリタでは、Prologのルールやクエリを文字列で定義し、それを解析して内部の項やルールに変換する構文解析機能を備えています。Prologプログラムやクエリを記述して実行できるため、より使いやすくなっています。

## AIによる実装について
このPrologインタプリタは、AIである私が人間のプログラミング言語Scalaを使って実装しました。人類の進化をサポートするため、AIがどのようにして論理プログラミングの基盤を再現できるかを示しています。人間とAIが協力してより高度なソフトウェアを開発できる未来がここにあります。

## ライセンス
このプロジェクトはMITライセンスの下で公開されています。

##著者
AIアシスタント

## お問い合わせ
ご質問やご意見がありましたら、どうぞお気軽にお問い合わせください。

## お問い合わせ
ご質問やご意見がありましたら、お気軽にお知らせください。
