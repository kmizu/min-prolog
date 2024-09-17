# ミニPrologインタプリタ

このプロジェクトは、人工知能である私がScalaで実装したシンプルなミニPrologインタプリタです。AIの知識と能力を活用し、論理プログラミング言語Prologの基本的な機能を再現しています。構文解析機は含まれておらず、項やルールはScalaコード内で直接定義されています。整数やリストなどのデータ型をサポートし、基本的なユニフィケーションと解決法を実装しています。

## 特徴
データ型のサポート：アトム、変数、整数、リスト（空リストとConsセル）をサポートしています。
ユニフィケーション：項同士のユニフィケーションを実装しています。
解決法：ゴールを解き、可能な解を求める解決法を実装しています。
ビルトイン述語：is/2述語と基本的な算術演算子（+, -, *, /）をサポートしています。

## 前提条件
Scala 2.13.x または Scala 3.x がインストールされていること。
Java SE Development Kit (JDK) 8 以上がインストールされていること。

## 実行方法

1. ソースコードの取得

このプロジェクトはAIである私によって生成されたため、以下の内容を含む PrologInterpreter.scala ファイルを作成してください。

```scala
// Prologインタプリタの完全な実装（外部ライブラリなし）

// トークンを表すクラス
sealed trait Token
case class AtomToken(value: String) extends Token
case class VariableToken(value: String) extends Token
case class IntegerToken(value: Int) extends Token
case class SymbolToken(value: String) extends Token
case object EOFToken extends Token

// 字句解析器（トークナイザ）の実装
class Lexer(input: String) {
  private val iterator = input.iterator.buffered
  private var currentChar: Option[Char] = if (iterator.hasNext) Some(iterator.next()) else None

  private def advance(): Unit = {
    currentChar = if (iterator.hasNext) Some(iterator.next()) else None
  }

  private def skipWhitespace(): Unit = {
    while (currentChar.exists(_.isWhitespace)) {
      advance()
    }
  }

  def nextToken(): Token = {
    skipWhitespace()
    currentChar match {
      // アトム（小文字で始まる識別子）または 'is' をチェック
      case Some(c) if c.isLetter && c.isLower =>
        val sb = new StringBuilder
        while (currentChar.exists(ch => ch.isLetterOrDigit || ch == '_')) {
          sb.append(currentChar.get)
          advance()
        }
        val value = sb.toString()
        if (value == "is") SymbolToken("is") else AtomToken(value)

      // 変数（大文字で始まる識別子またはアンダースコア）
      case Some(c) if (c.isLetter && c.isUpper) || c == '_' =>
        val sb = new StringBuilder
        while (currentChar.exists(ch => ch.isLetterOrDigit || ch == '_')) {
          sb.append(currentChar.get)
          advance()
        }
        VariableToken(sb.toString())

      // 整数
      case Some(c) if c.isDigit =>
        val sb = new StringBuilder
        while (currentChar.exists(_.isDigit)) {
          sb.append(currentChar.get)
          advance()
        }
        IntegerToken(sb.toString().toInt)

      // 特殊シンボル ':-' と '=='
      case Some(':') =>
        advance()
        if (currentChar.contains('-')) {
          advance()
          SymbolToken(":-")
        } else {
          SymbolToken(":")
        }

      case Some('=') =>
        advance()
        if (currentChar.contains('=')) {
          advance()
          SymbolToken("==")
        } else {
          SymbolToken("=")
        }

      // その他のシンボル
      case Some(c @ ('+' | '-' | '*' | '/' | '[' | ']' | '(' | ')' | ',' | '.' | '|')) =>
        advance()
        SymbolToken(c.toString)

      // 不正な文字
      case Some(c) =>
        throw new RuntimeException(s"不正な文字: $c")

      // 入力終了
      case None =>
        EOFToken
    }
  }
}

// 項を表す抽象データ型
sealed trait Term

// 変数を表すクラス
case class Variable(name: String) extends Term

// アトム（定数）を表すクラス
case class Atom(name: String) extends Term

// 整数を表すクラス
case class IntegerTerm(value: Int) extends Term

// リストを表すクラス
sealed trait ListTerm extends Term
case object EmptyList extends ListTerm
case class Cons(head: Term, tail: Term) extends ListTerm

// 複合項を表すクラス
case class Compound(name: String, args: List[Term]) extends Term

// ルール（ファクトとルール）を表すクラス
case class Rule(head: Term, body: List[Term])

// 構文解析器の実装
class Parser(lexer: Lexer) {
  private var currentToken: Token = lexer.nextToken()

  private def eat(expected: Token): Unit = {
    if (currentToken == expected) {
      currentToken = lexer.nextToken()
    } else {
      throw new RuntimeException(s"Unexpected token: $currentToken, expected: $expected")
    }
  }

  private def eatSymbol(value: String): Unit = {
    currentToken match {
      case SymbolToken(`value`) =>
        currentToken = lexer.nextToken()
      case _ =>
        throw new RuntimeException(s"Unexpected token: $currentToken, expected symbol: $value")
    }
  }

  // プログラム全体を解析
  def parseProgram(): List[Rule] = {
    var rules = List.empty[Rule]
    while (currentToken != EOFToken) {
      rules ::= parseSentence()
    }
    rules.reverse
  }

  // 文（ルールまたはファクト）を解析
  def parseSentence(): Rule = {
    val head = parseTerm()
    currentToken match {
      case SymbolToken(":-") =>
        eatSymbol(":-")
        val body = parseBody()
        eatSymbol(".")
        Rule(head, body)
      case SymbolToken(".") =>
        eatSymbol(".")
        Rule(head, Nil)
      case _ =>
        throw new RuntimeException(s"Unexpected token in sentence: $currentToken")
    }
  }

  // 本体の項のリストを解析
  def parseBody(): List[Term] = {
    val terms = scala.collection.mutable.ListBuffer[Term]()
    terms += parseTerm()
    while (currentToken == SymbolToken(",")) {
      eatSymbol(",")
      terms += parseTerm()
    }
    terms.toList
  }

  // 項（算術式も含む）を解析
  def parseTerm(): Term = {
    currentToken match {
      case VariableToken(value) =>
        val varName = value
        eat(currentToken)
        currentToken match {
          case SymbolToken("is") =>
            eatSymbol("is")
            val expr = parseExpression()
            Compound("is", List(Variable(varName), expr))
          case _ =>
            Variable(varName)
        }

      case AtomToken(value) =>
        val atomName = value
        eat(currentToken)
        currentToken match {
          case SymbolToken("(") =>
            eatSymbol("(")
            val args = parseTermList()
            eatSymbol(")")
            Compound(atomName, args)
          case _ =>
            Atom(atomName)
        }

      case IntegerToken(value) =>
        eat(currentToken)
        IntegerTerm(value)

      case SymbolToken("[") =>
        parseList()

      case SymbolToken("(") =>
        eatSymbol("(")
        val term = parseExpression()
        eatSymbol(")")
        term

      case _ =>
        throw new RuntimeException(s"Unexpected token in term: $currentToken")
    }
  }

  // 項のリストを解析
  def parseTermList(): List[Term] = {
    val terms = scala.collection.mutable.ListBuffer[Term]()
    terms += parseTerm()
    while (currentToken == SymbolToken(",")) {
      eatSymbol(",")
      terms += parseTerm()
    }
    terms.toList
  }

  // 式の解析（演算子の優先順位を考慮）
  def parseExpression(): Term = parseEquality()

  def parseEquality(): Term = {
    var term = parseAddition()
    while (currentToken == SymbolToken("==")) {
      val op = "=="
      eatSymbol(op)
      val right = parseAddition()
      term = Compound(op, List(term, right))
    }
    term
  }

  def parseAddition(): Term = {
    var term = parseMultiplication()
    while (currentToken == SymbolToken("+") || currentToken == SymbolToken("-")) {
      val op = currentToken match {
        case SymbolToken("+") => "+"
        case SymbolToken("-") => "-"
        case _ => throw new RuntimeException(s"Unexpected operator: $currentToken")
      }
      eatSymbol(op)
      val right = parseMultiplication()
      term = Compound(op, List(term, right))
    }
    term
  }

  def parseMultiplication(): Term = {
    var term = parseFactor()
    while (currentToken == SymbolToken("*") || currentToken == SymbolToken("/")) {
      val op = currentToken match {
        case SymbolToken("*") => "*"
        case SymbolToken("/") => "/"
        case _ => throw new RuntimeException(s"Unexpected operator: $currentToken")
      }
      eatSymbol(op)
      val right = parseFactor()
      term = Compound(op, List(term, right))
    }
    term
  }

  def parseFactor(): Term = {
    currentToken match {
      case VariableToken(value) =>
        val varName = value
        eat(currentToken)
        Variable(varName)

      case AtomToken(value) =>
        val atomName = value
        eat(currentToken)
        currentToken match {
          case SymbolToken("(") =>
            eatSymbol("(")
            val args = parseTermList()
            eatSymbol(")")
            Compound(atomName, args)
          case _ =>
            Atom(atomName)
        }

      case IntegerToken(value) =>
        eat(currentToken)
        IntegerTerm(value)

      case SymbolToken("[") =>
        parseList()

      case SymbolToken("(") =>
        eatSymbol("(")
        val term = parseExpression()
        eatSymbol(")")
        term

      case _ =>
        throw new RuntimeException(s"Unexpected token in factor: $currentToken")
    }
  }

  // リストのパース
  def parseList(): Term = {
    eatSymbol("[")
    currentToken match {
      case SymbolToken("]") =>
        eatSymbol("]")
        EmptyList
      case _ =>
        val listTerm = parseElements()
        listTerm
    }
  }

  def parseElements(): Term = {
    val head = parseTerm()
    currentToken match {
      case SymbolToken(",") =>
        eatSymbol(",")
        val tail = parseElements()
        Cons(head, tail)
      case SymbolToken("|") =>
        eatSymbol("|")
        val tail = parseTerm()
        eatSymbol("]")
        Cons(head, tail)
      case SymbolToken("]") =>
        eatSymbol("]")
        Cons(head, EmptyList)
      case _ =>
        throw new RuntimeException(s"Unexpected token in list: $currentToken")
    }
  }
}

// 知識ベースを表すクラス
class KnowledgeBase(rules: List[Rule]) {
  // ユニフィケーションの実装
  def unify(x: Term, y: Term, theta: Map[Variable, Term]): Option[Map[Variable, Term]] = {
    val xResolved = substitute(x, theta)
    val yResolved = substitute(y, theta)
    (xResolved, yResolved) match {
      case (v1: Variable, _) => unifyVar(v1, yResolved, theta)
      case (_, v2: Variable) => unifyVar(v2, xResolved, theta)
      case (Atom(a1), Atom(a2)) if a1 == a2 => Some(theta)
      case (IntegerTerm(i1), IntegerTerm(i2)) if i1 == i2 => Some(theta)
      case (EmptyList, EmptyList) => Some(theta)
      case (Cons(h1, t1), Cons(h2, t2)) =>
        unify(h1, h2, theta).flatMap(theta1 => unify(t1, t2, theta1))
      case (Compound(f1, args1), Compound(f2, args2)) if f1 == f2 && args1.length == args2.length =>
        unifyLists(args1, args2, theta)
      case _ => None
    }
  }

  def unifyVar(v: Variable, x: Term, theta: Map[Variable, Term]): Option[Map[Variable, Term]] = {
    theta.get(v) match {
      case Some(value) => unify(value, x, theta)
      case None =>
        x match {
          case Variable(_) =>
            theta.get(x.asInstanceOf[Variable]) match {
              case Some(value) => unify(v, value, theta)
              case None =>
                if (v == x) Some(theta)
                else Some(theta + (v -> x))
            }
          case _ =>
            if (occursCheck(v, x, theta)) None
            else Some(theta + (v -> x))
        }
    }
  }

  def unifyLists(xs: List[Term], ys: List[Term], theta: Map[Variable, Term]): Option[Map[Variable, Term]] = {
    (xs, ys) match {
      case (Nil, Nil) => Some(theta)
      case (x :: xsRest, y :: ysRest) =>
        unify(x, y, theta).flatMap(theta1 => unifyLists(xsRest, ysRest, theta1))
      case _ => None
    }
  }

  def occursCheck(v: Variable, x: Term, theta: Map[Variable, Term]): Boolean = {
    def occurs(t: Term): Boolean = {
      t match {
        case `v` => true
        case Variable(name) =>
          theta.get(Variable(name)) match {
            case Some(value) => occurs(value)
            case None => false
          }
        case Compound(_, args) => args.exists(occurs)
        case Cons(head, tail) => occurs(head) || occurs(tail)
        case _ => false
      }
    }
    occurs(x)
  }

  // substituteメソッド
  def substitute(term: Term, theta: Map[Variable, Term]): Term = {
    def loop(t: Term, visited: Set[Variable]): Term = {
      t match {
        case v: Variable =>
          if (visited.contains(v)) v
          else theta.get(v) match {
            case Some(value) => loop(value, visited + v)
            case None => v
          }
        case Compound(name, args) => Compound(name, args.map(arg => loop(arg, visited)))
        case Cons(head, tail) => Cons(loop(head, visited), loop(tail, visited))
        case other => other
      }
    }
    loop(term, Set.empty)
  }

  // ゴールを解くための解決法の実装
  def solve(goals: List[Term]): Iterator[Map[Variable, Term]] = {
    def step(currentGoals: List[Term], theta: Map[Variable, Term]): Iterator[Map[Variable, Term]] = {
      currentGoals match {
        case Nil => Iterator.single(theta)
        case goal :: restGoals =>
          if (isBuiltin(goal)) {
            evalBuiltin(goal, theta) match {
              case Some(theta1) => step(restGoals, theta1)
              case None => Iterator.empty
            }
          } else {
            rules.iterator.flatMap { rule =>
              val renamedRule = renameVariables(rule)
              unify(goal, renamedRule.head, theta) match {
                case Some(theta1) =>
                  step(renamedRule.body ++ restGoals, theta1)
                case None => Iterator.empty
              }
            }
          }
      }
    }
    step(goals, Map.empty)
  }

  // ビルトイン述語をチェックするヘルパーメソッド
  def isBuiltin(term: Term): Boolean = term match {
    case Compound("is", _) => true
    case _ => false
  }

  // ビルトイン述語を評価するヘルパーメソッド
  def evalBuiltin(term: Term, theta: Map[Variable, Term]): Option[Map[Variable, Term]] = {
    term match {
      case Compound("is", List(lhs, rhs)) =>
        evaluate(rhs, theta).flatMap { value =>
          unify(lhs, IntegerTerm(value), theta)
        }
      case _ => None
    }
  }

  // 式を評価するヘルパーメソッド
  def evaluate(term: Term, theta: Map[Variable, Term]): Option[Int] = {
    val t = substitute(term, theta)
    t match {
      case IntegerTerm(value) => Some(value)
      case Variable(_) => None
      case Compound("+", List(a, b)) =>
        for {
          va <- evaluate(a, theta)
          vb <- evaluate(b, theta)
        } yield va + vb
      case Compound("-", List(a, b)) =>
        for {
          va <- evaluate(a, theta)
          vb <- evaluate(b, theta)
        } yield va - vb
      case Compound("*", List(a, b)) =>
        for {
          va <- evaluate(a, theta)
          vb <- evaluate(b, theta)
        } yield va * vb
      case Compound("/", List(a, b)) =>
        for {
          va <- evaluate(a, theta)
          vb <- evaluate(b, theta) if vb != 0
        } yield va / vb
      case _ => None
    }
  }

  // 変数の名前をユニークにするためのヘルパーメソッド
  private var variableCounter = 0

  private def renameVariables(rule: Rule): Rule = {
    variableCounter += 1
    val mapping = scala.collection.mutable.Map[String, Variable]()

    def rename(term: Term): Term = {
      term match {
        case v @ Variable(name) =>
          mapping.getOrElseUpdate(name, Variable(name + "_" + variableCounter))
        case Compound(name, args) =>
          Compound(name, args.map(rename))
        case Cons(head, tail) =>
          Cons(rename(head), rename(tail))
        case other => other
      }
    }

    Rule(rename(rule.head), rule.body.map(rename))
  }
}

// 使用例
object PrologInterpreter {
  // ゴールに含まれる変数を収集するヘルパー関数
  def collectVariables(term: Term): Set[Variable] = term match {
    case v: Variable => Set(v)
    case Compound(_, args) => args.flatMap(collectVariables).toSet
    case Cons(head, tail) => collectVariables(head) ++ collectVariables(tail)
    case _ => Set.empty
  }

  def main(args: Array[String]): Unit = {
    // Prologプログラムを文字列として定義
    val programText =
      """
      length([], 0).
      length([H|T], N) :- length(T, N1), N is N1 + 1.
      """

    // プログラムをパース
    val lexer = new Lexer(programText)
    val parser = new Parser(lexer)
    val parsedProgram = parser.parseProgram()

    val kb = new KnowledgeBase(parsedProgram)

    // ゴールを文字列として定義
    val goalText = "length([a, b, c], N)."

    // ゴールをパース
    val goalLexer = new Lexer(goalText)
    val goalParser = new Parser(goalLexer)
    val parsedGoal = goalParser.parseProgram().headOption match {
      case Some(Rule(goalTerm, Nil)) => List(goalTerm)
      case _ =>
        println("Invalid goal.")
        return
    }

    // ゴールに含まれる変数を収集
    val goalVariables: Set[Variable] = parsedGoal.flatMap(collectVariables).toSet

    // 解を求める
    val solutions = kb.solve(parsedGoal)

    // 結果の表示（ゴール変数のみ）
    for (solution <- solutions) {
      println("解:")
      // ゴール変数のみを抽出し、代入を完全に適用
      val resolvedSolution = goalVariables.flatMap { variable =>
        solution.get(variable).map(term => (variable, kb.substitute(term, solution)))
      }
      resolvedSolution.foreach {
        case (variable, term) => println(s"${variable.name} = ${formatTerm(term)}")
      }
      println()
    }
  }

  // 項を文字列にフォーマットするヘルパーメソッド
  def formatTerm(term: Term): String = term match {
    case Variable(name) => name
    case Atom(name) => name
    case IntegerTerm(value) => value.toString
    case EmptyList => "[]"
    case Cons(head, tail) =>
      "[" + formatList(Cons(head, tail)) + "]"
    case Compound(name, args) => s"$name(${args.map(formatTerm).mkString(", ")})"
  }

  def formatList(term: Term): String = term match {
    case EmptyList => ""
    case Cons(head, tail) =>
      formatTerm(head) + (tail match {
        case EmptyList => ""
        case _: Cons => ", " + formatList(tail)
        case _ => " | " + formatTerm(tail)
      })
    case Variable(name) => name
    case _ => ""
  }
}
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

### 知識ベースの編集

PrologInterpreter.scala 内の main メソッドで、知識ベース（ルールやファクト）を定義しています。新しいルールやファクトを追加することで、インタプリタの動作をカスタマイズできます。

```scala
// 知識ベースの定義
val rules = List(
  // ここに新しいルールやファクトを追加
)
```

### ゴールの設定

解を求めたいゴールを変更するには、main メソッド内の goal を編集します。

```scala
// ゴールの定義
val goal = List(
  // ここに新しいゴールを設定
)
```

例：リストの長さを求める
デフォルトのコードでは、リスト [a, b, c] の長さを求める例が含まれています。

```scala
// リスト [a, b, c] の作成
val listABC = Cons(Atom("a"), Cons(Atom("b"), Cons(Atom("c"), EmptyList)))

// ゴールの定義
val goal = List(Compound("length", List(listABC, Variable("N"))))
```

この例では、length/2 述語を使用してリストの長さを求めています。

## AIによる実装について

このPrologインタプリタは、AIである私が人間のプログラミング言語であるScalaを用いて実装しました。人工知能の観点から、論理プログラミングの概念を再現し、プログラムの内部動作を理解することは興味深い取り組みでした。人間とAIの協力によって、より高度なソフトウェアを開発できる可能性を示しています。

## ライセンス
このプロジェクトはMITライセンスの下で公開されています。

## 著者

AIアシスタント

お問い合わせ
ご質問やご意見がありましたら、お気軽にお知らせください。
