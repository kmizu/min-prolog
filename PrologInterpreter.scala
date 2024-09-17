// Prologインタプリタの実装（整数とリストを含む）

// 項を表す抽象データ型
sealed trait Term

// 変数を表すクラス
case class Variable(name: String) extends Term

// アトム（定数）を表すクラス
case class Atom(name: String) extends Term

// 整数を表すクラス
case class IntegerTerm(value: Int) extends Term

// リストを表すクラス（空リストまたはConsセル）
sealed trait ListTerm extends Term
case object EmptyList extends ListTerm
case class Cons(head: Term, tail: Term) extends ListTerm

// 複合項を表すクラス
case class Compound(name: String, args: List[Term]) extends Term

// ルール（ファクトとルール）を表すクラス
case class Rule(head: Term, body: List[Term])

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

  // 修正した substitute メソッド
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
  def main(args: Array[String]): Unit = {
    // 知識ベースの定義
    val rules = List(
      // length/2 のファクトとルール
      Rule(Compound("length", List(EmptyList, IntegerTerm(0))), Nil),
      Rule(
        Compound("length", List(Cons(Variable("H"), Variable("T")), Variable("N"))),
        List(
          Compound("length", List(Variable("T"), Variable("N1"))),
          Compound("is", List(Variable("N"), Compound("+", List(Variable("N1"), IntegerTerm(1)))))
        )
      )
    )

    val kb = new KnowledgeBase(rules)

    // リスト [a, b, c] の作成
    val listABC = Cons(Atom("a"), Cons(Atom("b"), Cons(Atom("c"), EmptyList)))

    // ゴールの定義
    val goal = List(Compound("length", List(listABC, Variable("N"))))

    // ゴール内のユーザー変数を収集
    val userVariables = collectVariables(goal)

    // 解を求める
    val solutions = kb.solve(goal)

    // 結果の表示
    for (solution <- solutions) {
      println("解:")
      // 代入を完全に適用した解を生成
      val resolvedSolution = solution.mapValues(term => kb.substitute(term, solution))
      // ユーザー変数のみを表示
      for (variable <- userVariables) {
        resolvedSolution.get(variable) match {
          case Some(term) => println(s"${variable.name} = ${formatTerm(term)}")
          case None => // 何もしない
        }
      }
      println()
    }
  }

  // ユーザー変数を収集するヘルパーメソッド
  def collectVariables(terms: List[Term]): Set[Variable] = {
    terms.flatMap(collectVariablesFromTerm).toSet
  }

  def collectVariablesFromTerm(term: Term): Set[Variable] = term match {
    case v: Variable => Set(v)
    case Compound(_, args) => args.flatMap(collectVariablesFromTerm).toSet
    case Cons(head, tail) => collectVariablesFromTerm(head) ++ collectVariablesFromTerm(tail)
    case _ => Set.empty
  }

  // 項を文字列にフォーマットするヘルパー関数
  def formatTerm(term: Term): String = term match {
    case Variable(name) => name
    case Atom(name) => name
    case IntegerTerm(value) => value.toString
    case EmptyList => "[]"
    case list: ListTerm => "[" + formatList(list) + "]"
    case Compound(name, args) => s"$name(${args.map(formatTerm).mkString(", ")})"
  }

  def formatList(term: Term): String = term match {
    case EmptyList => ""
    case Cons(head, tail) =>
      formatTerm(head) + (tail match {
        case EmptyList => ""
        case _ => ", " + formatList(tail)
      })
    case Variable(name) => name
    case _ => ""
  }
}

