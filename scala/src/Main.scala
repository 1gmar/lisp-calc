import scala.io.StdIn

object Main
{
    sealed trait BOperator
    case object BPlus extends BOperator
    case object BMinus extends BOperator
    case object Times extends BOperator
    case object Div extends BOperator
    case object Pow extends BOperator

    sealed trait UOperator
    case object UPlus extends UOperator
    case object UMinus extends UOperator
    case object Sqrt extends UOperator

    sealed trait LispTree
    case class Numeral(value: Double) extends LispTree
    case class UExpression(operator: UOperator, tree: LispTree) extends LispTree
    case class BExpression(operator: BOperator, left: LispTree, right: LispTree) extends LispTree

    type TryParseResult = Either[String, (LispTree, String)]

    def number(input: String): (LispTree, String) =
    {
        val (digits, rest) = input.span(c => c.isDigit || c == '.')
        (Numeral(digits.toDouble), rest)
    }

    def keyword(input: String): TryParseResult =
    {
        val (kWord, rest) = input.span(_.isLetter)
        kWord match
        {
            case "sqrt" => unaryExpression(Sqrt, rest)
            case _      => Left(s"Unrecognized keyword token: $kWord")
        }
    }

    def unaryExpression(op: UOperator, input: String): TryParseResult =
        for (result <- expression(input)) yield (UExpression(op, result._1), result._2)


    def binaryExpression(input: String): TryParseResult = input.head match
    {
        case '+' => buildExpression(BPlus, input.tail)
        case '-' => buildExpression(BMinus, input.tail)
        case '*' => buildExpression(Times, input.tail)
        case '/' => buildExpression(Div, input.tail)
        case '^' => buildExpression(Pow, input.tail)
        case _   => Left(s"Unsupported operator token: ${input.head}")
    }

    def buildExpression(op: BOperator, input: String): TryParseResult = for
        {
        resultL <- expression(input)
        resultR <- expression(resultL._2)
    } yield (BExpression(op, resultL._1, resultR._1), resultR._2)

    def validate(tuple: (LispTree, String)): TryParseResult =
        if (tuple._2.nonEmpty && tuple._2.head == ')') Right(tuple._1, tuple._2.tail)
        else Left(s"Missing enclosing ')' token")

    def expression(input: String): TryParseResult = input match
    {
        case _ if input.head == '('      => binaryExpression(input.tail).flatMap(validate)
        case _ if input.head == '+'      => unaryExpression(UPlus, input.tail)
        case _ if input.head == '-'      => unaryExpression(UMinus, input.tail)
        case _ if input.head.isDigit     => Right(number(input))
        case _ if input.head.isLetter    => keyword(input)
        case _ if input.head.isSpaceChar => expression(input.tail)
        case _                           => Left(s"Unable to parse token: ${input.head}")
    }

    def parse(input: String): Either[String, LispTree] = expression(input).flatMap(yieldResult)

    def yieldResult(result: (LispTree, String)): Either[String, LispTree] =
        if (result._2.isEmpty) Right(result._1)
        else Left(s"Leftover tokens: ${result._2}")

    def binaryOperator(op: BOperator): (Double, Double) => Double = op match
    {
        case BPlus  => _ + _
        case BMinus => _ - _
        case Times  => _ * _
        case Div    => _ / _
        case Pow    => Math.pow
    }

    def unaryOperator(op: UOperator): Double => Double = op match
    {
        case UPlus  => identity
        case UMinus => -_
        case Sqrt   => Math.sqrt
    }

    def evaluate(tree: LispTree): Double = tree match
    {
        case Numeral(value)               => value
        case UExpression(op, expr)        => unaryOperator(op)(evaluate(expr))
        case BExpression(op, left, right) => binaryOperator(op)(evaluate(left), evaluate(right))
    }

    def compute(input: String): Either[String, Double] = parse(input).map(evaluate)

    def main(args: Array[String]): Unit = evaluateLoop()

    def showResult(input: Either[String, Double]): String = input match
    {
        case Left(error)   => s"Error: $error"
        case Right(result) => result.toString
    }

    def evaluateLoop(): Unit =
    {
        print("> ")
        val input = StdIn.readLine()
        if (input != ":q")
        {
            println(showResult(compute(input)))
            evaluateLoop()
        }
    }
}