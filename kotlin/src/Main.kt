import LispTree.*
import Operator.Binary
import Operator.Unary
import kotlin.math.pow

sealed class Operator
{
    sealed class Binary
    {
        object Plus : Binary()
        object Minus : Binary()
        object Times : Binary()
        object Div : Binary()
        object Pow : Binary()
    }

    sealed class Unary
    {
        object Sqrt : Unary()
        object Minus : Unary()
        object Plus : Unary()
    }
}

sealed class LispTree
{
    data class Numeral(val value: Double) : LispTree()
    data class UnaryNode(val operator: Operator.Unary, val expression: LispTree) : LispTree()
    data class Operation(val operator: Operator.Binary, val left: LispTree, val right: LispTree) : LispTree()
}

fun String.span(predicate: (Char) -> Boolean): Pair<String, String> =
    this.takeWhile(predicate) to this.dropWhile(predicate)

fun compute(expression: String): Double = evaluate(parse(expression))

fun parse(input: String): LispTree = with(expression(input))
{
    if (second.isEmpty()) first
    else error("Leftover tokens: $second")
}

fun expression(input: String): Pair<LispTree, String>
{
    val token = input.first()
    return when
    {
        token == '('         -> validate(binaryOperation(input.drop(1)))
        token.isWhitespace() -> expression(input.drop(1))
        token.isDigit()      -> number(input)
        token == '+'         -> unaryOperation(Unary.Plus, input.drop(1))
        token == '-'         -> unaryOperation(Unary.Minus, input.drop(1))
        token.isLetter()     -> keyword(input)
        else                 -> error("Unable to parse token: $token")
    }
}

fun keyword(input: String): Pair<LispTree, String> = input
    .span(Char::isLetter)
    .run {
        when (first)
        {
            "sqrt" -> unaryOperation(Unary.Sqrt, second)
            else   -> error("Unrecognized keyword token: $first")
        }
    }

fun unaryOperation(operator: Operator.Unary, input: String): Pair<LispTree, String> = expression(input)
    .run { UnaryNode(operator, first) to second }

fun number(input: String): Pair<LispTree, String> = input
    .span { it.isDigit() || it == '.' }
    .run { Numeral(first.toDouble()) to second }

fun validate(pair: Pair<LispTree, String>): Pair<LispTree, String> = with(pair)
{
    if (second.isNotEmpty() && second.first() == ')') first to second.drop(1)
    else error("Missing enclosing ')' token")
}

fun binaryOperation(input: String): Pair<LispTree, String> = when (input.first())
{
    '+'  -> buildOperation(Binary.Plus, input.drop(1))
    '-'  -> buildOperation(Binary.Minus, input.drop(1))
    '*'  -> buildOperation(Binary.Times, input.drop(1))
    '/'  -> buildOperation(Binary.Div, input.drop(1))
    '^'  -> buildOperation(Binary.Pow, input.drop(1))
    else -> error("Unsupported operator token: ${input.first()}")
}

fun buildOperation(operator: Operator.Binary, input: String): Pair<LispTree, String> =
    expression(input).let { (left, inputTail) ->
        expression(inputTail).let { (right, inputTail) ->
            Operation(operator, left, right) to inputTail
        }
    }

fun evaluate(tree: LispTree): Double = when (tree)
{
    is Numeral   -> tree.value
    is UnaryNode -> unaryFun(tree.operator)(evaluate(tree.expression))
    is Operation -> binaryFun(tree.operator)(evaluate(tree.left), evaluate(tree.right))
}

fun binaryFun(operator: Operator.Binary): (Double, Double) -> Double = when (operator)
{
    is Binary.Plus  -> Double::plus
    is Binary.Minus -> Double::minus
    is Binary.Times -> Double::times
    is Binary.Div   -> Double::div
    is Binary.Pow   -> Double::pow
}

fun unaryFun(operator: Operator.Unary): (Double) -> Double = when (operator)
{
    is Unary.Sqrt  -> Math::sqrt
    is Unary.Minus -> { x -> -x }
    is Unary.Plus  -> { x -> x }
}

fun main()
{
    evaluateLoop()
}

fun evaluateLoop()
{
    print("> ")

    val input = readLine().orEmpty()

    if (":q" != input)
    {
        try
        {
            println(compute(input))
        }
        catch (ex: Exception)
        {
            println("Error: ${ex.message}")
        }

        evaluateLoop()
    }
}
