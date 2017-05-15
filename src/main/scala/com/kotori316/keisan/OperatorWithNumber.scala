package com.kotori316.keisan

class OperatorWithNumber(val operator: Operator, val fractions: Fractions) {

    def ::(other: OperatorWithNumber): OperatorWithNumber = {
        new OperatorWithNumber(other.operator, operator.o(other.fractions, fractions))
    }

    override def toString: String = operator.toString + fractions.toString
}

object OperatorWithNumber {
    val plus1 = OperatorWithNumber(Plus, 1)
    val plus0 = OperatorWithNumber(Plus, 0)

    def apply(operator: Operator, fractions: Fractions): OperatorWithNumber = new OperatorWithNumber(operator, fractions)

    def apply(s: String): OperatorWithNumber = {
        val f = Fractions(s.substring(1))
        OperatorWithNumber(get(s), f)
    }

    def get(char: Char): Operator = {
        char match {
            case '+' => Plus
            case '-' => Minus
            case '*' => Multiply
            case '/' => Divide
            case '^' => Power
        }
    }

    def get(string: String): Operator = get(string(0))
}

trait Operator {
    def o(f1: Fractions, f2: Fractions): Fractions

    def toString: String

    def isPlus: Boolean = this == Plus || this == Minus

    def isMulti: Boolean = this == Multiply || this == Divide

    def isPower: Boolean = this == Power
}

object Plus extends Operator {
    override def o(f1: Fractions, f2: Fractions): Fractions = f1 + f2

    override def toString: String = "+"
}

object Minus extends Operator {
    override def o(f1: Fractions, f2: Fractions): Fractions = f1 - f2

    override def toString: String = "-"
}

object Multiply extends Operator {
    override def o(f1: Fractions, f2: Fractions): Fractions = f1 * f2

    override def toString: String = "*"
}

object Divide extends Operator {
    override def o(f1: Fractions, f2: Fractions): Fractions = f1 / f2

    override def toString: String = "/"
}

object Power extends Operator {
    override def o(f1: Fractions, f2: Fractions): Fractions = f1 ^ f2

    override def toString: String = "^"
}