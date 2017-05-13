package com.kotori316.keisan

/**
  *
  * @param d must be over 0.
  * @param n numerator
  */
class Fractions(d: BigDecimal, n: BigDecimal) extends Number {
    if (d.bigDecimal.equals(java.math.BigDecimal.ZERO))
        throw new java.lang.ArithmeticException("divide by zero")

    val gcd = BigDecimal(d.toBigInt().gcd(n.toBigInt()))
    val denominator: BigDecimal = d.abs / gcd
    val numerator: BigDecimal = if (d < 0) n * -1 / gcd else n / gcd

    override def intValue(): Int = (numerator / denominator).intValue()

    override def floatValue(): Float = (numerator / denominator).floatValue()

    override def doubleValue(): Double = (numerator / denominator).doubleValue()

    override def longValue(): Long = (numerator / denominator).longValue()

    def plus(fractions: Fractions): Fractions = {
        val ngcd = denominator.toBigInt().gcd(fractions.denominator.toBigInt())
        val nd = denominator * fractions.denominator / BigDecimal(ngcd)
        val nn = numerator * (nd / denominator) + fractions.numerator * (nd / fractions.denominator)
        new Fractions(nd, nn)
    }

    def subtract(fractions: Fractions): Fractions = {
        plus(fractions * Fractions.minus1)
    }

    def multiply(fractions: Fractions): Fractions = {
        Fractions(denominator * fractions.denominator, numerator * fractions.numerator)
    }

    def divide(fractions: Fractions): Fractions = {
        this * fractions.reciprocal
    }

    def +(fractions: Fractions): Fractions = this plus fractions

    def -(fractions: Fractions): Fractions = this subtract fractions

    def *(fractions: Fractions): Fractions = this multiply fractions

    def /(fractions: Fractions): Fractions = this divide fractions

    def reciprocal: Fractions = Fractions(numerator, denominator)

    override def hashCode(): Int = {
        numerator.hashCode() / denominator.hashCode()
    }

    override def equals(obj: scala.Any): Boolean = {
        if (super.equals(obj)) {
            true
        } else obj match {
            case o: Fractions => denominator == o.denominator && numerator == o.numerator
            case _ => false
        }
    }

    override def toString: String = {
        if (denominator.bigDecimal.equals(java.math.BigDecimal.ONE))
            numerator.toString()
        else
            numerator.toString() + " / " + denominator.toString()
    }
}

object Fractions {
    def apply(denominator: BigDecimal, numerator: BigDecimal): Fractions = new Fractions(denominator, numerator)

    /**
      * Fractions(a, b)
      *
      * @return a / b
      */
    def apply(n: Int, d: Int): Fractions = new Fractions(d, n)

    def apply(n: Int): Fractions = {
        n match {
            case 0 => zero
            case 1 => one
            case -1 => minus1
            case _ => Fractions(n, 1)
        }
    }

    def apply(d: Fractions, n: Fractions): Fractions = n / d

    val minus1 = new Fractions(BigDecimal(java.math.BigDecimal.ONE), BigDecimal(-1))
    val one = new Fractions(BigDecimal(java.math.BigDecimal.ONE), BigDecimal(java.math.BigDecimal.ONE))
    val zero = new Fractions(BigDecimal(java.math.BigDecimal.ONE), BigDecimal(java.math.BigDecimal.ZERO))

    implicit def i2f(i: Int): Fractions = Fractions(i)

    implicit def i2f(i: BigDecimal): Fractions = Fractions(1, i)

    implicit def i2f(i: BigInt): Fractions = Fractions(1, BigDecimal(i))
}
