package com.kotori316.keisan

import com.kotori316.keisan.Fractions._

/**
  *
  * @param d must be over 0.
  * @param n numerator
  */
class Fractions private(d: BigDecimal, n: BigDecimal, val gcd: BigDecimal) extends Number {

    def this(d: BigDecimal, n: BigDecimal) = {
        this(d, n, BigDecimal(d.toBigInt().gcd(n.toBigInt())))
    }

    if (d.bigDecimal.equals(java.math.BigDecimal.ZERO))
        throw new java.lang.ArithmeticException("divide by zero")

    val denominator: BigDecimal = d.abs / gcd
    val numerator: BigDecimal = if (d < 0) n * -1 / gcd else n / gcd

    private val actualValue = numerator / denominator

    override def intValue(): Int = actualValue.intValue()

    override def floatValue(): Float = actualValue.floatValue()

    override def doubleValue(): Double = actualValue.doubleValue()

    override def longValue(): Long = actualValue.longValue()

    def plus(fractions: Fractions): Fractions = {
        if (denominator == bigDemicalOne) {
            val nd = fractions.denominator
            val nn = numerator * nd + fractions.numerator
            Fractions(nd, nn)
        } else {
            val ngcd = denominator.toBigInt().gcd(fractions.denominator.toBigInt())
            val nd = denominator * fractions.denominator / BigDecimal(ngcd)
            val nn = numerator * (nd / denominator) + fractions.numerator * (nd / fractions.denominator)
            Fractions(nd, nn)
        }
    }

    def subtract(fractions: Fractions): Fractions = {
        plus(fractions * Fractions.minus1)
    }

    def multiply(fractions: Fractions): Fractions = {
        Fractions(denominator * fractions.denominator, numerator * fractions.numerator)
    }

    @throws[java.lang.ArithmeticException]
    def divide(fractions: Fractions): Fractions = {
        if (fractions.denominator.bigDecimal.equals(java.math.BigDecimal.ZERO))
            throw new java.lang.ArithmeticException("divide by zero")
        this * fractions.reciprocal
    }

    @throws[java.lang.IllegalArgumentException]
    def power(fractions: Fractions): Fractions = {
        if (fractions.denominator == bigDemicalOne) {
            val nd = denominator.pow(fractions.numerator.toInt)
            val nn = numerator.pow(fractions.numerator.toInt)
            new Fractions(nd, nn, bigDemicalOne)
        } else {
            throw new IllegalArgumentException("exponent must be an integer.")
        }
    }

    def +(fractions: Fractions): Fractions = this plus fractions

    def -(fractions: Fractions): Fractions = this subtract fractions

    def *(fractions: Fractions): Fractions = this multiply fractions

    @throws[java.lang.ArithmeticException]
    def /(fractions: Fractions): Fractions = this divide fractions

    @throws[java.lang.IllegalArgumentException]
    def ^(fractions: Fractions): Fractions = this power fractions

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

    def toDecimalString: String = actualValue.toString()

    def toString(decimal: Boolean): String = if (decimal) toDecimalString else toString
}

object Fractions {
    private val bigDemicalOne = BigDecimal(java.math.BigDecimal.ONE)

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
            case _ => Fractions(BigDecimal(n), bigDemicalOne)
        }
    }

    def apply(n: String): Fractions = {
        val decimal = BigDecimal(n)
        if (n.contains('.')) {
            val a = decimal.scale
            val tenPower = BigDecimal(10).pow(a)
            new Fractions(tenPower, (decimal * tenPower).setScale(0))
        } else {
            new Fractions(bigDemicalOne, decimal)
        }
    }

    def apply(d: Fractions, n: Fractions): Fractions = n / d

    val minus1 = new Fractions(bigDemicalOne, BigDecimal(-1))
    val one = new Fractions(bigDemicalOne, bigDemicalOne)
    val zero = new Fractions(bigDemicalOne, BigDecimal(java.math.BigDecimal.ZERO))

    implicit def i2f(i: Int): Fractions = Fractions(i)

    implicit def i2f(i: BigDecimal): Fractions = Fractions(bigDemicalOne, i)

    implicit def i2f(i: BigInt): Fractions = Fractions(bigDemicalOne, BigDecimal(i))
}
