package com.kotori316.keisan

import com.kotori316.keisan.Fraction._

/**
  *
  * @param d must be over 0.
  * @param n numerator
  */
class Fraction private(d: BigDecimal, n: BigDecimal, val gcd: BigDecimal) extends Number {

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

    def plus(fractions: Fraction): Fraction = {
        if (denominator == bigDemicalOne) {
            val nd = fractions.denominator
            val nn = numerator * nd + fractions.numerator
            Fraction(nd, nn)
        } else {
            val ngcd = denominator.toBigInt().gcd(fractions.denominator.toBigInt())
            val nd = denominator * fractions.denominator / BigDecimal(ngcd)
            val nn = numerator * (nd / denominator) + fractions.numerator * (nd / fractions.denominator)
            Fraction(nd, nn)
        }
    }

    def subtract(fractions: Fraction): Fraction = {
        plus(fractions * Fraction.minus1)
    }

    def multiply(fractions: Fraction): Fraction = {
        Fraction(denominator * fractions.denominator, numerator * fractions.numerator)
    }

    @throws[java.lang.ArithmeticException]
    def divide(fractions: Fraction): Fraction = {
        if (fractions.denominator.bigDecimal.equals(java.math.BigDecimal.ZERO))
            throw new java.lang.ArithmeticException("divide by zero")
        this * fractions.reciprocal
    }

    @throws[java.lang.IllegalArgumentException]
    def power(fractions: Fraction): Fraction = {
        if (fractions.denominator == bigDemicalOne) {
            val nd = denominator.pow(fractions.numerator.toInt)
            val nn = numerator.pow(fractions.numerator.toInt)
            new Fraction(nd, nn, bigDemicalOne)
        } else {
            throw new IllegalArgumentException("exponent must be an integer.")
        }
    }

    def +(fractions: Fraction): Fraction = this plus fractions

    def -(fractions: Fraction): Fraction = this subtract fractions

    def *(fractions: Fraction): Fraction = this multiply fractions

    @throws[java.lang.ArithmeticException]
    def /(fractions: Fraction): Fraction = this divide fractions

    @throws[java.lang.IllegalArgumentException]
    def ^(fractions: Fraction): Fraction = this power fractions

    def reciprocal: Fraction = Fraction(numerator, denominator)

    override def hashCode(): Int = {
        numerator.hashCode() / denominator.hashCode()
    }

    override def equals(obj: scala.Any): Boolean = {
        if (super.equals(obj)) {
            true
        } else obj match {
            case o: Fraction => denominator == o.denominator && numerator == o.numerator
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

object Fraction {
    private val bigDemicalOne = BigDecimal(java.math.BigDecimal.ONE)

    def apply(denominator: BigDecimal, numerator: BigDecimal): Fraction = new Fraction(denominator, numerator)

    /**
      * Fraction(a, b)
      *
      * @return a / b
      */
    def apply(n: Int, d: Int): Fraction = new Fraction(d, n)

    def apply(n: Int): Fraction = {
        n match {
            case 0 => zero
            case 1 => one
            case -1 => minus1
            case _ => Fraction(BigDecimal(n), bigDemicalOne)
        }
    }

    def apply(n: String): Fraction = {
        val decimal = BigDecimal(n)
        if (n.contains('.')) {
            val a = decimal.scale
            val tenPower = BigDecimal(10).pow(a)
            new Fraction(tenPower, (decimal * tenPower).setScale(0))
        } else {
            new Fraction(bigDemicalOne, decimal)
        }
    }

    def apply(d: Fraction, n: Fraction): Fraction = n / d

    val minus1 = new Fraction(bigDemicalOne, BigDecimal(-1))
    val one = new Fraction(bigDemicalOne, bigDemicalOne)
    val zero = new Fraction(bigDemicalOne, BigDecimal(java.math.BigDecimal.ZERO))

    implicit def i2f(i: Int): Fraction = Fraction(i)

    implicit def i2f(i: BigDecimal): Fraction = Fraction(bigDemicalOne, i)

    implicit def i2f(i: BigInt): Fraction = Fraction(bigDemicalOne, BigDecimal(i))
}
