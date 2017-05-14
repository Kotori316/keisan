package com.kotori316.keisan

import java.util.regex.Pattern

object Keisan {
    val pattern = Pattern.compile("[+\\-*/]\\(.+\\)|([+\\-*/]\\d+\\.\\d+)|([+\\-*/]\\d+)")

    def calculate(s: String): Fraction = {
        val string = '+' +: s.replace(" ", "").replace('x', '*')
        val matcher = pattern.matcher(string)
        val seqBulder = Seq.newBuilder[String]
        while (matcher.find()) seqBulder += matcher.group()
        val maped = seqBulder.result().flatMap(t => if (t.contains('(')) kakko(t) else Seq(t))
        val seq = maped.map(t => {
            if (t.contains('(')) {
                OperatorWithNumber(OperatorWithNumber.get(t(0)), calculate(t.substring(2, t.length - 1)))
            } else {
                OperatorWithNumber(t)
            }
        }) :+ OperatorWithNumber.plus0

        val bulder = Seq.newBuilder[OperatorWithNumber]
        var temp: OperatorWithNumber = OperatorWithNumber.plus1
        for (i <- 0 until seq.size - 1) {
            val seq1 = seq(i)
            if (seq1.operator.isPlus) {
                if (seq(i + 1).operator.isPlus) {
                    bulder += seq1
                } else {
                    temp = seq1
                }
            } else {
                if (seq(i + 1).operator.isPlus) {
                    bulder += temp :: seq1
                } else {
                    temp = temp :: seq1
                }
            }
        }

        bulder.result().fold(OperatorWithNumber.plus0) { (t, f) => t :: f }.fractions
    }

    def kakko(s1: String): List[String] = {
        val ss = s1.split('(').map("(" + _)
        ss.zipWithIndex.foldLeft(List.empty[String]) { case (l, t) =>
            if (t._1.contains(")")) {
                val i = t._1.count(_ == ')')
                val s1 = ss.slice(t._2 - i + 1, t._2).mkString + t._1.substring(0, t._1.lastIndexOf(")") + 1)
                l :+ (ss(t._2 - i).last + s1)
            } else {
                l
            }
        }
    }

    def calcMultiply(string: String, f: Fraction = Fraction.one): Fraction = {
        val multiplies = string.split('*')
        multiplies.map(s => {
            val divides = s.split('/')
            if (divides.length == 1) {
                Fraction(s)
            } else {
                divides.map(Fraction(_)).reduce((a, b) => a / b)
            }
        }).reduce((a, b) => a * b)
    }

    /*def main(args: Array[String]): Unit = {
        println(Fractions("8.3") + Fractions("6.2"))
    }*/

}
