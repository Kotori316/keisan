package com.kotori316.keisan

import java.util.regex.Pattern

object Keisan {
    val pattern: Pattern = Pattern.compile("[+\\-*/^]\\(.+\\)|([+\\-*/^]\\d+\\.\\d+)|([+\\-*/^]\\d+)")

    def calculate(s: String): Fractions = {
        val string = '+' +: s.replace(" ", "").replace('x', '*')
        val matcher = pattern.matcher(string)
        val seqBulder = Seq.newBuilder[String]
        while (matcher.find()) seqBulder += matcher.group()

        def kakko(s1: String): Seq[String] = {
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

        val maped = seqBulder.result().flatMap(t => if (t.contains('(')) kakko(t) else Seq(t))

        // change string to number
        val seq = maped.map(t => {
            if (t.contains('(')) {
                OperatorWithNumber(OperatorWithNumber.get(t(0)), calculate(t.substring(2, t.length - 1)))
            } else {
                OperatorWithNumber(t)
            }
        }) :+ OperatorWithNumber.plus0

        // calculate of power
        def power(op: OperatorWithNumber, list: List[OperatorWithNumber]): List[OperatorWithNumber] = {
            if (list.isEmpty) {
                List(op)
            } else if (list.head.operator.isPower) {
                (op :: list.head) :: power(list.tail.head, list.tail.tail)
            } else {
                op :: power(list.head, list.tail)
            }
        }

        val powered = power(seq.head, seq.toList.tail)

        // calculate
        var temp: OperatorWithNumber = OperatorWithNumber.plus1

        def calc(op: OperatorWithNumber, list: List[OperatorWithNumber]): List[OperatorWithNumber] = {
            if (list.isEmpty) {
                List(op)
            } else if (op.operator.isPlus) {
                if (list.head.operator.isPlus) {
                    op :: calc(list.head, list.tail)
                } else {
                    temp = op
                    calc(list.head, list.tail)
                }
            } else {
                if (list.head.operator.isPlus) {
                    (temp :: op) :: calc(list.head, list.tail)
                } else {
                    temp = temp :: op
                    calc(list.head, list.tail)
                }
            }
        }

        /*return*/ calc(powered.head, powered.tail).fold(OperatorWithNumber.plus0) { (t, f) => t :: f }.fractions
    }

    /*def main(args: Array[String]): Unit = {
        println(calculate("4+2*3^(1+1)"))
        println(Fractions("8.3") + Fractions("6.2"))
    }*/

}
