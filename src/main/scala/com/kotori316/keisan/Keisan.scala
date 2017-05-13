package com.kotori316.keisan

object Keisan {

    def calculate(s: String): Fractions = {
        val string = s.replace(" ", "").replace('x', '*')

        def kakko(s1: String): List[String] = {
            val ss = s1.split('(').map("(" + _)
            ss.zipWithIndex.foldLeft(List.empty[String]) { case (l, t) =>
                if (t._1.contains(")")) {
                    val i = t._1.count(_ == ')')
                    val s1 = ss.slice(t._2 - i + 1, t._2).mkString + t._1.substring(0, t._1.lastIndexOf(")") + 1)
                    l :+ s1
                } else {
                    l
                }
            }
        }

        val l1 = kakko(string).map(t => (t, calculate(t.stripPrefix("(").stripSuffix(")"))))
        val s2 = l1.foldLeft(string) { case (st, i) => st.replace(i._1, i._2.toString) }

        val ls = s2.zipWithIndex.collect { case ('+', i) => i; case ('-', i) => i } :+ s2.length
        var f = 0
        val l2 = for (i <- ls.indices) yield {
            val t = s2.substring(f, ls(i))
            f = ls(i)
            t
        }

        l2.filterNot(_.isEmpty).map(calcMultiply(_)).fold(Fractions.zero) {
            _ + _
        }
    }

    def calcMultiply(string: String, f: Fractions = Fractions.one): Fractions = {
        val multiplies = string.split('*')
        multiplies.map(s => {
            val divides = s.split('/')
            if (divides.length == 1) {
                Fractions(1, BigDecimal(s))
            } else {
                divides.map(BigDecimal(_)).map(Fractions(1, _)).reduce((a, b) => a / b)
            }
        }).reduce((a, b) => a * b)
    }

    /*def main(args: Array[String]): Unit = {
        println(calculate("(4+3)*2"))
        println(calculate("-3*7/2*3/2/3"))
        println(Fractions(4, -5) + Fractions(-3, 4))
    }*/

}
