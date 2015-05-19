package calculator

import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal
import scala.io.StdIn
import scala.math._

object Solution extends App with Calculator {
  
  println("Please, enter your expression")
  val str = "1+2*3/4.4-2^3" //StdIn.readLine() //"12+45.0-0.34"
  println("Expression: " + str)
  try {
    println("Result = " + compute(str))
  }
  catch {
    case _: Throwable => println("Wrong input")
  }

  override def compute(input: String): BigDecimal = {
    val num = ArrayBuffer[BigDecimal]()
    val op = ArrayBuffer[Char]()

    var i = 0
    while (i < input.length()) {
      var c = input(i)
      if (isOperator(c)) {
        while (!op.isEmpty && priority(op.last) >= priority(c))
          processOperator(num, removeLastChar(op))
        op += c
      }
      else {
        var operand = ""
        while (i < input.length() && (input(i).isDigit || isDot(input(i)))) {
          operand += input(i)
          i += 1
        }
        i -= 1
        num += operand.toDouble
      }
      i += 1
    }
	while(!op.isEmpty)
	  processOperator(num, removeLastChar(op))
    num(0)
  }

  def processOperator(arr: ArrayBuffer[BigDecimal], ch: Char): Unit = {
    val r = removeLastBigDecimal(arr)
    val l = removeLastBigDecimal(arr)

    ch match {
      case '+' => arr += l + r
      case '-' => arr += l - r
	  case '*' => arr += l * r
	  case '/' => arr += l / r
	  case '^' => arr += pow(l.toDouble, r.toDouble)
    }
  }

  def isDelim(ch: Char): Boolean = {
    ch == ' '
  }

  def isDot(ch: Char): Boolean = {
    ch == '.'
  }

  def isOperator(ch: Char): Boolean = {
    ch == '+' || ch == '-' || ch == '*' || ch == '/' || ch == '^'
  }

  def priority(ch: Char): Int = {
    ch match {
      case '+' | '-' => 1
	  case '*' | '/' => 2
	  case '^' => 3
      case _ => -1
    }
  }

  def removeLastChar(arr: ArrayBuffer[Char]) = {
    val a = arr.last
    arr.remove(arr.length - 1)
    a
  }

  def removeLastBigDecimal(arr: ArrayBuffer[BigDecimal]) = {
    val a = arr.last
    arr.remove(arr.length - 1)
    a
  }

}