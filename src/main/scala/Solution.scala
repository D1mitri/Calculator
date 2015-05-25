package calculator

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.math._
import util.control.Breaks._

object Solution extends App with Calculator {
  
  println("=========================================================================")
  println("Hello to my Calculator (v.1.0)")
  println("You can use operations (+,-,*,/,^), functions (sin and cos) and brackets.")
  println("Press 'x' and press Enter to draw back from the program")
  println("=========================================================================")
  var str = ""
  while(str != "x") {
	println("Please, enter your expression")
    str = StdIn.readLine() //"12+45.0-0.34", "1+2*3/4.4-2^3" , "-(23+1/4)*2!+sin(cos(1+2^4))"
    try {
	  if (str == "x")
		println("Exit")
	  else {
		println("Expression: " + str)
		println("Result = " + compute(str))
	  }
    }
    catch {
      case _: Throwable => println("Wrong input")
    }
  println("-------------------------------------------------------------------------")
  }

  override def compute(input: String): BigDecimal = {
    val num = ArrayBuffer[BigDecimal]()
    val op = ArrayBuffer[Char]()
	val func = ArrayBuffer[Char]()

    var i = 0
    while (i < input.length()) {
      var c = input(i)
	  
	  breakable {
	    if (isDelim(c))
		  break
	    else if (c =='(') {
	      op += c
		  if (input(i+1) == '-') {
		    num += -1
		    op += '*'
		    i+=1
		  }
	    }
	    else if (c == ')') {
	    while (op.last != '(')
          processOperator(num, removeLastChar(op));
		  if(func.length > 0)
		    processFunction(num, removeLastChar(func))
          removeLastChar(op)
        }
	    else if (isUnarniyMinus(c, i)) {
	      num += -1
		  op += '*'
	    }
        else if (isOperator(c)) {
          while (!op.isEmpty && priority(op.last) >= priority(c))
            processOperator(num, removeLastChar(op))
          op += c
        }
	    else if (isFactorial(c)) {
	      num += factorial(removeLastBigDecimal(num).toInt).toDouble
	    }
	    else if (isFunction(c)) {
	      func += c
		  i+=2
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
  
  def processFunction(arr: ArrayBuffer[BigDecimal], ch: Char): Unit = {
    val r = removeLastBigDecimal(arr)
	
	ch match {
	  case 's' => arr += sin((r.toDouble).toRadians)
	  case 'c' => arr += cos((r.toDouble).toRadians)
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
  
  def isFactorial(ch: Char) : Boolean = {
    ch == '!'
  }
  
  def isUnarniyMinus(ch: Char, i: Int) : Boolean = {
    ch == '-' && i == 0
  }
  
  def isFunction(ch: Char) : Boolean = {
    ch == 's' || ch == 'c'
  }
  
  def factorial(n: Int) : Int = {
    var res = 1
	for(x <- 1 until n+1) res *= x
	res
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