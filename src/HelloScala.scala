import java.io.FileReader
import java.util.{HashMap => JavaHashMap, Date, Properties}

import TrafficLightColor._
import TrafficLightColor.TrafficLightColor

import scala.beans.BeanProperty
import scala.collection.immutable.StringOps
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Int
import Point._

import scala.sys.process.FileProcessLogger

//import com.horstmann.impatient.peaple._
/**
 * Created by suhwan on 2015-09-20.
 */
object HelloScala extends App {

  val aaa = new SavingAccount

  val bbb = new Fraction(10, 8)

  println (bbb.gcd(200, 110))
  println (bbb.scm(110, 200, 1))
}

class Fraction (n: Int, d: Int) {
  private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n,d)
  private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n,d)

  override def toString = num + "/" + den

  def sign(a: Int) = if (a> 0) 1 else if (a < 0) -1 else 0
  def gcd(a: Int, b: Int) : Int = if (b==0) a else gcd(b,a%b)
  def scm(a: Int, b: Int, multiply: Int) : Int = {
    println (a +" " + b + " " + multiply)
    if (a == b) a
    else if (a > b) {
      if ((a * multiply) % b == 0) (a * multiply) else scm (a,b, multiply + 1)
    }
    else {
      if ((b * multiply) % a == 0) (b * multiply) else scm (a, b, multiply + 1)
    }
  }
}


class SavingAccount1 {

  def | (data: String): String = {
    "<td>"+ data + "</td>"
  }
}

object SavingAccount1 {
  def | (data: String): String = {
    "<td>"+ data + "</td>"
  }
}


trait Logged {
  val aa = 10
  def log(msg: String) {}
}

trait Account {
  def withdraw (amount: Double)
}

trait ConsoleLogger extends Logged {
  override val aa = 100
  override def log (msg: String) { println (msg + aa) }
}

class SavingAccount extends Account with Logged {
  def withdraw (amount: Double): Unit = {
    log ("ABCDEFG " + amount)
  }
}

//trait TimestampLogger1 extends Logger {
//
//  abstract  override def log (msg: String): Unit = {
//    super.log(msg + " " + new java.util.Date())
//  }
//}

trait TimestampLogger extends Logged {
  override val aa = 30
  override def log (msg: String): Unit = {
    super.log(msg + " " + new java.util.Date() + " " + aa)
  }
}

trait ShortLogger extends Logged {
  val maxLength = 70
  override val aa = 40
  override def log (msg : String): Unit = {
    super.log(
      if (msg.length <= maxLength) msg + aa else msg.substring(0,maxLength-3) + "..." + aa
    )
  }
}


//trait Logger {
//  def log(msg: String)
//}
//
//class ConsoleLogger extends Logger with Cloneable with Serializable {
//  def log(msg: String) { println (msg)}
//}

