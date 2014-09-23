package week7

/**
 * Created by rejeb on 03/01/14.
 */
object test {
  def main(args: Array[String]): Unit = {
    val pouring=new Pouring(Vector(4,9,24))
   println(pouring.solutions(21))
  }
}
