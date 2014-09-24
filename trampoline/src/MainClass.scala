/**
 * Created by r.benrejeb on 08/04/14.
 */
object MainClass extends App {

   var  listInt : List[Int] = (for (i<- 0 to 1000000) yield i).toList
 //  var state : State = new State[Int,List[Int]](zipIndex);
 // var result=testZipObject.zipIndex(listInt)
  var result=Parts4And5.zipIndex(listInt)
  println(result)

/*

  def even [A]( ns: List [A]): testStateTrampoline.Trampoline [ Boolean ] =
    ns match {
      case Nil => testStateTrampoline.Done (true)
      case x :: xs => testStateTrampoline.More (() => odd (xs ))
    }
  def odd [A]( ns: List [A ]): testStateTrampoline.Trampoline [ Boolean ] =
    ns match {
      case Nil => testStateTrampoline.Done (false)
      case x :: xs => testStateTrampoline.More (() => even (xs ))
    }


*/

}




