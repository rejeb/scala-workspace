package funsets

object Main extends App {
  import FunSets._
  def s:Set=Set(1,3,4,5,7,1000)
  def t:Set=Set(1,2,3,4)
 
  printSet(union(s, t))
  printSet(intersect(s, t))
  printSet(diff(s, t))
  printSet(filter(s, x=>x>8))
  printSet(map(s,x=>x*x))
  println("forall is  "+forall(Set(),x=>x>1))
  println("exists is  "+exists(t,x=> x==4))
}
