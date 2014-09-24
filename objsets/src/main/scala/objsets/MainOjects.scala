/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package objsets



object MainOjects {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
//    val tweets=new NonEmpty(new Tweet("rejeb","tweet10",35),new Empty,new Empty).incl(new Tweet("rami","tweet8",8)).
//    incl(new Tweet("rami","tweet6",6)).incl(new Tweet("rejeb","tweet26",26)).incl(new Tweet("rejeb","tweet18",5)).
//    incl(new Tweet("rami","tweet28",24)).incl(new Tweet("othmen","tweet2",25))
//   
//    println
//    println
//     val tweets2=new NonEmpty(new Tweet("rejeb","tweet10",35),new Empty,new Empty).incl(new Tweet("rami","tweet8",8)).
//    incl(new Tweet("rami","tweet6",6)).incl(new Tweet("rejeb","tweet26",26)).incl(new Tweet("rejeb","tweet18",5)).
//    incl(new Tweet("rami","tweet28",24)).incl(new Tweet("othmen","tweet2",25)).incl(new Tweet("rejeb","tweet11",35))
//      println
//      println
//    val empty=new NonEmpty(new Tweet("rejeb","tweet11",35),new Empty,new Empty)
//    val result = tweets.union(tweets2)
//    result foreach(println)
//   TweetReader.allTweets foreach(println)
  // Print the trending tweets
  GoogleVsApple.trending foreach println
  }

}
