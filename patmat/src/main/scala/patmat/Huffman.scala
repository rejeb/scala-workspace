package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {
  def main(args: Array[String]): Unit = {
  val sampleTree = makeCodeTree(makeCodeTree(Leaf('x', 1), Leaf('e', 1)),Leaf('t', 2))
  val text =List('f', 'i', 'e', 't', 'p', 't', 'c', 'c', 'e', ' ', ' ', '0', 'r', 'e', 's', ' ', 's', 'c', 's',',', 'a', ' ', 'i', 'b', 'e', ' ', 'c', 'n', 'a', 'i', 'i', 'y', ',', 'b', 't', 's', 'i', 'w', 'r', ' ', 'f', 'c', 'l', 'm', 'a', 'l', ' ', 'k', 'd', 'd', ' ', 's', 'r', 'l', 'r', 'a', 'n')
  val freqs=times(text)
  val leafs=makeOrderedLeafList(freqs)
    val singleTree = createCodeTree(text)
  val encodeMessage =encode(singleTree)(List('n', ' ', 'i', 'r'))
//
    println("encoded message"+singleTree)
    println("decoded message"+decodedSecret)
  }
  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree



  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(char,weight) => weight
    case Fork(left,right,chars,weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case  Leaf(char,weight) => List(char)
    case Fork(left,right,chars,weight) => chars
  }
  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case Nil=> Nil
    case x::xs => (x,chars.partition(y => y==x)._1.length)::times(chars.partition(y => y==x)._2)
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[CodeTree] ={
    def makeLeafs (freq : List[(Char,Int)]) : List[CodeTree] = freq match { 
    case List()=>Nil
    case x::xy=>   new Leaf(x._1,x._2)::makeLeafs(xy)
    }
   isort(makeLeafs(freqs))((x,y)=> weight(x)<weight(y))
  }

  def isort[T](list : List[T])(implicit f: (T,T) => Boolean ) : List[T] ={
      if(list.length/2==0) list
    else{

      merge(isort(list.splitAt(list.length/2)._1),isort(list.splitAt(list.length/2)._2))
    }

  }
  def  merge[T](xs :List[T], ys :List[T])(implicit f: (T,T) => Boolean ):List[T]=(xs,ys) match {
    case( Nil,ys) =>ys
    case (xs,Nil) => xs
    case (x::xs1,y::ys1)=> if(f(x,y)) x::merge(xs1,ys) else y::merge(xs,ys1)
  }
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = !trees.isEmpty&&trees.tail.isEmpty

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if(trees.size<2) trees
    else{
     isort(makeCodeTree(trees.head,trees.tail.head):: trees.tail.tail)((x,y )=> weight(x)<weight(y))
    }
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] =>List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if(trees.isEmpty) throw new Error("Empty trees List")
    if(singleton(trees)){  trees}
    else{ until(singleton,combine) (combine(trees))}
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {

    val tree =(until(singleton ,combine )(makeOrderedLeafList(times(chars))))
    if(tree.isEmpty) throw new Error("Empty List of chars")
    else tree.head
  }



  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeFork(subTree: CodeTree, subBits: List[Bit]): (List[Char],List[Bit]) = subTree match {
      case Leaf(c,_) => (chars(subTree),subBits)
      case Fork(l,r,_,_) => subBits match {
        case Nil => (Nil,Nil)
        case x::xy =>  if(subBits.head.equals(1)) decodeFork(r,subBits.tail) else decodeFork(l,subBits.tail)}
    }

    if(decodeFork(tree,bits)._2.isEmpty)
      decodeFork(tree,bits)._1 else decodeFork(tree,bits)._1 ::: decode(tree,decodeFork(tree,bits)._2)
  }



  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  val leafs  =List(Leaf('x',4),Leaf('t',2),Leaf('e',1)) //did not equal List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))
  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode,secret)



  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =text match {
    case Nil => Nil
    case x::xy => tree match{
      case Leaf(cc,_) => Nil
      case Fork(left,right,_,_) => if(chars(left).contains(x)) List(0):::encode(left)(x::Nil):::encode(tree)(text.tail)
      else if(chars(right).contains(x))  List(1):::encode(right)(x::Nil):::encode(tree)(text.tail)
      else Nil
    }

  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]
  type pair=(Char, List[Bit])
  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] ={
    if( table.filter( x => x._1==char).isEmpty)Nil
    else
    table.filter( x => x._1==char).head._2
  }


  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable ={
      def convertSub(subTree :  CodeTree,bits : List[Bit]): CodeTable = subTree match{
    case Leaf(cc,_) => List((cc,bits))
    case Fork(left,right,_,_) => mergeCodeTables(convertSub(left,bits:::List[Bit](0)),convertSub(right,bits:::List[Bit](1)))
  }
    convertSub(tree,List[Bit]())
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a match {
    case Nil => b
    case x::ys => b match {
      case Nil => a
      case xb::ysb=> if(a.filter(x=> x._1==xb._1).isEmpty)x::xb::mergeCodeTables(ys,ysb)else (x._1,x._2:::xb._2)::mergeCodeTables(ys,ysb)
    }
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    if(text.tail.isEmpty)
      codeBits(convert(tree))(text.head)
      else{
      codeBits(convert(tree))(text.head):::quickEncode(tree)(text.tail)
    }
  }
}
