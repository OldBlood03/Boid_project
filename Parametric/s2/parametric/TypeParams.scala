package s2.parametric

object TypeParams:

  /** NOTE!
    *
    * Some of the function signatures are readily given in this exercise but it is recommended to take a moment to
    * familiarize oneself with them - even if the exercise seemed easy.
    *
    * There are 4 assignments altogether - their score values are 5p + 10p + 10p + 25p = 50p
    */

  /*
   * a) Exercise 5p
   *
   * Implement the method identical which takes two parameters of the _same type_ and
   * returns true if the parameters are equal or false if they differ.
   *
   * This is the only assignment in which you will have to fill in the type parameter yourself.
   */
  def identical[T](first:T, second:T): Boolean = first == second

  /* SIDENOTE: tuples
   *
   * Some students have been puzzled by tuples so here's a quick recap:
   * The fall part of the course (Programming 1) handled the simplest of tuples, namely pairs
   * in chapter 9.2. A tuple is denoted by a pair of brackets "()" which holds elements that are separated by commas.
   * The elements of a tuple can be accessed as instance variables _1, _2, _3 etc.
   *
   * val x = (1,2,"cat")
   * val animal = x._3
   */

  /* b) Exercise 10p
   *
   * Implement the method orderPair, which takes as a parameter a tuple with 2 elements. The parameters must be of the same type.
   * If the elements are in ascending order, the method returns the original parameter tuple.
   * parametrina saadun tuplen. Otherwise, it returns a tuple in which the order of the elements has been swapped.

   * The Ordering object that is taken as an impliit parameter offers a host of handy methods for comparing elements.
   * Go have a look at eg. the method gt (greater than). You will need that or some other method to succesfully complete this exercise.
   *
   * http://www.scala-lang.org/api/current/index.html#scala.math.Ordering@gt(x:T,y:T):Boolean
   */
  def orderPair[T](pair: (T,T))(using ord: Ordering[T]): (T, T) = if ord.gt(pair._1,pair._2) then (pair._2,pair._1) else (pair._1,pair._2)

  /* c) Exercise 10p
   *
   * Implement the method firstSame. It takes as a parameter a collection of tuples
   * and returns the first tuple whose elements are equal in an Option wrapper.
   * If no such tuple is found, the method returns None.
   *
   * Once again, read the signature of the function.
   */
  def firstSame[T](collection: Iterable[(T, T)]): Option[(T, T)] =
    collection.find((x,y) => x==y)

  /* d) Assignment 25 p.
   *
   * The method zip in the class List "glues together" the lists 1,2,3,4 and a,b,c,d and
   * forms the list of pairs (1,a), (2,b), (3,c), (4,d).
   *
   * List(1,2,3,4).zip( List("a","b","c","d") )
   *
   * In this exercise, you will implement the method rotatingZip that performs the same basic procedure
   * with the exception that it accepts as parameters two lists with differing sizes if the second one is shorter than the first one and
   * the length of the second list is a multiple of the size of the first list. The second list will be iterated as many times as is necessary
   * to complete the zipping.
   *
   * rotatingZip(List(1,2,3,4), List("a","b"))
   *
   * > List((1,"a"),(2,"b"),(3,"a"),(4,"b"))
   */
  def rotatingZip[A, B](alist: List[A], blist: List[B]): List[(A, B)] =
    if ((alist.length > blist.length) && !blist.isEmpty) then
       alist.foldLeft((0,List[(A,B)]()))((x,y) => (x._1+1,x._2 :+ (y,blist(x._1%blist.length))))._2
    else if (alist.length < blist.length) then
      blist.foldLeft((0,List[(A,B)]()))((x,y) => (x._1+1,x._2 :+ (alist(x._1%alist.length),y)))._2
    else
      alist.zip(blist)


end TypeParams

/** NOTE! You can use this for testing your application
  */
