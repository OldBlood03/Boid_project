package s2.genericInterval

/*
 * This object is given as an example of testing the GenericInterval class.
 * The grading tests are broader than these though, so success in the operations
 * described in this object does not guarantee full score. You may add your own
 * tests freely as this file will not be submitted for grading.
 */

@main
def intervalUseExamples =

  val decade = GenericInterval(1990, 2000)
  val years = GenericInterval(1994, 2003)
  val names = GenericInterval("Matti", "Pekka")
  val otherNames = GenericInterval("Eeva", "Otto")

  import java.util.{GregorianCalendar as Cal}

  def makeTime(day: Int, month: Int, year: Int) = Cal(year, month - 1, day).getTime()

  val dates = GenericInterval(makeTime(12, 1, 1995), makeTime(22, 3, 1995))
  val dates2 = GenericInterval(makeTime(17, 1, 1994), makeTime(15, 2, 1995))

  /*
   * --String interpolation--
   *
   * s"variable $x is greater than ${y(z)}"
   *    is shorthand for
   * "variable " + x + " is greater than " + y(z)
   */

  println(s"Does $decade contain 1995?  ${decade.contains(1995)}")
  println(s"The intersection of $decade and $years is ${decade.intersection(years)}")

  println(s"Does $names contain Anneli? ${names.contains("Anneli")}")
  println(s"Does $names contain Niilo? ${names.contains("Niilo")}")
  println(s"The intersection of $names and $otherNames is ${names.intersection(otherNames)}")

  println(s"The intersection of\n\t$dates\n\t\tand\n\t$dates2 \n\t\tis\n\t${dates.intersection(dates2)}")

end intervalUseExamples
