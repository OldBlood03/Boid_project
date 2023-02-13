package s2.genericInterval

import scala.math.*

/*
 * The goal of this exercise is to practice generalization of an existing class.
 * This is a relatively normal refactoring procedure, when it is noticed that old code
 * can be used in a wider context than was initially planned.
 *
 * In the original exercise on round 3, the goal was to implement the class Interval which handled
 * a time interval between  two Moment objects. The exercise involved implementations for a number of methods including
 * union, intersection etc. which are common for intervals of any type.
 *
 * In this exercise, you will pull your old Interval class from naphtalene and create a
 * parameterized version GenericInterval[T] of it. The greatest part of the functionality offered by the Moment class
 * was related to comparing Moment objects, which was crucial for the proper functionality of the Interval class.
 * We will generalize this, too, by offering an Ordering[T] object with the Interval class can perform exactly the same comparisons.
 *
 * One feature that Ordering[T] cannot offer, and which the original Interval class used,
 * is the distance between two "moments". This is conciously omitted in the GenericInterval[T] class.
 * The length was used only in the toString method anyway so does not affect any other methods.
 */

/** Each instance of the class `GenericInterval` represents an interval -- an inclusive range -- on
  * a scale. An interval has a "start" and an "end", represented as objects of type T. An interval
  * always contains at least a single item.
  *
  * An interval object may be used represent a range of just about anything that has an Ordering.
  *
  * An `Interval` object is immutable after it has been created. That is, its state can not be
  * changed in any way.
  *
  * @param start
  *   the start of the interval (that is, the first item included in the interval)
  * @param end
  *   the end of the interval (that is, the last item included in the interval); equal to or higher
  *   than `start`
  */
class GenericInterval[T](val start: T, val end: T)(using ord: Ordering[T]):

  /** Returns a textual description of the interval. For instance, the interval from 1900 to 2013 is
    * represented by the string `"1900...2013"`.
    */

  override def toString = s"${this.start}...${this.end}"

  /** Determines whether this interval is after the given item. This is only deemed to be the case
    * if the entire interval comes after the given item.
    * @return
    */

  /** Determines whether this interval contains the given item. (An interval also includes its start
    * and end items.)
    *
    * @param item
    *   a single item
    * @return
    *   a boolean value indicating if the item is inside this interval
    */
    def isLaterThan(moment: T) = ord.gt(start, moment)


  /** Determines whether this interval is later than the given interval. This is only deemed
    * to be the case if this entire interval comes after the given interval on the time scale.
    * That is, no overlap is allowed. */
  def isLaterThan(another: GenericInterval[T]): Boolean = isLaterThan(another.end)


  /** Determines whether the given moment is inside this interval. (An interval also includes
    * its start and end moments.) */
  def contains(moment: T) = !this.isLaterThan(moment) && !ord.gt(moment, end)


  /** Determines whether this interval contains the given interval. This is the case if and
    * only if all moments within the other interval are contained within this interval. */
  def contains(another: GenericInterval [T]): Boolean = this.contains(another.start) && this.contains(another.end)


  /** Determines whether this interval overlaps (intersects) the given interval.
    * This is the case if (and only if) one or more of the moments within the other
    * interval are contained within this interval.
    *
    * Note: If one interval is entirely contained within the other, it counts
    * as overlapping, as does the case where one interval ends exactly where the
    * other one begins. */
  def overlaps(another: GenericInterval[T]) = !this.isLaterThan(another) && !another.isLaterThan(this)



  /** Creates, and returns a reference to, a new `Interval` object that represents the union
    * of this interval with the given interval. That is, the starting moment of the new interval
    * is the starting moment of one of the two original intervals, whichever is earlier.
    * Similarly, the end moment of the new interval is the later of the two original end moments.
    *
    * The two original intervals may overlap, but are not required to do so.
    *
    * Examples: The union of the interval from 1995 to 2003 with the interval from 2000 to 2013 is
    * a new interval from 1995 to 2013. The union of the interval from 2000 to 2001 with the interval
    * from 1995 to 1997 is a new interval from 1995 to 2001. */
  extension (t:T){
    def earlier (another: T):T = if ord.gt(t, another) then another else t
    def later (another: T) :T =  if ord.gt(t, another) then t else another
  }
  def union(another: GenericInterval[T]) = GenericInterval(start.earlier(another.start), end.later(another.end))


  /** Creates, and returns a reference to, a new `Interval` object that represents the intersection
    * of this interval with the given interval. That is, the starting moment of the new interval
    * is the starting moment of one of the two original intervals, whichever is later. Similarly,
    * the end moment of the new interval is the earlier of the two original end moments.
    *
    * However, this method only produces a new interval in case the two original intervals overlap.
    * In that case, the new interval is wrapped inside a `Some` object. If no intersection exists, ̀
    * the method returns `None` instead.
    *
    * Examples: The intersection of the interval from 1995 to 2003 with the interval from 2000 to
    * 2013 is a new interval from 2000 to 2003. The intersection of the interval from 2000 to 2001
    * with the interval from 1995 to 1997 does not exist.
    *
    * @see overlaps
    * @see union   */
  def intersection(another: GenericInterval[T]) =
    if this.overlaps(another) then
      val intersectingBit = GenericInterval(this.start.later(another.start), this.end.earlier(another.end))
      Some(intersectingBit)
    else
      None

end GenericInterval
