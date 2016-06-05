/**
 * Copyright (C) 2016 Lightbend Inc. <http://www.lightbend.com>
 */
package akka.util

/**
 * INTERNAL API
 */
private[akka] object OptionVal {

  def apply[A](x: A): OptionVal[A] =
    new OptionVal(x)

  /**
   * Represents non-existent values, `null` values.
   */
  val None = new OptionVal(null)
}

/**
 * INTERNAL API
 * Represents optional values similar to `scala.Option`, but
 * as a value class to avoid allocations.
 *
 * Note that it should not be used in pattern matching to avoid
 * allocations. That is also the reason why it's not a `case class`
 * with `unapply`.
 * See http://docs.scala-lang.org/overviews/core/value-classes.html#when-allocation-is-necessary
 */
private[akka] final class OptionVal[+A](val x: A) extends AnyVal {

  /**
   * Returns true if the option is `OptionVal.None`, false otherwise.
   */
  def isEmpty: Boolean =
    x == null

  /**
   * Returns true if the option is `OptionVal.None`, false otherwise.
   */
  def isDefined: Boolean = !isEmpty

  /**
   * Returns the option's value if the option is nonempty, otherwise
   * return `default`.
   */
  def getOrElse[B >: A](default: B): B =
    if (x == null) default else x

  /**
   *  Returns the option's value if it is nonempty, or `null` if it is empty.
   */
  def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = this getOrElse ev(null)

  /**
   * Returns the option's value.
   * @note The option must be nonEmpty.
   * @throws java.util.NoSuchElementException if the option is empty.
   */
  def get: A =
    if (x == null) throw new NoSuchElementException("OptionVal.None.get")
    else x

  override def toString: String =
    if (x == null) "None" else s"Some($x)"
}
