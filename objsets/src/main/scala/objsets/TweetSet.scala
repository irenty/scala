package objsets

import scala.util.Try

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}





/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def toList: TweetList
  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = {
    def add(remaining: TweetList, s: TweetSet): TweetSet = {
      if (remaining.isEmpty) s
      else add(remaining.tail, s.incl(remaining.head))
    }
    add(toList, that)
  }

  def unionWithMutableCollection(that: TweetSet): TweetSet = {
    val acc = scala.collection.mutable.ArrayBuffer.empty[Tweet]
    foreach(tweet => acc += tweet)

    def add(acc: scala.collection.mutable.ArrayBuffer[Tweet], s: TweetSet): TweetSet = {
      if (acc.isEmpty) s
      else add(acc.tail, s.incl(acc.head))
    }
    add(acc, that)
  }


  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def mostRetweeted: Tweet
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def descendingByRetweet: TweetList = {
      val headOption = Try(mostRetweeted).toOption
      headOption.fold[TweetList](Nil){ top => new Cons(top, remove(top).descendingByRetweet)}
    }
  
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def isEmpty: Boolean

}

class Empty extends TweetSet {
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  
  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def union(that: TweetSet): TweetSet = that

  override def mostRetweeted: Tweet = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def toList: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      if (p(elem)) left.filterAcc(p, right.filterAcc(p, acc incl elem))
      else left.filterAcc(p, right.filterAcc(p, acc))
    }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def unionSlow(that: TweetSet): TweetSet = {
    ((left union right) union that) incl elem
  }

  override def mostRetweeted: Tweet = {
    val leftOption = Try(left.mostRetweeted).toOption
    val rightOption = Try(right.mostRetweeted).toOption

    val tweets = Seq(Some(elem), leftOption, rightOption).flatten
    tweets.maxBy(_.retweets)
  }

  override def isEmpty: Boolean = false

  override def toList: TweetList =
    new Cons(elem, left.toList.addAll(right.toList))
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  def addAll(other: TweetList): TweetList = {

    def add(remainingList: TweetList, newList: TweetList): TweetList = {
      if (remainingList.isEmpty) newList
      else add(remainingList.tail, new Cons(remainingList.head, newList))
    }
    add(this, other)
  }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

  override def addAll(other: TweetList): TweetList = other
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false

}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  println("Started")
  val allSources = TweetReader.sources
  println("Sources done")
  val allTweets = TweetReader.allTweets
  println("Finished")
  def tweetFilter(words: Seq[String])(tweet: Tweet): Boolean =
    tweet.text.split(' ').intersect(words).nonEmpty


  val googleTweets: TweetSet = allTweets.filter(tweetFilter(google))
  val appleTweets: TweetSet = allTweets.filter(tweetFilter(apple))
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
  }

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
