package purity

import cats.Order
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertions.succeed
import org.scalatest.Assertions.fail
import purity.implicits._

class FormalSpecsPresentation extends PuritySuite {





  case class History(tweets: List[Tweet])

  case class Tweet(message: String, timestamp: Long)

  implicit val tweetOrder: Order[Tweet] =
    (x: Tweet, y: Tweet) =>
      if(x.timestamp == y.timestamp) 0
      else if(x.timestamp < y.timestamp) -1
      else 1

  implicit val tweetOrdering: Ordering[Tweet] =
    tweetOrder.toOrdering





  val tweetsAreOrdered: Proposition[History] =
    areOrdered[Tweet].in[History](_.tweets)

  val tweetIsRecent: Proposition[Tweet] =
    greaterOrEqual(5l).in[Tweet](_.timestamp)

  val allTweetsAreRecent: Proposition[History] =
    tweetIsRecent.forAll.in[History](_.tweets)

  def lessOrEqualTweetsThanInput(input: History): Proposition[History] =
    lessOrEqual(input.tweets.length).in[History](_.tweets.length)





  val cleanTweetsSpec1: Spec1[History, History] = Spec1[History, History] { input =>
    lessOrEqualTweetsThanInput(input)
  }

  val cleanTweetsSpec2: Spec1[History, History] = Spec1[History, History] { input =>
    lessOrEqualTweetsThanInput(input) && allTweetsAreRecent
  }

  val cleanTweetsSpec3: Spec1[History, History] = Spec1[History, History] { input =>
    (lessOrEqualTweetsThanInput(input) && allTweetsAreRecent) ==> tweetsAreOrdered
  }





  def cleanTweets1(history: History): History =
    history

  def cleanTweets2(history: History): History =
    history.copy(tweets = history.tweets.filter(_.timestamp >= 5l))

  def cleanTweets3(history: History): History =
    history.copy(tweets = history.tweets.filter(_.timestamp >= 5l).sorted)





  val history: History =
    History(
      List(
        Tweet("Tweet1", 7),
        Tweet("Tweet1", 5),
        Tweet("Tweet2", 2),
        Tweet("Tweet3", 6),
        Tweet("Tweet4", 1),
      )
    )

  test("test1") {
    val (message, complies) = cleanTweetsSpec1.check(history)(cleanTweets1)
    if(complies) succeed
    else fail(message)
  }

  ignore("test2") {
    val (message, complies) = cleanTweetsSpec2.check(history)(cleanTweets1)
    if(complies) succeed
    else fail(message)
  }

  ignore("test3") {
    val (message, complies) = cleanTweetsSpec2.check(history)(cleanTweets2)
    if(complies) succeed
    else fail(message)
  }

  ignore("test4") {
    val (message, complies) = cleanTweetsSpec3.check(history)(cleanTweets2)
    if(complies) succeed
    else fail(message)
  }

  ignore("test5") {
    val (message, complies) = cleanTweetsSpec3.check(history)(cleanTweets3)
    if(complies) succeed
    else fail(message)
  }
}
