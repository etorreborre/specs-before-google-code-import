package org.specs.matcher
import org.specs.runner._
import org.specs._
import org.scalacheck.Gen._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

class mapMatchersTest extends Runner(mapMatchersUnit) with JUnit with Console
object mapMatchersUnit extends MatchersSpecification with PartialFunctionGen with Scalacheck {
  "Map matchers" should {
    "not evaluate the expressions twice: haveKey" in {
      val map: Iterable[(String, Any)] = Map("" -> 1)
      haveKey("") must evalOnce(exp(map))
    }
    "not evaluate the expressions twice: haveValue" in {
      val map2: Iterable[(Any, Int)] = Map("" -> 1)
      haveValue(1) must evalOnce(exp(map2))
    }
    "not evaluate the expressions twice: havePair" in {
      val map3: Iterable[(String, Int)] = Map("" -> 1)
      havePair("" ->1) must evalOnce(exp(map3))
    }
  }
  "Partial Functions matchers" should {
    val f = new PartialFunction[Int, String] {
      def isDefinedAt(i: Int) = i % 2 == 0
      def apply(i: Int) = (i*2).toString
    }
    "provide a beDefinedAt matcher checking if a PartialFunction is defined at specific values" in {
      val beDefinedAtAValue = (f: PartialFunction[Int, String], list: List[Int]) => {
        beDefinedAt(list: _*)(f) match {
          case (true, _, _) => list forall {x:Int => f.isDefinedAt(x)}
          case (false, _, ko) => { list forall {x:Int => ko.contains(x.toString) || f.isDefinedAt(x)} }
        }
      }
      property(beDefinedAtAValue) must pass
    }
    "provide a beDefinedBy matcher checking if a PartialFunction is defined at specific values and returns appropriate results" in {
      val beDefinedByValueAndResult = (f: PartialFunction[Int, String], map: Map[Int, String]) => {
        beDefinedBy(map.toList : _*)(f) match {
          case (true, _, _) => map forall {x:(Int, String) => f.isDefinedAt(x._1) && f(x._1) == x._2}
          case (false, _, ko) => { map forall {x:(Int, String) => ko.contains(x.toString) || (f.isDefinedAt(x._1) && f(x._1) == x._2)} }
        }
      }
      property(beDefinedByValueAndResult) must pass
    }
  }
}
trait PartialFunctionGen {
  implicit def listInt: Arbitrary[List[Int]] = Arbitrary[List[Int]] {
    for {length <- choose(1, 4)
         l <- vectorOf(length, choose(1, 4))
    } yield l.toList
  }
  implicit def genPartialFunction: Arbitrary[PartialFunction[Int, String]] = Arbitrary[PartialFunction[Int, String]] {
    for {length <- choose(0, 4)
         keys <- vectorOf(length, choose(1, 4))
         values <- vectorOf(length, Arbitrary.arbitrary[String])
    } yield Map(keys.toList zip values.toList map {kv => kv._1 -> kv._2} : _*)
  }
  implicit def genMap: Arbitrary[Map[Int, String]] = Arbitrary[Map[Int, String]] {
    for {length <- choose(0, 4)
         keys <- vectorOf(length, choose(1, 4))
         values <- vectorOf(length, Arbitrary.arbitrary[String])
    } yield Map(keys.toList zip values.toList map {kv => kv._1 -> kv._2} : _*)
  }
}
