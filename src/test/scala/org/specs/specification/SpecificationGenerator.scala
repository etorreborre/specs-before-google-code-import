package org.specs.specification
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.specs.matcher._

trait SpecificationGenerator { self: Specification =>
  object spec extends Specification("generated spec")
  
  def genAssertion = for(value <- choose(0, 4)) yield { () => 
    value match {
      case 0 | 1 => spec.theValue(value) must_== 1 
      case 2     => spec.skip("this is a skipped example")
      case 3     => error("error in the example")
    } 
  }

  def genExample(sut: Sut) = for (a <- genAssertion) yield {
    val newExample = new Example("generated example", sut)
    sut.addExample(newExample) 
    newExample.in { a() }
  }
  
  def genSizedSut(size: Int): Gen[Sut] = genSizedSut(size, spec)
  def genSizedSut(size: Int, s: Specification): Gen[Sut] = { 
    val sut = new Sut("sut with " + size + " max examples", s)
    for { n <- choose(0, size) 
          e <- vectorOf(n, genExample(sut))
    } yield sut 
  }
  def genSut = sized(size => genSizedSut(size))
  def genSizedSpec(size: Int): Gen[Specification] = {
    val generatedSpec = new Specification("spec with " + size + " max sut") {}
    for { sutsNb <- choose(0, size)
          suts <- vectorOf(sutsNb, genSizedSut(size, generatedSpec))
          subSpecsNb <- choose(0, 1)
          subSpecs <- vectorOf(subSpecsNb, genSizedSpec(size))
    } yield {
      subSpecs.foreach(generatedSpec.include(_))
      generatedSpec
    } 
  }
  def genSpec = sized(size => genSizedSpec(size))
  
  implicit val arbitrarySut: Arbitrary[Sut] = Arbitrary { genSut }
  implicit val arbitrarySpec: Arbitrary[Specification] = Arbitrary { genSpec }
}
object generatorSpec extends Specification with SpecificationGenerator with Scalacheck {
  "a sut" should {
    "have a number of error + failure + successes + skipped == the number of examples" in {
      property {(sut: Sut) => 
        (sut.failures.size + sut.errors.size + sut.skipped.size + sut.successes.size) must be_==(sut.examples.size)
      } must pass(set(maxSize -> 5))
    } 
  }
  "a specification" should {
    "have a number of error + failure + successes + skipped == the number of examples" in {
      property {(spec: Specification) => 
        (spec.failures.size + spec.errors.size + spec.skipped.size + spec.successes.size) must be_==(spec.examples.size)
      } must pass(set(maxSize -> 5))
    } 
  }
}
