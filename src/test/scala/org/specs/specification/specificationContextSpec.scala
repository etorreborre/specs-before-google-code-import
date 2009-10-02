package org.specs.specification
import org.specs._

class specificationContextSpec extends SpecificationWithJUnit {
  "A specification context" can {
    "be used to specify the actions before the specification" in {
      specificationMustDo("beforeSpec")
    }    
    "be used to specify the actions after the specification" in {
      specificationMustDo("afterSpec")
    }    
    "be used to specify the actions before a sus" in {
      specificationMustDo("beforeSus")
    }    
    "be used to specify the actions after a sus" in {
      specificationMustDo("afterSus")
    }    
    "be used to specify the actions before an example" in {
      specificationMustDo("beforeExample")
    }    
    "be used to specify the actions after an example" in {
      specificationMustDo("afterExample")
    }    
  }
  def specificationMustDo(s: String*) = {
    val spec = new TestedSpecification(ContextParams(s:_*))
    spec.reportSpecs
    spec.out.toList must_== s.toList.map(_+"_ok")
  }
}
import org.specs.io.mock._

case class ContextParams(values: String*) {
  def contains(s: String) = values.contains(s)
}
class TestedSpecification(params: ContextParams) extends Specification with MockOutput {
  val out = new scala.collection.mutable.ListBuffer[String]
  new SpecContext { 
    if (params.contains("beforeSpec")) beforeSpec(out.append("beforeSpec_ok")) 
    if (params.contains("afterSpec")) afterSpec(out.append("afterSpec_ok")) 
    if (params.contains("beforeSus")) beforeSus(out.append("beforeSus_ok")) 
    if (params.contains("afterSus")) afterSus(out.append("afterSus_ok")) 
    if (params.contains("beforeExample")) beforeExample(out.append("beforeExample_ok")) 
    if (params.contains("afterExample")) afterExample(out.append("afterExample_ok"))
    def output(a: =>Any) = out.append("around_ok")  
    if (params.contains("around")) aroundExpectations(output(_)) 

  } 
  "this system" should { "have one example" in { 1 must_== 1 } }    
}
