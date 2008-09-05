package org.specs.samples
import org.specs.runner._

object runners {
  object spec extends Specification
  object spec2 extends Specification
  // those are different possibilities of runners combinations for specs
  // 1. run a spec in the console -- this is actually redundant as specification are automatically runnable with
  //    an embedded ConsoleRunner
  object runner1 extends ConsoleRunner(spec)
  // 2. run a spec with JUnit4. runner2 needs to be a class
  class runner2 extends JUnit4(spec)
  // 3. run a spec as an xml file (created in the 'target' directory)
  object runner3 extends XmlRunner(spec, "target")
  // 4. run a spec as an html file (created in the 'target' directory)
  object runner4 extends HtmlRunner(spec, "target")
  // 5. run a spec as an html file and in the console	
  object runner5 extends HtmlRunner(spec, "target") with Console 
  // 6. run a spec as an xml + html file + in the console
  object runner6 extends Runner(spec :: spec2, runner3 :: runner4) with Console
  // 8. run 2 specs as an xml + html file + in the console
  object runner8 extends Runner(spec, spec2) with Console
  // 9. run all specifications found on a given path as an xml + html file + in the console
  object runner9 extends Runner(SpecsFinder("src/test", ".*Spec.*"), new XmlRunner :: new HtmlRunner) with Console with JUnit 
}
