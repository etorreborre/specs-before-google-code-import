package org.specs.runner

object teamCityRunnerSpec extends teamCityRunnerRules("TeamCity Runner") { "TeamCity Runner Specification" ->> <wiki>
 
h3. Introduction

The TeamCity runner is a Specification runner designed to create TeamCity formatted messages to the standard output when running specifications.

A specification for the TeamCity output messages can be found "here":http://www.jetbrains.net/confluence/display/TCD3/Build+Script+Interaction+with+TeamCity#BuildScriptInteractionwithTeamCity-testReporting.

h3. Notifying specifications and sus as Test suites
   
When executed, <ex>the TeamCity runner should notify the start of a specification</ex> with:

@##teamcity[testSuiteStarted name=`specification name']@{specificationStart}
                      
<ex>The end of the specification should also be reported</ex>:

@##teamcity[testSuiteFinished name=`specification name']@{specificationEnd}
                                   
  </wiki>
}
class teamCityRunnerTest extends HtmlRunner(teamCityRunnerSpec, "target") with JUnit 


