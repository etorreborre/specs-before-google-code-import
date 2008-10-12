package org.specs.runner

object teamCityRunnerSpec extends teamCityRunnerRules { "TeamCity Runner Specification" ->> <wiki>
 
h3. Introduction

The TeamCity runner is a Specification runner designed to create TeamCity formatted messages to the standard output when running specifications.
Those messages are then picked up by TeamCity and used to report the status of the current build
  
A specification for the TeamCity output messages can be found "here":http://www.jetbrains.net/confluence/display/TCD3/Build+Script+Interaction+with+TeamCity#BuildScriptInteractionwithTeamCity-testReporting.

h3. Notifying specifications and sus as Test suites
   
When executed, <ex>the TeamCity runner should notify the start of a specification</ex> with:

{"##teamcity[testSuiteStarted name='specification name']".as(message) >@}{messageMustBeCreated}
                      
<ex>The end of the specification should also be reported</ex> as:

{"##teamcity[testSuiteFinished name='specification name']".as(message) >@}{messageMustBeCreated}
        
<ex>The systems under specification of a specification should be reported as test suite nested inside the specification suite</ex>:

{List("##teamcity[testSuiteStarted name='specification name']",
      "##teamcity[testSuiteStarted name='sus1 description']",
      "##teamcity[testSuiteFinished name='sus1 description']",
      "##teamcity[testSuiteFinished name='specification name']").as(messages).mkString("\n") >@}{messagesMustBeCreated}

<ex>The examples of system under specification should be reported as testStarted and testFinished inside the start and finished messages for the sus</ex>:

{List("##teamcity[testSuiteStarted name='specification name']",
      "##teamcity[testSuiteStarted name='sus1 description']",
      "##teamcity[testStarted name='specification name.good test']",
      "##teamcity[testFinished name='specification name.good test']",
      "##teamcity[testSuiteFinished name='sus1 description']",
      "##teamcity[testSuiteFinished name='specification name']").as(messages).mkString("\n") >@}{messagesMustBeCreated}

  </wiki>
}
class teamCityRunnerTest extends JUnit4(teamCityRunnerSpec) with Html {
  override def outputDir = "target"
}


