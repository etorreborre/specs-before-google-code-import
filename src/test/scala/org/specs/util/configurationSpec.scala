package org.specs.util

class configurationSpec extends spex.Specification {
  "A configuration object" should {
    "try to find the default configuration class, named DefaultConfiguration, in the default package" in {
      Configuration.getDefaultConfiguration must not be none
    }
    "try to find first the user configuration class, named configuration$ (an object), in the default package" +
    "defaulting to the default configuration if not found" in {
      Configuration.getUserConfiguration must not be none
      Configuration.getUserConfiguration.get must haveClass[DefaultConfiguration]
    }
    "try to find a configuration class, with a given name defaulting to the user configuration" +
    "then defaulting to the DefaultConfiguration if the user config is not found"  in {
      Configuration.getConfiguration("missing") must not be none
      Configuration.getConfiguration("missing").get must haveClass[DefaultConfiguration]
    }
    "try to find a configuration class, with a given name defaulting to the user configuration" in {
      Configuration.getConfiguration("org.specs.util.TestConfiguration") must not be none
      Configuration.getConfiguration("org.specs.util.TestConfiguration").get must haveClass[TestConfiguration]
    }
  }
}
class TestConfiguration extends Configuration
