package org.specs.specification
object literateSpec extends LiterateSpecRules { <wiki> 
 
h3. Description                                      
  
A literate specification is a text embedded in xml tags.
  
h3. Code inside the text description
    
A literate specification can execute code by enclosing it in accolades: {"{ (1 + 1) }"}.
It is possible to silence the code being executed by using several functions, for example the @shh@ function:
   {"{ (1 + 1).shh }"}{ (1 + 1).shh }
or the {"@<|@"} operator at the end of any expression: {"{ 1 + 1 <| }"}{ 1 + 1 <| }
 
h3. Examples
  
h4. Named examples
  
<ex>An assertion can be included in a literate specification as part of an example</ex>, like this:
 @"1 must be 1"@ @{"in {1 must_== 1}"}@ {exampleOk}
 
h4. Anonymous examples
    
<ex>The description from an example can also come from part of the description enclosed with an @ex@ xml tag</ex>, like that: 

* {"@<ex>1 must be 1</ex> { 1 must_== 1  }@"}{taggedExample}

h4. Tags

<ex>It is possible to specify tags on the examples by using the @tag@ or @tags@ attributes.</ex>{taggedExamples} 
Tag names must be a comma-separated list of values like that: 

* {"@<ex tags=@"}@"group1, group2"@{"@/>@"}

h4. Not implemented
    
It is possible to mark an example as not implemented yet:

* {"@<ex> this example is not yet implemented </ex> { notImplemented }@"} 
    
<ex>In that case, the example will be added but marked as skipped</ex>{notImplementedExample}
  
h3. Formatting
    
The format of the description can be indicated with different tags. The tags are then used by Runners to know how to interpret
the text of the description.
    
* <ex>The text format @"text"@ indicates text interpreted as simple text</ex>{isText}
* <ex>The wiki format @"wiki"@ indicates text interpreted as a markup language</ex>{isWiki}
* <ex>The html format @"html"@ indicates text interpreted as a html</ex>{isHtml}
  
</wiki> isSus
}
