package org.specs.runner
import org.specs.util.CommandLineOptions

/**
 * This trait describe the command-line options for the Reporter trait
 */
/*
private[specs] trait ReporterOptions extends CommandLineOptions {
  val usage = "usage scala -cp <classpath> package.mySpecificationObject [options]\n" +
              "      scala -cp <classpath> run package.mySpecificationClass [options]\n"   
  val optionsSummary = """
    [-h|--help]
    [-config|--configuration]
    [-ns|--nostacktrace]
    [-nostats|--nostatistics]
    [-finalstats|--finalstatistics]
    [-xonly | --failedonly]
    [[-acc | --accept] tag1,tag2,...] [[-rej | --reject] tag1,tag2,...]
    [-sus | --system]
    [-ex | --example]
    [-plan | --planOnly]
    [-c | --color]""".stripMargin
  val optionsDescription = """
    -h, --help                      print this message and doesn't execute the specification
    -config, --configuration        class name of an object extending the org.specs.util.Configuration trait
    -ns, --nostacktrace             remove the stacktraces from the reporting
    -nostats, --nostatistics        remove the statistics from the reporting
    -finalstats, --finalstatistics  print the final statistics only
    -xonly, --failedonly            report only failures and errors
    -acc, --accept tags             accept only the specified tags (comma-separated names)
    -rej, --reject tags             reject the specified tags (comma-separated names)
    -sus, --system                  only the systems under specifications matching this regular expression will be executed
    -ex, --example                  only the examples matching this regular expression will be executed
    -plan, --planOnly               only display the sus and first level descriptions without executing the examples
    -c, --color                     report with color""".stripMargin
}

*/