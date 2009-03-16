package org.specs.util
import scala.tools.nsc.{Interpreter, Settings, InterpreterResults}
import java.io.{PrintWriter, StringWriter}

trait ScalaInterpreter {
  private val writer = new StringWriter
  private val interpreter = new Interpreter(new Settings, new PrintWriter(writer))
  private def clear(writer: StringWriter) = {
    val buf = writer.getBuffer
    buf.delete(0, buf.length)
  }
  def interpret(s: String) = {
    clear(writer)
    var result: InterpreterResults.Result = InterpreterResults.Success
    var constructedLine: Option[String] = None
    val output = new java.lang.StringBuffer
    s.split("\n") foreach { line =>
      constructedLine = constructedLine match {
        case None => {
          if (line.trim.length > 0) Some(line)
          else None
        }
        case Some(cl) => Some(cl + line + "\n")
      }

      result = constructedLine match {
        case Some(cl) => {
          val res = interpreter.interpret(cl)
          res
        }
        case None => InterpreterResults.Incomplete
      }
      result match {
        case InterpreterResults.Error => ()
        case InterpreterResults.Incomplete => // no action necessary
        case InterpreterResults.Success => constructedLine = None
      }
      output.append("\n" + writer.toString)
      clear(writer)
    }
    if (output.toString.contains("error") || output.toString.contains("at RequestResult")) output.toString.trim
    else output.toString.split("\n").last.trim
  }
}
object ScalaInterpreter extends ScalaInterpreter