package wacc.test

import java.io.File
import scala.io.Source

final val WACC_EXAMPLES_PATH = "./src/test/wacc/wacc_examples/"
final val CONFIG_PATH        = "./src/test/wacc/tests.properties"
final val FLAG_SPLIT_INDEX   = 2

/* 
    PRE: wacc-examples/folder_path exist and is a valid folder
    PRE: the function is called from the root of the repository
    Returns a list of file contents
 */
def getTests(folder_path: String): List[File] = {
    val directory = new File(WACC_EXAMPLES_PATH + folder_path)
    assert(directory.exists() && directory.isDirectory())
    getTests(directory)
}

/* Recursively crawls the nested folders and returns all files as a list. */
def getTests(folder: File): List[File] = {
    if (folder.isDirectory())
        folder.listFiles().flatMap(getTests(_)).toList
    else
        List(folder)
}

/* Fetches flags from the customizable test properties. */
def getProperties(): Map[String, String] = {
    val source = Source.fromFile(CONFIG_PATH)
    try {
        source.getLines()
        .filter(line => line.trim.nonEmpty && !line.trim.startsWith("#"))
        .map { line =>
            val parts = line.split("=", FLAG_SPLIT_INDEX)
            (parts.head.trim, parts.tail.head.trim)
        }
        .toMap
    } finally {
        source.close()
    }
}

def parseExample(file: File): (Option[String], Option[String], Option[Int]) = {
    val lines = Source.fromFile(file).getLines().toList.takeWhile(line => !line.trim.startsWith("# Program:"))
    var input: Option[String] = None
    var outputLines: List[String] = Nil
    var exit: Option[Int] = None
    var currentSection: Option[String] = None

    lines.foreach { line =>
        line.trim match {
        case s if s.startsWith("# Input:") =>
            currentSection = Some("input")
            val content = s.stripPrefix("# Input:").trim
            if (content.nonEmpty) input = Some(content)
        case s if s.startsWith("# Output:") =>
            currentSection = Some("output")
            val content = s.stripPrefix("# Output:").trim
            if (content.nonEmpty) outputLines = outputLines :+ content
        case s if s.startsWith("# Exit:") =>
            currentSection = Some("exit")
            val content = s.stripPrefix("# Exit:").trim
            if (content.nonEmpty)
                try { exit = Some(content.toInt) } catch { case _: NumberFormatException => }
        case "#" =>
            currentSection = None
        case s if s.startsWith("#") =>
            currentSection match {
                case Some("input") if input.isEmpty =>
                input = Some(s.stripPrefix("#").trim)
                case Some("output") =>
                outputLines = outputLines :+ s.stripPrefix("#").trim
                case Some("exit") if exit.isEmpty =>
                try { exit = Some(s.stripPrefix("#").trim.toInt) } catch { case _: NumberFormatException => }
                case _ =>
            }
        case _ =>
        }
    }
    
    val output = if (outputLines.isEmpty) None else Some(outputLines.mkString("\n"))
    (input, output, exit)
}
