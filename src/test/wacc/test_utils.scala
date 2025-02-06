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

/* 
    Recursively trawls the nested folders and returns all files as a list
 */
def getTests(folder: File): List[File] = {
    if (folder.isDirectory())
        folder.listFiles().flatMap(getTests(_)).toList
    else
        List(folder)
}

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
