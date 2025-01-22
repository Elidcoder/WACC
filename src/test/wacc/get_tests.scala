package wacc.test

import java.io.File

final val PATH_TO_WACC_EXAMPLES = "./src/test/wacc/wacc_examples/"

/* 
    PRE: wacc-examples/folder_path exist and is a valid folder
    PRE: the function is called from the root of the repository
    Returns a list of file contents
 */
def get_tests(folder_path: String): List[File] = {
    val directory = new File(PATH_TO_WACC_EXAMPLES + folder_path)
    assert(directory.exists() && directory.isDirectory())
    get_tests(directory)
}

/* 
    Recursively trawls the nested folders and returns all files as a list
 */
def get_tests(folder: File): List[File] = {
    if (folder.isDirectory())
        folder.listFiles().flatMap(get_tests(_)).toList
    else
        List(folder)
}
