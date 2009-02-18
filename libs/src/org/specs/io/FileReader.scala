package org.specs.io;
import java.io._

/**
 * The FileReader trait provides functions to read files
 */
trait FileReader {

  /**
   * reads the content of a file
   * @param path the path of the file to read
   * @return the content of the file at path
   */
  def readFile(path: String): String = {
    def appendLines(result: StringBuffer, in: BufferedReader, line: String): Unit = {
      if (line != null) {
        result.append(line)
        result.append("\n")
        appendLines(result, in, in.readLine)
      }
    }
    val in = new BufferedReader(new java.io.FileReader(path));
    val result = new StringBuffer
    appendLines(result, in, in.readLine)
    in.close();
    result.toString
  }
}
