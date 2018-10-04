import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Parser extends App {
  def parse(fileName: String) = {
    val input = Source.fromFile(fileName).mkString
    val res = FlechaParser(input).parse
    val file = new File(s"$fileName.AST")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(res.toString)
    bw.close()
    res
  }
}
