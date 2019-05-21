import com.arpanetus.jbdsl.compiler.Compiler
import com.arpanetus.jbdsl.lexer.Lexer

object Main {

  def main(args: Array[String]): Unit = {
    val code =
      """
        |import file1
        |a = 3
        |c = "asdasd"
      """.stripMargin
//      d = a
    println(Compiler(code))

  }
}
