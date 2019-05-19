package com.arpanetus.jbdsl.lexer

import com.arpanetus.jbdsl.compiler.{LexerError, Location}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {

  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \n\t\r\f]+".r

  def processGenVars(tokens: List[Token]): List[Token] = {
    var finalTokens = tokens
    for(index <- finalTokens.indices){
      if(finalTokens(index) == IMPORTER()) {
        finalTokens = finalTokens.slice(0,index+1):::processImport(finalTokens.slice(index+1, finalTokens.length))
      }
      if(finalTokens(index) == ASSIGNATION()) {
        finalTokens =
            processAssign(finalTokens.slice(0,index)):::
            List[Token](ASSIGNATION()):::
            processAssign(finalTokens.slice(index+1,finalTokens.length),isLeft = false)
      }
    }
    finalTokens
  }

  def processImport(tokens: List[Token]): List[Token] = {
    val token = tokens.head
    token match {
      case genVar: GENVAR =>
        IMPORTINGFILE(genVar.str) :: tokens.tail
      case _ =>
        tokens
    }
  }

  def processAssign(tokens: List[Token], isLeft:Boolean = true):List[Token] = {
      if(isLeft) {
        val token = tokens.last
        token match {
          case genVar: GENVAR =>
            tokens.dropRight(1) ::: List[Token](VARIABLE(genVar.str))
          case _ => tokens
        }
      } else {
        val token = tokens.head
        token match {
          case genVar: GENVAR =>
            VARIABLE(genVar.str) :: tokens.tail
          case _ => tokens
        }
      }
    }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(
      importer | genVar | assignation | value
    ))^^ { rawTokens => processGenVars(rawTokens)
    }
  }

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def genVar : Parser[GENVAR] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => GENVAR(str) }
  }

//  def toNum(str: String):Int = {
//    case str:Int =>
//    print()
//      str.toInt

//    case str:Double => str.toDouble
//  }

  def value: Parser[VALUE] = positioned {
    "[0-9_]+(\\.[0-9_]*)?".r ^^ { str =>
//      val content = str.substring(1, str.length - 1)
      val num = Integer.parseInt(str)
//      num match {
//        case _ : throws
//      }
      NUMBER(num)
    }
//    "\"[a-zA-Z_][a-zA-Z0-9_]*\"".r ^^ { str =>
//      val content = str.substring(1, str.length - 1)
//      STRING(content)
//    }
  }



  def importer : Parser[IMPORTER]       = { "import" ^^ (_ => IMPORTER()) }

  def assignation : Parser[ASSIGNATION] = { "="      ^^ (_ => ASSIGNATION()) }

}

