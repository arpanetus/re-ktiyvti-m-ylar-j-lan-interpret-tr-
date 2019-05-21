package com.arpanetus.jbdsl.parser

import com.arpanetus.jbdsl.compiler.{Location, ParserError}
import com.arpanetus.jbdsl.lexer.{ASSIGNATION, DOUBLE, IMPORTER, IMPORTINGFILE, INTEGER, STRING, Token, VALUE, VARIABLE}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Parser extends Parsers{
  override type Elem = Token

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }


  def apply(tokens: Seq[Token]): Either[ParserError, AST]= {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[AST] = positioned {
    phrase(block)
  }

  def block: Parser[AST] = positioned {
    rep(statement) ^^ { case stmtList => stmtList reduceLeft Start }
  }

  def statement: Parser[AST] = positioned {
    importer | variable
  }

  def importer: Parser[Import] = positioned {
    (IMPORTER() ~ importingFile) ^^ {case _ ~ IMPORTINGFILE(filename) => Import(filename)}
  }

  def importingFile: Parser[IMPORTINGFILE] = positioned {
    accept("importing file", { case id @ IMPORTINGFILE(_) => id })
  }

  def variable: Parser[Variable] = positioned {
    val invariable = accept("variable", { case id @ VARIABLE(_) => id})
//    val invariable = accept("variable", { case id @ VARIABLE(_) => id})
    (invariable ~ ASSIGNATION() ~ value) ^^ {case VARIABLE(name) ~ _ ~ value => Variable(name, value) }
  }

  def value: Parser[VALUE] = positioned {
    accept("value", {
      case id @ INTEGER(_) => id
      case id @ DOUBLE(_) => id
      case id @ STRING(_) => id
    })
  }


}
