package com.arpanetus.jbdsl.lexer

import scala.util.parsing.input.Positional


sealed trait Token extends Positional


case class ASSIGNATION() extends Token
case class IMPORTER() extends Token
case class IMPORTINGFILE(str: String) extends Token
case class VARIABLE(str: String) extends Token

case class GENVAR(str: String) extends Token

trait VALUE extends Token {
  def value: Any
}
case class INTEGER(value: Int) extends VALUE
case class DOUBLE(value: Double) extends VALUE
case class STRING(value: String) extends VALUE



