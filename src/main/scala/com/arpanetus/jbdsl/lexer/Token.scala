package com.arpanetus.jbdsl.lexer

import scala.util.parsing.input.Positional


sealed trait Token extends Positional


case class ASSIGNATION() extends Token
case class IMPORTER() extends Token
case class IMPORTINGFILE(str: String) extends Token
case class VARIABLE(str: String) extends Token

case class GENVAR(str: String) extends Token

sealed trait VALUE extends Token
case class NUMBER(num: Integer) extends VALUE
case class STRING(str: String) extends VALUE



