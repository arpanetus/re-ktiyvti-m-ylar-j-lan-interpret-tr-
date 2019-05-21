package com.arpanetus.jbdsl.parser

import scala.util.parsing.input.Positional

sealed trait AST extends Positional

case class Start(step1: AST, step2: AST) extends AST
case class Import(str: String) extends AST
case class Variable(name: String, value: Any) extends AST
