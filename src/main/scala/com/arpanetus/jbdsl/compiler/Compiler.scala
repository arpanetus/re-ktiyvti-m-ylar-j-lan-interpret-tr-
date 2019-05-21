package com.arpanetus.jbdsl.compiler

import com.arpanetus.jbdsl.lexer.Lexer
import com.arpanetus.jbdsl.parser.{AST, Parser}

object Compiler {
  def apply(code: String): Either[CompilationError, AST] = {
    for {
      tokens <- Lexer(code).right
      ast <- Parser(tokens).right
    } yield ast
  }
}
