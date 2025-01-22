package wacc

import parsley.Parsley
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*

import wacc.ast.Ident

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(_.isLetter),
            identifierLetter = Basic((c: Char) => c.isLetterOrDigit || (c =='_')),
        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#"
        ),
        textDesc = TextDesc.plain.copy(
            escapeSequences = EscapeDesc.haskell
        )
    )
    private val lexer = new Lexer(desc)

    val implicits = lexer.lexeme.symbol.implicits

    val integer = lexer.lexeme.integer.decimal32
    val ident: Parsley[Ident] = Ident(lexer.lexeme.names.identifier)
    val asciiChar = lexer.lexeme.character.ascii
    val asciiString = lexer.lexeme.string.ascii

    def parens[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.parens(p)
    def brackets[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.brackets(p)
    def commaSep[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.commaSep(p)
    def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.semiSep1(p)
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
