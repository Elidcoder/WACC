package wacc

import parsley.Parsley
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.token.errors.*

import wacc.ast.Ident
import parsley.token.Unicode

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
            escapeSequences = EscapeDesc.plain.copy(
                literals = Set('\"', '\'', '\\'),
                mapping = Map(
                    "0" -> 0x00,
                    "b" -> 0x08,
                    "t" -> 0x09,
                    "n" -> 0x0a,
                    "f" -> 0x0c,
                    "r" -> 0x0d,
                ),
            ),
            graphicCharacter = Unicode((x: Int) => !Set('\"', '\'', '\\').contains(x) && x >= ' '.toInt)
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("if", "true", "false", "len", "ord", "chr", "int", "bool", "char",
            "string", "pair", "null", "begin", "end", "skip", "read", "free", "return", "exit", "print", "println", "if",
            "then", "else", "fi", "while", "do", "done", "call", "fst", "snd", "newpair")
        )
    )

    private val errConfig = new ErrorConfig {
    }

    private val lexer = new Lexer(desc, errConfig)

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
