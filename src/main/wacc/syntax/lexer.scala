package wacc.syntax

import wacc.ast.{Ident, Typeless}
import wacc.error.SyntaxErrBuilder

import parsley.Parsley
import parsley.token.{Basic, Lexer, Unicode}
import parsley.errors.tokenextractors.LexToken
import parsley.token.errors.{ErrorConfig, Label, LabelAndReason, LabelConfig, LabelWithExplainConfig}
import parsley.token.descriptions.{EscapeDesc, LexicalDesc, NameDesc, SpaceDesc, SymbolDesc, TextDesc}

object lexer {
    /* Error message taken from the WACC Reference Compiler. */
    val ESCAPE_ERR_MSG = "valid escape sequences are \\0, \\n, \\t, \\b, \\f, \\r, \\\", \\\' or \\\\"
    
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic((c: Char) => c.isLetter || (c == '_')),
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
            hardKeywords = Set("if", "true", "false", "int", "bool", "char",
            "string", "pair", "null", "begin", "end", "skip", "read", "free", "return", "exit", "print", "println", "if",
            "then", "else", "fi", "while", "do", "done", "call", "fst", "snd", "newpair"),
            hardOperators = Set("*", "/", "%", "+", "-", ">", ">=", "<", "<=", "==", "!=", "&&", "||", "=",
                "!", "len", "ord", "chr")
        )
    )

    private val errConfig = new ErrorConfig {
        override def labelCharAsciiEnd: LabelConfig = Label("end of character literal")
        override def labelStringAsciiEnd(a: Boolean, b: Boolean): LabelConfig = Label("end of string literal")
        override def labelEscapeEnd:LabelWithExplainConfig = LabelAndReason(reason = ESCAPE_ERR_MSG, label = "end of escape sequence")
    }

    private val lexer = new Lexer(desc, errConfig)

    val implicits   = lexer.lexeme.symbol.implicits
    val integer     = lexer.lexeme.integer.decimal32
    val asciiChar   = lexer.lexeme.character.ascii
    val asciiString = lexer.lexeme.string.ascii

    val ident: Parsley[Ident[String, Typeless]] = Ident(lexer.lexeme.names.identifier)

    def fully[A](p: Parsley[A]): Parsley[A]          = lexer.fully(p)
    def parens[A](p: => Parsley[A]): Parsley[A]      = lexer.lexeme.parens(p)
    def brackets[A](p: => Parsley[A]): Parsley[A]    = lexer.lexeme.brackets(p)
    def commaSep[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.commaSep(p)
    def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.semiSep1(p)

    val lexErrBuilder = new SyntaxErrBuilder with LexToken {
        override def tokens: Seq[Parsley[String]] = 
            desc.symbolDesc.hardKeywords.map(kw => lexer.nonlexeme.symbol(kw).as(s"keyword $kw")).toSeq ++ 
            desc.symbolDesc.hardOperators.map(kw => lexer.nonlexeme.symbol(kw).as(s"operator $kw")).toSeq ++ Seq(
                lexer.nonlexeme.integer.decimal32.map("number ".+),
                lexer.nonlexeme.character.ascii.map("character ".+),
                lexer.nonlexeme.string.ascii.map("string ".+),
                lexer.nonlexeme.names.identifier.map("identifier ".+)
            )
    }
}
