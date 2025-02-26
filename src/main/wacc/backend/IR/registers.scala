package wacc.backend.ir

sealed trait Register

/* Caller */
case object rax extends Register

case object rcx extends Register
case object rdx extends Register
case object rsi extends Register
case object rdi extends Register

case object r8 extends Register
case object r9 extends Register
case object r10 extends Register
case object r11 extends Register

/* Callee */
case object rbx extends Register

case object rsp extends Register
case object rbp extends Register

case object r12 extends Register
case object r13 extends Register
case object r14 extends Register
case object r15 extends Register

sealed trait DataSize {
    val bytes: Int
}

case class BYTE() extends DataSize {
    override val bytes = 1
}
case class WORD() extends DataSize {
    override val bytes = 2
}
case class DWORD() extends DataSize {
    override val bytes = 4
}
case class QWORD() extends DataSize {
    override val bytes = 8
}

sealed trait DataSizeExtractor[S <: DataSize] {
    def size: DataSize
}
implicit val byteExtractor: DataSizeExtractor[BYTE] = new DataSizeExtractor[BYTE] {
    def size: DataSize = BYTE()
}
implicit val wordExtractor: DataSizeExtractor[WORD] = new DataSizeExtractor[WORD] {
    def size: DataSize = WORD()
}
implicit val dwordExtractor: DataSizeExtractor[DWORD] = new DataSizeExtractor[DWORD] {
    def size: DataSize = DWORD()
}
implicit val qwordExtractor: DataSizeExtractor[QWORD] = new DataSizeExtractor[QWORD] {
    def size: DataSize = QWORD()
}
def extractDataSize[S <: DataSize](op: Operand[S])(implicit extractor: DataSizeExtractor[S]): DataSize = {
    extractor.size
}
