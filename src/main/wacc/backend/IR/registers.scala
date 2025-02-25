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

enum DataSize {
    case BYTE, WORD, DWORD, QWORD
}
