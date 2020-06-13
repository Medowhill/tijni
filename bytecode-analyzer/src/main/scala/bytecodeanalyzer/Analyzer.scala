package info.hjaem.bytecodeanalyzer

import org.objectweb.asm.tree._

object Analyzer {

  def run(pg: Program): Unit = {
    val (varNum, instrs) = pg.mainMethod
    var ctx = Ctx(
      Frame(Vector.fill(varNum)(Undef), Nil, 0, instrs) :: Nil,
      Map(),
      pg
    )
    // println(ctx)
    while (!ctx.terminated) {
      ctx = step(ctx)
      // println(ctx)
    }
  }

  def step(ctx: Ctx): Ctx = {
    val nctx = ctx.instruction match {
      case Label(_) => ctx

      case Load(i) => ctx.load(i)
      case Store(i) => ctx.store(i)

      case Getstatic(_) => ctx.getSysout
      case Getfield(f) => ctx.get(f)
      case Putfield(f) => ctx.put(f)

      case Invokevirtual(m, d) => ctx.invoke(m, d)
      case Return(_) => ctx.ret
      case Lreturn(_) => ctx.lret

      case New(_) => ctx.newObj.next.next
      case Lconst(l) => ctx.const(l)
      case Pop(_) => ctx.pop

      case Ladd(_) => ctx.add
      case Lsub(_) => ctx.sub
      case Lcmp(_) => ctx.cmp

      case Ifeq(l) => ctx.ifeq(l)
      case Ifne(l) => ctx.ifne(l)
      case Goto(l) => ctx.go(l)

      case in =>
        sys.error(s"Unknown opcode (${Program.insnToString(in)})")
    }
    if (nctx.terminated) nctx else nctx.next
  }

  sealed trait Value {
    override lazy val toString: String = this match {
      case NumV(l) => l.toString
      case ObjV(a) => s"@$a"
      case Sysout => s"sys.out"
      case Undef => "undef"
    }
  }
  case class NumV(value: Long) extends Value
  case class ObjV(addr: Addr) extends Value
  case object Undef extends Value
  case object Sysout extends Value

  type Addr = Int
  type Heap = Map[Addr, Map[String, Value]]

  case class Frame(
    variables: Vector[Value],
    stack: List[Value],
    pc: Int,
    instructions: Vector[AbstractInsnNode],
  ) {
    lazy val head: Value = stack.head
    lazy val tail: List[Value] = stack.tail
    def instruction: AbstractInsnNode = instructions(pc)
    def next: Frame = copy(pc = pc + 1)
    def load(i: Int): Frame = copy(stack = variables(i) :: stack)
    def store(i: Int): Frame =
      copy(variables = variables.updated(i, head), stack = tail)
    def get(f: String, heap: Heap): Frame = {
      val ObjV(a) = head
      copy(stack = heap(a)(f) :: tail)
    }
    def put(f: String, heap: Heap): (Frame, Heap) = {
      val ObjV(a) :: tail2 = tail
      val nheap = heap + (a -> (heap(a) + (f -> head)))
      (copy(stack = tail2), nheap)
    }
    def cmp: Frame = {
      val NumV(v1) = head
      val NumV(v0) :: tail2 = tail
      val v = NumV(
        if (v0 > v1) 1
        else if (v0 == v1) 0
        else -1
      )
      copy(stack = v :: tail2)
    }
    def ifeq(l: LabelNode): Frame = {
      val NumV(v) = head
      if (v == 0)
        copy(stack = tail, pc = instructions.indexOf(l) - 1)
      else
        copy(stack = tail)
    }
    def ifne(l: LabelNode): Frame = {
      val NumV(v) = head
      if (v != 0)
        copy(stack = tail, pc = instructions.indexOf(l) - 1)
      else
        copy(stack = tail)
    }
    def go(l: LabelNode): Frame =
      copy(pc = instructions.indexOf(l) - 1)
    def add: Frame = {
      val NumV(v1) = head
      val NumV(v0) :: tail2 = tail
      copy(stack = NumV(v0 + v1) :: tail2)
    }
    def sub: Frame = {
      val NumV(v1) = head
      val NumV(v0) :: tail2 = tail
      copy(stack = NumV(v0 - v1) :: tail2)
    }
    def push(v: Value): Frame = copy(stack = v :: stack)
    def pop: Frame = copy(stack = tail)
    def popN(n: Int): (List[Value], Frame) =
      (stack.take(n), copy(stack = stack.drop(n)))

    override lazy val toString: String = {
      val v = variables.mkString(", ")
      val s = stack.mkString(", ")
      val i = instructions.zipWithIndex.map{
        case (i, ind) => s"${ind}: ${Program.insnToString(i)}"
      }.mkString("\n")
      s"[Variables]\n$v\n\n[Stack]\n$s\n\nPC=$pc\n\n[Instructions]\n$i"
    }
  }

  case class Ctx(
    callStack: List[Frame],
    heap: Heap,
    program: Program,
  ) {
    lazy val head: Frame = callStack.head
    lazy val tail: List[Frame] = callStack.tail
    def terminated: Boolean = callStack.isEmpty
    def instruction: AbstractInsnNode = head.instruction
    def next: Ctx =
      copy(callStack = head.next :: tail)
    def load(i: Int): Ctx =
      copy(callStack = head.load(i) :: tail)
    def store(i: Int): Ctx =
      copy(callStack = head.store(i) :: tail)
    def get(f: String): Ctx =
      copy(callStack = head.get(f, heap) :: tail)
    def getSysout: Ctx =
      copy(callStack = head.push(Sysout) :: tail)
    def put(f: String): Ctx = {
      val (nhead, nheap) = head.put(f, heap)
      copy(callStack = nhead :: tail, heap = nheap)
    }
    def cmp: Ctx = copy(callStack = head.cmp :: tail)
    def ifeq(l: LabelNode): Ctx = copy(callStack = head.ifeq(l) :: tail)
    def ifne(l: LabelNode): Ctx = copy(callStack = head.ifne(l) :: tail)
    def go(l: LabelNode): Ctx = copy(callStack = head.go(l) :: tail)
    def add: Ctx = copy(callStack = head.add :: tail)
    def sub: Ctx = copy(callStack = head.sub :: tail)
    def newObj: Ctx = {
      val a = heap.keys.maxOption.getOrElse(0) + 1
      val obj = program.fields.map(f => f -> NumV(0)).toMap
      copy(callStack = head.push(ObjV(a)) :: tail, heap = heap + (a -> obj))
    }
    def invoke(m: String, d: String): Ctx = {
      val argNum = d.indexOf(")") - d.indexOf("(") - 1
      val (rargs, nhead) = head.popN(argNum + 1)
      val args = rargs.reverse
      if (args.head == Sysout) {
        println(args(1))
        copy(callStack = nhead :: tail)
      } else {
        val (varNum, instrs) = program.methods(m)
        val vars = (0 until varNum).map(
          i => if (i < args.length) args(i) else Undef
        ).toVector
        val nf = Frame(vars, Nil, -1, instrs)
        copy(callStack = nf :: nhead :: tail)
      }
    }
    def ret: Ctx = copy(callStack = tail)
    def lret: Ctx = {
      val nhead :: tail2 = tail
      copy(callStack = nhead.push(head.head) :: tail2)
    }
    def const(l: Long): Ctx =
      copy(callStack = head.push(NumV(l)) :: tail)
    def pop: Ctx = copy(callStack = head.pop :: tail)

    override lazy val toString: String = {
      val c = callStack.zipWithIndex.map{
        case (f, i) => s"\n[[Frame $i]]\n$f\n"
      }.mkString("\n\n")
      s"===CTX===$c\n\n$heap\n===END==="
    }
  }

}
