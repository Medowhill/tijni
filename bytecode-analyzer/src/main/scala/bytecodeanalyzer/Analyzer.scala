package info.hjaem.bytecodeanalyzer

import org.objectweb.asm.Type
import org.objectweb.asm.tree._

class Analyzer(program: Program, summary: Summary) {

  val ND = SetNumDomain
  val LD = SetLocDomain
  val CLD = SetCLocDomain
  val VD = ProdValDomain(ND, LD, CLD)
  object MDM extends Make[MemDomain, VD.type] {
    def apply(vd: VD.type): MemDomain[VD.type] = MapMemDomain(vd)
  }
  object ODM extends Make[ObjDomain, VD.type] {
    def apply(vd: VD.type): ObjDomain[VD.type] = MapObjDomain(vd)
  }
  object HDM extends Make2[HeapDomain, LD.type, VD.type] {
    def apply(ld: LD.type, vd: VD.type): HeapDomain[LD.type, VD.type] =
      MapHeapDomain(ld, vd, ODM)
  }
  object CHDM extends Make2C[CHeapDomain, CLD.type, VD.type] {
    def apply(ld: CLD.type, vd: VD.type): CHeapDomain[CLD.type, VD.type] =
      MapCHeapDomain(ld, vd)
  }
  object SDM extends Make[StackDomain, VD.type] {
    def apply(vd: VD.type): StackDomain[VD.type] = ListStackDomain(vd)
  }
  val CD: CtxDomain[VD.type, LD.type, CLD.type] =
    StdCtxDomain(VD, LD, CLD, MDM, HDM, CHDM, SDM)

  type AbsCtx = CD.Elem

  def run(): Unit = {
    val entry = Loc("main", 1)
    val wl = Set(entry)
    val table = Map(entry -> CD.bottom)
    val res = iter(wl, table)
    // res.toList.sortBy{ case (Loc(m, i), _) => (m, i) } foreach {
    //   case (l, t) => println(l); println(t); println()
    // }
    for ((Loc(m, i), ctx) <- res) {
      program.methods(m).instrs(i) match {
        case Invokevirtual("println", _) =>
          println(s"$m:$i ${ctx.stackTop(m)}")
        case _ =>
      }
    }
    for ((loc @ Loc(method, i), ctx) <- res) {
      program.methods(method).instrs(i) match {
        case Invokevirtual(m, d) => summary.get(m) match {
          case Some(fsum) =>
            val argNum = Type.getMethodType(d).getArgumentTypes.length
            val args = ctx.stackTopN(method, argNum + 1).reverse
            val pas = fsum.params.tail.zip(args)
            pas.foreach{
              case (p, a) =>
                if (p.freed && ctx.cHeapIsFreed(CLD.ofCLocs(a.toCLocs)))
                  println(s"$loc $a (double-free)")
                if (p.used && ctx.cHeapIsFreed(CLD.ofCLocs(a.toCLocs)))
                  println(s"$loc $a (use-after-free)")
            }
          case _ =>
        }
        case _ =>
      }
    }
  }

  @scala.annotation.tailrec
  final def iter(
    worklist: Set[Loc], table: Map[Loc, AbsCtx]
  ): Map[Loc, AbsCtx] =
    if (worklist.isEmpty)
      table
    else {
      val res = worklist.flatMap(l => step(l, table(l)))
      val ntable = res.toList.groupBy(_._1).map{
        case (loc, ctxs) =>
          loc -> ctxs.map(_._2).reduce(_ | _)
      }
      val wl = ntable.filter{
        case (loc, ctx) => !(table.contains(loc) && ctx <= table(loc))
      }.map(_._1).toSet
      val merged = (table.keys ++ ntable.keys).map(
        loc =>
          loc -> List(table, ntable).flatMap(_.get(loc)).reduce(_ | _)
      ).toMap
      iter(wl, merged)
    }

  def step(loc: Loc, ctx: AbsCtx): Set[(Loc, AbsCtx)] = {
    val method = loc.method
    program.methods(method).instrs(loc.pc) match {
      case Label(_) | Getstatic(_) | Return(_) | Goto(_) =>
        nexts(loc).map(_ -> ctx)

      case Load(i) =>
        val v = ctx.memGet(method, i)
        Set(loc.next -> ctx.stackPush(method, v, recursives))

      case Store(i) =>
        val v = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        if (recursives(method))
          Set(loc.next -> ctx1.memWeakUpdate(method, i, v))
        else
          Set(loc.next -> ctx1.memStrongUpdate(method, i, v))

      case Getfield(f) =>
        val a = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        val obj = ctx1.heapGet(LD.ofLocs(a.toLocs))
        val v = obj.get(f)
        Set(loc.next -> ctx1.stackPush(method, v, recursives))

      case Putfield(f) =>
        val v = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        val a = ctx1.stackTop(method)
        val ctx2 = ctx1.stackPop(method)
        val l = LD.ofLocs(a.toLocs)
        val obj = ctx.heapGet(l)
        val obj1 = obj.weakUpdate(f, v)
        Set(loc.next -> ctx2.heapWeakUpdate(l, obj1))

      case Invokevirtual("println", d) =>
        Set(loc.next -> ctx.stackPop(method))

      case Invokevirtual(m, d) =>
        val argNum = Type.getMethodType(d).getArgumentTypes.length
        val args = ctx.stackTopN(method, argNum + 1).reverse
        val ctx1 = ctx.stackPopN(method, argNum + 1)
        program.methods.get(m) match {
          case Some(callee) =>
            val indArgs = callee.argIndexes.zip(args)
            val rec = recursives(m)
            def update(c: AbsCtx, i: Int, a: VD.Elem) =
              if (rec)
                c.memWeakUpdate(m, i, a)
              else
                c.memStrongUpdate(m, i, a)
            val ctx2 =
              indArgs.foldLeft(ctx1){ case (c, (i, a)) => update(c, i, a) }
            Set(Loc(m, 0) -> ctx2)
          case None =>
            val callee = program.natives(m)
            val fsum = summary(m)

            val pas = fsum.params.tail.zip(args)
            val tctx = pas.foldLeft(ctx1){
              case (c, (p, a)) => c.cHeapWeakUpdate(CLD.ofCLoc(p.name), a)
            }

            def subst(l: CLoc): VD.Elem = l match {
              case Symbol(l) => tctx.cHeapGet(CLD.ofCLocs(subst(l).toCLocs))
              case _ => VD.ofCLoc(l)
            }
            def need(l: CLoc): Boolean = l match {
              case _: Allocsite => true
              case _ => false
            }
            def toVal(l: CLoc): VD.Elem =
              subst(l) |
              (if (need(l)) VD.ofCLoc(l) else VD.bottom) |
              VD.numTop

            val ctx2 = pas.foldLeft(ctx1){
              case (c, (p, a)) =>
                if (p.freed)
                  c.cHeapFree(CLD.ofCLocs(a.toCLocs))
                else
                  c
            }
            val ctx3 = fsum.filtered.foldLeft(ctx2){
              case (c, (k, v)) =>
                val locs = toVal(k).toCLocs
                val value = v.map(toVal).foldLeft(VD.bottom)(_ | _)
                c.cHeapWeakUpdate(CLD.ofCLocs(locs), value)
            }
            val ctx4 =
              if (callee.returnVoid)
                ctx3
              else {
                val retv = fsum.ret.map(toVal).foldLeft(VD.bottom)(_ | _)
                ctx3.stackPush(method, retv, recursives)
              }
            Set(loc.next -> ctx4)
        }

      case Lreturn(_) =>
        val v = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        nexts(loc).map(
          l => {
            val Loc(m, _) = l
            l -> ctx1.stackPush(m, v, recursives)
          }
        )

      case Ifeq(_) | Ifne(_) =>
        nexts(loc).map(_ -> ctx.stackPop(method))

      case New(_) =>
        val obj = CD.HD.OD.init(program.fields)
        val ctx1 = ctx.heapWeakUpdate(LD.ofLoc(loc), obj)
        val ctx2 = ctx1.stackPush(method, VD.ofLoc(loc), recursives)
        Set(loc.next.next.next -> ctx2)

      case Lconst(l) =>
        val v = VD.ofNum(l)
        Set(loc.next -> ctx.stackPush(method, v, recursives))

      case Pop(_) =>
        Set(loc.next -> ctx.stackPop(method))

      case Ladd(_) =>
        val v2 = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        val v1 = ctx1.stackTop(method)
        val ctx2 = ctx1.stackPop(method)
        Set(loc.next -> ctx2.stackPush(method, v1 + v2, recursives))

      case Lsub(_) =>
        val v2 = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        val v1 = ctx1.stackTop(method)
        val ctx2 = ctx1.stackPop(method)
        Set(loc.next -> ctx2.stackPush(method, v1 - v2, recursives))

      case Lcmp(_) =>
        val v2 = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        val v1 = ctx1.stackTop(method)
        val ctx2 = ctx1.stackPop(method)
        Set(loc.next -> ctx2.stackPush(method, v1 cmp v2, recursives))

      case in =>
        sys.error(s"Unknown opcode (${Program.insnToString(in)})")
    }
  }

  val callgraph: Map[String, Set[String]] =
    program.methods.map{
      case (caller, m) =>
        caller -> m.instrs.flatMap{
          case Invokevirtual(callee, _) => Some(callee)
          case _ => None
        }.toSet
    }

  val transitiveClosure: Map[String, Set[String]] = {
    def fix[T](f: T => T, x: T): T = {
      val fx = f(x)
      if (fx == x) fx else fix(f, fx)
    }
    def aux(m: Map[String, Set[String]]): Map[String, Set[String]] = {
      m.map{
        case (caller, callees) =>
          caller -> (callees ++ callees.flatMap(m.get).flatten)
      }
    }
    fix(aux, callgraph)
  }

  val recursives: Set[String] =
    program.methods.keySet.filter(
      m => transitiveClosure(m) contains m
    )

  val callSites: Map[String, Set[Loc]] =
    program.methods.toList.flatMap{
      case (caller, m) =>
        m.instrs.zipWithIndex.flatMap{
          case (Invokevirtual(callee, _), i) =>
            Some((callee, Loc(caller, i)))
          case _ => None
        }
    }.groupBy(_._1).map{
      case (callee, l) => callee -> l.map(_._2).toSet
    }

  val nexts: Map[Loc, Set[Loc]] =
    program.methods.toList.flatMap{
      case (method, m) =>
        val is = m.instrs
        def indexOf(l: LabelNode): Int = is.indexOf(l)
        is.zipWithIndex.map{
          case (instr, ind) =>
            val curr = Loc(method, ind)
            val nextSet: Set[Loc] = instr match {
              case Label(_) | Load(_) | Store(_) | Getstatic(_) | Getfield(_) |
              Putfield(_) | Lconst(_) | Pop(_) | Ladd(_) | Lsub(_) | Lcmp(_) |
              Invokevirtual("println", _) =>
                Set(curr.next)

              case Invokevirtual(m, _) =>
                if (program.methods contains m)
                  Set(Loc(m, 0))
                else
                  Set(curr.next)
              case Return(_) | Lreturn(_) =>
                if (method == "main")
                  Set()
                else
                  callSites.getOrElse(method, Set()).map(l => l.next).toSet

              case New(_) => Set(curr.next.next.next)

              case Ifeq(l) =>
                Set(curr.next, Loc(method, indexOf(l)))
              case Ifne(l) =>
                Set(curr.next, Loc(method, indexOf(l)))
              case Goto(l) =>
                Set(Loc(method, indexOf(l)))

              case _ => Set()
            }
          curr -> nextSet
        }
    }.toMap
}
