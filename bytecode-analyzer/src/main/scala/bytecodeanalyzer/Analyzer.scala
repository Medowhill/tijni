package info.hjaem.bytecodeanalyzer

import org.objectweb.asm.Type
import org.objectweb.asm.tree._

class Analyzer(program: Program) {

  val ND = SetNumDomain
  val LD = SetLocDomain
  val VD = ProdValDomain(ND, LD)
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
  object SDM extends Make[StackDomain, VD.type] {
    def apply(vd: VD.type): StackDomain[VD.type] = ListStackDomain(vd)
  }
  val CD: CtxDomain[VD.type, LD.type] = StdCtxDomain(VD, LD, MDM, HDM, SDM)

  type AbsCtx = CD.Elem

  def run(): Unit = {
    val entry = JLoc("main", 1)
    val wl = Set(entry)
    val table = Map(entry -> CD.bottom)
    val res = iter(wl, table)
    // res.toList.sortBy{ case (JLoc(m, i), _) => (m, i) } foreach {
    //   case (l, t) => println(l); println(t); println()
    // }
    for ((JLoc(m, i), ctx) <- res) {
      program.methods(m).instrs(i) match {
        case Invokevirtual("println", _) =>
          println(s"$m:$i ${ctx.stackTop(m)}")
        case _ =>
      }
    }
  }

  @scala.annotation.tailrec
  final def iter(
    worklist: Set[JLoc], table: Map[JLoc, AbsCtx]
  ): Map[JLoc, AbsCtx] =
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

  def step(loc: JLoc, ctx: AbsCtx): Set[(JLoc, AbsCtx)] = {
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
        val obj :: args = ctx.stackTopN(method, argNum + 1).reverse
        val ctx1 = ctx.stackPopN(method, argNum + 1)
        val nctx =
          if (recursives(m)) {
            val ctx2 = ctx1.memWeakUpdate(m, 0, obj)
            args.zipWithIndex.foldLeft(ctx2){
              case (c, (a, i)) => c.memWeakUpdate(m, 2 * i + 1, a)
            }
          } else {
            val ctx2 = ctx1.memStrongUpdate(m, 0, obj)
            args.zipWithIndex.foldLeft(ctx2){
              case (c, (a, i)) => c.memStrongUpdate(m, 2 * i + 1, a)
            }
          }
          Set(JLoc(m, 0) -> nctx)

      case Lreturn(_) =>
        val v = ctx.stackTop(method)
        val ctx1 = ctx.stackPop(method)
        nexts(loc).map(
          l => {
            val JLoc(m, _) = l
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

  val callSites: Map[String, Set[JLoc]] =
    program.methods.toList.flatMap{
      case (caller, m) =>
        m.instrs.zipWithIndex.flatMap{
          case (Invokevirtual(callee, _), i) =>
            Some((callee, JLoc(caller, i)))
          case _ => None
        }
    }.groupBy(_._1).map{
      case (callee, l) => callee -> l.map(_._2).toSet
    }

  val nexts: Map[JLoc, Set[JLoc]] =
    program.methods.toList.flatMap{
      case (method, m) =>
        val is = m.instrs
        def indexOf(l: LabelNode): Int = is.indexOf(l)
        is.zipWithIndex.map{
          case (instr, ind) =>
            val curr = JLoc(method, ind)
            val nextSet: Set[JLoc] = instr match {
              case Label(_) | Load(_) | Store(_) | Getstatic(_) | Getfield(_) |
              Putfield(_) | Lconst(_) | Pop(_) | Ladd(_) | Lsub(_) | Lcmp(_) |
              Invokevirtual("println", _) =>
                Set(curr.next)

              case Invokevirtual(m, _) => Set(JLoc(m, 0))
              case Return(_) | Lreturn(_) =>
                if (method == "main")
                  Set()
                else
                  callSites(method).map(l => l.next).toSet

              case New(_) => Set(curr.next.next.next)

              case Ifeq(l) =>
                Set(curr.next, JLoc(method, indexOf(l)))
              case Ifne(l) =>
                Set(curr.next, JLoc(method, indexOf(l)))
              case Goto(l) =>
                Set(JLoc(method, indexOf(l)))

              case _ => Set()
            }
          curr -> nextSet
        }
    }.toMap
}
