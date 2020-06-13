package info.hjaem.bytecodeanalyzer

trait Make[F[_ <: ValDomain with Singleton], VD <: ValDomain with Singleton] {
  def apply(vd: VD): F[vd.type]
}

trait Make2[
  F[_ <: LocDomain with Singleton, _ <: ValDomain with Singleton],
  LD <: LocDomain with Singleton,
  VD <: ValDomain with Singleton
] {
  def apply(ld: LD, vd: VD): F[ld.type, vd.type]
}

trait Domain {
  type Elem <: ElemImpl

  trait ElemImpl { this: Elem =>
    def <=(that: Elem): Boolean
    def |(that: Elem): Elem
  }

  def bottom: Elem
}

trait NumDomain extends Domain {
  type Elem <: NElemImpl

  trait NElemImpl extends ElemImpl { this: Elem =>
    def +(that: Elem): Elem
    def -(that: Elem): Elem
    def cmp(that: Elem): Elem
  }

  def top: Elem
  def ofNum(l: Long): Elem
}

object SimpleNumDomain extends NumDomain {
  sealed trait Elem extends NElemImpl {
    def <=(that: Elem): Boolean = (this, that) match {
      case (Bottom, _) | (_, Top) => true
      case _ => false
    }
    def |(that: Elem): Elem = (this, that) match {
      case (Top, _) | (Top, _) => Top
      case _ => Bottom
    }
    def +(that: Elem): Elem = (this, that) match {
      case (Bottom, _) | (_, Bottom) => Bottom
      case _ => Top
    }
    def -(that: Elem): Elem = (this, that) match {
      case (Bottom, _) | (_, Bottom) => Bottom
      case _ => Top
    }
    def cmp(that: Elem): Elem = (this, that) match {
      case (Bottom, _) | (_, Bottom) => Bottom
      case _ => Top
    }
  }
  case object Top extends Elem
  case object Bottom extends Elem

  val top = Top
  val bottom = Bottom
  def ofNum(l: Long) = Top
}

case class Loc(method: String, pc: Int)

trait LocDomain extends Domain {
  type Elem <: LElemImpl

  trait LElemImpl extends ElemImpl { this: Elem =>
    def toLocs: Set[Loc]
  }

  def ofLoc(l: Loc): Elem
}

object SetLocDomain extends LocDomain {
  case class Elem(locs: Set[Loc]) extends LElemImpl {
    def <=(that: Elem): Boolean = this.locs subsetOf that.locs
    def |(that: Elem): Elem = Elem(this.locs | that.locs)
    def toLocs: Set[Loc] = locs
  }

  val bottom: Elem = Elem(Set())
  def ofLoc(l: Loc): Elem = Elem(Set(l))
}

trait ValDomain extends Domain {
  type Elem <: VElemImpl

  trait VElemImpl extends ElemImpl { this: Elem =>
    def +(that: Elem): Elem
    def -(that: Elem): Elem
    def cmp(that: Elem): Elem
    def toLocs: Set[Loc]
  }

  def ofNum(l: Long): Elem
  def ofLoc(l: Loc): Elem
}

case class ProdValDomain(
  ND: NumDomain,
  LD: LocDomain
) extends ValDomain {

  case class Elem(n: ND.Elem, l: LD.Elem) extends VElemImpl {
    def <=(that: Elem): Boolean = {
      val Elem(tn, tl) = that
      n <= tn && l <= tl
    }
    def |(that: Elem): Elem = {
      val Elem(tn, tl) = that
      Elem(n | tn, l | tl)
    }
    def +(that: Elem): Elem = {
      val Elem(tn, _) = that
      Elem(n + tn, LD.bottom)
    }
    def -(that: Elem): Elem = {
      val Elem(tn, _) = that
      Elem(n - tn, LD.bottom)
    }
    def cmp(that: Elem): Elem = {
      val Elem(tn, _) = that
      Elem(n cmp tn, LD.bottom)
    }
    def toLocs: Set[Loc] = l.toLocs
  }

  val bottom = Elem(ND.bottom, LD.bottom)
  def ofNum(l: Long): Elem = Elem(ND.ofNum(l), LD.bottom)
  def ofLoc(l: Loc): Elem = Elem(ND.bottom, LD.ofLoc(l))
}

abstract class MemDomain[VD <: ValDomain with Singleton](
  val VD: VD
) extends Domain {
  type Elem <: MElemImpl

  trait MElemImpl extends ElemImpl { this: Elem =>
    def get(i: Int): VD.Elem
    def strongUpdate(i: Int, v: VD.Elem): Elem
    def weakUpdate(i: Int, v: VD.Elem): Elem
  }
}

case class MapMemDomain[VD <: ValDomain with Singleton](
  vd: VD
) extends MemDomain[VD](vd) {
  case class Elem(m: Map[Int, VD.Elem]) extends MElemImpl {
    def <=(that: Elem): Boolean =
      m.forall{ case (i, v) => v <= that.get(i) }
    def |(that: Elem): Elem =
      Elem((m.keySet ++ that.m.keySet).map(
        i => i -> (this.get(i) | that.get(i))
      ).toMap)
    def get(i: Int): VD.Elem = m.getOrElse(i, VD.bottom)
    def strongUpdate(i: Int, v: VD.Elem): Elem = Elem(m + (i -> v))
    def weakUpdate(i: Int, v: VD.Elem): Elem = Elem(m + (i -> (v | get(i))))
  }

  val bottom = Elem(Map())
}

abstract class ObjDomain[VD <: ValDomain with Singleton](
  val VD: VD
) extends Domain {
  type Elem <: OElemImpl

  trait OElemImpl extends ElemImpl { this: Elem =>
    def get(f: String): VD.Elem
    def strongUpdate(f: String, v: VD.Elem): Elem
    def weakUpdate(i: String, v: VD.Elem): Elem
  }

  def init(fs: List[String]): Elem
}

case class MapObjDomain[VD <: ValDomain with Singleton](
  vd: VD
) extends ObjDomain[VD](vd) {

  case class Elem(m: Map[String, VD.Elem]) extends OElemImpl {
    def <=(that: Elem): Boolean =
      m.forall{ case (f, v) => v <= that.get(f) }
    def |(that: Elem): Elem =
      Elem((m.keySet ++ that.m.keySet).map(
        f => f -> (this.get(f) | that.get(f))
      ).toMap)
    def get(f: String): VD.Elem = m.getOrElse(f, VD.bottom)
    def strongUpdate(f: String, v: VD.Elem): Elem = Elem(m + (f -> v))
    def weakUpdate(f: String, v: VD.Elem): Elem = Elem(m + (f -> (v | get(f))))
  }

  def bottom: Elem = Elem(Map())
  def init(fs: List[String]): Elem =
    Elem(fs.map(f => f -> VD.ofNum(0)).toMap)
}

abstract class HeapDomain[
  LD <: LocDomain with Singleton,
  VD <: ValDomain with Singleton,
](
  val LD: LD, val VD: VD, val ODM: Make[ObjDomain, VD]
) extends Domain {
  type Elem <: HElemImpl

  val OD: ObjDomain[VD.type] = ODM(VD)

  trait HElemImpl extends ElemImpl { this: Elem =>
    def get(l: LD.Elem): OD.Elem
    def weakUpdate(l: LD.Elem, v: OD.Elem): Elem
  }
}

case class MapHeapDomain[
  LD <: LocDomain with Singleton,
  VD <: ValDomain with Singleton,
](
  ld: LD, vd: VD, odm: Make[ObjDomain, VD]
) extends HeapDomain[LD, VD](ld, vd, odm) {

  case class Elem(m: Map[Loc, OD.Elem]) extends HElemImpl {
    def <=(that: Elem): Boolean =
      m.forall{ case (l, v) => v <= that.get(l) }
    def |(that: Elem): Elem =
      Elem((m.keySet ++ that.m.keySet).map(
        l => l -> (this.get(l) | that.get(l))
      ).toMap)
    def get(l: Loc): OD.Elem = m.getOrElse(l, OD.bottom)
    def get(l: LD.Elem): OD.Elem =
      l.toLocs.foldLeft(OD.bottom){ case (v, l) => v | get(l) }
    def weakUpdate(l: LD.Elem, v: OD.Elem): Elem =
      Elem(l.toLocs.foldLeft(m){
        case (m, l) => m + (l -> (v | get(l)))
      }.toMap)
  }

  def bottom = Elem(Map())
}

abstract class StackDomain[VD <: ValDomain with Singleton](
  val VD: VD
) extends Domain {
  type Elem <: SElemImpl

  trait SElemImpl extends ElemImpl { this: Elem =>
    def top: VD.Elem = topN(1).head
    def topN(n: Int): List[VD.Elem]
    def pop: Elem = popN(1)
    def popN(n: Int): Elem
    def push(v: VD.Elem): Elem
  }
}

case class ListStackDomain[VD <: ValDomain with Singleton](
  vd: VD
) extends StackDomain[VD](vd) {

  sealed trait Elem extends SElemImpl {
    def <=(that: Elem): Boolean = (this, that) match {
      case (WL(v1), WL(v2)) => v1 <= v2
      case (L(l1), L(l2)) =>
        if (l1.length != l2.length) false
        else l1.indices.forall(i => l1(i) <= l2(i))
      case (L(l), WL(v)) => l.forall(_ <= v)
      case (WL(_), L(_)) => false
    }
    def |(that: Elem): Elem = (this, that) match {
      case (WL(v1), WL(v2)) => WL(v1 | v2)
      case (L(l1), L(l2)) =>
        if (l1.length != l2.length)
          WL((l1 ++ l2).reduce(_ | _))
        else L(l1.indices.map(i => l1(i) | l2(i)).toList)
      case (L(l), WL(v)) => WL(l.foldLeft(v)(_ | _))
      case (WL(v), L(l)) => WL(l.foldLeft(v)(_ | _))
    }
  }
  case class L(l: List[VD.Elem]) extends Elem {
    def topN(n: Int): List[VD.Elem] = l.take(n)
    def popN(n: Int): Elem = L(l.drop(n))
    def push(v: VD.Elem): Elem = L(v :: l)
  }
  case class WL(v: VD.Elem) extends Elem {
    def topN(n: Int): List[VD.Elem] = List.fill(n)(v)
    def popN(n: Int): Elem = this
    def push(v1: VD.Elem): Elem = WL(v | v1)
  }

  val bottom: Elem = L(Nil)
}

abstract class CtxDomain[
  VD <: ValDomain with Singleton,
  LD <: LocDomain with Singleton,
](
  val VD: VD, val LD: LD,
  val MDM: Make[MemDomain, VD],
  val HDM: Make2[HeapDomain, LD, VD],
  val SDM: Make[StackDomain, VD]
) extends Domain {
  type Elem <: CElemImpl

  val MD: MemDomain[VD.type] = MDM(VD)
  val HD = HDM(LD, VD)
  val SD = SDM(VD)

  trait CElemImpl extends ElemImpl { this: Elem =>
    def memGet(m: String, i: Int): VD.Elem
    def memStrongUpdate(m: String, i: Int, v: VD.Elem): Elem
    def memWeakUpdate(m: String, i: Int, v: VD.Elem): Elem

    def heapGet(l: LD.Elem): HD.OD.Elem
    def heapWeakUpdate(l: LD.Elem, v: HD.OD.Elem): Elem

    def stackTop(m: String): VD.Elem = stackTopN(m, 1).head
    def stackTopN(m: String, n: Int): List[VD.Elem]
    def stackPop(m: String): Elem = stackPopN(m, 1)
    def stackPopN(m: String, n: Int): Elem
    def stackPush(m: String, v: VD.Elem): Elem
  }
}

case class StdCtxDomain[
  VD <: ValDomain with Singleton,
  LD <: LocDomain with Singleton,
](
  vd: VD, ld: LD,
  mdm: Make[MemDomain, VD],
  hdm: Make2[HeapDomain, LD, VD],
  sdm: Make[StackDomain, VD]
) extends CtxDomain[VD, LD](vd, ld, mdm, hdm, sdm) {

  case class Elem(
    mems: Map[String, MD.Elem],
    heap: HD.Elem,
    stacks: Map[String, SD.Elem],
  ) extends CElemImpl {
    def <=(that: Elem): Boolean = (this, that) match {
      case (Elem(m1, h1, s1), Elem(m2, h2, s2)) =>
        m1.forall{
          case (m, mem) => mem <= m2.getOrElse(m, MD.bottom)
        } &&
        h1 <= h2 &&
        s1.forall{
          case (m, stk) => stk <= s2.getOrElse(m, SD.bottom)
        }
    }
    def |(that: Elem): Elem = (this, that) match {
      case (Elem(m1, h1, s1), Elem(m2, h2, s2)) => Elem(
        (m1.keys ++ m2.keys).map{
          case m => m -> (m1.getOrElse(m, MD.bottom) | m2.getOrElse(m, MD.bottom))
        }.toMap,
        h1 | h2,
        (s1.keys ++ s2.keys).map{
          case m => m -> (s1.getOrElse(m, SD.bottom) | s2.getOrElse(m, SD.bottom))
        }.toMap
      )
    }

    def memGet(m: String, i: Int): VD.Elem =
      mems.get(m).map(_.get(i)).getOrElse(VD.bottom)
    def memStrongUpdate(m: String, i: Int, v: VD.Elem): Elem = {
      val mem = mems.getOrElse(m, MD.bottom).strongUpdate(i, v)
      copy(mems = mems + (m -> mem))
    }
    def memWeakUpdate(m: String, i: Int, v: VD.Elem): Elem = {
      val mem = mems.getOrElse(m, MD.bottom).weakUpdate(i, v)
      copy(mems = mems + (m -> mem))
    }

    def heapGet(l: LD.Elem): HD.OD.Elem = heap.get(l)
    def heapWeakUpdate(l: LD.Elem, v: HD.OD.Elem): Elem =
      copy(heap = heap.weakUpdate(l, v))

    def stackTopN(m: String, n: Int): List[VD.Elem] =
      stacks.get(m).map(_.topN(n)).getOrElse(List.fill(n)(VD.bottom))
    def stackPopN(m: String, n: Int): Elem = {
      val stack = stacks.getOrElse(m, SD.bottom).popN(n)
      copy(stacks = stacks + (m -> stack))
    }
    def stackPush(m: String, v: VD.Elem): Elem = {
      val stack = stacks.getOrElse(m, SD.bottom).push(v)
      copy(stacks = stacks + (m -> stack))
    }
  }

  val bottom: Elem = Elem(Map(), HD.bottom, Map())
}
