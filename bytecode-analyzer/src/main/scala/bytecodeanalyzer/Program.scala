package info.hjaem.bytecodeanalyzer

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.util._
import java.io.{File, FileInputStream, StringWriter, PrintWriter}
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

class Program(
  val fields: List[String],
  val methods: Map[String, Method],
  val natives: Map[String, Method]
) {
  val mainMethod = methods("main")

  override lazy val toString: String = {

    val f = fields.mkString(", ")
    val m = methods.values.mkString("\n")
    val n = natives.values.mkString("\n")
    s"[Fields]\n$f\n\n[Methods]\n$m\n\n[Natives]\n$n"
  }
}

class Method(mn: MethodNode) {
  val name: String = mn.name
  val desc: String = mn.desc
  val typ: Type = Type.getMethodType(desc)
  val argTypes: Vector[Type] = typ.getArgumentTypes.to(Vector)
  val retType: Type = typ.getReturnType
  val maxLocals: Int = mn.maxLocals
  val argIndexes: List[Int] = {
    val buf = ListBuffer(0)
    var i = 1
    for (s <- argTypes.map(_.getSize)) {
      buf += i
      i += s
    }
    buf.toList
  }
  val returnVoid: Boolean = retType == Type.VOID_TYPE
  val instrs =
    mn.instructions.asScala.filter(
      i => i.getOpcode != -1 || i.isInstanceOf[LabelNode]
    ).toVector
  val isNative: Boolean = instrs.isEmpty

  override lazy val toString: String =
    if (isNative)
      s"$name $desc"
    else {
      val is = instrs.zipWithIndex.map{
        case (i, ind) => s"  $ind: ${Program.insnToString(i)}"
      }.mkString("\n")
      s"$name $desc\n$is"
    }
}

object Program {
  def apply(filename: String): Program = {
    val in = new FileInputStream(new File(filename))
    val reader = new ClassReader(in)
    val cn = new ClassNode
    reader.accept(cn, 0)
    val fields = cn.fields.asScala.map(_.name).toList
    val (natives, methods) =
      cn.methods
        .asScala
        .map(new Method(_))
        .filter(m => m.name != "<init>" && m.name != "<clinit>")
        .partition(_.isNative)
    in.close()
    new Program(
      fields,
      methods.map(m => m.name -> m).toMap,
      natives.map(m => m.name -> m).toMap
    )
  }

  private val printer: Printer = new Textifier
  private val tmv: TraceMethodVisitor = new TraceMethodVisitor(printer)

  private def name(s: String): String = {
    val i = s.lastIndexOf(".")
    s.substring(i + 1)
  }

  def insnToString(in: AbstractInsnNode): String = {
    in.accept(tmv)
    val sw = new StringWriter
    printer.print(new PrintWriter(sw))
    printer.getText.clear()
    val str = sw.toString.replaceAll("\\v", "").trim
    s"$str (${in.getOpcode}, ${name(in.getClass.getName)})"
  }
}
