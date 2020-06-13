package info.hjaem.bytecodeanalyzer

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.util._
import java.io.{File, FileInputStream, StringWriter, PrintWriter}
import scala.jdk.CollectionConverters._

class Program(
  val fields: List[String],
  val methods: Map[String, (Int, Vector[AbstractInsnNode])],
  val natives: List[String]
) {
  val mainMethod = methods("main")

  override lazy val toString: String = {

    val f = fields.mkString(", ")
    val m = methods.map{
      case (n, (_, is)) =>
        s"$n\n${is.map(i => s"  ${Program.insnToString(i)}").mkString("\n")}"
    }.mkString("\n\n")
    val n = natives.mkString(", ")
    s"[Fields]\n$f\n\n[Methods]\n$m\n\n[Natives]\n$n"
  }
}

object Program {
  def apply(filename: String): Program = {
    val in = new FileInputStream(new File(filename))
    val reader = new ClassReader(in)
    val cn = new ClassNode
    reader.accept(cn, 0)
    val fields = cn.fields.asScala.map(_.name).toList
    val (methods, natives) = cn.methods.asScala.map(
      mn => {
        val instrs =
          mn.instructions.asScala.filter(
            i => i.getOpcode != -1 || i.isInstanceOf[LabelNode]
          ).toVector
        val maxIndex = instrs.flatMap{
          case Load(i) => Some(i)
          case Store(i) => Some(i)
          case _ => None
        }.maxOption.getOrElse(-1) + 1
        val desc = mn.desc
        val argNum = desc.indexOf(")") - desc.indexOf("(") - 1
        mn.name -> ((maxIndex max argNum, instrs))
      }
    ).partition(_._2._2.nonEmpty)
    in.close()
    new Program(fields, methods.toMap, natives.map(_._1).toList)
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
