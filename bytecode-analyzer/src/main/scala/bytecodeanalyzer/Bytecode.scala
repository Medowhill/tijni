package info.hjaem.bytecodeanalyzer

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree._

object Load {
  def unapply(in: AbstractInsnNode): Option[Int] = in match {
    case in: VarInsnNode if in.getOpcode == Opcodes.ALOAD =>
      Some(in.`var`)
    case in: VarInsnNode if in.getOpcode == Opcodes.LLOAD =>
      Some(in.`var`)
    case _ => None
  }
}

object Store {
  def unapply(in: AbstractInsnNode): Option[Int] = in match {
    case in: VarInsnNode if in.getOpcode == Opcodes.ASTORE =>
      Some(in.`var`)
    case in: VarInsnNode if in.getOpcode == Opcodes.LSTORE =>
      Some(in.`var`)
    case _ => None
  }
}

object Getfield {
  def unapply(in: AbstractInsnNode): Option[String] = in match {
    case in: FieldInsnNode if in.getOpcode == Opcodes.GETFIELD =>
      Some(in.name)
    case _ => None
  }
}

object Getstatic {
  def unapply(in: AbstractInsnNode): Option[String] = in match {
    case in: FieldInsnNode if in.getOpcode == Opcodes.GETSTATIC =>
      Some(in.name)
    case _ => None
  }
}

object Putfield {
  def unapply(in: AbstractInsnNode): Option[String] = in match {
    case in: FieldInsnNode if in.getOpcode == Opcodes.PUTFIELD =>
      Some(in.name)
    case _ => None
  }
}

object New {
  def unapply(in: AbstractInsnNode): Option[String] = in match {
    case in: TypeInsnNode if in.getOpcode == Opcodes.NEW =>
      Some(in.desc)
    case _ => None
  }
}

object Invokevirtual {
  def unapply(in: AbstractInsnNode): Option[(String, String)] = in match {
    case in: MethodInsnNode if in.getOpcode == Opcodes.INVOKEVIRTUAL =>
      Some(in.name, in.desc)
    case _ => None
  }
}

object Return {
  def unapply(in: AbstractInsnNode): Option[Unit] = in match {
    case in: InsnNode if in.getOpcode == Opcodes.RETURN =>
      Some(())
    case _ => None
  }
}

object Lreturn {
  def unapply(in: AbstractInsnNode): Option[Unit] = in match {
    case in: InsnNode if in.getOpcode == Opcodes.LRETURN =>
      Some(())
    case _ => None
  }
}

object Lconst {
  def unapply(in: AbstractInsnNode): Option[Long] = in match {
    case in: InsnNode if in.getOpcode == Opcodes.LCONST_0 =>
      Some(0)
    case in: InsnNode if in.getOpcode == Opcodes.LCONST_1 =>
      Some(1)
    case in: LdcInsnNode if in.getOpcode == Opcodes.LDC =>
      Some(in.cst.asInstanceOf[Long])
    case _ => None
  }
}

object Pop {
  def unapply(in: AbstractInsnNode): Option[Unit] = in match {
    case in: InsnNode if in.getOpcode == Opcodes.POP =>
      Some(())
    case in: InsnNode if in.getOpcode == Opcodes.POP2 =>
      Some(())
    case _ => None
  }
}

object Lcmp {
  def unapply(in: AbstractInsnNode): Option[Unit] = in match {
    case in: InsnNode if in.getOpcode == Opcodes.LCMP =>
      Some(())
    case _ => None
  }
}

object Ifeq {
  def unapply(in: AbstractInsnNode): Option[LabelNode] = in match {
    case in: JumpInsnNode if in.getOpcode == Opcodes.IFEQ =>
      Some(in.label)
    case _ => None
  }
}

object Ifne {
  def unapply(in: AbstractInsnNode): Option[LabelNode] = in match {
    case in: JumpInsnNode if in.getOpcode == Opcodes.IFNE =>
      Some(in.label)
    case _ => None
  }
}

object Goto {
  def unapply(in: AbstractInsnNode): Option[LabelNode] = in match {
    case in: JumpInsnNode if in.getOpcode == Opcodes.GOTO =>
      Some(in.label)
    case _ => None
  }
}

object Ladd {
  def unapply(in: AbstractInsnNode): Option[Unit] = in match {
    case in: InsnNode if in.getOpcode == Opcodes.LADD =>
      Some(())
    case _ => None
  }
}

object Lsub {
  def unapply(in: AbstractInsnNode): Option[Unit] = in match {
    case in: InsnNode if in.getOpcode == Opcodes.LSUB =>
      Some(())
    case _ => None
  }
}

object Label {
  def unapply(in: AbstractInsnNode): Option[Unit] = in match {
    case in: LabelNode => Some(())
    case _ => None
  }
}
