package ucesoft.smd.debugger

import ucesoft.smd.cpu.m68k.{DisassembledInstruction, M6800X0, Memory}

/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/10/2023 16:35  
 */
trait Disassemble68KAnnotator:
  def getNoteFor(dis: DisassembledInstruction, m68k: M6800X0, memory: Memory): String
  
object EmptyAnnotator extends Disassemble68KAnnotator:
  override def getNoteFor(dis: DisassembledInstruction, m68k: M6800X0, memory: Memory): String = ""
