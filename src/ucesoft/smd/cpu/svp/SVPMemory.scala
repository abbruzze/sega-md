package ucesoft.smd.cpu.svp

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/05/2024 14:31  
 */
trait SVPMemory:
  def svpExternalRead(address:Int): Int
  def svpExternalWrite(address:Int,value:Int): Unit
  def svpReadIRamRom(address:Int): Int
  def svpWriteIRamRom(address:Int,value:Int): Unit
  
