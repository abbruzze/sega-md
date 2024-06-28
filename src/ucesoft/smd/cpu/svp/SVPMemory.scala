package ucesoft.smd.cpu.svp

/**
 * @author Alessandro Abbruzzetti
 *         Created on 13/05/2024 14:31  
 */
trait SVPMemory:
  val iramRomWord : Array[Int]
  def svpExternalRead(address:Int): Int
  def svpExternalWrite(address:Int,value:Int): Unit
  
