package ucesoft.smd.cheat

import ucesoft.smd.cheat.Cheat.CheatCode

/**
 * @author Alessandro Abbruzzetti
 *         Created on 12/03/2024 10:51  
 */
trait CheatManager:
  def addCheat(cheat: CheatCode): Unit
  def removeCheat(cheat: CheatCode): Unit
  def removeAllCheats(): Unit
  def getCheats: List[CheatCode]
