package ucesoft.smd

import ucesoft.smd.cpu.m68k.M6800X0
import ucesoft.smd.cpu.z80.Z80

/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/11/2023 15:36  
 */
class BusArbiter extends SMDComponent:
  private enum Z80ResetProcess:
    case STARTED, STOPPED
  private enum Z80BusState:
    case Z80_OWNER, M68K_OWNER
  private enum M68KBusState:
    case M68K_OWNER,VDP_OWNER

  import Z80ResetProcess.*
  import Z80BusState.*

  private var z80 : Z80 = _
  private var m68k : M6800X0 = _
  private var z80ResetProcess = STOPPED
  private var z80BusState = Z80_OWNER
  private var m68kBusState = M68KBusState.M68K_OWNER

  override def reset(): Unit = {
    z80ResetProcess = STOPPED
    z80BusState = Z80_OWNER
    m68kBusState = M68KBusState.M68K_OWNER
    m68k.setBUSAvailable(true)
    z80.requestBUS(false)
  }

  def set(m68k:M6800X0,z80:Z80): Unit =
    this.m68k = m68k
    this.z80 = z80
  
  final def vdpRequest68KBUS(): Unit =
    m68kBusState match
      case M68KBusState.M68K_OWNER =>
        m68k.setBUSAvailable(false)
        m68kBusState = M68KBusState.VDP_OWNER
      case _ =>
  final def vdpRelease68KBUS(): Unit =
    m68kBusState match
      case M68KBusState.VDP_OWNER =>
        m68k.setBUSAvailable(true)
        m68kBusState = M68KBusState.M68K_OWNER
      case _ =>
  final def m68kRequestZ80BUS(): Unit =
    z80BusState match
      case Z80_OWNER =>
        z80BusState = M68K_OWNER
        z80.requestBUS(true)
      case M68K_OWNER =>
  final def m68kReleaseZ80BUS(): Unit =
    z80BusState match
      case Z80_OWNER =>
      case M68K_OWNER =>
        z80BusState = Z80_OWNER
        z80.requestBUS(false)

  final def isZ80BUSAcquiredBy68K: Boolean =
    z80BusState == M68K_OWNER
  final def isZ80StartedResetProcess: Boolean =
    z80ResetProcess == STARTED
  final def z80StartResetProcess(): Unit =
    z80ResetProcess match
      case STOPPED =>
        z80ResetProcess = STARTED
        if z80BusState == Z80_OWNER then
          log.warning("Z80 start reset process without bus requested")
      case STARTED =>
  final def z80StopResetProcess(): Unit =
    z80ResetProcess match
      case STARTED =>
        z80ResetProcess = STOPPED
        log.info("Z80 RESET request")
        val sp = z80.ctx.SP // TODO check if correct
        z80.resetComponent()
        z80.ctx.SP = sp
      case STOPPED =>
