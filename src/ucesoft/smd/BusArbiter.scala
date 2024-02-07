package ucesoft.smd

import ucesoft.smd.audio.FM
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
    case Z80_OWNER, M68K_OWNER, Z80_WAITING_RESET
  private enum M68KBusState:
    case M68K_OWNER,VDP_OWNER

  import Z80ResetProcess.*
  import Z80BusState.*

  private var z80 : Z80 = _
  private var m68k : M6800X0 = _
  private var fm : FM = _
  private var z80ResetProcess = STOPPED
  private var z80BusState = Z80_OWNER
  private var m68kBusState = M68KBusState.M68K_OWNER
  private var z80Waiting68KBUS = false

  override def reset(): Unit =
    z80ResetProcess = STOPPED
    z80BusState = Z80_OWNER
    m68kBusState = M68KBusState.M68K_OWNER
    m68k.setBUSAvailable(true)
    z80.requestBUS(false)

  def set(m68k:M6800X0,z80:Z80,fm:FM): Unit =
    this.m68k = m68k
    this.z80 = z80
    this.fm = fm
    
  final def isVDPRequestedBUS: Boolean = m68kBusState == M68KBusState.VDP_OWNER
  
  final def z80Request68KBUS(): Unit =
    m68kBusState match
      case M68KBusState.VDP_OWNER =>
        z80.requestBUS(true)
        z80Waiting68KBUS = true
      case _ =>
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
        if z80Waiting68KBUS then
          z80Waiting68KBUS = false
          z80.requestBUS(false)
      case _ =>
  final def m68kRequestZ80BUS(): Unit =
    z80BusState match
      case Z80_OWNER =>
        z80BusState = M68K_OWNER
        z80.requestBUS(true)
        log.info("Z80 BUS requested by 68K. Z80 stopped")
      case _ =>
        log.info("Z80 bus request ignored, already owned by 68k")
  final def m68kReleaseZ80BUS(): Unit =
    z80BusState match
      case Z80_OWNER =>
      case M68K_OWNER|Z80_WAITING_RESET =>
        z80ResetProcess match
          case STOPPED =>
            z80BusState = Z80_OWNER
            z80.requestBUS(false)
            log.info("Z80 BUS released by 68K. Z80 running")
          case STARTED =>
            z80BusState = Z80_WAITING_RESET
            log.info("Z80 BUS waiting reset")

  final def isZ80BUSAcquiredBy68K: Boolean =
    z80BusState == M68K_OWNER && z80ResetProcess == STOPPED
  final def isZ80StartedResetProcess: Boolean =
    z80ResetProcess == STARTED
  final def z80StartResetProcess(): Unit =
    z80ResetProcess match
      case STOPPED if z80BusState == M68K_OWNER =>
        z80ResetProcess = STARTED
        log.info("Z80 RESET process started")
      case _ =>
        log.warning("Z80 RESET process ignored, z80BusState=%s",z80BusState)
  final def z80StopResetProcess(): Unit =
    z80ResetProcess match
      case STARTED =>
        z80ResetProcess = STOPPED
        log.info("Z80 RESET request")
        z80.resetComponent()
        // FM sound must be reset as well
        fm.reset()
        z80BusState match
          case Z80_WAITING_RESET =>
            m68kReleaseZ80BUS()
          case _ =>
      case STOPPED =>
        log.warning("Z80 stop reset sequence ignored, never started")
