package ucesoft.smd

import ucesoft.smd.cpu.m68k.{M6800X0, Memory, Size}
import ucesoft.smd.cpu.z80.Z80

import java.awt.RenderingHints
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object VDP:
  final val SCREEN_WIDTH : Int = HMode.H40.totalWidth

  case class VDPMemoryDump(ram:Array[Int],cram:Array[Int],vsram:Array[Int])
  case class VDPProperty(name:String,value:String,valueDetails:String = "",description:String = "",register:Option[Int] = None)
  case class VDPPropertiesDump(properties:Array[VDPProperty],registerWriter:(Int,Int) => Unit)
  case class VDPSpriteCacheDump(spriteIndex:Int,
                                x:Int,
                                y:Int,
                                w:Int,
                                h:Int,
                                gfx:Int,
                                hf:Boolean,
                                vf:Boolean,
                                palette:Int,
                                priority:Boolean,
                                var next:Option[VDPSpriteCacheDump] = None)


/**
 * @author Alessandro Abbruzzetti
 *         Created on 07/09/2023 13:26
 *
 *
 *  +=========== PATTERN ======================+     T10-T0: Tile index. The number of the tile in VRAM to use, or its address divided by $20 (>> 5)
 *  | 7 | 6 5 | 4 | 3 | 2 1 0 ||7 6 5 4 3 2 1 0|     V: Vertical flip. 1 = flipped
 *  |PR | PL  | V | H |T10-T8 ||    T7-T0      |     H: Horizontal flip. 1 = flipped
 *  +------------------------------------------+     PL: Palette line
 *                                                   PR: Priority. 1 = high
 *
 *  1 tile = 8 x 8 pixels = 32 byte
 *  1 pixel = 4 bits (16 possible colors for current palette PL)
 */
class VDP(busArbiter:BusArbiter) extends SMDComponent with Clock.Clockable with M6800X0.InterruptAckListener:
  inline private val A = 0
  inline private val B = 1
  inline private val S = 2

  import VDP.*
  private enum VSCROLL_MODE:
    case FULL, EACH_2_CELL
  private enum HSCROLL_MODE:
    case FULL, PROHIBITED, EACH_1_CELL, EACH_1_LINE

  private enum INTERLACE_MODE(val patternSizeShift: Int, val yScrollMask: Int):
    case NO_INTERLACE extends INTERLACE_MODE( 5, 0x7FF)
    case INTERLACE_1 extends INTERLACE_MODE( 5, 0x7FF)
    case PROHIBITED extends INTERLACE_MODE( 5, 0x7FF)
    case INTERLACE_2 extends INTERLACE_MODE( 6, 0x3FF)

  private enum SCROLL_SIZE(val cell:Int,val shift:Int):
    val mask : Int = cell - 1
    case _32CELL extends SCROLL_SIZE(32,5)
    case _64CELL extends SCROLL_SIZE(64,6)
    case PROHIBITED extends SCROLL_SIZE(32,5)
    case _128CELL extends SCROLL_SIZE(128,7)
    case _1CELL extends SCROLL_SIZE(1,0)
  private enum DMA_MODE:
    case MEMORY_TO_VRAM, VRAM_FILL, VRAM_COPY

  private case class FifoEntry(commandCode:Int,
                               address:Int,
                               data:Int,
                               var vramFirstByteWritten:Boolean = false)

  private class FIFO:
    private inline val MAX_SIZE = 4
    private final val fifo = Array.ofDim[FifoEntry](MAX_SIZE)
    private var tail, head = -1
    private var size = 0
    private var lastWritten : FifoEntry = _
    private var lastPopped : FifoEntry = _

    def getLastWritten: FifoEntry = lastWritten
    def getLastPopped: FifoEntry = lastPopped

    def length: Int = size
    def isEmpty: Boolean = size == 0
    def reset(): Unit =
      head = -1
      tail = -1
      size = 0
      lastWritten = null
      lastPopped = null

    final def peek: FifoEntry =
      if head == -1 then
        throw new IllegalStateException()
      else
        fifo(head)

    final def enqueue(value:FifoEntry): Boolean =
      if size < MAX_SIZE then
        if tail == -1 then
          tail = 0
          head = 0
          fifo(0) = value
        else
          tail = (tail + 1) % MAX_SIZE
          fifo(tail) = value
        size += 1
        lastWritten = value
        if size == MAX_SIZE then
          statusRegister |= STATUS_FIFO_FULL_MASK
        statusRegister &= ~STATUS_FIFO_EMPTY_MASK
        true
      else
        false

    final def dequeue(): FifoEntry =
      if size > 0 then
        size -= 1
        lastPopped = fifo(head)
        if size == 0 then
          tail = -1
          head = -1
          statusRegister |= STATUS_FIFO_EMPTY_MASK
        else
          head = (head + 1) % MAX_SIZE
          statusRegister &= ~STATUS_FIFO_FULL_MASK

        // check overflow entry
        if writeOverflowFIFOEntry != null then
          log.info("FIFO:dequeue Restoring overflow entry %s and assert DTACK",writeOverflowFIFOEntry)
          enqueue(writeOverflowFIFOEntry)
          val dtackCond = writeOverflowFIFOEntry2 == null
          writeOverflowFIFOEntry = writeOverflowFIFOEntry2
          writeOverflowFIFOEntry2 = null
          m68k.setDTACK(dtackCond)
        lastPopped
      else
        throw new IllegalStateException()
  end FIFO

  private final val BLANK_COLOR = java.awt.Color.BLACK.getRGB

  private var model : Model = _
  private val VRAM = Array.ofDim[Int](0x10000)
  private val CRAM = Array.ofDim[Int](128)      // 128 bytes,  64 word entries: |----bbb-|ggg-rrr-|
  private val CRAM_COLORS = Array.fill[Int](4,3,16)(Palette.getColor(0))  // 16 colors x 4 palette
  private val VSRAM = Array.ofDim[Int](80)
  private val regs = Array.ofDim[Int](0x20)

  private var m68k : M6800X0 = _
  private var z80 : Z80 = _

  private inline val STATUS_FIFO_EMPTY_MASK   = 0x0200
  private inline val STATUS_FIFO_FULL_MASK    = 0x0100
  private inline val STATUS_F_MASK            = 0x0080  // V interrupted happened
  private inline val STATUS_SOVR_MASK         = 0x0040  // SpriteCache overflow occurred
  private inline val STATUS_C_MASK            = 0x0020  // SpriteCache collision occurred
  private inline val STATUS_ODD_MASK          = 0x0010  // Odd frame in interlace mode
  private inline val STATUS_VB_MASK           = 0x0008
  private inline val STATUS_HB_MASK           = 0x0004
  private inline val STATUS_DMA_MASK          = 0x0002
  private inline val STATUS_PAL_MASK          = 0x0001

  private inline val PATTERN_H_MASK           = 0x0800
  private inline val PATTERN_V_MASK           = 0x1000

  // VRAM ACCESS MODE
  private inline val VRAM_WRITE   = 1
  private inline val CRAM_WRITE   = 3
  private inline val VSRAM_WRITE  = 5
  private inline val VRAM_READ    = 0
  private inline val CRAM_READ    = 8
  private inline val VSRAM_READ   = 4

  // INTERRUPT LEVELS
  private inline val HINT_LEVEL = 4
  private inline val VINT_LEVEL = 6

  private var statusRegister = STATUS_FIFO_EMPTY_MASK | STATUS_VB_MASK

  /*
   VRAM Access
   ------------------------------------------------------------------------------
   You can access VRAM, CRAM, or VSRAM by writing a 32-bit command word to
   the control port. The data written has the following format:

      CD1 CD0 A13 A12 A11 A10 A09 A08     (D31-D24)
      A07 A06 A05 A04 A03 A02 A01 A00     (D23-D16)
       ?   ?   ?   ?   ?   ?   ?   ?      (D15-D8)
      CD5 CD4 CD3 CD2  ?   ?  A15 A14     (D7-D0)

      CDx = VDP code (0-3F)
      Axx = VDP address (00-FFFF)

   The state of D15 through D8, D3, and D2 are ignored.

   The VDP has an address and code register. They are used in conjunction to
   handle data port accesses. The address register provides an offset into
   VDP RAM to write or read data from. The code register specifies if
   data port accesses will be reads or writes, and the kind of VDP RAM
   to perform these operations on.

   In order for the VDP to know if the first or second 16-bit half of the
   command word has been written to the control port, it maintains an internal
   write-pending flag. This flag is updated when these conditions are met:

   - It is set when the first half of the command word is written.
   - It is cleared when the second half of the command word is written.
   - It is cleared when the data port is written to or read from.
   - It is cleared when the control port is read.

   It is perfectly valid to write the first half of the command word only.
   In this case, _only_ A13-A00 and CD1-CD0 are updated to reflect the new
   values, while the remaining address and code bits _retain_ their former
   value.

   You cannot write to a VDP register if the pending flag is set to one,
   since the VDP is expecting the 2nd half of a command word.

   Writing to a VDP register will clear the code register. Games that rely
   on this are Golden Axe II (will display missing SEGA logo) and Sonic 3D.
   (will show intro movie in wrong colors for a few frames) It is not known
   if the address register is cleared as well, but the TMS9918 manual
   indicates that this is so, perhaps it applies to the Genesis as well.

   Here is a table of code register settings:

   Bits CD3-CD0
   0000b : VRAM read
   0001b : VRAM write
   0011b : CRAM write
   0100b : VSRAM read
   0101b : VSRAM write
   1000b : CRAM read

   You cannot write data after setting up a read operation, or read data
   after setting up a write operation. The write or read is ignored.

   CD4 is only set for the the VRAM copy DMA mode.
   For data port accesses and 68k to VDP DMA, the state of CD4 is ignored.

   Setting CD5 will trigger a DMA operation.
   */
  private var addressRegister = 0
  private var codeRegister = 0
  private var writePendingFlag = false

  private var pendingRead = false
  private var pendingReadValue = 0

  private var pendingControlPortCommand = 0
  private var pendingControlPortRequest = false
  private var pendingControlPortRequestLong = false

  private val fifo = new FIFO
  private var writeOverflowFIFOEntry,writeOverflowFIFOEntry2 : FifoEntry = _

  private var dmaFillWriteDone = false
  private var readCopyCache = -1 // -1 means not read yet

  private class VDP4ReadAddress:
    private var readAddress = 0
    private var readBuffer = 0
    private var readCount = 0
    private var swapNibbles = false

    final def count: Int = readCount
    final def modifyAddress(address:Int): Unit =
      readAddress = address
    final def buffer: Int = readBuffer
    final def reset(): Unit =
      readCount = 0
      //readBuffer = 0
    final def incCount(): Unit =
      readCount += 1

    final def set(address:Int,swap:Boolean = false): Unit =
      readAddress = address
      swapNibbles = swap
      readCount = 0

    final def readVRAMByte(): Boolean =
      var byte = VRAM(readAddress & 0xFFFF)
      if swapNibbles then byte = (byte & 0xF) << 4 | (byte >> 4) & 0xF
      readBuffer = readBuffer << 8 | byte

      if swapNibbles then
        readAddress -= 1
      else
        readAddress += 1
      readCount += 1
      readCount == 4
  end VDP4ReadAddress

  private val vdp4read = new VDP4ReadAddress
  private var vdpAccessSlot = 0

  private var minModX,maxModX,minModY,maxModY = 0
  private var pixelMod = false

  /*
     Each entry in the PatternBitCache is 7 bits wide and represent a bit to draw, with priority, palette and colour information:
     +=== Pattern Bit =======+
     | 7 | 6 | 5 4 | 3 2 1 0 |    PR      = priority taken from Layer Mapping
     | - | PR| PAL | BIT COL |    PAL     = palette number taken from Layer Mapping
     +-----------------------+    BIT COL = bit color index
   */
  private class PatternBitCache(cells: Int):
    private val cache = Array.ofDim[Int](cells * 8) // max size 40 + 1 cells of 8 bits
    private var readIndex, writeIndex = 0
    private var skipFirst = 0

    final def reset(): Unit =
      readIndex = 0
      writeIndex = 0
      skipFirst = 0

    final def clear(): Unit =
      java.util.Arrays.fill(cache,0)
      reset()

    final def skip(skipFirst: Int): Unit =
      this.skipFirst = skipFirst

    final def put(index:Int,v:Int): Unit =
      cache(index) = v
    final def get(index:Int): Int =
      cache(index)

    final def enqueueBit(v: Int): Unit =
      if skipFirst > 0 then
        skipFirst -= 1
      else
        cache(writeIndex) = v
        writeIndex += 1

    final def peekBit: Int = cache(readIndex)

    final def dequeueBit(): Int =
      if readIndex < cache.length then
        val bit = cache(readIndex)
        readIndex += 1
        bit
      else
        0
    final def dequeueBitAndClear(): Int =
      if readIndex < cache.length then
        val bit = cache(readIndex)
        cache(readIndex) = 0
        readIndex += 1
        bit
      else
        0

  private class VDPLayerAddress(layer:Int):
    private var baseAddress = 0
    private var windowBaseAddress = 0
    private var windowActive = false
    private var scrollx = 0
    private var posy = 0
    private var cellx = 0
    private var celly = 0
    private var cellxSize : SCROLL_SIZE = SCROLL_SIZE._32CELL
    private var cellySize : SCROLL_SIZE = SCROLL_SIZE._32CELL

    private val windowX = Array(0,0)
    private val windowY = Array(0,0)

    final def cellX: Int = cellx
    final def cellY: Int = celly

    inline private def updateWindow(): Unit =
      windowActive = layer == A && (windowX(1) - windowX(0)) > 0 || (windowY(1) - windowY(0)) > 0

    final def setWindowX(regValue:Int): Unit =
      val right = (regValue & 0x80) > 0
      val width = (regValue & 0x1F) << 1
      if right then
        windowX(0) = width
        windowX(1) = cellxSize.cell - 1
      else
        windowX(0) = 0
        windowX(1) = width
      updateWindow()

    final def setWindowY(regValue:Int): Unit =
      val down = (regValue & 0x80) > 0
      val height = regValue & 0x1F
      if down then
        windowY(0) = height
        windowY(1) = cellySize.cell - 1
      else
        windowY(0) = 0
        windowY(1) = height
      updateWindow()

    final def set(scrollx:Int,cellxSize:SCROLL_SIZE,cellySize:SCROLL_SIZE): Unit =
      this.baseAddress = if layer == A then REG_PATTERN_A_ADDRESS else REG_PATTERN_B_ADDRESS
      this.scrollx = scrollx
      cellx = 0
      celly = 0
      this.cellxSize = cellxSize
      this.cellySize = cellySize
      if layer == A then
        windowBaseAddress = REG_PATTERN_WINDOW_ADDRESS

    final def setCellY(line:Int,scrollY:Int): Boolean =
      celly = line >> 3
      posy = (line + scrollY) >> 3
      isInWindow

    inline private def isInWindow: Boolean = windowActive && ((cellx >= windowX(0) && cellx < windowX(1)) || (celly >= windowY(0) && celly < windowY(1)))

    final def isInsideWindow(celly:Int): Boolean = windowActive && ((cellx >= windowX(0) && cellx < windowX(1)) || (celly >= windowY(0) && celly < windowY(1)))

    final def address: Int =
      if isInWindow then
        val winCellXSizeShift = if REG_H32 then 5 else 6 // H32 = 32 cells, H40 = 64 cells; celly = 32
        windowBaseAddress | (celly << winCellXSizeShift) << 1 | cellx << 1
      else
        baseAddress | ((posy & cellySize.mask) << cellxSize.shift) << 1 | ((scrollx + cellx) & cellxSize.mask) << 1

    final def incCellX(): Unit =
      cellx += 1

  private val vdpLayer2CellMappingBuffer = Array(0,0)   // each entry contains 2 entry of 16 bits
  private val vdpLayerMappingAddress = Array(new VDPLayerAddress(A),new VDPLayerAddress(B))       // each entry reference the current x-cell position (0 - 32/40)

  private val vdpLayerPatternBuffer = Array(new PatternBitCache(40 + 2), new PatternBitCache(40 + 2), new PatternBitCache(40 + 2)) // A, B, S

  private var hmode : HMode = HMode.H32
  private val xscroll = Array(0,0)  // xscroll for layer A and B
  private val yscroll = Array(0,0)  // yscroll for layer A and B
  private val firstCellToXScroll = 0

  private var activeDisplayLine = 0 // scanline within active raster lines (0 - V28 or V30 * 8)
  private var activeDisplayXPos = 0 // current pixel position within active scan line (256/320)

  private var xborderCount = 0
  private var xborderIsLeft = false
  private var inXActiveDisplay,inYActiveDisplay = false

  private var frameCount = 0
  private var verticalBlanking = true
  private var isVerticalBorder = false
  private var rasterLine = 0        // absolute raster line
  private var xpos = 0              // absolute x position within raster line

  private var hcounter = 0          // internal 9 bit h-counter
  private var vcounter = 0          // internal 9 bit v-counter
  private var latchedHVCounter = 0  // latched value of hcounter + vcounter
  private var vcounterInc = 0
  private var hInterruptCounter = 0 // horizontal interrupt counter

  private val layerPixels = Array(0,0,0) // pixels from A, B and S
  private var colorMode = Palette.PaletteType.NORMAL

  private var vInterruptPending = false
  private var hInterruptPending = false

  private var display : Display = _
  private var videoPixels : Array[Int] = _
  private final val SCREEN_WIDTH = VDP.SCREEN_WIDTH

  private var interlaceModeEnabled = false
  private var interlaceMode: INTERLACE_MODE = INTERLACE_MODE.NO_INTERLACE

  private var layerAEnabled = true
  private var layerBEnabled = true
  private var layerSEnabled = true

  /*
   8 bytes info
   first 4 are cached, others are read
   +----------------------------------------------------------------+
   | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 || 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
   +----------------------------------------------------------------+
   | 0 | 0 | 0 | 0 | 0 | 0 |   Y   ||               Y               |   Y = Vertical Position
   +----------------------------------------------------------------+
   | 0 | 0 | 0 | 0 |   HS  |   VS  || 0 |          NEXT             |   HS/VS = Size, Next = Next sprite number to jump to. Earlier sprites go on top of later ones. The final sprite must jump to 0
   +----------------------------------------------------------------+
   | PR|   PL  | VF|HF|    GFX     ||              GFX              |   PR = Priority, PL = Palette, VF/HF = Flipping, GFX = Tile number from VRAM. i.e. VRAM address to read graphics data, divided by $20
   +----------------------------------------------------------------+
   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | X ||               X               |   X = Horizontal Position
   +----------------------------------------------------------------+
   */
  private class SpriteCache(val index:Int):
    private var _y = 0
    private var width, height = 0
    private var next = 0
    private var address = 0

    final def y : Int = if interlaceModeEnabled then _y >> 1 else _y
    final def w : Int = width
    final def h : Int = height
    final def link : Int = next

    final def setCache(address:Int): Unit =
      this.address = address
      _y = (VRAM(address) << 8 | VRAM(address + 1)) & 0x3FF
      val hsvs = VRAM(address + 2)
      width = (hsvs >> 2) & 3
      height = hsvs & 3
      next = VRAM(address + 3) & 0x7F
      log.info("SpriteCache %d cache updated: y=%d width=%d height=%d next=%d",index,_y,width,height,next)

    def dump(): VDPSpriteCacheDump =
      val priority = (VRAM(address + 4) & 0x80) != 0
      val palette = (VRAM(address + 4) >> 13) & 3
      val vf = (VRAM(address + 4) & 0x10) != 0
      val hf = (VRAM(address + 4) & 0x08) != 0
      val gfx = (VRAM(address + 4) << 8 | VRAM(address + 5)) & 0x7FF
      val x = (VRAM(address + 6) << 8 | VRAM(address + 7)) & 0x1FF
      VDPSpriteCacheDump(index,x,y,width,height,gfx,hf,vf,palette,priority)

  end SpriteCache

  private class SpriteInfo:
    private var thirdByte = 0
    private var horizontalPosition = 0
    private var cellReadCount = 0
    private var spriteCacheIndex = 0
    private var address = 0
    private var zeroPos = false

    final def index: Int = spriteCacheIndex
    final def xpos: Int = horizontalPosition
    final def patternAddress: Int = address
    final def priorityAndPalette: Int = (thirdByte >> 13) & 7
    final def horizontalFlipped: Boolean = (thirdByte & 0x0800) > 0

    final def isFirstHorizontalCell: Boolean = cellReadCount == 0
    final def isZeroPos : Boolean = zeroPos

    final def incCell(): Boolean =
      cellReadCount += 1
      horizontalPosition += 8
      val cellDelta = (spriteCache(spriteCacheIndex).h + 1) << interlaceMode.patternSizeShift
      if horizontalFlipped then
        address -= cellDelta
      else
        address += cellDelta
      cellReadCount > spriteCache(spriteCacheIndex).w

    /*
      048C  0
      159D  1 01
      26AE
      37BF
     */
    final def set(spriteCacheIndex:Int, thirdWord:Int, hpos:Int): Unit =
      this.spriteCacheIndex = spriteCacheIndex
      this.thirdByte = thirdWord
      horizontalPosition = hpos - 128
      zeroPos = hpos == 0
      val deltaY = (activeDisplayLine - (spriteCache(spriteCacheIndex).y - 128)) & 0x1F
      cellReadCount = 0
      address = (thirdWord & interlaceMode.yScrollMask) << interlaceMode.patternSizeShift
      val h = spriteCache(spriteCacheIndex).h
      val vf = (thirdWord & 0x1000) > 0
      val ycell = if vf then h - (deltaY >> 3) else deltaY >> 3
      var yline = if vf then 7 ^ (deltaY & 7) else deltaY & 7
      if interlaceModeEnabled then
        yline = (yline << 1) | frameCount
      address += (ycell << interlaceMode.patternSizeShift) | yline << 2
      val hf = (thirdWord & 0x0800) > 0
      if hf then
        address += ((h + 1) << interlaceMode.patternSizeShift) * spriteCache(spriteCacheIndex).w + 3

      //println(s"Sprite $spriteCacheIndex X=${(horizontalPosition + 128).toHexString} Y=${spriteCache(spriteCacheIndex).y.toHexString} line=$activeDisplayLine deltaY=$deltaY address=${address.toHexString}")
  end SpriteInfo

  private class SpriteVisibleSR:
    private val cache = Array.ofDim[Int](MAX_SPRITES_PER_ROW)
    private var readIndex, writeIndex = 0
    private var size = 0

    def reset(): Unit =
      readIndex = 0
      writeIndex = 0
      size = 0

    final def enqueueIndex(v: Int): Unit =
      cache(writeIndex) = v
      size += 1
      writeIndex = (writeIndex + 1) % cache.length

    final def empty: Boolean = size == 0
    final def getSize: Int = size

    final def peekIndex(): Int = cache(readIndex)

    final def dequeueIndex(): Int =
      val bit = cache(readIndex)
      size -= 1
      readIndex = (readIndex + 1) % cache.length
      bit
  end SpriteVisibleSR

  private final val MAX_SPRITES_PER_ROW = math.max(HMode.H32.maxSpritePerLine,HMode.H40.maxSpritePerLine)
  private val MAX_SPRITE_PER_FRAME = math.max(HMode.H32.maxSpritePerFrame,HMode.H40.maxSpritePerFrame)
  private val SPRITE_ATTRIBUTES_SIZE = 8 * MAX_SPRITE_PER_FRAME
  private final val spriteCache = {
    val sp = Array.ofDim[SpriteCache](MAX_SPRITE_PER_FRAME)
    for i <- sp.indices do
      sp(i) = new SpriteCache(i)
    sp
  }
  // for sprite phase 1
  private var sprite1VisibleSR,sprite1VisibleNextLineSR = new SpriteVisibleSR
  private var sprite1VisibleCurrentIndex = 0
  private var sprite1FirstFetch = true
  // for sprite phase 2
  private final val sprite2Info = Array.fill[SpriteInfo](MAX_SPRITES_PER_ROW)(new SpriteInfo)
  private var sprite2CurrentIndex,sprite2Size = 0
  // for sprite evaluation
  private inline val SPRITE_PHASE1_ACCESS_SLOT = 1
  // for sprite bug
  private var lastSpriteXPosZero = false
  // sprite evaluation
  private inline val SPRITE_EVAL_IDLE = 0
  private inline val SPRITE_EVAL_IN_PROGRESS = 1
  private inline val SPRITE_EVAL_TERMINATED = 2
  private var spriteLineRenderingEnabled = true
  private var spriteEvaluationPhaseInProgress = SPRITE_EVAL_IDLE
  private var spriteEvaluationIndex = 0
  private var spriteEvaluationCycles = 0

  // references to other components
  private var m68KMemory : Memory = _
  private var m68KBUSRequsted = false
  private var masterClock : Clock = _

  // ============================= Constructor =========================================
  initRegisters()
  // ===================================================================================
  def getMemoryDump: VDPMemoryDump =
    VDPMemoryDump(VRAM,CRAM,VSRAM)

  def setDisplay(display:Display): Unit =
    this.display = display
    videoPixels = display.displayMem

  def set68KMemory(mem:Memory): Unit =
    m68KMemory = mem

  def setMasterClock(clock:Clock): Unit =
    masterClock = clock

  override def init(): Unit =
    vcounter = model.videoType.topBlankingInitialVCounter(REG_M2)
    hcounter = hmode.hCounterInitialValue

  override def reset(): Unit =
    for pal <- 0 to 3; mode <- 0 to 2 do
      java.util.Arrays.fill(CRAM_COLORS(pal)(mode),Palette.getColor(0))

    m68KBUSRequsted = false
    statusRegister = STATUS_FIFO_EMPTY_MASK | (statusRegister & 1) | STATUS_VB_MASK // preserve PAL/NTSC flag
    readCopyCache = -1
    writePendingFlag = false
    pendingRead = false
    addressRegister = 0
    codeRegister = 0
    pendingControlPortCommand = 0
    pendingControlPortRequest = false
    dmaFillWriteDone = false
    fifo.reset()
    vdpLayerPatternBuffer(A).reset()
    vdpLayerPatternBuffer(B).reset()
    vdpLayerPatternBuffer(S).reset()
    //sprite1VisibleCurrentIndex = 0
    //java.util.Arrays.fill(sprite1VisibleIndexes,-1)
    changeVDPClockDivider(hmode.initialClockDiv)
    verticalBlanking = true
    frameCount = 0

    spriteLineRenderingEnabled = true
    lastSpriteXPosZero = false

  def setCPUs(m68k:M6800X0,z80:Z80): Unit =
    this.m68k = m68k
    this.z80 = z80
    m68k.setInterruptAckListener(this)

  def setModel(model:Model): Unit =
    this.model = model
    if model.videoType == VideoType.PAL then
      statusRegister |= 1
    else
      statusRegister &= ~1

  def getPatternAAddress: Int = REG_PATTERN_A_ADDRESS
  def getPatternBAddress: Int = REG_PATTERN_B_ADDRESS
  def getPatternWindowAddress: Int = REG_PATTERN_WINDOW_ADDRESS
  def getPlayfieldSize: (Int,Int) = (REG_HSCROLL_SIZE.cell,REG_VSCROLL_SIZE.cell)
  def getScreenCells: (Int,Int) = (hmode.cells,if REG_M2 then 30 else 28)

  def getSpritesDump(): VDPSpriteCacheDump = getSpritesDump(0,new mutable.HashSet[Int]).get
  private def getSpritesDump(i:Int,indexes:mutable.HashSet[Int]): Option[VDPSpriteCacheDump] =
    if indexes.contains(i) then None
    else
      indexes += i
      val dump = spriteCache(i).dump()
      val next = spriteCache(i).link
      if next == 0 then
        Some(dump)
      else
        dump.next = getSpritesDump(next,indexes)
        Some(dump)

  def setLayerAEnabled(enabled:Boolean): Unit = layerAEnabled = enabled
  def setLayerBEnabled(enabled:Boolean): Unit = layerBEnabled = enabled
  def setLayerSEnabled(enabled:Boolean): Unit = layerSEnabled = enabled

  /*
    writePendingFlag is cleared when the control port is read
   */
  final def readControlPort(): Int =
    log.info("Reading status register: %X",statusRegister)
    writePendingFlag = false
    // check REG_DE according to results of VDPFIFOTesting rom
    if REG_DE then
      statusRegister
    else
      statusRegister | STATUS_VB_MASK

  /*
   Byte-wide writes
   Writing to the VDP control or data ports is interpreted as a 16-bit
   write, with the LSB duplicated in the MSB. This is regardless of writing
   to an even or odd address.

   Byte-wide reads
   Reading from even VDP addresses returns the MSB of the 16-bit data,
   and reading from odd address returns the LSB

   The MMU must handle these cases and always call the Port methods appropriately.
   */
  final def writeDataPort(value:Int): Unit =
    writePendingFlag = false
    val validWrite = getVRAMAccessMode match
      case acc@(VRAM_WRITE|CRAM_WRITE|VSRAM_WRITE) =>
        log.info("writeDataPort(%d): address=%X = %X codeRegister=%d",acc,addressRegister,value,codeRegister)
        true
      case mode =>
        log.warning("writeDataPort: writing data when access mode is not writing: %d",mode)
        false
    if validWrite then
      val entry = FifoEntry(codeRegister,addressRegister,value)
      if !fifo.enqueue(entry) then
        if writeOverflowFIFOEntry != null then // can happen when writeDataPort is called twice in a row for a long data write and there's no space to set dtack to false
          if writeOverflowFIFOEntry2 != null then
            log.error("writeOverflowFIFOEntry2 != null must never happen")
          writeOverflowFIFOEntry2 = entry
        else
          writeOverflowFIFOEntry = entry
        log.info("writeDataPort: FIFO full, going to stop M68K...")
        m68k.setDTACK(false)

      if isDMAInProgress && !dmaFillWriteDone && REG_DMA_MODE == DMA_MODE.VRAM_FILL then
        dmaFillWriteDone = true

      updateTargetAddress()

  /*
   Setting up the VDP for read: after writing the read command the VDP will immediately fetch the desired value from its internal memory into a buffer (same FIFO? or separate?) for the CPU to read;
   once the CPU read that value the VDP reads the next value. And after each read it increments the address register. But especially when reading from VRAM (2 reads needed instead of 1?)
   and during active scan (VDP will need to wait until the next access slot) if the 68k immediately writes to the command port after reading a value from VRAM through the data port this can cause a conflict
   because the VDP will only increment the address after having fetched the next value; funny effects occur if the 68k tried to write to a 68k register,
   typically leading to the register number and value being ANDed with the address after increment, or with the address before increment and then incremented, before being used as new register number and value to write to.
   (Since a register write was already triggered by the 2 topmost bits it doesn’t matter if ANDing alters these 2 bits, the register write will still occur.)
   */
  final def readDataPort(): Int =
    writePendingFlag = false
    val readValue = pendingReadValue
    updateTargetAddress()
    performVRAMRead() // read next
    readValue

  final def readHVCounter: Int =
    if REG_M3 then
      latchedHVCounter
    else
      val vcounter = if interlaceModeEnabled then (this.vcounter << 1 | (this.vcounter & 0x100) >> 8) & 0xFF else this.vcounter
      vcounter << 8 | (hcounter >> 1) & 0xFF
  /*
    Register set
    |1 0 0 RS4 RS3 RS2 RS1 RS0|D7 D6 D5 D4 D3 D2 D1 D0|
    Address set
    1 write |CD1 CD0 A13 A12 A11 A10 A9 A8|A7 A6 A5 A4 A3 A2 A1 A0|
    2 write |0 0 0 0 0 0 0 0|CD5 CD4 CD3 CD2 0 0 A15 A14|
   */
  final def writeControlPort(value:Int): Unit =
    if pendingControlPortRequest && !writePendingFlag then
      //log.warning(s"New control port request before last one completed")
      pendingControlPortRequestLong = true
      pendingControlPortCommand = value << 16 | pendingControlPortCommand
    else
      pendingControlPortCommand = value

    pendingControlPortRequest = true


  private def processPendingControlPortCommand(): Unit =
    pendingControlPortRequest = false
    /*
     You cannot write to a VDP register if the pending flag is set to one,
     since the VDP is expecting the 2nd half of a command word.
     */
    if !writePendingFlag && (pendingControlPortCommand & 0xC000) == 0x8000 then
      val register = (pendingControlPortCommand >> 8) & 0x1F
      val value = pendingControlPortCommand & 0xFF
      log.info("Processing register write reg=%d value=%d",register,value)
      writeRegister(register,value)
    else
      if !writePendingFlag then
        writePendingFlag = true
        codeRegister = codeRegister & 0x3C | (pendingControlPortCommand >> 14) & 3 // update CD0-CD1 only
        val oldAdr = addressRegister
        addressRegister = addressRegister & 0xC000 | pendingControlPortCommand & 0x3FFF // update A0-A13 only
        //println(s"1st write: pendingControlPortCommand=${pendingControlPortCommand.toHexString} oldaddress=${oldAdr.toHexString} address=${addressRegister.toHexString}")
      else
        writePendingFlag = false
        codeRegister = (pendingControlPortCommand & 0x00F0) >> 2 | codeRegister & 3
        val oldAdr = addressRegister
        addressRegister = (pendingControlPortCommand & 3) << 14 | addressRegister & 0x3FFF
        //println(s"2nd write: pendingControlPortCommand=${pendingControlPortCommand.toHexString} oldaddress=${oldAdr.toHexString} address=${addressRegister.toHexString}")
        // check DMA bit CD5
        if (codeRegister & 0x20) != 0 && REG_M1_DMA_ENABLED then
          statusRegister |= STATUS_DMA_MASK
          //println(s"DMA request $REG_DMA_MODE: codeRegister=${codeRegister.toHexString} code=${codeRegister & 0xF} dmaCode=${codeRegister >> 6} address=${addressRegister.toHexString}")
        log.info("Preparing VRAM access: codeRegister=%X code=%d dmaCode=%d address=%X",codeRegister,codeRegister & 0xF,codeRegister >> 6,addressRegister)

        if (getVRAMAccessMode(codeRegister) & 1) == 0 then // VRAM/CRAM/VSRAM read request
          if isDMAInProgress then println("DMA in progress + READ")
          performVRAMRead()

  private def performVRAMRead(): Unit =
    var address = addressRegister & ~1 // consider even addresses
    getVRAMAccessMode(codeRegister) match
      case VRAM_READ =>
        // The data is always read in word units. A0 is ignored during the read; no swap of bytes occurs if A0=1.
        // Subsequent reads are from address incremented by REGISTER #15. A0 is used in calculation of the next address.
        val value = VRAM(address & 0xFFFF) << 8 | VRAM((address + 1) & 0xFFFF)
        log.info("VRAM read request: address=%X value=%X",address,value)
        pendingReadValue = value
      case CRAM_READ =>
        val value = CRAM(address & 0x7F) << 8 | CRAM((address + 1) & 0x7F)
        log.info("CRAM read request: address=%X value=%X",address,value)
        pendingReadValue = value & 0xEEE // TODO fifoData & 0xF111 | value & 0xEEE
      case VSRAM_READ =>
        address &= 0x7F
        val value = if address < 0x49 then
          VSRAM(address) << 8 | VSRAM(address + 1)
        else VSRAM(0) << 8 | VSRAM(0)
        log.info("VSRAM read request: address=%X value=%X",address,value)
        pendingReadValue = value & 0x7FF // TODO fifoData & 0xF800 | value & 0x7FF
      case _ =>
        // was not a read

  /*
   Writing to a VDP register will clear the code register.
   */
  private def writeRegister(reg:Int,value:Int): Unit =
    //codeRegister = 0
    log.info("Register %d write %X",reg,value)
    val oldValue = regs(reg)
    regs(reg) = value & 0xFF

    reg match
      case 0 => // REG #0 |0 0 L IE1 0 1 M3 DE|
        if ((oldValue ^ regs(reg)) & 0x2) > 0 then // M3 changed
          if REG_M3 then
            latchedHVCounter = (vcounter & 0xFF) << 8 | (hcounter >> 1) & 0xFF
        //println(s"DE=$REG_DE line=$activeDisplayLine REG0_DE=${value & 1}")
      //case 10 =>
      //  println(s"HCOUNTER=$value at $rasterLine enabled:$REG_IE1")
      case 12 => // REG #12 |RS0 0 0 0 S/TE LSM1 LSM0 RS1|
        val h32 = REG_H32
        interlaceMode = INTERLACE_MODE.fromOrdinal((value >> 1) & 3)
        interlaceMode match
          case INTERLACE_MODE.INTERLACE_2 =>
            interlaceModeEnabled = true
            display.setInterlaceMode(true)
            videoPixels = display.displayMem
            display.setClipArea(model.videoType.getClipArea(h40 = !h32).getInterlacedTuple)
          case _ =>
            display.setInterlaceMode(false)
            display.setClipArea(model.videoType.getClipArea(h40 = !h32).getTuple)
            videoPixels = display.displayMem
            interlaceModeEnabled = false
        val mode = if h32 then HMode.H32 else HMode.H40
        if h32 then
          display.setRenderingHints(RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        else
          display.setRenderingHints(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
        if hmode != mode then
          hmode = mode
          changeVDPClockDivider(hmode.initialClockDiv)
          log.info("HMode set to %s",hmode)
          println(s"HMode set to $hmode")
          display.setClipArea(model.videoType.getClipArea(h40 = !h32).getTuple)
          vdpAccessSlot = vdpAccessSlot % hmode.vramAccessMatrix.length
          // TODO avoid index out of bound errors
      case 17 => // REG #17 |RIGT 0 0 WHP5 WHP4 WHP3 WHP2 WHP1|
        vdpLayerMappingAddress(A).setWindowX(value)
        log.info("Window width set to %X",value)
      case 18 => // REG #18 |DOWN 0 0 WVP4 WVP3 WVP2 WVP1 WVP0|
        vdpLayerMappingAddress(A).setWindowY(value)
        log.info("Window height set to %X",value)
      case _ =>


  inline private def getVRAMAccessMode: Int = codeRegister & 0xF
  inline private def getVRAMAccessMode(code:Int): Int = code & 0xF
  inline private def isDMAInProgress: Boolean = (statusRegister & STATUS_DMA_MASK) != 0

  def getProperties: VDPPropertiesDump =
    val props = new ListBuffer[VDPProperty]

    props += VDPProperty("Reg 0",s"${"%02X".format(regs(0))}",s"L=$REG_L IE1=$REG_IE1 M3=$REG_M3","REG #0 |0 0 L IE1 0 1 M3 DD|",Some(0))
    props += VDPProperty("Reg 1",s"${"%02X".format(regs(1))}",s"DE=$REG_DE IE0=$REG_IE0 M1=$REG_M1_DMA_ENABLED M2=$REG_M2 M5=$REG_M5","REG #1 |VR DE IE0 M1 M2 M5 0 0|",Some(1))
    props += VDPProperty("Reg 2",s"${"%02X".format(regs(2))}",s"${"%04X".format(REG_PATTERN_A_ADDRESS)}","REG #2 |0 SA16 SA15 SA14 SA13 0 0 0|",Some(2))
    props += VDPProperty("Reg 3",s"${"%02X".format(regs(3))}",s"${"%04X".format(REG_PATTERN_WINDOW_ADDRESS)}","REG #3 |0 0 WD15 WD14 WD13 WD12 WD11 0|",Some(3))
    props += VDPProperty("Reg 4",s"${"%02X".format(regs(4))}",s"${"%04X".format(REG_PATTERN_B_ADDRESS)}","REG #4 |0 0 0 0 SB16 SB15 SB14 SB13|",Some(4))
    props += VDPProperty("Reg 5",s"${"%02X".format(regs(5))}",s"${"%04X".format(REG_SPRITE_ATTR_ADDRESS)}","REG #5 |AT16 AT15 AT14 AT13 AT12 AT11 AT10 AT9|",Some(5))
    props += VDPProperty("Reg 7",s"${"%02X".format(regs(7))}",s"PALETTE=$REG_BACKGROUND_PALETTE COLOR=$REG_BACKGROUND_COLOR","REG #7 |0 0 CPT1 CPT0 COL3 COL2 COL1 COL0|",Some(7))
    props += VDPProperty("Reg 10",s"${"%02X".format(regs(10))}","","REG #10 |BIT7 BIT6 BIT5 BIT4 BIT3 BIT2 BIT1 BIT0|",Some(10))
    props += VDPProperty("Reg 11",s"${"%02X".format(regs(11))}",s"IE2=$REG_IE2 VS=$REG_VSCR HS=$REG_HSCR","REG #11 |0 0 0 0 IE2 VS HS1 HS2|",Some(11))
    props += VDPProperty("Reg 12",s"${"%02X".format(regs(12))}",s"H32=$REG_H32 SHADOW=$REG_SHADOW_HIGHLIGHT_ENABLED INTERLACE=$interlaceMode","REG #12 |RS0 0 0 0 S/TE LSM1 LSM0 RS1|",Some(12))
    props += VDPProperty("Reg 13",s"${"%02X".format(regs(13))}",s"${"%04X".format(REG_HSCROLL_ADDRESS)}","REG #13 |0 HS16 HS15 HS14 HS13 HS12 HS11 HS10|",Some(13))
    props += VDPProperty("Reg 15",s"${"%02X".format(regs(15))}","","REG #15 |INC7 INC6 INC5 INC4 INC3 INC2 INC1 INC0|",Some(15))
    props += VDPProperty("Reg 16",s"${"%02X".format(regs(16))}",s"PLANE_WIDTH=$REG_HSCROLL_SIZE PLANE_HEIGHT=$REG_VSCROLL_SIZE","REG #16 |0 0 VSZ1 VSZ0 0 0 HSZ1 HSZ0|",Some(16))
    props += VDPProperty("Reg 17",s"${"%02X".format(regs(17))}",s"RIGHT=${(regs(17) & 0x80) > 0} SIZE=${regs(17) & 0x1F}","REG #17 |RIGT 0 0 WHP5 WHP4 WHP3 WHP2 WHP1|",Some(17))
    props += VDPProperty("Reg 18",s"${"%02X".format(regs(18))}",s"DOWN=${(regs(18) & 0x80) > 0} SIZE=${regs(18) & 0x1F}","REG #18 |DOWN 0 0 WVP4 WVP3 WVP2 WVP1 WVP0|",Some(18))
    props += VDPProperty("Reg 19",s"${"%02X".format(regs(19))}",s"DMA_COUNT=${"%04X".format(REG_DMA_COUNTER_LEN)}","REG #19 |LG7 LG6 LG5 LG4 LG3 LG2 LG1 LG0|",Some(19))
    props += VDPProperty("Reg 20",s"${"%02X".format(regs(20))}",s"DMA_COUNT=${"%04X".format(REG_DMA_COUNTER_LEN)}","REG #20 |LG15 LG14 LG13 LG12 LG11 LG10 LG9 LG8|",Some(20))
    props += VDPProperty("Reg 21",s"${"%02X".format(regs(21))}",s"DMA_ADDR=${"%04X".format(REG_DMA_SOURCE_ADDRESS)} DMA_LOW_ADDR=${"%04X".format(REG_DMA_LOW_SOURCE_ADDRESS)}","REG #21 |SA8 SA7 SA6 SA5 SA4 SA3 SA2 SA1| for 68k->vram or |S07 S06 S05 S04 S03 S02 S01 S00| for vram copy",Some(21))
    props += VDPProperty("Reg 22",s"${"%02X".format(regs(22))}",s"DMA_ADDR=${"%04X".format(REG_DMA_SOURCE_ADDRESS)} DMA_LOW_ADDR=${"%04X".format(REG_DMA_LOW_SOURCE_ADDRESS)}","REG #22 |SA16 SA15 SA14 SA13 SA12 SA11 SA10 SA9| for 68k->vram or |S15 S14 S13 S12 S11 S10 S09 S08| for vram copy",Some(22))
    props += VDPProperty("Reg 23",s"${"%02X".format(regs(23))}",s"DMA_ADDR=${"%04X".format(REG_DMA_SOURCE_ADDRESS)} DMA_LOW_ADDR=${"%04X".format(REG_DMA_LOW_SOURCE_ADDRESS)} DMA_MODE=$REG_DMA_MODE","REG #23 |DMD1 DMD0/SA23 SA22 SA21 SA20 SA19 SA18 SA17|",Some(23))

    props += VDPProperty("H mode",s"$hmode")
    props += VDPProperty("Raster line",s"$rasterLine")
    props += VDPProperty("Active display line",s"$activeDisplayLine")
    props += VDPProperty("Active display xpos",s"$activeDisplayXPos")
    props += VDPProperty("Absolute xpos",s"$xpos")
    props += VDPProperty("Is in active display",s"$isActiveDisplayArea")
    props += VDPProperty("Vertical blanking",s"$verticalBlanking")
    props += VDPProperty("H counter","%03X".format(hcounter))
    props += VDPProperty("V counter","%03X".format(vcounter))
    props += VDPProperty("H interrupt counter",s"$hInterruptCounter")
    props += VDPProperty("Address register",s"$addressRegister")
    props += VDPProperty("Code register",s"$codeRegister")
    props += VDPProperty("FIFO size",s"${fifo.length}")
    props += VDPProperty("Access slot",s"$vdpAccessSlot")

    VDPPropertiesDump(props.toArray,(reg,value) => writeRegister(reg,value))

  private def initRegisters(): Unit = {
    // TODO check if it's correct
    java.util.Arrays.fill(regs,0)
    writeRegister(0, 4)
    writeRegister(1, 20)
    writeRegister(2, 48)
    writeRegister(3, 60)
    writeRegister(4, 7)
    writeRegister(5, 108)
    writeRegister(6, 0)
    writeRegister(7, 0)
    writeRegister(8, 0)
    writeRegister(9, 0)
    writeRegister(10, 255)
  }

  // utility methods to read registers' bits
  // REG #0 |0 0 L IE1 0 1 M3 DE|
  inline private def REG_L: Boolean = (regs(0) & 0x20) > 0          // leftmost 8 pixels are blanked to background colour
  inline private def REG_IE1: Boolean = (regs(0) & 0x10) > 0        // Enable H interrupt (68000 Level 4)
  inline private def REG_M3: Boolean = (regs(0) & 0x2) > 0          // HV. Counter stop
  // REG #1 |VR DE IE0 M1 M2 M5 0 0|
  inline private def REG_VR: Boolean = (regs(1) & 0x80) > 0         // use 128kB of VRAM. Will not work correctly on standard consoles with 64kB VRAM
  /*
   The one in register 1 (well, it's register $0 actually) is just a bit to configure CYSNC as input instead of output.
   It produces a black screen on consoles where CSYNC is being used by video encoder obviously.
   So it's an analog thing actually, which is very easy (but quite useless) to emulate.
   The display enable bit is more important as it changes the internal behavior of VDP.
   The effect is also different : in first case, you just got a blank screen while in second case you got a screen filled with background color.
   */
  inline private def REG_DE_BLACK_SCREEN : Boolean = (regs(0) & 0x1) > 0
  inline private def REG_DE: Boolean = (regs(1) & 0x40) > 0         // Enable display
  inline private def REG_IE0: Boolean = (regs(1) & 0x20) > 0        // Enable V interrupt (68000 Level 6)
  inline private def REG_M1_DMA_ENABLED: Boolean =
    (regs(1) & 0x10) > 0                                            // DMA Enable
  inline private def REG_M2: Boolean = (regs(1) & 0x08) > 0         // V 30 cell mode (PAL mode)
  inline private def REG_M5: Boolean = (regs(1) & 0x04) > 0         // 1 = Mega Drive (mode 5) display; 0 = Master System (mode 4) display
  // REG #2 |0 0 SA15 SA14 SA13 0 0 0|
  inline private def REG_PATTERN_A_ADDRESS: Int = ((regs(2) >> 3) & 7) << 13
  // REG #3 |0 0 WD15 WD14 WD13 WD12 WD11 0|
  inline private def REG_PATTERN_WINDOW_ADDRESS: Int =
    var address = regs(3) >> 1
    if !REG_H32 then address &= 0xFE // WD11 should be 0 in H40 cell mode
    (address & 0x1F) << 11
  // REG #4 |0 0 0 0 0 SB15 SB14 SB13|
  inline private def REG_PATTERN_B_ADDRESS: Int = (regs(4) & 7) << 13
  // REG #5 |0 AT15 AT14 AT13 AT12 AT11 AT10 AT9|
  inline private def REG_SPRITE_ATTR_ADDRESS: Int =
    var address = regs(5) & 0x7F
    if !REG_H32 then address &= 0xFE // AT9 should be 0 in H 40 cell mode
    address << 9
  // REG #7 |0 0 CPT1 CPT0 COL3 COL2 COL1 COL0|
  inline private def REG_BACKGROUND_PALETTE: Int = (regs(7) >> 4) & 3
  inline private def REG_BACKGROUND_COLOR: Int = regs(7) & 0xF
  // REG #10 |BIT7 BIT6 BIT5 BIT4 BIT3 BIT2 BIT1 BIT0|
  inline private def REG_H_INT: Int = regs(10)
  // REG #11 |0 0 0 0 IE2 VSCR HSCR LSCR|
  inline private def REG_IE2: Boolean = (regs(11) & 0x8) > 0        // Enable external interrupt (68000 Level 2)
  inline private def REG_VSCR: VSCROLL_MODE = VSCROLL_MODE.fromOrdinal((regs(11) >> 2) & 1)
  inline private def REG_HSCR: HSCROLL_MODE = HSCROLL_MODE.fromOrdinal(regs(11) & 3)
  // REG #12 |RS0 0 0 0 S/TE LSM1 LSM0 RS1|
  inline private def REG_H32: Boolean = (regs(12) & 0x81) == 0x00 // RS1/RS0: 1 = 320 pixel (40 cell) wide mode; 0 = 256 pixel (32 cell) wide mode. Both bits must be the same.
  inline private def REG_SHADOW_HIGHLIGHT_ENABLED: Boolean = (regs(12) & 8) > 0
  // REG #13 |0 0 HS15 HS14 HS13 HS12 HS11 HS10|
  inline private def REG_HSCROLL_ADDRESS: Int = (regs(13) & 0x3F) << 10
  // REG #15 |INC7 INC6 INC5 INC4 INC3 INC2 INC1 INC0|
  inline private def REG_AUTO_INCREMENT: Int = regs(15)
  // REG #16 |0 0 VSZ1 VSZ0 0 0 HSZ1 HSZ0|
  inline private def REG_HSCROLL_SIZE: SCROLL_SIZE = SCROLL_SIZE.fromOrdinal(regs(16) & 3)
  inline private def REG_VSCROLL_SIZE: SCROLL_SIZE =
    import SCROLL_SIZE.*
    val hsize = regs(16) & 3
    (regs(16) >> 4) & 3 match
      case 0 =>
        if hsize == 2 then _1CELL else _32CELL
      case 1|2 =>
        if hsize == 2 then _1CELL
        else if hsize == 3 then _32CELL else _64CELL
      case 3 =>
        if hsize == 2 then _1CELL
        else if hsize == 3 then _32CELL
        else if hsize == 1 then _64CELL else _128CELL
        
  // REG #17 |RIGT 0 0 WHP5 WHP4 WHP3 WHP2 WHP1|
  inline private def REG_WINDOW_HPOS: Int = regs(17)
  // REG #18 |DOWN 0 0 WVP4 WVP3 WVP2 WVP1 WVP0|
  inline private def REG_WINDOW_VPOS: Int = regs(18)
  // REG #19 |LG7 LG6 LG5 LG4 LG3 LG2 LG1 LG0|
  // REG #20 |LG15 LG14 LG13 LG12 LG11 LG10 LG9 LG8|
  inline private def REG_DMA_COUNTER_LEN: Int = regs(20) << 8 | regs(19)
  // REG #21 |SA8 SA7 SA6 SA5 SA4 SA3 SA2 SA1| for 68k->vram or
  //         |S07 S06 S05 S04 S03 S02 S01 S00| for vram copy
  // REG #22 |SA16 SA15 SA14 SA13 SA12 SA11 SA10 SA9| for 68k->vram or
  //         |S15 S14 S13 S12 S11 S10 S09 S08| for vram copy
  // REG #23 |DMD1 DMD0/SA23 SA22 SA21 SA20 SA19 SA18 SA17|
  inline private def REG_DMA_SOURCE_ADDRESS: Int =
    if REG_DMA_MODE == DMA_MODE.MEMORY_TO_VRAM then
      (regs(23) & 0x7F) << 16 | REG_DMA_LOW_SOURCE_ADDRESS
    else
      REG_DMA_LOW_SOURCE_ADDRESS
  inline private def REG_DMA_LOW_SOURCE_ADDRESS: Int = regs(22) << 8 | regs(21)
  inline private def REG_DMA_MODE: DMA_MODE =
    val mode = (regs(23) >> 6) & 3
    mode match
      case 0|1 =>
        DMA_MODE.MEMORY_TO_VRAM
      case 2 =>
        DMA_MODE.VRAM_FILL
      case 3 =>
        DMA_MODE.VRAM_COPY

  // address/counter update methods
  private def updateTargetAddress(): Unit =
    addressRegister = (addressRegister + REG_AUTO_INCREMENT) & 0xFFFF

  private def updateDMACounterLen(): Boolean =
    val len = (REG_DMA_COUNTER_LEN - 1) & 0xFFFF
    regs(20) = len >> 8
    regs(19) = len & 0xFF
    len == 0

  inline private def updateVRAMByte(address:Int,value:Int): Unit =
    VRAM(address) = value
    // check address against [sprite address, sprite address + 640] range to check if ypos,hs,vs or link info is changed (first 4 bytes)
    val spriteAttrAddr = REG_SPRITE_ATTR_ADDRESS
    if address >= spriteAttrAddr && address < spriteAttrAddr + SPRITE_ATTRIBUTES_SIZE then
      val offset = address - spriteAttrAddr
      if (offset & 7) < 4 then
        val spriteIndex = offset >> 3
        spriteCache(spriteIndex).setCache(spriteAttrAddr + (spriteIndex << 3))

  /*
    DMA registers are modified "live", so their modified state is retained between DMA operations, and of course, the third DMA source register 0x17(23),
    which contains the DMD1/DMD0 flags in the upper bits,is never modified by the DMA state advance process, only the lower two are modified.
    This is what causes DMA transfers to "wrap" on a 0x20000 byte boundary (0x20000 bytes because there's no bit 0 for the source address).
   */
  private def updateDMASourceAddress(): Unit =
    // if 68K->VRAM the +1 is automatically +2 because the address starts with SA1
    val address = (REG_DMA_LOW_SOURCE_ADDRESS + 1) & 0xFFFF
    regs(21) = address & 0xFF
    regs(22) = (address >> 8) & 0xFF

  /*
    Every DMA operation also performs the exact same set of steps after it is advanced one step, which is to firstly add 1 to the lower 2 DMA source address registers,
    then to subtract 1 from the DMA length counter register,and then if the resulting DMA length counter is 0, clear CD5 in the command code register,
    which signals that a DMA operation is complete.
    Note that this means that the DMA source registers need to be advanced for a DMA fill, even though it doesn't use them.
   */
  private def dmaEpilogue(): Unit =
    updateDMASourceAddress()
    val dmaFinished = updateDMACounterLen()
    if dmaFinished then
      //codeRegister &= 0x1F // clear CD5
      statusRegister &= ~STATUS_DMA_MASK
      dmaFillWriteDone = false
      if m68KBUSRequsted then
        m68KBUSRequsted = false
        //m68k.setBUSAvailable(true)
        busArbiter.vdpRelease68KBUS()
      log.info("DMA %s finished",REG_DMA_MODE)
      //println(s"DMA $REG_DMA_MODE finished")

  private def doExternalAccessSlot(): Unit =
    if fifo.isEmpty then
      doDMA()

    if !fifo.isEmpty then
      doVRAMWriteWord()

  private def doDMA(): Unit =
    import DMA_MODE.*
    if isDMAInProgress then
      REG_DMA_MODE match
        case VRAM_FILL =>
          if dmaFillWriteDone then
            doDMAFill()
            dmaEpilogue()
        case VRAM_COPY =>
          doDMACopy()
          dmaEpilogue()
        case MEMORY_TO_VRAM =>
          doDMAMemory()
          dmaEpilogue()

  private def doDMAFill(): Unit =
    getVRAMAccessMode match
      case VRAM_WRITE =>
        /*
         Any write to the data port will then start a VRAM fill. The LSB of the
         data is written to the address specified, then the MSB is written to
         the adjacent address. The address register is incremented by the value
         in VDP register 15, and the upper 8 bits are written again to the next
         adjacent address, and so on.
         */
        val fifoEntry = fifo.getLastWritten

        if !fifoEntry.vramFirstByteWritten then
          fifoEntry.vramFirstByteWritten = true
          updateVRAMByte(addressRegister,fifoEntry.data & 0xFF)
          log.info("doDMAFill: first byte %X = %X",addressRegister,fifoEntry.data & 0xFF)

        updateVRAMByte(addressRegister ^ 1,(fifoEntry.data >> 8) & 0xFF)
        log.info("doDMAFill: VRAM => %X = %X",addressRegister ^ 1,(fifoEntry.data >> 8) & 0xFF)
        updateTargetAddress()
      /*
       When it comes to DMA fills to CRAM and VSRAM, there's a bug.
       When VRAM is the write target, DMA fill behaves as I've described above, but when CRAM or VSRAM is the write target,
       DMA fill seems to fail to latch the fill data correctly.
       The apparent effect you see is that instead of using the data in the last written FIFO slot,
       it uses the data in the next available FIFO slot, or in other words, the data that was written 4 writes ago to the data port.
       Apart from retrieving the data from the wrong write, the fill operation works, with a bonus in fact that it performs a full 2-byte write in each "step".
       This means you can perform a DMA fill to CRAM or VSRAM if you want, all you have to do is write the data you want to use for the fill 4 times,
       the first 3 of which you perform before setting up the fill, and the last one to trigger it.
      */
      case CRAM_WRITE =>
        val fifoEntry = fifo.getLastPopped
        val address = fifoEntry.address & 0x7E // ignore odd addresses
        writeByteCRAM(address,(fifoEntry.data >> 8) & 0xFF)
        writeByteCRAM(address + 1,fifoEntry.data & 0xFF)
        log.info("doDMAFill: CRAM addressRegister=%X; %X = %X",addressRegister,address,fifoEntry.data)
        updateTargetAddress()
      case VSRAM_WRITE =>
        val fifoEntry = fifo.getLastPopped
        val address = fifoEntry.address & 0x7E // ignore odd addresses
        writeByteVSRAM(address, (fifoEntry.data >> 8) & 0xFF)
        writeByteVSRAM(address + 1, fifoEntry.data & 0xFF)
        log.info("doDMAFill: VSRAM addressRegister=%X; %X = %X",addressRegister,address,fifoEntry.data)
        updateTargetAddress()
      case mode =>
        log.error("Unexpected doDMAFill memory target: %d",mode)
        println(s"Unexpected doDMAFill memory target: $mode")

  private def writeByteCRAM(_address:Int,value:Int): Unit =
    val address = _address & 0x7F
    log.info("Write CRAM byte: %X = %X",address,value)
    CRAM(address) = value

    val cramColorPalette = address >> 5
    val cramColorIndex = (address & 0x1F) >> 1
    val adr = address & 0x7E
    val color = CRAM(adr) << 8 | CRAM(adr + 1)
    CRAM_COLORS(cramColorPalette)(0)(cramColorIndex) = Palette.getColor(color)
    CRAM_COLORS(cramColorPalette)(1)(cramColorIndex) = Palette.getColor(color,Palette.PaletteType.SHADOW)
    CRAM_COLORS(cramColorPalette)(2)(cramColorIndex) = Palette.getColor(color,Palette.PaletteType.HIGHLIGHT)

  end writeByteCRAM

  /*
   Even though there are 40 words of VSRAM, the address register will wrap
   when it passes 7Fh. Writes to the addresses beyond 50h are ignored.
   */
  private def writeByteVSRAM(_address:Int,value:Int): Unit =
    val address = _address & 0x7F
    if address < VSRAM.length then
      VSRAM(address) = value
      log.info("Write VSRAM byte: %X = %X",address,value)
    else
      log.info("Writing VSRAM over its size: %X",address)

  /*
   At any rate, during the VDP update cycle, if CD5, DMD1, and DMD0 are all set, a DMA copy operation will advance one step,
   which simply involves reading a byte from the current target address in VRAM based on the DMA source address register, and writing that byte to VRAM using the current incremented command address register,
   which will then be incremented afterwards. Once the write has been performed.
   */
  private def doDMACopy(): Unit =
    if readCopyCache == -1 then
      val address = REG_DMA_SOURCE_ADDRESS // TODO check why ^ 1
      readCopyCache = VRAM(address)
      log.info("doDMACopy: 1st phase readCopyCache read from %X = %X",address,readCopyCache)
    else
      updateVRAMByte(addressRegister,readCopyCache) // TODO check why ^ 1
      log.info("doDMACopy: 2nd phase readCopyCache write to %X = %X",addressRegister,readCopyCache)
      readCopyCache = -1

  private def doDMAMemory(): Unit =
    if !m68KBUSRequsted then
      m68KBUSRequsted = true
      //m68k.setBUSAvailable(false)
      busArbiter.vdpRequest68KBUS()
    val data = m68KMemory.read(REG_DMA_SOURCE_ADDRESS << 1,Size.Word,MMU.VDP_MEM_OPTION)
    log.info("doDMAMemory: pushing codeRegister=%X address=%X data=%X",codeRegister,addressRegister,data)
    if !fifo.enqueue(FifoEntry(codeRegister,addressRegister,data)) then
      log.error("doDMAMemory: FIFO full")
    updateTargetAddress()


  private def doVRAMWriteWord(): Unit =
    val fifoEntry = fifo.peek
    val address = fifoEntry.address & ~1 // ignore A0
    getVRAMAccessMode(fifoEntry.commandCode) match
      case VRAM_WRITE => // 2 phases
        if fifoEntry.vramFirstByteWritten then
          fifo.dequeue()
          log.info("doVRAMWriteWord: remaining fifo entries: %d",fifo.length)
          if (fifoEntry.address & 1) == 1 then // swap low / high
            updateVRAMByte(address,fifoEntry.data & 0xFF)
            updateVRAMByte((address + 1) & 0xFFFF,(fifoEntry.data >> 8) & 0xFF)
            log.info("doVRAMWriteWord: 2nd byte; bytes swapped %X = %X",address,fifoEntry.data)
          else
            updateVRAMByte(address,(fifoEntry.data >> 8) & 0xFF)
            updateVRAMByte((address + 1) & 0xFFFF,fifoEntry.data & 0xFF)
            log.info("doVRAMWriteWord: 2nd byte; bytes %X = %X",address,fifoEntry.data)
        else
          fifoEntry.vramFirstByteWritten = true
          log.info("doVRAMWriteWord: 1st byte")
      case CRAM_WRITE =>
        fifo.dequeue()
        writeByteCRAM(address, (fifoEntry.data >> 8) & 0xFF)
        writeByteCRAM(address + 1, fifoEntry.data & 0xFF)
      case VSRAM_WRITE =>
        fifo.dequeue()
        writeByteVSRAM(address, (fifoEntry.data >> 8) & 0xFF)
        writeByteVSRAM(address + 1, fifoEntry.data & 0xFF)
      case mode =>
        log.error("doVRAMWriteWord: mode mismatch: %d",mode)

  // ==========================================================================================================

  private def doAccessSlotHScroll(): Unit =
    import HSCROLL_MODE.*
    if vdp4read.count == 0 then
      val address = REG_HSCR match
        case FULL =>
          REG_HSCROLL_ADDRESS
        case EACH_1_LINE =>
          REG_HSCROLL_ADDRESS + (activeDisplayLine << 2) // every line consumes 2 words = 4 bytes
        case EACH_1_CELL =>
          REG_HSCROLL_ADDRESS + ((activeDisplayLine & ~7) << 2)
        case PROHIBITED => // First eight lines
          /*
           A scroll mode setting of 01b is not valid; however the unlicensed version
           of Populous uses it. This mode is identical to per-line scrolling, however
           the VDP will only read the first sixteen entries in the scroll table for
           every line of the display.
           */
          REG_HSCROLL_ADDRESS + (activeDisplayLine & 7) << 2
      vdp4read.set(address)

    if vdp4read.readVRAMByte() then
      val buffer = vdp4read.buffer
      xscroll(A) = (buffer >>> 16) & 0x3FF
      xscroll(B) = buffer & 0x3FF
      val hscrollSize = REG_HSCROLL_SIZE

      var layer = A
      val vscrollSize = REG_VSCROLL_SIZE
      while layer <= B do
        val xscrollFine = xscroll(layer) & 7
        val hscrollCells = ((xscroll(layer) >> 3) + (if xscrollFine == 0 then 0 else 1)) & hscrollSize.mask
        val horizontalScrollOffset = hscrollSize.cell - hscrollCells
        vdpLayerMappingAddress(layer).set(
          horizontalScrollOffset,
          hscrollSize,
          vscrollSize
        )
        if xscrollFine > 0 && !vdpLayerMappingAddress(layer).isInsideWindow(activeDisplayLine >> 3) then
          vdpLayerPatternBuffer(layer).skip(8 - xscrollFine)

        layer += 1

    end if
  end doAccessSlotHScroll

  /*
    TODO: https://gendev.spritesmind.net/forum/viewtopic.php?f=22&t=737&start=30
    Ok, I took some time to test how it works on real hardware. i were right about VSRAM addresses $4E and $4C being used for vertical scrolling
    of the left-most column when partially scrolled horizontally, but it's not exactly as I thought.
    Here are the conclusions of my tests in column vscroll mode:
      1) When fine horizontal scrolling value is applied (hscroll % 16 != 0), the left-most column is partially displayed and the vertical scroll value applied is
         VSRAM[$4C] & VSRAM[$4E]. The same value is applied for both A & B planes.
      2) The above statement is only true in 40-cell mode. In 32-cell mode, the left-most column cannot be scrolled vertically,
         i.e vscroll value is fixed to zero for both planes. I verified that writing vscroll value to other VSRAM addresses had no effects as well.
      3) hscroll mode has no effect on the above statements
   */
  private def doAccessSlotLayerMapping(layer:Int): Unit =
    import VSCROLL_MODE.*
    if vdp4read.count == 0 then
      val hscrollSize = REG_HSCROLL_SIZE
      yscroll(layer) = REG_VSCR match
        case FULL =>
          val layerOffset = layer << 1
          (VSRAM(layerOffset) << 8 | VSRAM(layerOffset + 1)) & 0x3FF
        case EACH_2_CELL =>
          val layerOffset = layer << 1
          val _2cell = layerOffset + ((vdpLayerMappingAddress(layer).cellX >> 1) << 2)
          if _2cell < VSRAM.length then
            (VSRAM(_2cell) << 8 | VSRAM(_2cell + 1)) & 0x3FF
          else
            println(s"VSRAM overflow with EACH_2_CELL mode ${_2cell}")
            0
      if interlaceModeEnabled then
        yscroll(layer) >>= 1

      if vdpLayerMappingAddress(layer).setCellY(activeDisplayLine,yscroll(layer)) then yscroll(layer) = 0
      vdp4read.set(vdpLayerMappingAddress(layer).address)
    end if
    if vdp4read.readVRAMByte() then
      vdpLayer2CellMappingBuffer(layer) = vdp4read.buffer

    if (vdp4read.count & 1) == 0 then
      vdpLayerMappingAddress(layer).incCellX()
      vdp4read.modifyAddress(vdpLayerMappingAddress(layer).address)
  end doAccessSlotLayerMapping

  private def doAccessSlotLayerPattern(layer:Int): Unit =
    val map = vdpLayer2CellMappingBuffer(layer) >>> 16

    if vdp4read.count == 0 then
      val activeDisplayLineScrolled = activeDisplayLine + yscroll(layer)
      var line = if (map & PATTERN_V_MASK) != 0 then 7 ^ (activeDisplayLineScrolled & 7) else activeDisplayLineScrolled & 7 // vertical flip
      if interlaceModeEnabled then line = line << 1 | frameCount
      val hflip = (map & PATTERN_H_MASK) != 0
      var address = (map & 0x7FF) << interlaceMode.patternSizeShift | line << 2
      if hflip then address = (address + 3) & 0xFFFF
      vdp4read.set(address,hflip)

    if vdp4read.readVRAMByte() then
      var bit = 0
      val prpl = (map >> 9) & 0x70
      var buffer = vdp4read.buffer
      while bit < 8 do
        val patternBit = (buffer >>> 28) & 0x0F
        buffer <<= 4
        vdpLayerPatternBuffer(layer).enqueueBit(prpl | patternBit) // |pr|pl1|pl0|b3|b2|b1|b0|
        bit += 1

      vdpLayer2CellMappingBuffer(layer) <<= 16 // next entry
  end doAccessSlotLayerPattern

  private def doAccessSlotSpriteMapping(): Unit =
    val spriteIndex = if sprite1VisibleSR.getSize > 0 then sprite1VisibleSR.peekIndex() else -1
    if vdp4read.count == 0 then
      // For unused sprite slots sprite 0 is accessed but the data read is discarded
      val si = if spriteIndex == -1 then 0 else spriteIndex
      vdp4read.set(REG_SPRITE_ATTR_ADDRESS + (spriteIndex << 3) + 4) // ignores first 4 bytes
      if sprite1FirstFetch then
        sprite2CurrentIndex = 0
        sprite2Size = sprite1VisibleSR.getSize
        sprite1FirstFetch = false

    if vdp4read.readVRAMByte() && spriteIndex != -1 then
      sprite1VisibleSR.dequeueIndex()
      sprite2Info(sprite1VisibleCurrentIndex).set(spriteIndex,vdp4read.buffer >>> 16,vdp4read.buffer & 0x1FF)
      val spriteZeroXPos = sprite2Info(sprite1VisibleCurrentIndex).isZeroPos
      spriteLineRenderingEnabled &= !(!lastSpriteXPosZero && spriteZeroXPos)
      lastSpriteXPosZero = spriteZeroXPos
      // go to next sprite
      sprite1VisibleCurrentIndex += 1
  end doAccessSlotSpriteMapping

  /*
    048C
    159D
    26AE
    37BF
   */
  private def doAccessSlotSpritePattern(): Unit =
    val spriteInfo = sprite2Info(if sprite2CurrentIndex < sprite2Size then sprite2CurrentIndex else 0)

    if vdp4read.count == 0 && spriteInfo.isFirstHorizontalCell then
      vdp4read.set(spriteInfo.patternAddress,spriteInfo.horizontalFlipped)

    if vdp4read.readVRAMByte() && sprite2CurrentIndex < sprite2Size then
      if spriteLineRenderingEnabled then
        var xpos = spriteInfo.xpos
        var buffer = vdp4read.buffer
        var bit = 0
        val activeWidth = hmode.activePixels
        val prpl = spriteInfo.priorityAndPalette << 4
        while bit < 8 do
          val patternBit = (buffer >>> 28) & 0x0F
          buffer <<= 4
          if patternBit != 0 && xpos >= 0 && xpos < activeWidth then // ok sprite is visible
            val pattern = vdpLayerPatternBuffer(S).get(xpos)
            if (pattern & 0xF) == 0 then
              vdpLayerPatternBuffer(S).put(xpos,prpl | patternBit)
            else // collision between 2 non transparent pixels
              statusRegister |= STATUS_C_MASK
          bit += 1
          xpos += 1
        end while
      if spriteInfo.incCell() then
        sprite2CurrentIndex += 1
      else
        vdp4read.set(spriteInfo.patternAddress,spriteInfo.horizontalFlipped)
    end if
  end doAccessSlotSpritePattern

  inline private def doAccessSlotRead(): Boolean =
    import VRAMAccess.*
    if REG_DE && !verticalBlanking && (!isVerticalBorder || isSpriteFirstEvaluationLine) then
      hmode.vramAccessMatrix(vdpAccessSlot) match
        case H__ =>
          if !isSpriteFirstEvaluationLine then doAccessSlotHScroll() else vdp4read.incCount()
        case A_M =>
          if !isSpriteFirstEvaluationLine then doAccessSlotLayerMapping(layer = A) else vdp4read.incCount()
        case B_M =>
          if !isSpriteFirstEvaluationLine then doAccessSlotLayerMapping(layer = B) else vdp4read.incCount()
        case A_P =>
          if !isSpriteFirstEvaluationLine then doAccessSlotLayerPattern(layer = A) else vdp4read.incCount()
        case B_P =>
          if !isSpriteFirstEvaluationLine then doAccessSlotLayerPattern(layer = B) else vdp4read.incCount()
        case S_M =>
          doAccessSlotSpriteMapping()
        case S_P =>
          doAccessSlotSpritePattern()
        case REF =>
          /* do nothing ? */
          vdp4read.incCount()
        case EXT =>
          if vdp4read.count == 0 then
            doExternalAccessSlot()

          vdp4read.incCount()
    else
      hmode.vramAccessMatrix(vdpAccessSlot) match
        case REF =>
          /* do nothing ? */
        case _ =>
          if vdp4read.count == 0 then
            doExternalAccessSlot()

      vdp4read.incCount()
    end if

    val slotCompleted = vdp4read.count == 4
    if slotCompleted then
      vdp4read.reset()

    slotCompleted
  end doAccessSlotRead

  private def endOfFrame(): Unit =
    vdpLayerPatternBuffer(S).clear()
    rasterLine = 0
    activeDisplayLine = 0
    vcounter = model.videoType.topBlankingInitialVCounter(REG_M2)
    //hInterruptCounter = REG_H_INT

    if interlaceModeEnabled then
      if frameCount == 0 then
        display.showFrame()
      else
        display.showFrame(0, 0, 0, 0,updateFrameRateOnly = true)
    else if pixelMod then
      display.showFrame(minModX, minModY, maxModX + 1, maxModY + 1)
    else
      display.showFrame(0, 0, 0, 0,updateFrameRateOnly = true)
    minModY = -1
    minModX = Integer.MAX_VALUE
    maxModX = 0
    pixelMod = false
    frameCount ^= 1

    sprite1VisibleSR.reset()
    sprite1VisibleNextLineSR.reset()

  private def endOfLine(): Unit =
    val videoType = model.videoType
    val v30 = REG_M2
    val bottomBorderPos = videoType.bottomBorderPos(v30)

    vdp4read.reset()

    //sprite1VisibleCurrentIndex = 0
    statusRegister &= ~STATUS_SOVR_MASK // TODO check if it's correct
    statusRegister &= ~STATUS_C_MASK    // TODO check if it's correct

    vdpLayerPatternBuffer(A).reset()
    vdpLayerPatternBuffer(B).reset()
    vdpLayerPatternBuffer(S).reset()

    xpos = 0
    hcounter = hmode.hCounterInitialValue
    rasterLine += 1

    if inYActiveDisplay then
      activeDisplayLine += vcounterInc
      if activeDisplayLine == (if v30 then 240 else 224) then
        inYActiveDisplay = false

    vcounterInc = 0

    verticalBlanking = rasterLine < videoType.topBlankingPixels || rasterLine >= bottomBorderPos + videoType.bottomBorderPixels
    isVerticalBorder = !verticalBlanking && (rasterLine >= bottomBorderPos || rasterLine < videoType.topBlankingPixels + videoType.topBorderPixels)

    if rasterLine == model.videoType.totalLines then
      endOfFrame()

    //if rasterLine > 224 then
    //  hInterruptCounter = REG_H_INT

    vdpAccessSlot = 0
    spriteLineRenderingEnabled = true
    spriteEvaluationPhaseInProgress = SPRITE_EVAL_IDLE

    val tmp = sprite1VisibleNextLineSR
    sprite1VisibleNextLineSR = sprite1VisibleSR
    sprite1VisibleSR = tmp
    sprite1FirstFetch = true
  end endOfLine

  inline private def interlaceActiveDisplayLine : Int =
    if interlaceModeEnabled then
      activeDisplayLine << 1 | frameCount
    else
      activeDisplayLine

  inline private def isSpriteFirstEvaluationLine: Boolean = vcounter == 0x1FE
  inline private def isActiveDisplayArea: Boolean = inXActiveDisplay && inYActiveDisplay
  inline private def isSpriteEvaluationEnabled : Boolean = REG_DE && (inYActiveDisplay || isSpriteFirstEvaluationLine)

  // colorIndex < 16, palette < 4
  inline private def getColor(colorIndex:Int,palette:Int): Int =
    val address = palette << 5 | colorIndex << 1
    CRAM(address) << 8 | CRAM(address + 1)

  inline private def setPixel(x:Int,y:Int,pixelColor:Int): Unit =
    val iy = if interlaceModeEnabled then y << 1 | frameCount else y
    val pos = iy * SCREEN_WIDTH + x

    if interlaceModeEnabled then
      videoPixels(pos) = pixelColor
    else if videoPixels(pos) != pixelColor then
      videoPixels(pos) = pixelColor
      if (x < minModX) minModX = x
      else if (x > maxModX) maxModX = x
      if (minModY == -1) minModY = iy
      maxModY = iy
      pixelMod = true

  inline private def checkSpriteHS(pixelIndex:Int,palette:Int,color:Int,priorities:Int): Boolean =
    val spriteColor = palette << 4 | color
    var recheckPixels = false
    // Sprite color 63: S is not drawn and the A/B/G color becomes shadowed if it isn't already
    if spriteColor == 63 then
      colorMode = colorMode.darker()
      layerPixels(pixelIndex) = 0 // sprite pixel is not drawn
      recheckPixels = true
    // Sprite color 62: S is not drawn and the A/B/G color becomes highlighted if it was normal and normal if it was shadowed
    else if spriteColor == 62 then
      colorMode = colorMode.brighter()
      layerPixels(pixelIndex) = 0 // sprite pixel is not drawn
      recheckPixels = true
    // Otherwise S is drawn normally, or dark when all 3 S+A+B are low prio and S is not of either of the colors 14,30,46
    else if priorities == 0 && (spriteColor != 14 && spriteColor != 30 && spriteColor != 46) then
      colorMode = colorMode.darker()

    recheckPixels

  inline private def pixelClock(): Unit =
    val videoType = model.videoType
    val v30 = REG_M2
    val bottomBorderPos = videoType.bottomBorderPos(v30)
    val isBlanking = verticalBlanking || (!inXActiveDisplay && xborderCount == 0) || REG_DE_BLACK_SCREEN

    if isBlanking then
      setPixel(xpos,rasterLine,BLANK_COLOR)
    else if xborderCount > 0 || isVerticalBorder then
      setPixel(xpos, rasterLine, CRAM_COLORS(REG_BACKGROUND_PALETTE)(0)(REG_BACKGROUND_COLOR))
    else
      val backgroundColor = REG_BACKGROUND_COLOR
      val backgroundPalette = REG_BACKGROUND_PALETTE
      var color = 0
      var palette = 0
      val hsEnabled = REG_SHADOW_HIGHLIGHT_ENABLED
      colorMode = Palette.PaletteType.NORMAL
      var spritePixelIndex = 0

      if REG_DE then
        var bitA = vdpLayerPatternBuffer(A).dequeueBit()
        var bitB = vdpLayerPatternBuffer(B).dequeueBit()
        var bitS = vdpLayerPatternBuffer(S).dequeueBitAndClear()

        if !layerAEnabled then bitA = 0
        if !layerBEnabled then bitB = 0
        if !layerSEnabled then bitS = 0

        if !(REG_L && activeDisplayXPos < 8) then
          val priorities = (bitS & 0x40) >> 4 | (bitA & 0x40) >> 5 | (bitB & 0x40) >> 6 // SAB

          if hsEnabled && (priorities & 3) == 0 then
            colorMode = Palette.PaletteType.SHADOW

          priorities match
            case 0|4|6|7 => // S > A > B > G
              layerPixels(0) = bitS
              layerPixels(1) = bitA
              layerPixels(2) = bitB
            case 2 => // A > S > B > G
              layerPixels(0) = bitA
              layerPixels(1) = bitS
              layerPixels(2) = bitB
              spritePixelIndex = 1
            case 1 => // B > S > A > G
              layerPixels(0) = bitB
              layerPixels(1) = bitS
              layerPixels(2) = bitA
              spritePixelIndex = 1
            case 5 => // S > B > A > G
              layerPixels(0) = bitS
              layerPixels(1) = bitB
              layerPixels(2) = bitA
            case 3 => // A > B > S > G
              layerPixels(0) = bitA
              layerPixels(1) = bitB
              layerPixels(2) = bitS
              spritePixelIndex = 2

          var checkAgain = true
          while checkAgain do
            checkAgain = false
            var pixelColor = layerPixels(0) & 0xF
            if pixelColor != 0 then
              color = pixelColor
              palette = (layerPixels(0) >> 4) & 3
              if hsEnabled && spritePixelIndex == 0 then
                checkAgain = checkSpriteHS(0,palette, color, priorities)
            else
              pixelColor = layerPixels(1) & 0xF
              if pixelColor != 0 then
                color = pixelColor
                palette = (layerPixels(1) >> 4) & 3
                if hsEnabled && spritePixelIndex == 1 then
                  checkAgain = checkSpriteHS(1,palette, color, priorities)
              else
                pixelColor = layerPixels(2) & 0xF
                if pixelColor != 0 then
                  color = pixelColor
                  palette = (layerPixels(2) >> 4) & 3
                  if hsEnabled && spritePixelIndex == 2 then
                    checkAgain = checkSpriteHS(2,palette, color, priorities)
                else
                  color = backgroundColor
                  palette = backgroundPalette
          end while
        end if
      end if

      setPixel(xpos, rasterLine, CRAM_COLORS(palette)(colorMode.ordinal)(color)) // TODO
    end if
    // epilogue
    xpos += 1
    hcounter = (hcounter + 1) & 0x1FF // 9-bit counter

    if xpos >= hmode.totalWidth then
      endOfLine()
    else if inXActiveDisplay then
      activeDisplayXPos += 1
      if activeDisplayXPos >= hmode.activePixels then
        inXActiveDisplay = false
        xborderCount = hmode.rightBorderPixel
    else if xborderCount > 0 then
      xborderCount -= 1
      if xborderCount == 0 then
        if xborderIsLeft then
          xborderIsLeft = false
          inXActiveDisplay = true
          if !isVerticalBorder && !inYActiveDisplay then
            inYActiveDisplay = true
            activeDisplayLine = 0
          activeDisplayXPos = 0

    if checkHCounter() then
      checkVCounter()
  end pixelClock

  // =============== Interrupt handling ==========================
  private def generateVInterrupt(): Unit =
    statusRegister |= STATUS_F_MASK
    if REG_IE0 then
      m68k.interrupt(VINT_LEVEL)
      vInterruptPending = true
    /*
     The Z80 will receive an IRQ from the VDP on scanline E0h. This happens
     once per frame, every frame, regardless of frame interrupts being
     disabled by the 68000
     */
    z80.irq(low = true, im2LowByte = 0xFF)

  private def generateHInterrupt(): Unit =
    m68k.interrupt(HINT_LEVEL)
    hInterruptPending = true
  // =============================================================

  // returns true if vcounter must be incremented
  inline private def checkHCounter(): Boolean =
    if hcounter == hmode.hCounterFirstVisible && !verticalBlanking then
      xborderCount = hmode.leftBorderPixel
      xborderIsLeft = true
    else if hcounter == hmode.backPorchJump._1 then
      hcounter = hmode.backPorchJump._2
    else if hcounter == hmode.hBlankSet then
      statusRegister |= STATUS_HB_MASK
    else if hcounter == hmode.hBlankCleared then
      statusRegister &= ~STATUS_HB_MASK
    val clockDiv = hmode.clockAdjustFun(hcounter)
    if clockDiv != -1 then
      changeVDPClockDivider(clockDiv)

    val v30 = REG_M2
    val fFlagSetAt = model.videoType.fFlagSetAt(v30)
    if vcounter == fFlagSetAt && hcounter == hmode.fFlagSet then
      generateVInterrupt()
    else if vcounter == fFlagSetAt + 1 && hcounter == hmode.fFlagSet then
      z80.irq(low = false) // is it correct ?

    hcounter == hmode.vCounterIncrement

  inline private def checkVCounter(): Unit =
    // check horizontal counter
    hInterruptCounter -= 1

    if (statusRegister & STATUS_VB_MASK) != 0 then
      hInterruptCounter = REG_H_INT
    else if hInterruptCounter < 0 then
      if REG_IE1 then
        generateHInterrupt()
      hInterruptCounter = REG_H_INT


    vcounter = (vcounter + 1) & 0x1FF // 9-bit counter
    vcounterInc += 1

    val v30 = REG_M2
    val videoType = model.videoType
    
    if vcounter == videoType.vBlankSetAt(v30) then
      statusRegister |= STATUS_VB_MASK
    else if vcounter == videoType.vBlankClearedAt then
      statusRegister &= ~STATUS_VB_MASK

  inline private def changeVDPClockDivider(clockDiv:Int): Unit =
    //masterClock.setVDPClockDivider(clockDiv)
    masterClock.setClockDivider(0,clockDiv)

  /*
   Every VDP cycle 2 sprites are checked, that means one sprite every pixel.
   This scanning seems to start 6 cycles after the first sprite tile fetch and continues for exactly 40 cycles
   */
  private def spriteEvaluation(): Unit =
    var c = 0
    val line = activeDisplayLine + 1 // check if some sprite is visible on next line
    // 2 sprites per cycle
    while c < 2 && spriteEvaluationPhaseInProgress == SPRITE_EVAL_IN_PROGRESS do
      val spriteIndex = spriteEvaluationIndex
      val sy = spriteCache(spriteIndex).y - 128
      val height = (spriteCache(spriteIndex).h + 1) << 3 // 8 * sprite height pixels
      spriteEvaluationIndex = spriteCache(spriteIndex).link
      if !(spriteEvaluationIndex != 0 && spriteEvaluationIndex < spriteCache.length && spriteEvaluationCycles < 40) then
        spriteEvaluationPhaseInProgress = SPRITE_EVAL_TERMINATED

      if line >= sy && line < sy + height then
        if sprite1VisibleNextLineSR.getSize == hmode.maxSpritePerLine then
          statusRegister |= STATUS_SOVR_MASK
          spriteEvaluationPhaseInProgress = SPRITE_EVAL_TERMINATED
        else
          sprite1VisibleNextLineSR.enqueueIndex(spriteIndex)
          //println(s"Sprite $spriteIndex visible at line $line: sy=$sy height=$height")

      c += 1
    end while
    spriteEvaluationCycles += 1

  override final def clock(cycles: Long): Unit =
      if pendingControlPortRequest then
        if pendingControlPortRequestLong then
          pendingControlPortRequestLong = false
          val next = pendingControlPortCommand >>> 16
          pendingControlPortCommand &= 0xFFFF
          processPendingControlPortCommand()
          pendingControlPortCommand = next
          processPendingControlPortCommand()
        else
          processPendingControlPortCommand()
      // check if it's time to do sprite evaluation
      if vdpAccessSlot == SPRITE_PHASE1_ACCESS_SLOT && spriteEvaluationPhaseInProgress == SPRITE_EVAL_IDLE && isSpriteEvaluationEnabled then
        spriteEvaluationPhaseInProgress = SPRITE_EVAL_IN_PROGRESS
        spriteEvaluationIndex = 0 // start with sprite #0
        spriteEvaluationCycles = 0
      if spriteEvaluationPhaseInProgress == SPRITE_EVAL_IN_PROGRESS then
        spriteEvaluation()
        if spriteEvaluationPhaseInProgress == SPRITE_EVAL_TERMINATED then
          sprite1VisibleCurrentIndex = 0

      if doAccessSlotRead() then // 4 bytes read
        vdpAccessSlot += 1
        if vdpAccessSlot >= hmode.vramAccessMatrix.length then
          vdpAccessSlot = 0
        //println(s"raster=$rasterLine vdpAccessSlot=$vdpAccessSlot")


      if (cycles & 1) == 1 then
        pixelClock()

  end clock

  // interrupt ack from M68000
  override final def intAcknowledged(level:Int): Unit =
    level match
      case HINT_LEVEL =>
        hInterruptPending = false
      case VINT_LEVEL =>
        vInterruptPending = false
        statusRegister &= ~STATUS_F_MASK
      case _ =>
        log.error("Unknown interrupt ack level: %d",level)

    var intLevel = 0
    if hInterruptPending then
      intLevel |= HINT_LEVEL
    if vInterruptPending then
      intLevel |= VINT_LEVEL

    log.info("VDP ack interrupt level %d: new level is %d",level,intLevel)
    m68k.interrupt(intLevel)

  end intAcknowledged






