package ucesoft.smd

import java.awt.Dimension

/**
 * @author Alessandro Abbruzzetti
 *         Created on 30/08/2023 16:41  
 */
enum ModelType:
  case Domestic, Oversea

//-------------------------------------------------------------------------------------------
//|           Video |NTSC             |NTSC             |PAL              |PAL              |
//|            Mode |H32/H40(RSx00/11)|H32/H40(RSx00/11)|H32/H40(RSx00/11)|H32/H40(RSx00/11)|
//|                 |V28     (M2=0)   |V30     (M2=1)   |V28     (M2=0)   |V30     (M2=1)   |
//|                 |Int none(LSMx=*0)|Int none(LSMx=*0)|Int none(LSMx=*0)|Int none(LSMx=*0)|
//|                 |------------------------------------------------------------------------
//|                 | VCounter  |Line | VCounter  |Line | VCounter  |Line | VCounter  |Line |
//| Screen section  |  value    |count|  value    |count|  value    |count|  value    |count|
//|-----------------|-----------|-----|-----------|-----|-----------|-----|-----------|-----|
//|Active display   |0x000-0x0DF| 224 |0x000-0x1FF| 240*|0x000-0x0DF| 224 |0x000-0x0EF| 240 |
//|-----------------|-----------|-----|-----------|-----|-----------|-----|-----------|-----|
//|Bottom border    |0x0E0-0x0E7|   8 |           |   0 |0x0E0-0x0FF|  32 |0x0F0-0x107|  24 |
//|-----------------|-----------|-----|-----------|-----|-----------|-----|-----------|-----|
//|Bottom blanking  |0x0E8-0x0EA|   3 |           |   0 |0x100-0x102|   3 |0x108-0x10A|   3 |
//|-----------------|-----------|-----|-----------|-----|-----------|-----|-----------|-----|
//|Vertical sync    |0x1E5-0x1E7|   3 |           |   0 |0x1CA-0x1CC|   3 |0x1D2-0x1D4|   3 |
//|-----------------|-----------|-----|-----------|-----|-----------|-----|-----------|-----|
//|Top blanking     |0x1E8-0x1F4|  13 |           |   0 |0x1CD-0x1D9|  13 |0x1D5-0x1E1|  13 |
//|-----------------|-----------|-----|-----------|-----|-----------|-----|-----------|-----|
//|Top border       |0x1F5-0x1FF|  11 |           |   0 |0x1DA-0x1FF|  38 |0x1E2-0x1FF|  30 |
//|-----------------|-----------|-----|-----------|-----|-----------|-----|-----------|-----|
//|TOTALS           |           | 262 |           | 240*|           | 313 |           | 313 |
//-------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
//|        Video |NTSC             |NTSC             |NTSC             |NTSC             |
//|         Mode |H32/H40(RSx00/11)|H32/H40(RSx00/11)|H32/H40(RSx00/11)|H32/H40(RSx00/11)|
//|              |V28     (M2=0)   |V28     (M2=0)   |V30     (M2=1)   |V30     (M2=1)   |
//| Event        |Int none(LSMx=*0)|Int both(LSMx=*1)|Int none(LSMx=*0)|Int both(LSMx=*1)|
//|--------------------------------------------------------------------------------------|
//|VCounter      |[1]0x000-0x0EA   |[1]0x000-0x0EA   |[1]0x000-0x1FF   |[1]0x000-0x1FF   |
//|progression   |[2]0x1E5-0x1FF   |[2]0x1E4(#ODD)   |                 |                 |
//|9-bit internal|                 |[3]0x1E5-0x1FF   |                 |                 |
//|--------------------------------------------------------------------------------------|
//|VBlank set    |VCounter changes |                 |VCounter changes |                 |
//|              |from 0x0DF to    |     <Same>      |from 0x0EF to    |     <Same>      |
//|              |0x0E0 in [1].    |                 |0x0F0 in [1].    |                 |
//|--------------------------------------------------------------------------------------|
//|VBlank cleared|VCounter changes |                 |VCounter changes |                 |
//|              |from 0x1FE to    |     <Same>      |from 0x1FE to    |     <Same>      |
//|              |0x1FF in [2].    |                 |0x1FF in [1].    |                 |
//|--------------------------------------------------------------------------------------|
//|F flag set    |At indicated     |                 |At indicated     |                 |
//|              |HCounter position|                 |HCounter position|                 |
//|              |while VCounter is|     <Same>      |while VCounter is|     <Same>      |
//|              |set to 0x0E0 in  |                 |set to 0x0F0 in  |                 |
//|              |[1].             |                 |[1].             |                 |
//|--------------------------------------------------------------------------------------|
//|VSYNC asserted|VCounter changes |                 |      Never      |                 |
//|              |from 0x0E7 to    |     <Same>      |                 |     <Same>      |
//|              |0x0E8 in [1].    |                 |                 |                 |
//|--------------------------------------------------------------------------------------|
//|VSYNC cleared |VCounter changes |                 |      Never      |                 |
//|              |from 0x1F4 to    |     <Same>      |                 |     <Same>      |
//|              |0x1F5 in [2].    |                 |                 |                 |
//|--------------------------------------------------------------------------------------|
//|ODD flag      |At indicated     |                 |At indicated     |                 |
//|toggled       |HCounter position|                 |HCounter position|                 |
//|              |while VCounter is|     <Same>      |while VCounter is|     <Same>      |
//|              |set to 0x0E0 in  |                 |set to 0x0F0 in  |                 |
//|              |[1].             |                 |[1].             |                 |
//----------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------
//|        Video |PAL              |PAL              |PAL              |PAL              |
//|         Mode |H32/H40(RSx00/11)|H32/H40(RSx00/11)|H32/H40(RSx00/11)|H32/H40(RSx00/11)|
//|              |V28     (M2=0)   |V28     (M2=0)   |V30     (M2=1)   |V30     (M2=1)   |
//| Event        |Int none(LSMx=*0)|Int both(LSMx=*1)|Int none(LSMx=*0)|Int both(LSMx=*1)|
//|--------------------------------------------------------------------------------------|
//|VCounter      |[1]0x000-0x102   |[1]0x000-0x101   |[1]0x000-0x10A   |[1]0x000-0x109   |
//|progression   |[2]0x1CA-0x1FF   |[2]0x1C9(#ODD)   |[2]0x1D2-0x1FF   |[2]0x1D1(#ODD)   |
//|9-bit internal|                 |[3]0x1CA-0x1FF   |                 |[3]0x1D2-0x1FF   |
//|--------------------------------------------------------------------------------------|
//|VBlank set    |VCounter changes |                 |VCounter changes |                 |
//|              |from 0x0DF to    |     <Same>      |from 0x0EF to    |     <Same>      |
//|              |0x0E0 in [1].    |                 |0x0F0 in [1].    |                 |
//|--------------------------------------------------------------------------------------|
//|VBlank cleared|VCounter changes |                 |VCounter changes |                 |
//|              |from 0x1FE to    |     <Same>      |from 0x1FE to    |     <Same>      |
//|              |0x1FF in [2].    |                 |0x1FF in [2].    |                 |
//|--------------------------------------------------------------------------------------|
//|F flag set    |At indicated     |                 |At indicated     |                 |
//|              |HCounter position|                 |HCounter position|                 |
//|              |while VCounter is|     <Same>      |while VCounter is|     <Same>      |
//|              |set to 0x0E0 in  |                 |set to 0x0F0 in  |                 |
//|              |[1].             |                 |[1].             |                 |
//|--------------------------------------------------------------------------------------|
//|VSYNC asserted|VCounter changes |                 |VCounter changes |                 |
//|              |from 0x0FF to    |     <Same>      |from 0x107 to    |     <Same>      |
//|              |0x100 in [1].    |                 |0x108 in [1].    |                 |
//|--------------------------------------------------------------------------------------|
//|VSYNC cleared |VCounter changes |                 |VCounter changes |                 |
//|              |from 0x1D9 to    |     <Same>      |from 0x1E1 to    |     <Same>      |
//|              |0x1DA in [2].    |                 |0x1E2 in [2].    |                 |
//|--------------------------------------------------------------------------------------|
//|ODD flag      |At indicated     |                 |At indicated     |                 |
//|toggled       |HCounter position|                 |HCounter position|                 |
//|              |while VCounter is|     <Same>      |while VCounter is|     <Same>      |
//|              |set to 0x0E0 in  |                 |set to 0x0F0 in  |                 |
//|              |[1].             |                 |[1].             |                 |
//----------------------------------------------------------------------------------------
case class DisplayClipArea(x1:Int,y1:Int,x2:Int,y2:Int):
  def getTuple: (Int,Int,Int,Int) = (x1,y1,x2,y2)
  def getPreferredSize(zoomFactor:Int): Dimension = new Dimension((x2 - x1) * zoomFactor,(y2 - y1) * zoomFactor)
  def getInterlacedTuple: (Int,Int,Int,Int) = (x1,y1 << 1,x2,y2 << 1) 
enum VideoType(val clockFrequency:Int,
               val topBlankingPixels:Int,
               val topBorderPixels:Int,
               val bottomBorderPixels:Int,
               val bottomBlankingPixels:Int,
               val totalLines:Int,
               val maxActiveLines:Int):
  val vBlankClearedAt = 0x1FE

  final def topBlankingInitialVCounter(v30:Boolean): Int = this match
    case NTSC => 0x1E8
    case PAL =>
      if !v30 then 0x1CD else 0x1D5

  final def vBlankSetAt(v30:Boolean): Int = if !v30 then 0x0DF else 0x0EF
  final def fFlagSetAt(v30:Boolean): Int = if !v30 then 0x0E0 else 0x0F0
  final def oddFlagToggledAt(v30:Boolean): Int = if !v30 then 0x0E0 else 0x0F0
  final def bottomBlankingJump(v30:Boolean): (Int,Int) = this match
    case NTSC => if !v30 then 0x0EA -> 0x1E5 else -1 -> -1
    case PAL => if !v30 then 0x102 -> 0x1CA else 0x10A -> 0x1D2

  final def bottomBorderPos(v30:Boolean): Int =
    topBlankingPixels + topBorderPixels + (if v30 then 240 else 224)

  final def getClipArea(h40:Boolean): DisplayClipArea =
    if h40 then
      // remaining hsync = 4, left black = 32, left border = 13, display = 320, right border = 14
      DisplayClipArea(32 + 4,topBlankingPixels,379 + 4,topBlankingPixels + topBorderPixels + maxActiveLines + bottomBorderPixels)
    else
      // remaining hsync = 10, left black = 24, left border = 13, display = 256, right border = 14
      DisplayClipArea(24 + 10,topBlankingPixels,307 + 10,topBlankingPixels + topBorderPixels + maxActiveLines + bottomBorderPixels)

  case NTSC extends VideoType(clockFrequency = 53_693_175,
                              topBlankingPixels = 13,
                              topBorderPixels = 11,
                              bottomBorderPixels = 8,
                              bottomBlankingPixels = 3,
                              totalLines = 262,
                              maxActiveLines = 8 * 28)
  case PAL extends VideoType(clockFrequency = 53_203_424,
                             topBlankingPixels = 13,
                             topBorderPixels = 38,
                             bottomBorderPixels = 32,
                             bottomBlankingPixels = 3,
                             totalLines = 312,
                             maxActiveLines = 8 * 30)

enum VRAMAccess:
  case
  H__,  // HScroll Data
  A_M,  // Layer A mapping
  A_P,  // Layer A pattern
  B_M,  // Layer B mapping
  B_P,  // Layer B pattern
  S_M,  // Sprite mapping
  S_P,  // Sprite pattern
  EXT,  // External access slot
  REF   // Refresh

/*
 H = Hscroll
 AB = A/B tiles
 ab = A/B pixel data
 S = sprite tile/x
 s = sprite pixel data
 ~ = 68k access slot
 r = refresh
 */
// H32: Hssss AsaaBsbb ((A~aaBSbb)*3 AraaBSbb)*4 ~~ s*13 ~ s*13 ~
private val H32_VRAM_ACCESS_PATTERN = "Hssss AsaaBsbb" + (("A~aaBSbb" * 3) + "AraaBSbb") * 4 + "~~" + "s" * 13 + "~" + "s" * 13 + "~"
// H40: Hssss AsaaBsbb ((A~aaBSbb)*3 AraaBSbb)*5 ~~ s*23 ~ s*11
private val H40_VRAM_ACCESS_PATTERN = "Hssss AsaaBsbb" + (("A~aaBSbb" * 3) + "AraaBSbb") * 5 + "~~" + "s" * 23 + "~" + "s" * 11

import VRAMAccess.*

import java.awt.Dimension
private def string2VRAMAccessPattern(s:String): Array[VRAMAccess] =
  s.filterNot(_.isSpaceChar).map {
    case 'H' => H__
    case 'A' => A_M
    case 'a' => A_P
    case 'B' => B_M
    case 'b' => B_P
    case 'S' => S_M
    case 's' => S_P
    case 'r' => REF
    case '~' => EXT
  }.toArray

//-----------------------------------------------------------------
//| Screen section | HCounter  |Pixel| Pixel |Serial|Serial |MCLK |
//| (PAL/NTSC H32) |  value    |clock| clock |clock |clock  |ticks|
//|                |           |ticks|divider|ticks |divider|     |
//|----------------|-----------|-----|-------|------|-------|-----|
//|Left border     |0x00B-0x017|  13 |SCLK/2 |   26 |MCLK/5 | 130 |
//|----------------|-----------|-----|-------|------|-------|-----|
//|Active display  |0x018-0x117| 256 |SCLK/2 |  512 |MCLK/5 |2560 |
//|----------------|-----------|-----|-------|------|-------|-----|
//|Right border    |0x118-0x125|  14 |SCLK/2 |   28 |MCLK/5 | 140 |
//|----------------|-----------|-----|-------|------|-------|-----|
//|Back porch      |0x126-0x127|   9 |SCLK/2 |   18 |MCLK/5 |  90 |
//|(Right Blanking)|0x1D2-0x1D8|     |       |      |       |     |
//|----------------|-----------|-----|-------|------|-------|-----|
//|Horizontal sync |0x1D9-0x1F2|  26 |SCLK/2 |   52 |MCLK/5 | 260 |
//|----------------|-----------|-----|-------|------|-------|-----|
//|Front porch     |0x1F3-0x00A|  24 |SCLK/2 |   48 |MCLK/5 | 240 |
//|(Left Blanking) |           |     |       |      |       |     |
//|----------------|-----------|-----|-------|------|-------|-----|
//|TOTALS          |           | 342 |       |  684 |       |3420 |
//-----------------------------------------------------------------
//Analog screen sections in relation to HCounter (H40 mode):
//--------------------------------------------------------------------
//| Screen section |   HCounter    |Pixel| Pixel |EDCLK| EDCLK |MCLK |
//| (PAL/NTSC H40) |    value      |clock| clock |ticks|divider|ticks|
//|                |               |ticks|divider|     |       |     |
//|----------------|---------------|-----|-------|-----|-------|-----|
//|Left border     |0x00D-0x019    |  13 |EDCLK/2|  26 |MCLK/4 | 104 |
//|----------------|---------------|-----|-------|-----|-------|-----|
//|Active display  |0x01A-0x159    | 320 |EDCLK/2| 640 |MCLK/4 |2560 |
//|----------------|---------------|-----|-------|-----|-------|-----|
//|Right border    |0x15A-0x167    |  14 |EDCLK/2|  28 |MCLK/4 | 112 |
//|----------------|---------------|-----|-------|-----|-------|-----|
//|Back porch      |0x168-0x16C    |   9 |EDCLK/2|  18 |MCLK/4 |  72 |
//|(Right Blanking)|0x1C9-0x1CC    |     |       |     |       |     |
//|----------------|---------------|-----|-------|-----|-------|-----|
//|Horizontal sync |0x1CD.0-0x1D4.5| 7.5 |EDCLK/2|  15 |MCLK/5 |  75 |
//|                |0x1D4.5-0x1D5.5|   1 |EDCLK/2|   2 |MCLK/4 |   8 |
//|                |0x1D5.5-0x1DC.0| 7.5 |EDCLK/2|  15 |MCLK/5 |  75 |
//|                |0x1DD.0        |   1 |EDCLK/2|   2 |MCLK/4 |   8 |
//|                |0x1DE.0-0x1E5.5| 7.5 |EDCLK/2|  15 |MCLK/5 |  75 |
//|                |0x1E5.5-0x1E6.5|   1 |EDCLK/2|   2 |MCLK/4 |   8 |
//|                |0x1E6.5-0x1EC.0| 6.5 |EDCLK/2|  13 |MCLK/5 |  65 |
//|                |===============|=====|=======|=====|=======|=====|
//|        Subtotal|0x1CD-0x1EC    | (32)|       | (64)|       |(314)|
//|----------------|---------------|-----|-------|-----|-------|-----|
//|Front porch     |0x1ED          |   1 |EDCLK/2|   2 |MCLK/5 |  10 |
//|(Left Blanking) |0x1EE-0x00C    |  31 |EDCLK/2|  62 |MCLK/4 | 248 |
//|                |===============|=====|=======|=====|=======|=====|
//|        Subtotal|0x1ED-0x00C    | (32)|       | (64)|       |(258)|
//|----------------|---------------|-----|-------|-----|-------|-----|
//|TOTALS          |               | 420 |       | 840 |       |3420 |
//--------------------------------------------------------------------
//Digital render events in relation to HCounter:
//----------------------------------------------------
//|        Video |PAL/NTSC         |PAL/NTSC         |
//|         Mode |H32     (RSx=00) |H40     (RSx=11) |
//|              |V28/V30 (M2=*)   |V28/V30 (M2=*)   |
//| Event        |Int any (LSMx=**)|Int any (LSMx=**)|
//|--------------------------------------------------|
//|HCounter      |[1]0x000-0x127   |[1]0x000-0x16C   |
//|progression   |[2]0x1D2-0x1FF   |[2]0x1C9-0x1FF   |
//|9-bit internal|                 |                 |
//|--------------------------------------------------|
//|VCounter      |HCounter changes |HCounter changes |
//|increment     |from 0x109 to    |from 0x149 to    |
//|              |0x10A in [1].    |0x14A in [1].    |
//|--------------------------------------------------| //Logic analyzer tests conducted on 2012-11-03 confirm 18 SC
//|HBlank set    |HCounter changes |HCounter changes | //cycles between HBlank set in status register and HSYNC
//|              |from 0x125 to    |from 0x165 to    | //asserted in H32 mode, and 21 SC cycles in H40 mode.
//|              |0x126 in [1].    |0x166 in [1].    | //Note this actually means in H40 mode, HBlank is set at 0x166.5.
//|--------------------------------------------------| //Logic analyzer tests conducted on 2012-11-03 confirm 46 SC
//|HBlank cleared|HCounter changes |HCounter changes | //cycles between HSYNC cleared and HBlank cleared in status
//|              |from 0x009 to    |from 0x00A to    | //register in H32 mode, and 61 SC cycles in H40 mode.
//|              |0x00A in [1].    |0x00B in [1].    | //Note this actually means in H40 mode, HBlank is cleared at 0x00B.5.
//|--------------------------------------------------|
//|F flag set    |HCounter changes |HCounter changes | //Logic analyzer tests conducted on 2012-11-03 confirm 28 SC
//|              |from 0x000 to    |from 0x000 to    | //cycles between HSYNC cleared and odd flag toggled in status
//|              |0x001 in [1]     |0x001 in [1]     | //register in H32 mode, and 40 SC cycles in H40 mode.
//|--------------------------------------------------|
//|ODD flag      |HCounter changes |HCounter changes | //Logic analyzer tests conducted on 2012-11-03 confirm 30 SC
//|toggled       |from 0x001 to    |from 0x001 to    | //cycles between HSYNC cleared and odd flag toggled in status
//|              |0x002 in [1]     |0x002 in [1]     | //register in H32 mode, and 42 SC cycles in H40 mode.
//|--------------------------------------------------|
//|HINT flagged  |HCounter changes |HCounter changes | //Logic analyzer tests conducted on 2012-11-02 confirm 74 SC
//|via IPL lines |from 0x109 to    |from 0x149 to    | //cycles between HINT flagged in IPL lines and HSYNC
//|              |0x10A in [1].    |0x14A in [1].    | //asserted in H32 mode, and 78 SC cycles in H40 mode.
//|--------------------------------------------------|
//|VINT flagged  |HCounter changes |HCounter changes | //Logic analyzer tests conducted on 2012-11-02 confirm 28 SC
//|via IPL lines |from 0x000 to    |from 0x000 to    | //cycles between HSYNC cleared and VINT flagged in IPL lines
//|              |0x001 in [1].    |0x001 in [1].    | //in H32 mode, and 40 SC cycles in H40 mode.
//|--------------------------------------------------|
//|HSYNC asserted|HCounter changes |HCounter changes |
//|              |from 0x1D8 to    |from 0x1CC to    |
//|              |0x1D9 in [2].    |0x1CD in [2].    |
//|--------------------------------------------------|
//|HSYNC negated |HCounter changes |HCounter changes |
//|              |from 0x1F2 to    |from 0x1EC to    |
//|              |0x1F3 in [2].    |0x1ED in [2].    |
//----------------------------------------------------
enum HMode(val cells:Int,
           val vCounterIncrement:Int,
           val hBlankSet:Int,
           val hBlankCleared:Int,
           val hCounterInitialValue:Int,
           val hCounterFirstVisible:Int,
           val leftBorderPixel:Int,
           val rightBorderPixel:Int,
           val backPorchJump:(Int,Int),
           val horizontalSync:Int,
           val clockAdjustFun: Int => Int,
           val totalWidth:Int,
           val vramAccessMatrix:Array[VRAMAccess],
           val maxSpritePerFrame:Int,
           val initialClockDiv:Int):

  final val maxSpritePerLine = vramAccessMatrix.count(_ == VRAMAccess.S_M)
  final val activePixels = cells << 3
  final val fFlagSet = 0x00
  final val oddFlagToggled = 0x01
  final val hIntFlagged = vCounterIncrement
  final val vIntFlagged = 0

  case H32 extends HMode(cells = 32,
    vCounterIncrement = 0x109,
    hBlankSet = 0x125,
    hBlankCleared = 0x09,
    0x1E7,
    0x09,
    13,
    14,
    0x127 -> 0x1D2,
    0x1D7,
    _ => -1,
    342,
    string2VRAMAccessPattern(H32_VRAM_ACCESS_PATTERN),
    64,
    5)
  case H40 extends HMode(cells = 40,
    vCounterIncrement = 0x149,
    hBlankSet = 0x165,
    hBlankCleared = 0x0A,
    0x1E7,
    0xB,
    13,
    14,
    0x16C -> 0x1C9,
    0x1CD,
    {
    case 0x1CD => 5
    case 0x1D4 => 4
    case 0x1D5 => 5
    case 0x1DD => 4
    case 0x1DE => 5
    case 0x1E5 => 4
    case 0x1E6 => 5
    case 0x1EE => 4
    case _ => -1
  },
    420,
    string2VRAMAccessPattern(H40_VRAM_ACCESS_PATTERN),
    80,
    4)

case class Model(modelType: ModelType,videoType: VideoType,versionNumber:Int)
