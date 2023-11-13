package ucesoft.smd

/**
 * @author Alessandro Abbruzzetti
 *         Created on 07/09/2023 14:41
 *
 *  +======== PALETTE ==============+
 *  |7|6|5|4|3 2 1|0|7 6 5|4|3 2 1|0|
 *  |0|0|0|0|  B  |0|  G  |0|  R  |0|
 *  +-------------------------------+
 */
object Palette:
  enum PaletteType:
    case NORMAL, SHADOW, HIGHLIGHT
    def brighter(): PaletteType =
      if this == NORMAL then HIGHLIGHT
      else if this == SHADOW then NORMAL
      else HIGHLIGHT
    def darker(): PaletteType =
      if this == NORMAL then SHADOW
      else if this == HIGHLIGHT then NORMAL
      else SHADOW

  // http://gendev.spritesmind.net/forum/viewtopic.php?t=1389
  private final val LEVELS = Array(
    Array(0, 0.9, 1.6, 2.2, 2.7, 3.2, 3.8, 4.7),    // NORMAL
    Array(0, 0.5, 0.9, 1.3, 1.6, 1.9, 2.2, 2.4),    // SHADOW
    Array(2.4, 2.7, 2.9, 3.2, 3.5, 3.8, 4.2, 4.7)   // HIGHLIGHT
  )

  private val VDP_MAX_COLOR_LEVEL = LEVELS.map(_.max).max // 4.7

  private final val palette = {
    val factor = (256 - 1) / VDP_MAX_COLOR_LEVEL // 256 possible values per r/g/b

    val palette = Array.ofDim[Int](PaletteType.values.length,8,8,8)
    for ptype <- PaletteType.values do
      for r <- 0 to 7 do
        for g <- 0 to 7 do
          for b <- 0 to 7 do
            val red = Math.round(LEVELS(ptype.ordinal)(r) * factor).toInt
            val green = Math.round(LEVELS(ptype.ordinal)(g) * factor).toInt
            val blue = Math.round(LEVELS(ptype.ordinal)(b) * factor).toInt
            val color = 0xFF000000 | red << 16 | green << 8 | blue
            palette(ptype.ordinal)(r)(g)(b) = color
    palette
  }

  final def getColor(cramColor:Int,paletteType: PaletteType = PaletteType.NORMAL): Int =
    val r = (cramColor >> 1) & 7
    val g = (cramColor >> 5) & 7
    val b = (cramColor >> 9) & 7
    palette(paletteType.ordinal)(r)(g)(b)


