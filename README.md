![workflow status](https://github.com/abbruzze/sega-md/actions/workflows/scala.yml/badge.svg)
[![Release](https://img.shields.io/github/v/release/abbruzze/sega-md)](https://github.com/abbruzze/sega-md/releases)
[![Language](https://img.shields.io/github/languages/top/abbruzze/sega-md)]()
[![Downloads](https://img.shields.io/github/downloads/abbruzze/sega-md/total)](https://github.com/abbruzze/sega-md/releases/latest)

<p align="center">
  <img src="images/logo.png">
</p>

# ScalaGen ver 1.0
Sega Mega Drive (Genesis) Scala Emulator

### Emulator features
-----------
- PAL/NTSC regions
- TMSS
- Full debugger
  - Motorola 68000
  - Z80
  - SVP (Sega Virtual Processor)
  - Memory viewer & editor
      - VDP VRAM, CRAM, VSRAM
      - Cartridge's rom
      - 68K ram
      - Z80 ram
      - SVC DRAM, RAM A, RAM B
  - Pattern layers viewer
  - Pattern dump viewer
  - Layer A/B/S on/off
  - Sprite Cache viewer
  - Sprite boundaries on/off
  - DMA trace
  - Disassembler
  - Breakpoints
  - Frame by frame mode
  - Live disassembly on file
- Snapshots: load/save
- Events saving & playback
- Game Genie cheats DB
- Controllers: keyboard, USB joystick, mouse, lightgun
- VDP debug register
- PSG emulation: thanks to Chris White (http://www.smspower.org/dev/docs/wiki/Sound/PSG)
- FM emulation: Ym2612Nuke, thanks to Federico Berti for his Java porting (https://github.com/fedex81/helios)
- SVP emulation
  - Full emulation of internal rom (https://github.com/jdesiloniz/svpdev/wiki/Internal-ROM). The rom source has been compiled with my own compiler.
  - See also https://github.com/jdesiloniz/svpdev/tree/master/samples
- Performance monitor

### Download
-----------
Go to https://github.com/abbruzze/sega-md/releases/latest and download and unzip on your computer the latest version.
Be sure to have a jre (17 or above, best performance on 22) in the path and launch in the bin directory:
- On Windows: **scalagen.bat**
- On Linux: **scalagen.sh**

### Default keybindings
-----------

**Player 1**

 Button        | Mapped to
 --------------|-------------
 Start         | Enter
 A             | A
 B             | S
 C             | D
 X             | Q
 Y             | W
 Z             | E
 Mode          | R
 Up            | Cursor Up
 Down          | Cursor Down
 Left          | Cursor Left
 Right         | Cursor Right


**Player 2**

  Button        | Mapped to
 --------------|-------------
 Start         | Backspace
 A             | J
 B             | K
 C             | L
 X             | U
 Y             | I
 Z             | O
 Mode          | P
 Up            | Y
 Down          | B
 Left          | H
 Right         | L

# Games gallery
||||
|-|-|-|
|![](images/aladdin.PNG "Aladdin")|![](images/anotherworld.PNG "Another World")|![](images/atomicrunner.PNG "Atomic Runner")|
|![](images/batman_robin.PNG "Batman & Robin")|![](images/battlemania2.PNG "Battle Mania 2")|![](images/castelvania.PNG "Castlevania - Bloodlines")|
|![](images/comix.PNG "Comix Zone")|![](images/dragonball.PNG "Dragon Ball Z - Buyuu Retsuden")|![](images/dukenukem.PNG "Duke Nukem 3D")|
|![](images/Earthworm.PNG "Earthworm Jim")|![](images/fifa98.PNG "FIFA 98 - Road to World Cup")|![](images/flashback.PNG "Flashback - The Quest for Identity")|
|![](images/formula1.PNG "Formula One")|![](images/ghost.PNG "Ghouls'n Ghosts")|![](images/gunstarheroes.PNG "Gunstar Heroes")|
|![](images/lemmings.PNG "Lemmings")|![](images/lighteningforce.PNG "Lightening Force - Quest for the Darkstar")|![](images/megaturrican.PNG "Mega Turrican")|
|![](images/mk1.PNG "Mortal Kombat")|![](images/nbajam.PNG "NBA Jam")|![](images/outrun.PNG "OutRun")|
|![](images/pop.PNG "Prince of Persia")|![](images/roadrash.PNG "Road Rash")|![](images/rocketnight.PNG "Rocket Knight Adventures")|
|![](images/shinobi3.PNG "Shinobi III - Return of the Ninja Master")|![](images/skeletoncrew.PNG "Skeleton Krew")|![](images/sonic1.PNG "Sonic The Hedgehog")|
|![](images/sonic2.PNG "Sonic The Hedgehog 2")|![](images/sonic3.PNG "Sonic The Hedgehog 3")|![](images/sonic3d.PNG "Sonic 3D Blast ~ Sonic 3D Flickies' Island")|
|![](images/ssf2.PNG "Super Street Fighter II")|![](images/streeracer.PNG "Street Racer")|![](images/streetfight2.PNG "Street Fighter II' - Special Champion Edition")|
|![](images/streetofrage3.PNG "Streets of Rage 3")|![](images/streetsofrage.PNG "Streets of Rage 2")|![](images/subterrania.PNG "Sub-Terrania")|
|![](images/vectorman.PNG "Vectorman")|![](images/virtuafighter2.PNG "Virtua Fighter 2")|![](images/vr.PNG "Virtua Race")|
|![](images/wolfstein3d.PNG "Wolfstein 3D")|||

# Demos gallery
||||
|-|-|-|
|![](images/titanoverdrive.PNG "Overdrive")|![](images/titanoverdrive2.PNG "Overdrive 2")|![](images/limpninja.PNG "Limp Ninja")|
|![](images/The_Kessler_Incident_by_Resistance_2023.PNG "The Kessler Incident by Resistance")|![](images/The_Spiral_by_Resistance.PNG "The Spiral by Resistance")|![](images/titan_niccc.PNG "NICCC by Titan")|

# Debugger gallery
|||
|-|-|
|![](images/debugger.PNG)|![](images/debugger2.PNG)|
|![](images/perfmonitor.PNG)||
