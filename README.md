 C02 Pocket SBC 2.5 - prototype

Hardware:

 Note that there is not an existing PCB layout as of yet.

 The schematics are done with ExpressPCB.

 The prototype system is built on a pair of slightly modified PCBs for the C02 Pocket SBC 2 and the CF-Card/RTC adapter (more like cobbled together).

 A hand-wired Adapters was made for the SC28L92 DUART
 A 32-pin DIP socket and a few wires were used to add the 128KB SRAM

 A small breakout PCB and flat cable have been wired to the bottom of the CF Card/RTC adapter to attach the Microdrive via it’s small 35-pin FPC cable.

WinCUPL is used for the two PLDs

ATF22LV10C:
 PLD code is slightly changed for the memory mapping of SRAM and EEPROM only
 - 56KB SRAM and 8KB EEPROM

ATF16LV8C
 PLD code for the CF Card/RTC adapter is standard


******************************************************************************************************************

Software:

DOS/65 Core OS contains Version 3.20.

Utilities folder contains:

 - Version 2.x Source files for Utilities (can be assembled on C02 Pocket)

- V3 Utils Folder contains:
 - Assembler, MakeCOM, Submit, Super Directory, User Copy and Xmodem utilities for WDC Tools
 - Standard Source files are provided with DOS/65 V3.0

/Software/ROM Code directory contains:
 - BIOS and Monitor for C02 Pocket SBC 2.5 Prototype
 - Microdrive/Realtime Clock utility

******************************************************************************************************************

 WDC Tools is used to build:
 - all of the ROM code (BIOS, Monitor) for the SBC
 - the Microdrive/RTC utility
 - DOS/65 Version 3.20
 - V3 Utils Folder; SD, ASM, Makecom, Submit, Ucopy, Xmodem

 CO2 Monitor provides Xmodem Upload/Download capability via the Console port.
 - The Download will automagically sense and process the S19 records created by WDC Tools.

 Once you have the hardware assembled and the ROM code loaded (working system).
  - you can use Xmodem download from CO2 Monitor to:
	load DOS/65 RAM Image
	load MD/RTC Utility

 Use the utility (g 800 <enter>) to put the DOS image on the Microdrive
 - S to perform System Load
 - 131072 offset for the LBA number to start at
 - B800 for the RAM start address
 - 20 for the block count
 - Y <enter> to write it to disk.

 Use Ctrl-B from Monitor to load and start DOS/65
 - use go 57344 <enter> to return to Monitor

You can download the WDC created utilities via the CO2 Monitor, then invoke DOS/65 and use the
 Save command to move them to disk as follows:

 Save # name.com, where # is the number of 256-byte pages to save and name.com is the utility name you want it saved as.

******************************************************************************************************************

 Once you have the main DOS system and the base utilities loaded, you can use Xmodem to download the main utilities. There is a makeutil.sub file that will build the entire set on the CO2 Pocket SBC. It assumes the source files are located on drive B:

 If you prefer to use a different drive, be sure to edit the .sub file.

 Have Fun!


 
