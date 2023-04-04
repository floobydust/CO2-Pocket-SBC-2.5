;
;**************************************************************************************************
;DOS/65 C02 (CMOS) RAM Version
;
;This is Version 3.20 - 2nd April, 2023 - KM
;This is the bootable image for a RAM loaded version of DOS/65 (CMOS version)
;
;**************************************************************************************************
; Based on Richard A. Leary's DOS/65 V3 code - many thanks Richard!!
; below is Richard's license text from his V3 ROM version.
;
; Note that CCM and PEM have undergone a major rewrite by Kevin E. Maier.
; SIM is a fresh rewrite that interfaces directly to my C02BIOS and C02Monitor code.
; I also make all of my code with the same GNU GPL license below.
;
; The DOS/65 V3 ROM software and all other DOS/65 software are free software;
; you can redistribute it and/or modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 3 of the license, or any later version.
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; See the GNU General Public License for more details.
; You should have received a copy of the GNU General Public License along with this
; program; if not, see <http://www.gnu.org/licenses/>.
; CP/M is a trademark of Caldera.
;
;**************************************************************************************************
; The target platform for this build is:
;  A prototype 3.3-Volt system based on the C02 Pocket SBC - KM
;  This prototype uses a NXP 28L92 DUART and a Hitachi MicroDrive PATA interface (IDE)
;  The system has been modified for more RAM, now 56KB RAM and 8KB ROM (less I/O), 8MHz CPU
;
;**************************************************************************************************
; Assembler directives
;
                PW      132
                PL      66
                CHIP    W65C02S ;Enable WDC 65C02 instructions
                GLOBALS ON
                CASE    OFF
                INCLIST ON
;
;**************************************************************************************************
;
; This version uses CMOS instructions and addressing modes!
;
; There are multiple modules that make up this bootable version and use "include" to
; add them into the main code. The Code Modules are:
;       DOS65-MAIN-V320.ASM (This file)
;       DOS65-CCM-V320.ASM - Concurrent Command Module
;       DOS65-PEM-V320.ASM - Primitive Execution Module
;       DOS65-SIM-V320.ASM - System Interface Module
;
; Other files required contain system variables and constants for C02BIOS and C02Monitor,
; which are included in the SIM source module.
;       C02Constants.asm - C02 BIOS/Monitor constants file
;       C02JMP_Table.asm - C02 BIOS/Monitor JMP table for routines
;
;  The current BIOS and Monitor versions for the Prototype system are 4.02
;
;**************************************************************************************************
;
;               18 February 2023 - KM
;Major revision! This is an entirely RAM based version of DOS/65
;
; Based on my 3.04 CMOS ROM Version
; - All data areas are included in the linked modules
; - Allocation Maps for 8 drives are zeroed and adds 4KB to the 6KB of DOS/65 code
; - Supports 8- 8MB drives configured on the Microdrive.
;
; ALL modules (CCM, PEM, SIM) have been reworked for RAM base and include data areas.
; - Page Zero usage has been reduced - currently, 8 bytes are used.
;
; Major updates to this version:
; - Traditional Track/Sector call structure has been eliminated!
; - All calls for disk drives are at a 128-byte record level.
; - PEM has been changed to only make calls for record numbers.
; - SIM has been re-written to accept record numbers and perform blocking/de-blocking for LBAs.
; - All checksum code has been removed from PEM, no longer needed with IDE block devices.
; - Drive range (A-H) is managed with an invalid drive message shown for invalid drives.
; - DCB structure has changed: track/sector to record based and no checksum table (now 11 bytes).
;
;               02 April 2023 - KM
; Update to SIM for this version:
; - Record Writes are now more efficient with proper blocking to the 512-byte block size of the
;   physical media being used. This provides a significant performance advantage.
; - PEM has been updated to remove the rest of the code associated with diskette based media.
;
;**************************************************************************************************
;
;Base addresses and definitions
pzstrt          .EQU    $98             ;start of page zero system RAM
btejmp          .EQU    $100            ;warm boot jump
pemjmp          .EQU    $103            ;jump to PEM
iostat          .EQU    $106            ;I/O status
dflfcb          .EQU    $107            ;default FCB
dflbuf          .EQU    $128            ;default buffer
tea             .EQU    $800            ;TEA start
;
;C02 Pocket SBC memory parameters - 3.3V prototype
; NOTE: The ramstrt should be changed to $B800!
; this provides a TEA area from $0800 to $B7FF, which is 44KB
;
ramstrt         .EQU    $B800           ;RAM location assembly
hstbuf          .EQU    $0600           ;IDE host buffer
;
;Fixed parameters
lf              .EQU    $0A             ;linefeeed
cr              .EQU    $0D             ;return
eof             .EQU    $1A             ;end of file
null            .EQU    0               ;null
ctlc            .EQU    3               ;abort
ctle            .EQU    5               ;physical cr lf
ctli            .EQU    9               ;tab character
ctlp            .EQU    $10             ;toggle printer
ctlr            .EQU    $12             ;repeat line
ctls            .EQU    $13             ;freeze
ctlx            .EQU    $18             ;cancel
semico          .EQU    $3B             ;semicolon
delete          .EQU    $7F             ;delete character
numcmd          .EQU    37              ;number of PEM commands
numvar          .EQU    6               ;number of page zero variable for switch
maxdrv          .EQU    8               ;maximum number of drives suppported
;
;Page Zero use
                PAGE0
;CCM / PEM
addinp          .EQU    pzstrt          ;initialized to a,y
bufadd          .EQU    addinp+2        ;buffer address
alcpnt          .EQU    bufadd+2        ;allocation map pointer
;
d65flg          .EQU    alcpnt+2        ;DOS/65 flag for CCM/PEM usage
                                        ;bit 7 used for blmode flag in PEM
                                        ;bit 6 used for exrwfl flag in PEM
                                        ;bit 5 used for drvflg flag in PEM
                                        ;bit 4 used for outflg flag in PEM
                                        ;bits 3,2,1 unused
                                        ;bit 0 used for submit flag in CCM
;
;SIM
sim_recflg      .EQU    d65flg+1        ;Disk I/O status flag for SIM
                                        ;bit 7 used for valid LBA
                                        ;bit 6 used for dirty block
                                        ;bit 5 used for read operation
                                        ;bit 4 used for write operation
                                        ;bits 3,2 unused
                                        ;bits 1,0 used for record offset
;
                ENDS
;
;**************************************************************************************************
;
; Main program code starts here:
                .ORG     ramstrt
;
LOAD_HEADER
;Below is the Load Header required by the BOOT BLOCK!
; it contains details required to complete the loading and
; and configuration of the Boot Image before execution.
; The load header is a 16-byte field located at the start of
; the Boot Image file. It is structured as:
;
; - The 16-bit address to start loading the Boot Image.
; - The 16-bit address to start the Boot Image (BOOT code jumps to this).
; - The 16-bit address for loading the offset to the start of disk data.
; - A long word for the size of the required disk data (for assigned drives).
; - A 5 character string ID to validate the Boot Image.
; - A 1 byte checksum to verify the header itself.
;
;Note that this is not yet being used...
; this will be used when the boot record setup is finalized.
; timing is everything... and the timing for this part is not yet.
;
        .DW     ramstrt         ;Address to load Boot Image
        .DW     simram          ;Cold Start location for SIM
        .DW     part_offset     ;Location to copy LBA offset
        .LONG   131072          ;64MB for 8-drives (at 8MB each)
;
string_id
        .DB     "dos65"         ;ID string to validate Boot image
;
        .DB     #$76            ;checksum (must be manually calculated/entered)
;
;**************************************************************************************************
;
        include         DOS65-CCM-V320.ASM      ;Include CCM Module
;
                .ORG    $/256*256+256           ;Align to Page address
;
        include         DOS65-PEM-V320.ASM      ;Include PEM Module
;
                .ORG    $/256*256+256           ;Align to Page address
;
        include         DOS65-SIM-V320.ASM      ;Include SIM Module
;
;**************************************************************************************************
;
                .ORG    $/256*256+256           ;Align to Page address
;
alloc_maps
;allocation and checksum maps
;
;drive a
almpa           .EQU    alloc_maps
                .BLKW   256
;drive b
almpb           .EQU    almpa+512
                .BLKW   256
;drive c
almpc           .EQU    almpb+512
                .BLKW   256
;drive d
almpd           .EQU    almpc+512
                .BLKW   256
;drive e
almpe           .EQU    almpd+512
                .BLKW   256
;drive f
almpf           .EQU    almpe+512
                .BLKW   256
;drive g
almpg           .EQU    almpf+512
                .BLKW   256
;drive h
almph           .EQU    almpg+512
                .BLKW   256
;
        END

