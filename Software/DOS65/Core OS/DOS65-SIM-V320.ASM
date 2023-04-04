;
;DOS/65 system interface module (SIM)
;Version 3.20 - RAM based
;
;This version is designed to work with the:
;C02 Pocket SBC with RTC/CF-Card adapter:
; - modified for 3.3V for MicroDrive, SC28L92 DUART and DS1511Y RTC
;
;Microdrive Disk configuration:
; a 8MB
; b 8MB
; c 8MB
; d 8MB
; e 8MB
; f 8MB
; g 8MB
; h 8MB
;
;This SIM version no longer uses Track/Sector parameters to access the Microdrive!
; - All calls and paraneters from PEM are now based on Record numbers.
; - Records are still set as 128-bytes per record.
; - Record number is a 16-bit value (0-65,535), so an 8MB limit still exists.
;
;The Disk Control Block parameter table has been modified.
; This is done to accomodate the change from Track/Sector parameters to records, and
; eliminate the checksum table, as IDE and other nonremovable block devices don't require it.
; The new DCB table entry is configured as:
;
; Note: example shows an 8MB drive setup.
; - A total of 4096 blocks, where each block is 2048 bytes
; - the number of records per physical block is 4 (4*128=512)
; - the total number of reserved records is 256 (256*128=32,768 bytes)
; - Note: Reserved Records must be a multiple of 4!
;
;     .DW     4079            ;max block number
;     .DW     4               ;records per physical block (LBA)
;     .DW     256             ;number of reserved records (OS Boot area)
;     .DB     1               ;block size = 2048
;     .DW     1023            ;max directory
;     .DW     almpa           ;address of allocation map
;
;A modified PEM module is required to use this new SIM version.
; PEM 3.20 also contains the changes for using Record numbers only!
;
;Note: This SIM version makes calls to the C02BIOS directly!
; This results in a smaller code size and one less module to write and maintain.
;
;As this SIM supports a prototype C02 Pocket, the following devices exist:
; - Microdrive - supports 8 drives at 8MB each
; - Dual port UART - provides console and reader/punch functions
; - Realtime Clock - not currently supported/used
;
;Note that this SIM version uses an additional 4-byte offset for LBA accesses!
; - This is required due to being a bootable system. The Microdrive uses a Partition Record
; - which will load the Boot Block from the active partition. The Boot Block and Boot Image are
; - part of the allocated disk space that becomes the drive.
;
; - The Boot Block code will use the Load Header at the start of drive image to provide the offset
; - to allocated space and update the Partition Offset variable in this SIM module (part_offset).
; - This design allows the Boot Image to be easily moved to a new location on the drive and not
; - have to worry about the disk access. Only the Boot Block needs to be updated.
;
;**************************************************************************************************
;Include required constants/equates/tables for SIM to assemble
;
        NOLIST
        INCLUDE         C02Constants.asm        ;Constants/Equates - C02 BIOS/Monitor/Hardware
        INCLUDE         C02JMP_Table.asm        ;Jump Table entries for C02 BIOS/Monitor
        LIST
;
;**************************************************************************************************
;Fixed parameters
;
CLREOL          .EQU     1              ;clear to eol (ctrl-a)
CLREOS          .EQU     2              ;clear to eos (ctrl-b)
BS              .EQU     8              ;backspace (ctrl-h)
ESC             .EQU     $1B            ;escape character
;
;**************************************************************************************************
;
;Main program, Start of SIM
; this is the cold start for DOS/65. The boot block code jumps to here.
;
; Note: The following calls are no longer needed or supported, but are still listed in SIM to
;       maintain the SIM functions at the correct address offsets.
;       Also, the Console Definition Table has a hard-coded section within SIM.
;
;       SIMRAM+24: Set Home Drive
;       SIMRAM+30: Set Reserved Tracks
;       SIMRAM+51: Translate Sector
;
; Set Home Drive is specific to diskette based media.
; - As multiple drive letters now exist on a single block device, there's no need to
;   tell the drive to go to home (which would be cylinder zero).
; 
; Reserved Records are defined in the updated Disk Control Block.
; - SIM doesn't require any action based on reserved records. PEM uses this parameter
;   from the DCB to calculate the correct Record to read or write.
;
; Translate Sectors is obsolete.
; - There are no diskette drives supported in this version of DOS/65.
;
; Note: Printer Output is not supported, but could be using additional hardware if required.
;
simram          jmp     sim_boot        ;cold start SIM-DOS/65  simram+0 (3)
sim_wboote      jmp     sim_wboot       ;warm boot DOS/65       simram+3 (3)
                jmp     mon_consts      ;check for input        simram+6 (3)
                jmp     mon_conrde      ;get input              simram+9 (3)
                jmp     mon_conwrt      ;send to terminal       simram+12 (3)
                jmp     dummy           ;printer output         simram+15 (3)
                jmp     mon_punch       ;punch output           simram+18 (3)
                jmp     mon_reader      ;reader input           simram+21 (3)
                jmp     dummy           ;set home drive         simram+24 (3)
                jmp     sim_seldsk      ;select active disk     simram+27 (3)
                jmp     dummy           ;set reserved tracks    simram+30 (2)
                jmp     sim_setrec      ;set record number      simram+33 (3)
                jmp     sim_setdma      ;set buffer address     simram+36 (3)
                jmp     sim_read        ;read PEM record        simram+39 (3)
                jmp     sim_write       ;write PEM record       simram+42 (3)
                lda     #1              ;printer always ready   simram+45 (2)
                rts                     ; (6)
                ldx     #128            ;clock entry (disabled) simram+48 (2)
                rts                     ; (6)
                jmp     dummy           ;translate (obsolete)   simram+51 (2)
;
;console definition block
                .DB     0               ;scratch
sysdef          .DB     8               ;backspace
                .DB     1               ;clear to end of line
                .DB     $0C             ;forward space
                .DB     0               ;normal video
                .DB     '^'             ;invert video
                .DB     24              ;lines per screen
                .DB     80              ;char per line
                .DB     $0C             ;form feed
                .DB     $1E             ;home
                .DB     2               ;clear to end of screen
;
dummy           rts                     ;dummy rts for unused SIM calls (6)
;
;SIM Messages
sim_opnmsg      .DB     CR,LF,"DOS/65 Version 3.20"
                .DB     CR,LF,"SIM Version 3.20",0
;
io_err_msg      .DB     CR,LF,"Disk I/O Error!",0
;
warm_msg        .DB     CR,LF,"*",0
;
;Start of SIM cold boot
sim_boot        jsr     b_ide_reset     ;Reset Microdrive (just in case) (6)
;
                lda     #<sim_opnmsg    ;Point to message (2)
                ldy     #>sim_opnmsg    ; (2)
                jsr     m_promptr       ;Send it via C02 Monitor routine (6)
;
                ldx     #$FF            ;Set stack pointer (2)
                txs                     ; to top of stack space (2)
;
; To improve Disk I/O performance, the partition offset will be added
; to the drive offsets. This has to be a one-time calculation when
; SIM is started, otherwise the disk offset will be added each time
; SIM is reset, which doesn't work! This will take some additional
; memory space, but saves execution time in the get_lba routine.
; Once added in, the partition offset values below are zeroed out.
;
                ldx     #0              ;Zero index for drive offset (2)
                ldy     #maxdrv         ;Load index for Max drive count (2)
drv_update_lp
                clc                     ;Clear carry for add (2)
                lda     drv_offset+0,x  ;Get the drive offset (4)
                adc     part_offset+0   ;Add the partition offset (4)
                sta     drv_offset+0,x  ;Save it back (5)
;
                lda     drv_offset+1,x  ;Get the drive offset (4)
                adc     part_offset+1   ;Add the partition offset (4)
                sta     drv_offset+1,x  ;Save it back (5)
;
                lda     drv_offset+2,x  ;Get the drive offset (4)
                adc     part_offset+2   ;Add the partition offset (4)
                sta     drv_offset+2,x  ;Save it back (5)
;
                inx                     ;Increment drive offset (2)
                inx                     ; by 4 bytes per drive (2)
                inx                     ; (2)
                inx                     ; (2)
;
                dey                     ;Decrement drive count (2)
                bne     drv_update_lp   ;Loop back for all drives (2/3)
;
; Now that all drive offsets have been updated with the partition offset,
; we will zero out the partition offset data (which is in RAM) so that it
; can't be added in again ;-) Ah... RAM based routines!
;
                stz     part_offset+0   ;Zero out Partition Offset (4)
                stz     part_offset+1   ; (4)
                stz     part_offset+2   ; (4)
                stz     part_offset+3   ; (4)
;
;Initialize iostat - resets to user 0 and drive 0
; Note: this can be changed to a specific drive/user if needed.
                stz     iostat          ;Clear I/O status (4)
;
;SIM Setup
;First clear disk access flags
                stz     sim_recflg      ;Clear SIM record flag byte (3)
;
;Setup PEM entry in Page 1
; note that a warm boot also branches here to finish up
sim_wsetup
                ldy     #6              ;Set index to 6 (2)
simset_lp       lda     sim_inttbl-1,y  ;Get SIM table data (4)
                sta     $0100-1,y       ;Store into page $01 PEM area (5)
                dey                     ;Decrement index (2)
                bne     simset_lp       ;Loop until done (2/3)
;
;Setup default buffer for PEM (128 bytes)
sim_finish
                lda     #<dflbuf        ;Get default buffer (2)
                ldy     #>dflbuf        ; address (2)
                jsr     sim_setdma      ;then set DMA for DOS/65 (6)
;
;Setup the BIOS for the default host buffer and block count.
; The BIOS reads or writes a single 512-byte block into it's own
; Host Buffer. The SIM code calls the C02 Pocket BIOS for all hardware
; access.
                lda     #<hstbuf        ;Get host buffer address (2)
                ldy     #>hstbuf        ; high byte (2)
                ldx     #1              ;Block count =1 (2)
                jsr     b_ide_set_addr  ;Call BIOS (6)
;
                jmp     ccmram          ;Then jump to CCM to complete startup (3)
;
;Initialization table
sim_inttbl      .DB     $4C             ;JMP opcode
                .DW     sim_wboote      ;Warm Boot location
                .DB     $4C             ;JMP opcode
                .DW     pemram          ;PEM RAM location
;
;SIM Warm Boot - When any application exits, it should make a call to Warm Boot
; the system. That call will be handled here. As CCM can be over-written by any
; application to access additional memory if/when needed, the warm boot routine
; is responsible for reloading CCM. With DOS/65, both CCM and PEM are reloaded by
; default. As certain PEM routines are self-modifying and the PEM data is usually
; heavily altered during all operations, perhaps that's why Richard is specifying
; to reload it during a warm boot. SIM however, is NOT reloaded!
;
;NOTE: IOSTAT and hence the default drive/user are not altered by Warm Boot.
; After CCM/PEM are reloaded, the current user/drive will be restored to the
; same as before a warm boot was initiated.
;
;Clear out the user space in Page Zero, this can cause intermittant crashes on
; warm start if leftover data from the last app is present.
; Apps like the Basic Compiler and Runtime are good examples.
; Granted, this is likely something specific in CCM, but clearing out page zero
; is just a good idea and can be an aid in any application debugging.
;
sim_wboot
                ldx     #$00            ;zero index (2)
clr_pg0
                stz     $00,x           ;clear page zero user space (4)
                inx                     ;increment index (2)
                cpx     #pzstrt         ;check for end (2)
                bne     clr_pg0         ;loop back till done (2/3)
;
                ldx     #$FF            ;set stack pointer (2)
                txs                     ;to top of stack - safety (2)
;
;Show a simple asterisk when a warm boot is executed
                lda     #<warm_msg      ;Point to message (2)
                ldy     #>warm_msg      ; (2)
                jsr     m_promptr       ;Send it (6)
;
;As this is a bootable version of DOS/65, we need to reload CCM and PEM from disk.
; CCM, PEM and SIM are stored in the Reserved Records area of the bootable drive partition.
; Note that this also includes the drive allocations maps, which are zeroed. This eliminates
; the need to clear the allocation maps when the system is initially booted.
;
;The actual Cold Boot is handled by the Boot Record, which is loaded from the Drive.
; This is the result of the Partition Loader loading and executing the active Boot Record.
; All of this is setup outside of DOS/65 and SIM. A separate disk utility is used to setup
; the drive and load the DOS/65 code to the drive in the reserved area of the disk.
;
;As the default boot drive is A:, we only need to load the first set of drive offsets and
; set these up in the BIOS. Note that these values will contain the actual starting LBA
; for the first drive, as the partition offset value has been added to all drive offsets.
;
;As Blocking/De-blocking is handled in SIM, we need to test the Dirty Block flag to see
; if there is a pending block write. If there is, we'll flush the buffer to disk.
;
                bbr6    sim_recflg,no_flush     ;Check dirty block flag, branch if clear (5/6)
;
;SIM Flush Drive
; - When PEM calls SIM to read or write a Record, blocking and de-blocking is done
;   here in the routines below. To enhance performance for block writes, SIM will
;   delay a block write as there are four records per block. If a different record
;   is required by PEM, the current disk buffer will be checked via a dirty block
;   flag and flushed to disk before loading the new calculated block. It's also
;   important to check the dirty block flag via a warm boot, as applications don't
;   know about this SIM feature.
;
; Write the current LBA in the bufer to the disk.
;
                jsr     b_ide_write_lba ;Call BIOS to Write LBA (6)
                lda     ide_status_ram  ;Get Status from BIOS call (3)
                lsr     a               ;Shift error bit to carry (2)
                bcs     ide_rw_err      ;Branch if error (2/3)
;
                rmb6    sim_recflg      ;reset dirty block flag (5)
;
no_flush
                lda     drv_offset+0    ;get LBA parameters (4)
                ldy     drv_offset+1    ; (4)
                ldx     drv_offset+2    ; (4)
                jsr     b_ide_set_lba   ;call BIOS to set parameters (6)
;
; Next, set the load address for BIOS to load the CCM and PEM modules.
; The X reg contains the number of blocks to load. CCM starts at "ramstrt", which
; is defined in the main source file.
; CCM uses 9 pages (256 bytes per page) and PEM uses 11 pages for a total of 20 pages.
; This equates to a total of 5120 bytes, or 10- blocks from the Microdrive.
;
                lda     #<ramstrt       ;get load address lo (2)
                ldy     #>ramstrt       ;get load address hi (2)
                ldx     #10             ;get block count (5KB for CCM and PEM) (2)
                jsr     b_ide_set_addr  ;call BIOS to set address and size (6)
                jsr     b_ide_read_lba  ;call BIOS to load CCM/PEM to memory (6)
                lda     ide_status_ram  ;get Status from BIOS call (3)
                lsr     a               ;shift error bit to carry (2)
                bcc     sim_wsetup      ;if no error, reset PEM and start CCM (2/3)
ide_rw_err
                jmp     get_lba_err     ;else, use common error routine (3)
;
;**************************************************************************************************
;
;Console Input and Output Routines
; There are three routines for Console I/O
; 1- CONSTS - Console Status: tests to see if there is a character available
; 2- CONRDE - Console Read: waits for a character to be available, then returns with it
; 3- CONWRT - Console Write: sends a character to the console
;
;The console status routine does NOT read any data from the console itself.
; It just checks to see if data has been entered. If no data is waiting, the A Reg has to be
; zero. If data is waiting, the A Reg has to be a non-zero value, hence any or all bits set
; active will work. The BIOS uses the page zero variable icnt_a to track the data count in
; the input buffer. By simply loading the A Reg from icnt_a, the A reg will be either zero
; or have a non-zero value. Note that the Z flag is also set if icnt_a is zero.
;
mon_consts
                lda     icnt_a          ;Load Input Buffer count (3)
                rts                     ;Return to Caller (6)
;
;The console read routine calls the C02BIOS routine to get a character from the console.
; As the C02BIOS can transfer 8-bit binary data, the upper bit is masked off to ensure ASCII
; only. Also, the standard Delete key code from the console is a Backspace ($08). The PEM
; routine checks for the Delete key, so this routine will check for Backspace and replace it
; with a Delete code.
;
mon_conrde
                jsr     b_chrin         ;Call BIOS for character in (waits) (6)
                and     #$7F            ;Strip off high bit (ASCII only) (2)
                cmp     #bs             ;Check for Backspace character (2)
                bne     conrdx          ;If not, branch (2/3)
                lda     #delete         ;Else, replace with Delete code (2)
conrdx          rts                     ;Return to Caller (6)
;
;The console write routine is straightforward. As per the System Interface Guide, the SIM
; routine does not need to preserve the character being written to the console. We only take the
; approach of stripping off the high bit to ensure ASCII only.
;
mon_conwrt
                and     #$7F            ;strip off high bit for ASCII (2)
                jmp     b_chrout        ;send to BIOS and return (3)
;
;**************************************************************************************************
;
;Additional routines for using the second serial port via C02BIOS routines
; these are simple binary passthru routines, no data input or output is modified.
;
mon_punch
                jmp     b_chrout2       ;send data to serial port 2 (3)
;
mon_reader
                jmp     b_chrin2        ;get data from serial port 2 (3)
;
;**************************************************************************************************
;
;Start of Disk I/O Routines:
; Note that these routines rely on the C02BIOS calls for physical access of the Microdrive!
; 
; Blocking/De-blocking is always used for IDE devices (default LBA size = 512 bytes)
; All drives are configured with the same defaults as:
;
; - 4096 total blocks
; - 4 records per LBA (drive physical block size)
; - 16 records per block (DOS/65 Allocation size per extent)
;
; This section contains all new routines that are designed to work with Record Numbers!
;
; Records is a 16-bit field and record lengths are 128-bytes. The new routines provide a simple
; translation layer that will read and write blocks from the Microdrive. Each 512-byte block
; contains 4- 128-byte records. This calculates to a total of 8MB of disk space allocated via
; the 64K of records per drive letter.
;
; There are several routines which are required to support the SIM functions
;
;Record Move routines
; there are two sets of Record Move routines;
; - set one moves a Record from the LBA buffer to the DMA address specified by PEM
; - set two moves a Record from the DMA address specified by PEM to the LBA buffer
; In short, Set one is for reading a Record from disk and moving to memory, while
; set two is for writing a Record from memory and moving to disk.
;
;Select disk
; drive range is now validated by PEM before making this SIM call.
;
sim_seldsk      sta     sim_seldrv      ;save as current drive (4)
                asl     a               ;multiply by two (2)
                tax                     ;make an index (2)
                lda     sim_dcbtbl,x    ;get DCB address (4)
                ldy     sim_dcbtbl+1,x  ; in A/Y (4)
                rts                     ;and return (6)
;
;Table of dcb addresses.
; All 8 drives are accounted for, if not present, use a word value of "0000".
;
sim_dcbtbl      .DW     dcb_a
                .DW     dcb_b
                .DW     dcb_c
                .DW     dcb_d
                .DW     dcb_e
                .DW     dcb_f
                .DW     dcb_g
                .DW     dcb_h
;
;Set DOS/65 memory buffer address
;
sim_setdma      sta     sim_dmaadr      ;save address low (4)
                sty     sim_dmaadr+1    ;save address high (4)
                rts                     ;return (6)
;
;Set Requested Record
; Note that PEM calculates the Record number to include any Reserved Records.
;
sim_setrec      sta     sim_pemrec      ;save record low (4)
                sty     sim_pemrec+1    ;save record high (4)
                rts                     ;return (6)
;
;*************************************************************************************************
;
;Read the selected DOS/65 record.
;
sim_read
                smb5    sim_recflg      ;set read op flag (5)
                jsr     get_lba         ;calculate/get LBA to read (6)
;
;Transfer the record from buffer to DMA
;
                lda     sim_recflg      ;get recflg (4)
                and     #%00000011      ;mask for lower 2-bits (2)
                asl     a               ;multiply by 2 for index (2)
                tax                     ;xfer to X reg as index (2)
;
                lda     buff_offset,x   ;get buffer offset address (4)
                ldy     buff_offset+1,x ; (4)
                sta     mover+1         ;save to routine lo (4)
                sty     mover+2         ;save to routine hi (4)
;
                lda     sim_dmaadr+0    ;get dma address lo (4)
                ldy     sim_dmaadr+1    ;get dma address hi (4)
                sta     mover+4         ;save to routine lo (4)
                sty     mover+5         ;save to routine hi (4)
                ldy     #0              ;zero index (2)
;
mover
                lda     $FFFF,y         ;get from source address (4)
                sta     $FFFF,y         ;move to destination address (5)
                iny                     ;increment offset (2)
                bpl     mover           ;loop back until done (2/3)
;
                rmb5    sim_recflg      ;reset read op flag (5)
                lda     #0              ;zero for no error (2)
                rts                     ;return (6)
;
;Write the selected DOS/65 record.
;
sim_write
                smb4    sim_recflg      ;set write op flag (5)
                jsr     get_lba         ;calculate/get LBA to write (6)
;
;Transfer the record from DMA to buffer
;
                lda     sim_recflg      ;get recflg (4)
                and     #%00000011      ;mask for lower 2-bits (2)
                asl     a               ;multiply by 2 for index (2)
                tax                     ;xfer to X reg as index (2)
;
                lda     buff_offset,x   ;get buffer offset address (4)
                ldy     buff_offset+1,x ; (4)
                sta     movew+4         ;save to routine lo (4)
                sty     movew+5         ;save to routine hi (4)
;
                lda     sim_dmaadr+0    ;get dma address lo (4)
                ldy     sim_dmaadr+1    ;get dma address hi (4)
                sta     movew+1         ;save to routine lo (4)
                sty     movew+2         ;save to routine hi (4)
                ldy     #0              ;zero index (2)
;
movew
                lda     $FFFF,y         ;get from source address (4)
                sta     $FFFF,y         ;move to destination address (5)
                iny                     ;increment offset (2)
                bpl     movew           ;loop back until done (2/3)
;
                smb6    sim_recflg      ;set dirty block flag (5)
;
                rmb4    sim_recflg      ;reset write op flag (5)
                lda     #0              ;zero for no error (2)
                rts                     ;return to caller (6)
;
;Buffer Offsets.
; The offsets here point to the 4- 128-byte Records in the main Disk Buffer.
;
buff_offset
                .DW     LBA_BUFFER+0    ;buffer offset 00
                .DW     LBA_BUFFER+128  ;buffer offset 01
                .DW     LBA_BUFFER+256  ;buffer offset 10
                .DW     LBA_BUFFER+384  ;buffer offset 11
;
;**************************************************************************************************
;
;Get LBA routine
; This is a core routine for SIM and all disk level activity. Now that PEM
; and SIM have been moved to a Record level access, the conversion between
; 128-byte records and 512-byte LBA block sizes can be streamlined.
;
;This routine is common for both Reading and Writing of Records. As there
; is still a requirement to perform blocking/de-blocking from PEM Records
; to disk block sizes, this routine will perform the following tasks:
;
; 1- Calculate the Block Number from the Record number.
; 2- Save the Record offset within the calculated block number.
; 3- Check to see if a valid disk block is loaded in the buffer.
; 4- Perform a block read if there is no valid block loaded.
; 5- Test the valid block loaded against the required block number.
; 6- Test for a pending write for the valid block number.
; 7- Write a pending block and clear the dirty block flag if required.
; 8- Only load a block if needed, else return without disk I/O access.
; 9- Add the Drive offset to the required Block.
;
;This is a fair amount of work, but this core routine ensures we have the
; correct block in the default disk buffer on which to read or write
; records to or from. This routine also simplifies the Record Read and Write
; routines to focus only on handling the required 128-byte record.
;
; Another advantage is that this routine will only load a new block when
; it needs to, i.e., if the calculated block is already loaded, it will
; not reload it, but simply return and allow the Record Read and Write
; routines to carry out their record level function.
; This improves the overall disk I/O performance.
;
;Calculate the block number (LBA) based on the Record number.
; The Record number is a 16-bit value (0-65,535) and is 128-bytes in size.
; To calculate the LBA, we simply divide it by 4, as there are 4 records
; per 512-byte block (LBA). We also save the lower 2-bits as this becomes
; the offset of the requested record into the block (as binary, 00,01,10,11).
;
;First, we need to take the Record Number and do the following:
; 1- Save the Record offset (the lower 2-bits of the Record number).
; 2- Perform a 16-bit Logical Right Shift by 2-bits (divide by 4).
; 3- Add the Drive number offset to the required LBA.
; 4- Save the calculated LBA, then check the flag byte for an LBA loaded.
;
;Second, we need to test for the following conditions:
; 1- If a LBA is loaded, check to see if it is the same LBA that we need.
; 2- If it is, just exit back to the calling routine, saving a disk read.
; 3- If it it NOT, check the dirty block flag in case it's been altered.
; 4- If the dirty block is set, write the current block and clear the flag,
;    then update the parameters and load the new LBA.
; 5- If the dirty block is not set, update the parameters and load the new LBA.
;
;The C02BIOS used here is Version 4.02.
; This BIOS supports 24-bit LBA addressing.
; Note: IDE uses 28-bit addressing, but the BIOS zeros the upper 4-bits,
; resulting in a limit of 8GB for the attached IDE device.
;
;The SIM configuration is set for 8 drives (A thru H), each at 8MB.
; Any of the defined drives can have Reserved Records, but in practical
; use, only one drive should have Reserved Records to be use for holding
; the bootable image for the OS (CCM, PEM, SIM, Allocation Tables).
; The first drive will normally have the Reserved Records to hold the
; bootable OS image, where a total of 256 Records will equate to 32KB.
;
; NOTE: The PEM routine that calls SIM for the requested record also
; adds in the reserved record count, so we don't need to do that here!
;
;Once the Block number has been calculated from the Record number,
; an offset must be added which is based on the Drive number (0-7).
; Partition sizes are limited to 8MB
; Number of drives is limited to 8 (A - H)
; Maximum available Data is limited to 64MB
;
;The Offset table is 4 bytes per entry and adds an LBA count to
; shift the absolute LBA numbers to the appropriate drive number
; being acessed (16384 blocks per 8MB drive area).
;
;First, calculate the record number to LBA number and
; store the LBA number to sim_pemrec (3 bytes).
;
get_lba
                lda     #%00000011      ;Get mask for record offset bits (2)
                trb     sim_recflg      ;Reset record offset bits in flag (5)
;
                stz     sim_rectemp+2   ;Clear extended temp byte (4)
                lda     sim_pemrec+1    ;Get PEM Record number high byte )4)
                sta     sim_rectemp+1   ;Save to temp (4)
                lda     sim_pemrec+0    ;Get PEM Record number low byte (4)
                sta     sim_rectemp+0   ;Save to temp (4)
;
                and     #%00000011      ;Mask off upper 6 bits (2)
                ora     sim_recflg      ;OR in other record flag bits (3)
                sta     sim_recflg      ;Update the record flag for new offset (3)
;
; now do the 16-bit shift to the right to divide by 4 and get the LBA number
;
                lsr     sim_rectemp+1   ;Shift high to right, LSB to carry (6)
                ror     sim_rectemp     ;Rotate low to right, MSB from carry (6)
                lsr     sim_rectemp+1   ;Shift high to the right, LSB to carry (6)
                ror     sim_rectemp     ;Rotate low to right, MSB from carry (6)
;
; the sim_rectemp pointer now has the logical LBA number requested by PEM.
; the sim_recflg also contains the record offset in the lower 2 bits,
; plus the other flags have been preserved (very important).
;
; Note that the partition offset (common for all drives) has been added as
; a one-time calculation during the first startup of SIM. As all drive
; offsets have been updated with the parition offset, we only need to
; apply the drive offset based on the current drive A-H (0-7).
;
                lda     sim_seldrv      ;Get active drive number (4)
                asl     a               ;Multiply by two (2)
                asl     a               ;Multiply by two (now = 4) (2)
                tax                     ;Xfer offset to X reg (2)
;
                clc                     ;Clear Carry for Add (2)
                lda     sim_rectemp+0   ;Get Low order LBA (4)
                adc     drv_offset+0,x  ;Add in Low order offset (4)
                sta     sim_rectemp+0   ;Save to Low order LBA (4)
;
                lda     sim_rectemp+1   ;Get Middle order LBA (4)
                adc     drv_offset+1,x  ;Add in Middle order offset (4)
                sta     sim_rectemp+1   ;Save to Middle order LBA (4)
;
                lda     sim_rectemp+2   ;Get High order LBA (4)
                adc     drv_offset+2,x  ;Add in High order offset (4)
                sta     sim_rectemp+2   ;Save to High order LBA (4)
;
; now, the actual required LBA has been calculated and is stored as a 24-bit value.
; this is stored in variable "sim_rectemp" (3 consecutive bytes).
; We now need to compare the actual LBA needed to the currently loaded LBA, if there
; is one. We first test for a valid LBA loaded by checking bit7 of the sim_recflg.
;
                bbr7    sim_recflg,need_lba     ;Test sim_recflg for valid LBA (5/6)
;
; we have determined that a valid LBA is loaded, so we need to compare the LBA
; that's been requested against the currently loaded LBA. If they are the same, we're
; done and can return. If they are not, then we need to check the dirty block flag to
; ensure the active block is written before we over-write it with the new one.
;
                 lda    sim_rectemp+0   ;Get low order requested LBA (4)
                 cmp    sim_curlba+0    ;Compare to current LBA (4)
                 bne    test_dirty      ;Bad compare, test for dirty block (2/3)
                 lda    sim_rectemp+1   ;Get middle order requested LBA (4)
                 cmp    sim_curlba+1    ;Compare to current LBA (4)
                 bne    test_dirty      ;Bad compare, test for dirty block (2/3)
                 lda    sim_rectemp+2   ;Get high order requested LBA (4)
                 cmp    sim_curlba+2    ;Compare to current LBA (4)
                 bne    test_dirty      ;Bad compare, test for dirty block (2/3)
                 rts                    ;Good compare, requested LBA is loaded (6)
;
; test the dirty block flag.
; the currently loaded LBA is not the one we need loaded.
; before we overwrite it, we'll test the dirty block flag.
; if it's set, we'll write that block to disk to update it, then transfer
; the parameters for the new LBA and load it.
;
test_dirty
                bbr6    sim_recflg,need_lba     ;Test sim_recflg for dirty block flag (5/6)
;
; we have a dirty block in the buffer... so we need to write the current LBA back
; to the drive to ensure we don't get corrupted data on the drive!
; note that the existing BIOS parameters still point to the currently loaded LBA.
;
                jsr     b_ide_write_lba ;Call BIOS to Write LBA (6)
                lda     ide_status_ram  ;Get Status from BIOS call (3)
                lsr     a               ;Shift error bit to carry (2)
                bcs     get_lba_err     ;Branch if error (2/3)
                rmb6    sim_recflg      ;reset dirty block flag (5)
;
; now that the dirty block has been cleared, we can load the required LBA
; into the disk buffer, update the flags and return.
;
; if we branched here from above, no valid LBA is loaded, so we just transfer the
; LBA parameters to the BIOS and read the block into the default LBA buffer.
;
need_lba
                lda     sim_rectemp+0   ;Get required LBA parameters (Low byte) (4)
                ldy     sim_rectemp+1   ; Middle byte (4)
                ldx     sim_rectemp+2   ; High byte (4)
;
                sta     sim_curlba+0    ;Save as Current LBA (4)
                sty     sim_curlba+1    ; for check the next (4)
                stx     sim_curlba+2    ; time get_lba is called (4)
;
                jsr     b_ide_set_lba   ;Call BIOS to set LBA parameters (6)
;
                jsr     b_ide_read_lba  ;Call BIOS to Read LBA (6)
                lda     ide_status_ram  ;Get Status from BIOS call (3)
                lsr     a               ;Shift error bit to carry (2)
                bcs     get_lba_err     ;Branch if error (2/3)
                smb7    sim_recflg      ;set valid LBA flag bit (5)
                rts                     ;return to caller, we're done (6)
get_lba_err
                rmb7    sim_recflg      ;reset valid LBA flag bit (5)
                lda     #<io_err_msg    ;Point to error message (2)
                ldy     #>io_err_msg    ; (2)
                jsr     m_promptr       ;Send it (6)
                jmp     sim_boot        ;Restart SIM (3)
;
;**************************************************************************************************
; SIM Data area
;
;Record level data area required by SIM
sim_seldrv      DEFS    1               ;Current selected drive number (0-7)
sim_dmaadr      DEFS    2               ;DMA address to Record transfer
sim_pemrec      DEFS    2               ;Record number requested by PEM
sim_rectemp     DEFS    3               ;Calculated LBA required by PEM (with Partition/Drive offsets)
sim_curlba      DEFS    3               ;LBA number currently loaded in Buffer (plus warm boot LBA)
;
;**************************************************************************************************
;
;Table of LBA offsets for all drives A thru H (8MB each, 16KB Blocks per drive)
;
;NOTE: These offsets are based on a non-partitioned drive!!! Relative to each other only!
; The partition offset below as "part_offset" is added to the "drv_offset" values the first time
; SIM is started. To ensure this only happens once, the "part_offset" data is zeroed out after it
; is added to the "drv_offset". Once the new values are loaded, the active drive being accessed
; is used as a base offset and the block offset calculated by the requested Record from PEM is
; added to get the actual 512-byte block address on the drive.
;
drv_offset
                .LONG   0               ;Drive A default (w/OS boot image)
                .LONG   16384           ;Drive B
                .LONG   32768           ;Drive C
                .LONG   49152           ;Drive D
                .LONG   65536           ;Drive E
                .LONG   81920           ;Drive F
                .LONG   98304           ;Drive G
                .LONG   114688          ;Drive H
;
;Partition Offset
; this offset is patched in during boot time from the boot loader.
; this compensates for the drive partition being located anywhere on the drive.
;
part_offset
                .LONG   131072          ;Partition offset for drive letters
;
;**************************************************************************************************
;
;Disk Control Blocks (DCBs)
; Updated to support 8 drives by default - KM
;
; Note: The Disk Control Block format has been changed with Version 3.20!
;       The second parameter is now defined as the number of records per block.
;       - this is the physical block size of the storage device. For any standard IDE device,
;         the default block size is 512 bytes. This calculates to 4 records per block.
;       The third parameter is now defined as the number of reserved records (128 bytes/record).
;       The last two parameters have been deleted! These last two parameters are not needed
;       for a standard block device, as the device itself can not be removed or ejected.
;
; The changes here frees up 3 bytes per DCB. Also note that the code that supports the checksum
; functions in PEM have also been eliminated. This frees up space in PEM and also provides
; a very slight performance improvement.
;
;drive a (Microdrive 8MB)
dcb_a           .DW     4079            ;max block number-1
                .DW     4               ;records per physical block
                .DW     256             ;number of reserved records (32KB OS-boot)
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almpa           ;address of allocation map
;
;drive b (Microdrive 8MB)
dcb_b           .DW     4095            ;max block number-1
                .DW     4               ;records per physical block
                .DW     0               ;number of reserved records
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almpb           ;address of allocation map

;
;drive c (Microdrive 8MB)
dcb_c           .DW     4095            ;max block number-1
                .DW     4               ;records per physical block
                .DW     0               ;number of reserved records
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almpc           ;address of allocation map
;
;drive d (Microdrive 8MB)
dcb_d           .DW     4095            ;max block number-1
                .DW     4               ;records per physical block
                .DW     0               ;number of reserved records
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almpd           ;address of allocation map
;
;drive e (Microdrive 8MB)
dcb_e           .DW     4095            ;max block number-1
                .DW     4               ;records per physical block
                .DW     0               ;number of reserved records
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almpe           ;address of allocation map
;
;drive f (Microdrive 8MB)
dcb_f           .DW     4095            ;max block number-1
                .DW     4               ;records per physical block
                .DW     0               ;number of reserved records
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almpf           ;address of allocation map
;
;drive g (Microdrive 8MB)
dcb_g           .DW     4095            ;max block number-1
                .DW     4               ;records per physical block
                .DW     0               ;number of reserved records
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almpg           ;address of allocation map
;
;drive h (Microdrive 8MB)
dcb_h           .DW     4095            ;max block number-1
                .DW     4               ;records per physical block
                .DW     0               ;number of reserved records
                .DB     1               ;block size = 2048
                .DW     1023            ;max directory-1
                .DW     almph           ;address of allocation map
;
;**************************************************************************************************
;
                END

