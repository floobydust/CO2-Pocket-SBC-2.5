;**************************************************************************************************
;*                                                                                                *
;*                Utility program for the RTC/MicroDrive PATA Adapter 0.83                        *
;*                                                                                                *
;*        This Utility provides functions to setup the DS15x1 Realtime Clock Date/Time            *
;*         - Note: Time is kept in 24hr format only                                               *
;*                 A 2-byte signature is loaded into NVRAM to identify RTC presence               *
;*                                                                                                *
;*       This Utility provides functions to identify, test and edit a MicroDrive when present     *
;*         The following functions are provided:                                                  *
;*           - Identify MicroDrive: Vendor String, Device Serial, LBA Count in Hex/Decimal)       *
;*           - Read any LBA into memory and display contents in Hex/ASCII                         *
;*           - Write any LBA from memory and display contents in Hex/ASCII                        *
;*           - Read Full LBA count sequentially or staggered                                      *
;*           - Write Full LBA count sequentially or staggered                                     *
;*         - Note: Write pattern is $55AA for Sequential Write                                    *
;*                 Write pattern is User defined for Write Benchmark                              *
;*                                                                                                *
;**************************************************************************************************

        PL      66      ;Page Length
        PW      132     ;Page Width (# of char/line)
        CHIP    W65C02S ;Enable WDC 65C02 instructions
        PASS1   OFF     ;Set ON when used for debug
        INCLIST ON      ;Set ON for listing Include files
;**************************************************************************************************
;
; C02BIOS/C02Monitor Version 4.0x is the supported BIOS/Monitor level for this utility!
;
; This BIOS and Monitor version also use a common source file for constants and variables used by
; both. This just simplifies keeping both code pieces in sync.
;
;**************************************************************************************************
;
; Page Zero definitions $00 to $9F reserved for user routines
; NOTES:- Locations $00 and $01 are used to zero RAM (calls CPU reset)
;       - EEPROM Byte Write routine loaded into Page Zero at $90-$A4
;
;       - Page Zero definitions for HEX2BCD and BCD2HEX routines, RTC setup
;
HPHANTOM        .EQU    $0010           ; HPHANTOM MUST be located (in target memory)
HEX0AND1        .EQU    $11             ;  immediately below the HEX0AND1 variable
HEX2AND3        .EQU    $12
HEX4AND5        .EQU    $13
HEX6AND7        .EQU    $14
;
DPHANTOM        .EQU    $0014           ; DPHANTOM MUST be located (in target memory)
DEC0AND1        .EQU    $15             ; immediately below the DEC0AND1 variable
DEC2AND3        .EQU    $16
DEC4AND5        .EQU    $17
DEC6AND7        .EQU    $18
DEC8AND9        .EQU    $19
;
BUFADR          .EQU    $20
BUFADRH         .EQU    $21
;
IBUFF           .EQU    $30             ;Input buffer... lots of space available
;
RTC_LOAD        .EQU    $40             ;Start of Data to set hardware RTC
;
;**************************************************************************************************
;
        INCLUDE         C02Constants4.asm       ;C02 BIOS/Monitor variables, etc.
;
;**************************************************************************************************
;       BIOS JUMP Table starts at $FF00
;       - BIOS calls are listed below - total of 32
;       - Reserved calls are for future hardware support
;**************************************************************************************************
;
;The following 32 functions are provided by BIOS via the JMP Table below
;
B_IDE_RESET     .EQU    $FF00   ;Reset IDE and Run Diagnostics
B_IDE_GET_STAT  .EQU    $FF03   ;Get IDE Status and Extended Error codes
B_IDE_IDENTIFY  .EQU    $FF06   ;Get IDE Identification Block
B_IDE_READ_LBA  .EQU    $FF09   ;Read a Block from IDE device
B_IDE_WRITE_LBA .EQU    $FF0C   ;Write a Block to IDE device
B_IDE_VERFY_LBA .EQU    $FF0F   ;Verify the last Block from IDE device
B_IDE_SET_LBA   .EQU    $FF12   ;Set the LBA Block ID for Read/Write/Verify
B_IDE_SET_ADDR  .EQU    $FF15   ;Set the Memory Address to transfer Block data to/from
B_IDE_EN_CACHE  .EQU    $FF18   ;Enable Write Cache
B_IDE_DIS_CACHE .EQU    $FF1B   ;Disable Write Cache
;
B_RTC_NVRD      .EQU    $FF1E   ;Read the NVRAM from the RTC
B_RTC_NVWR      .EQU    $FF21   ;Write the NVRAM to the RTC
B_RTC_INIT      .EQU    $FF24   ;Initialize the Software RTC from the hardware RTC
;
B_CHRIN2        .EQU    $FF27   ;Data Input from second serial port
B_CHROUT2       .EQU    $FF2A   ;Data Output from second serial port
;
B_CNT_INIT      .EQU    $FF2D   ;Initialize/Start benchmark counter
B_CNT_STRT      .EQU    $FF30   ;Reset/Start benchmark counter
B_CNT_STOP      .EQU    $FF33   ;Stop benchmark counter
;
B_CHRIN_NW      .EQU    $FF36   ;Data input from console, no waiting, clear carry if none
B_CHRIN         .EQU    $FF39   ;Data input from console, carry set if data
B_CHROUT        .EQU    $FF3C   ;Data output to console, sent data preserved
;
B_SET_DLY       .EQU    $FF3F   ;Set delay value for milliseconds and 16-bit counter
B_EXE_MSDLY     .EQU    $FF42   ;Execute millisecond delay 1-256 * 10 milliseconds
B_EXE_LGDLY     .EQU    $FF45   ;Execute long delay; millisecond delay * 16-bit count
;
B_RESERVE       .EQU    $FF48   ;Reserved BIOS call, executes to a RTS
;
B_INIT_VEC      .EQU    $FF4B   ;Initialize soft vectors at $0300 from ROM
B_INIT_CFG      .EQU    $FF4E   ;Initialize soft config values at $0320 from ROM
B_INIT_2691     .EQU    $FF51   ;Initialize SCC2691 console 38.4K, 8-N-1 RTS/CTS
B_RESET_2691    .EQU    $FF54   ;Reset SCC2691 - called before INIT_2691
;
B_WRMMNVEC0     .EQU    $FF57   ;Monitor warm start - jumps to page $03
B_CLDMNVEC0     .EQU    $FF5A   ;Monitor cold start - jumps to page $03
B_COLDSTRT      .EQU    $FF5D   ;System cold start - RESET vector for 65C02
;
;**************************************************************************************************
;       Monitor JUMP table starts at $E000
;       - Monitor calls are listed below - total of 32
;       - Reserved calls are for future functions
;**************************************************************************************************
;
M_COLD_MON      .EQU    $E000   ;Monitor Cold start entry
M_WARM_MON      .EQU    $E003   ;Monitor Warm start entry
;
M_RESERVE2      .EQU    $E006   ;Reserved function call
;
M_MOVE_RAM      .EQU    $E009   ;Move RAM block
M_FILL_LP       .EQU    $E00C   ;Fill Memory Block
;
M_BSOUT         .EQU    $E00F   ;Send a Backspace to console
M_XMDM_SAVE     .EQU    $E012   ;Xmodem Save - entry point for external use
M_XMDM_LOAD     .EQU    $E015   ;Xmodem Load - entry point for external use
M_BENCH         .EQU    $E018   ;Start Benchmark Timer (resets count)
M_QUITB         .EQU    $E01B   ;Quits Benchmark Timer (displays elapsed time)
M_UPTIME        .EQU    $E01E   ;Displays SW-RTC Date/Time
M_PRSTAT1       .EQU    $E021   ;Displays Processor Status
M_DIS_LINE      .EQU    $E024   ;Disassembles a line of code (Mnemonic/Operand)
M_INCINDEX      .EQU    $E027   ;Increments 16-bit Index pointer
M_DECINDEX      .EQU    $E02A   ;Decrements 16-bit Index pointer
M_RDLINE        .EQU    $E02D   ;Read a Line of Data from console
M_RDCHAR        .EQU    $E030   ;Read a Character from console
M_HEXIN2        .EQU    $E033   ;Get two Hex digits from console entry (1-byte)
M_HEXIN4        .EQU    $E036   ;Get four Hex digits from console entry (2-bytes)
M_HEX2ASC       .EQU    $E039   ;Converts Hex into ASCII (1-byte to 2-ASCII digits)
M_BIN2ASC       .EQU    $E03C   ;Converts Binary to ASCII
M_ASC2BIN       .EQU    $E03F   ;Converts two ASCII digits to one byte
M_BEEP          .EQU    $E042   ;Send a Bell character to console
M_DOLLAR        .EQU    $E045   ;Send a $ character to console
M_CROUT         .EQU    $E048   ;Send a CR/LF sequence to console
M_SPC           .EQU    $E04B   ;Send a Space character to console
M_PRBYTE        .EQU    $E04E   ;Print a byte as two ASCII digits to console
M_PRWORD        .EQU    $E051   ;Print a word as four ASCII digits to console
M_PRASC         .EQU    $E054   ;Print ASCII character to console
M_PROMPT        .EQU    $E057   ;Print a Message number to console
M_PROMPTR       .EQU    $E05A   ;Print a String to console (via 16-bit address)
M_CONTINUE      .EQU    $E05D   ;Prompt to continue, else pull return address from stack and return
;
PEM             .EQU    $0103   ;PEM Entry
;**************************************************************************************************
;
;       User program code can start here. Default to $0800, can be chaged as required
;
;**************************************************************************************************
;
        .ORG    $0800                           ;Start of User RAM for programs
;
; First, send an intro message to the console, Utility name and version.
; Second, send the User Menu to the console, then enter Command mode.
;
START
                LDA     #<INTRO_MSG             ;Load Message address
                LDY     #>INTRO_MSG             ;into A/Y Regs
                JSR     M_PROMPTR               ;Call Monitor routine
MENU_LOOP
                LDA     #<MENU_MSG              ;Load Menu address
                LDY     #>MENU_MSG              ;into A/Y Regs
                JSR     M_PROMPTR               ;Call Monitor routine
;
MAIN_LOOP
                JSR     M_RDCHAR                ;Wait for keystroke (converts to upper-case)
                LDX     #MONTAB-MONCMD-1        ;Get command list count
CMD_LP          CMP     MONCMD,X                ;Compare to command list
                BNE     CMD_DEC                 ;Check for next command and loop
                PHA                             ;Save keystroke
                TXA                             ;Xfer Command index to A reg
                ASL     A                       ;Multiply keystroke value by 2 (command offset)
                TAX                             ;Xfer Command offsett address to table MONTAB
                PLA                             ;Restore keystroke (some commands send to terminal)
                JSR     DOCMD                   ;Call Monitor command processor as a subroutine
                BRA     MENU_LOOP               ;Command processed, branch/wait for next command
DOCMD           JMP     (MONTAB,X)              ;Execute command from Table
;
CMD_DEC         DEX                             ;Decrement index count
                BPL     CMD_LP                  ;If more to check, loop back
                JSR     M_BEEP                  ;Beep for error, not valid command character
                BRA     MAIN_LOOP               ;Branch back and re-enter Monitor
;
; Command Code List
;
MONCMD
        .DB     "1"                             ;Set Date and Time to RTC
        .DB     "2"                             ;Read NVRAM and Display
        .DB     "3"                             ;Write NVRAM and Display
;
        .DB     "4"                             ;Identify MicroDrive and display
        .DB     "5"                             ;Read LBA and display
        .DB     "6"                             ;Write LBA and verify
        .DB     "7"                             ;Sequential Read of all LBA
        .DB     "8"                             ;Sequential Write of all LBA
        .DB     "9"                             ;Benchmark MicroDrive Read/Write
        .DB     "M"                             ;Menu display
        .DB     "S"                             ;System Transfer RAM to Disc
        .DB     "Q"                             ;Quit Utility
MONTAB
        .DW     RTC_TD                          ;Address of RTC Date and Time routine
        .DW     NVRAM_RD                        ;Address of RTC NVRAM Read
        .DW     NVRAM_WR                        ;Address of RTC NVRAM Write
        .DW     IDE_IDENTIFY                    ;Address of IDE Identify routine
        .DW     IDE_READ_LBA                    ;Address of IDE Read/Display LBA
        .DW     IDE_WRITE_LBA                   ;Address of IDE Write/Verify LBA
        .DW     IDE_SEQ_READ                    ;Address of Sequential LBA Read
        .DW     IDE_SEQ_WRITE                   ;Address of Sequential LBA Write
        .DW     IDE_BENCHMARK                   ;Address of IDE Benchmark
        .DW     MENU_LOOP                       ;Address of Main Menu
        .DW     SYS_XFER                        ;Address of System Transfer
        .DW     QUIT                            ;Address of Quit Utility
;
; Realtime Clock Routines
;
RTC_TD
; This routine will allow the Alarm and Interrupt functions to be cleared and
; - allow the currend Date and Time to be set. Once these have been completed,
; - a signature will be added to the last two bytes of the NVRAM.
;
; The format for the current date and time:
; - Year: 1900 - 2100 (not checked for range)
; - Month: 1 - 12 (not checked for range)
; - Day: 1 - 31 (not checked for number per month, common sense for input)
; - Day of week: 1 - 7 (Saturday is 1.... Friday is 7)
;
; - Hours: 0 - 23 (24 hour default)
; - Minutes: 0 - 59
; - Seconds: 0 - 59
;
; - Once confirmed, the RTC data will be updated, followed by the SW RTC
;   being setup via the BIOS call.
;
; Now prompt the user to reset the Alarm functions of the DS15x1
; - In some cases, an errant program might write the registers of the RTC chip and
; - enable one or more of the interrupts. As the current BIOS does not handle these
; - interrupts, it's possible the RTC will generate an interrupt and thereby lock up
; - the system. An IRQ jumper on the board alleviates this.
;
                LDA     #<ALARM_CLEAR           ;Load Message address
                LDY     #>ALARM_CLEAR           ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
ALARM_PROMPT
                JSR     M_RDCHAR                ;Get character from user
                JSR     B_CHROUT                ;Send to console
                CMP     #$59                    ;Test for yes
                BEQ     ALARM_RESET             ;If yes, go reset alarm functions
                CMP     #$4E                    ;Test for no
                BEQ     ALARM_END               ;If no, skip reset alarm functions
                JSR     M_BEEP                  ;Else, error, send beep
                BRA     ALARM_PROMPT            ;Branch back and try again
;
ALARM_RESET
                LDA     RTC_ALARM_SEC           ;Get Alarm Seconds
                AND     #%01111111              ;Mask off Alarm bit
                STA     RTC_ALARM_SEC           ;Save it back
;
                LDA     RTC_ALARM_MIN           ;Get Alarm Minutes
                AND     #%01111111              ;Mask off Alarm bit
                STA     RTC_ALARM_MIN           ;Save it back
;
                LDA     RTC_ALARM_HRS           ;Get Alarm Hours
                AND     #%01111111              ;Mask off Alarm bit
                STA     RTC_ALARM_HRS           ;Save it back
;
                LDA     RTC_ALARM_DYDT          ;Get Alarm Day/Date
                AND     #%01111111              ;Mask off Alarm bit
                STA     RTC_ALARM_DYDT          ;Save it back
;
; Now clear the Control Register bits that enable all interrupts
;
                LDA     RTC_CONTROL_A           ;Get Control A register
                AND     #%11110000              ;Mask off all Alarm IRQ bits
                STA     RTC_CONTROL_A           ;Save it back to disable alarms
;
                LDA     RTC_CONTROL_B           ;Get Control B register
                AND     #%11100001              ;Mask off TPE, TIE, KIE, WDE bits
                STA     RTC_CONTROL_B           ;Save it back to disable alarms
;
ALARM_END
;
                LDA     #<RTC_INTRO             ;Load Message address
                LDY     #>RTC_INTRO             ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
;User prompts for RTC data, i.e., year, month, date, day, etc.
; Note: Entry values are NOT checked for proper range!
;
                LDA     #<RTC_YEAR_M            ;Load Message address
                LDY     #>RTC_YEAR_M            ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                LDX     #$04                    ; X-REGISTER = maximum number of digits allowed
                JSR     RDLINE                  ;Request ASCII DECIMAL digit(s) input from terminal
;
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                LDA     DEC6AND7                ;Get Decimal for Century (19-21)
                STA     RTC_LOAD+0              ;Save into Data space
                LDA     DEC8AND9                ;Get Decimal for Year (00-99)
                STA     RTC_LOAD+1              ;Save into Data space
;
                LDA     #<RTC_MONTH_M           ;Load Message address
                LDY     #>RTC_MONTH_M           ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                LDX     #$02                    ; X-REGISTER = maximum number of digits allowed
                JSR     RDLINE                  ;Request ASCII DECIMAL digit(s) input from terminal
;
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                LDA     DEC8AND9                ;Get Decimal for Month (1-12)
                STA     RTC_LOAD+2              ;Save into Data space
;
                LDA     #<RTC_DATE_M            ;Load Message address
                LDY     #>RTC_DATE_M            ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                LDX     #$02                    ; X-REGISTER = maximum number of digits allowed
                JSR     RDLINE                  ;Request ASCII DECIMAL digit(s) input from terminal
;
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                LDA     DEC8AND9                ;Get Decimal for Date (1-31)
                STA     RTC_LOAD+3              ;Save into Data space
;
                LDA     #<RTC_DAY_M             ;Load Message address
                LDY     #>RTC_DAY_M             ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                LDX     #$02                    ; X-REGISTER = maximum number of digits allowed
                JSR     RDLINE                  ;Request ASCII DECIMAL digit(s) input from terminal
;
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                LDA     DEC8AND9                ;Get Decimal for Day of week (1-7)
                STA     RTC_LOAD+4              ;Save into Data space
;
                LDA     #<RTC_HOURS_M           ;Load Message address
                LDY     #>RTC_HOURS_M           ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                LDX     #$02                    ; X-REGISTER = maximum number of digits allowed
                JSR     RDLINE                  ;Request ASCII DECIMAL digit(s) input from terminal
;
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                LDA     DEC8AND9                ;Get Decimal for Hours (00-23)
                STA     RTC_LOAD+5              ;Save into Data space
;
                LDA     #<RTC_MINUTES_M         ;Load Message address
                LDY     #>RTC_MINUTES_M         ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                LDX     #$02                    ; X-REGISTER = maximum number of digits allowed
                JSR     RDLINE                  ;Request ASCII DECIMAL digit(s) input from terminal
;
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                LDA     DEC8AND9                ;Get Decimal for Minutes (00-59)
                STA     RTC_LOAD+6              ;Save into Data space
;
                LDA     #<RTC_SECONDS_M         ;Load Message address
                LDY     #>RTC_SECONDS_M         ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                LDX     #$02                    ; X-REGISTER = maximum number of digits allowed
                JSR     RDLINE                  ;Request ASCII DECIMAL digit(s) input from terminal
;
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                LDA     DEC8AND9                ;Get Decimal for Seconds (00-59)
                STA     RTC_LOAD+7              ;Save into Data space
;
                LDA     #<RTC_WRITE             ;Load Message address
                LDY     #>RTC_WRITE             ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
RTC_PROMPT
                JSR     M_RDCHAR                ;Get character from user
                JSR     B_CHROUT                ;Send to console
                CMP     #$59                    ;Test for yes
                BEQ     RTC_TIME_SYNC           ;If yes, go set RTC
                CMP     #$4E                    ;Test for no
                BEQ     RTC_END                 ;If no, skip RTC set
                JSR     M_BEEP                  ;Else, error, send beep
                BRA     RTC_PROMPT              ;Branch back and try again
;
RTC_TIME_SYNC
                JSR     M_RDCHAR                ;Get character from user
                CMP     #$0D                    ;Check for C/R
                BNE     RTC_TIME_SYNC           ;Loop until C/R entered
;
RTC_TIME_SET
                LDA     #%10000000              ;Get TE Bit mask
                TRB     RTC_CONTROL_B           ;Turn off TE Bit for update
;
                LDA     RTC_LOAD+7              ;Get Seconds
                STA     RTC_SECONDS             ;Set Seconds
;
                LDA     RTC_LOAD+6              ;Get Minutes
                STA     RTC_MINUTES             ;Set Minutes
;
                LDA     RTC_LOAD+5              ;Get Hours
                STA     RTC_HOURS               ;Set Hours
;
                LDA     RTC_LOAD+4              ;Get Day
                STA     RTC_DAY                 ;Set Day
;
                LDA     RTC_LOAD+3              ;Get Date
                STA     RTC_DATE                ;Set Date
;
                LDA     RTC_MONTH               ;Get RTC Month data
                AND     #$11100000              ;Mask off old Month (save upper 3 bits)
                ORA     RTC_LOAD+2              ;OR in new Month
                STA     RTC_MONTH               ;Set Month
;
                LDA     RTC_LOAD+1              ;Get Year
                STA     RTC_YEAR                ;Set Year
;
                LDA     RTC_LOAD+0              ;Get Century
                STA     RTC_CENTURY             :Set Century
;
                LDA     #%10000000              ;Get TE Bit mask
                TSB     RTC_CONTROL_B           ;Turn on TE Bit to update
;
                LDA     #<RTC_CONFIRM           ;Load Message address
                LDY     #>RTC_CONFIRM           ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
RTC_END
;
; Now write the signature to NVRAM
; - A signature of "KM" is used by BIOS to test for the presence of the RTC chip.
; - if this siganture is found as the last two bytes ($FE and $FF), then the software
; - RTC routine variables will be loaded from the RTC and become the active Date and
; - Time. If not, the start of Epoch Date will be loaded as the default.
;
                LDX     #$FE                    ;Load NVRAM Offset
                STX     RTC_RAM_ADDR            ;Set index to start of signature
                LDA     #"K"                    ;Get first signature byte
                STA     RTC_RAM_DATA            ;Store in NVRAM
                INX                             ;Increment Offset
                STX     RTC_RAM_ADDR            ;Set index to start of signature
                LDA     #"M"                    ;Get second signature byte
                STA     RTC_RAM_DATA            ;Store in NVRAM
;
                JSR     B_RTC_INIT              ;Init software RTC from hardware RTC
                BRA     JUI_0                   ;Prompt User to return or continue
;
NVRAM_RD
; This routine will use the BIOS function to read the contents of NVRAM into memory
;  The contents will be displayed in Hex and ASCII.
;
                LDA     #<NVRAM_DATA            ;Load NVRAM Data address
                LDY     #>NVRAM_DATA            ;into A/Y regs
                STA     INDEXL                  ;And same into
                STY     INDEXH                  ;Index for display
                JSR     B_RTC_NVRD              ;Call BIOS to load NVRAM
                LDX     #$10                    ;Set index for 16 rows (256 bytes)
                JSR     DUMP                    ;Display NVRAM data
;
                BRA     JUI_0                   ;Prompt user for what to do
;
NVRAM_WR
; This routine will use the BIOS function to write the contents of NVRAM from memory
;  The contents are written from the NVRAM Data area, normally $0500 or from a user
;  entered address. The user can edit the memory before invoking the MDUTIL program.
;  Note that the signature to validate the presence of the RTC is not checked before
;  it is written. This allows the RTC to be removed from the BIOS detection by changing
;  the signature to an invalid 2-bytes.
;
; First, present a message to warn the user, then request an address for the block of
;  data to be written to the NVRAM. If no input from the user is received, the default
;  NVRAM_DATA location ($0500) will be used. Once this is done, the user is prompted to
;  confirm the NVRAM write.
;
                LDA     #<NVRAM_WRMSG           ;Load Message address
                LDY     #>NVRAM_WRMSG           ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
                JSR     M_HEXIN4                ;Get load address from user
                BNE     SET_NVRAM               ;If data entered, use it
;
                LDA     #<NVRAM_DATA            ;Else, load NVRAM Data address
                LDY     #>NVRAM_DATA            ;into A/Y regs
SET_NVRAM
                STA     INDEXL                  ;Else, save user entered
                STY     INDEXH                  ; address for NVRAM write
                PHA                             ;Save address to stack
                PHY                             ;to confirm write later
NV_SHOW
                LDX     #$10                    ;Set index for display page
                JSR     DUMP                    ;Show data to be written to NVRAM
;
                LDA     #<NVRAM_CONF            ;Load Confirm message address
                LDY     #>NVRAM_CONF            ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
;
NVRAM_PROMPT
                JSR     M_RDCHAR                ;Get character from user
                JSR     B_CHROUT                ;Send to console
                CMP     #$59                    ;Test for yes
                BEQ     NV_WRITE_DATA           ;If yes, go reset alarm functions
                CMP     #$4E                    ;Test for no
                BEQ     NV_NO_WRITE             ;If no, skip RTC set
                JSR     M_BEEP                  ;Else, error, send beep
                BRA     NVRAM_PROMPT            ;Branch back and try again
;
NV_WRITE_DATA
                JSR     M_CROUT                 ;Send C/R to console
                PLY                             ;Restore address from stack
                PLA                             ;for NVRAM write
                JSR     B_RTC_NVWR              ;Call BIOS to write NVRAM
                LDA     #<NVRAM_DONE            ;Load Complete message address
                LDY     #>NVRAM_DONE            ;into A/Y regs
                JSR     M_PROMPTR               ;Send message to console
                BRA     JUI_0                   ;Prompt User
;
NV_NO_WRITE
                PLY                             ;Restore address from stack
                PLA                             ;for NVRAM write
                JSR     M_CROUT                 ;Send C/R to console
JUI_0           JMP     USER_INPUT              ;Prompt User
;
;[D] HEX/TEXT DUMP command:
; Display in HEX followed by TEXT, the contents of 256 consecutive memory addresses
;
DUMP
                STX     ROWS                    ;Save Row count
                STZ     TEMP1L                  ;Clear Offset to data
                STZ     TEMP1H                  ;used for showing loaded data from device
;
LINED           JSR     DMPGR                   ;Send address offsets to terminal
                JSR     GLINE                   ;Send horizontal line to terminal
                JSR     M_CROUT                 ;Send CR,LF to terminal
                LDX     ROWS                    ;Set line count for rows displayed
DLINE           JSR     M_SPC                   ;Send 4 Spaces to terminal
                JSR     M_SPC
                JSR     M_SPC
                JSR     M_SPC
                JSR     PROFFSET                ;Print INDEX value
                JSR     M_SPC                   ;Send 2 Spaces to terminal
                JSR     M_SPC
                LDY     #$00                    ;Initialize line byte counter
GETBYT
                LDA     (INDEXL)
                STA     SRCHBUFF,Y              ;Save in Search buffer (16 bytes)
                JSR     M_PRBYTE                ;Display byte as a HEX value
                JSR     M_SPC                   ;Send Space to terminal
                JSR     M_INCINDEX              ;Increment Index to next byte location
                JSR     INCOFFSET               ;Increment Offset address
                INY                             ;Increment index
                CPY     #$10                    ;Check for all 16
                BNE     GETBYT                  ;Loop back until 16 bytes have been displayed
                JSR     M_SPC                   ;Send a space
                LDY     #$00                    ;Reset index for SRCHBUFF
GETBYT2         LDA     SRCHBUFF,Y              ;Get buffered line (16 bytes)
                JSR     M_PRASC                 ;Print ASCII character
                INY                             ;Increment index to next byte
                CPY     #$10                    ;Check for 16 bytes
                BNE     GETBYT2                 ;Loop back until 16 bytes have been displayed
                JSR     M_CROUT                 ;Else, send CR,LF to terminal
                DEX                             ;Decrement line count
                BNE     DLINE                   ;Branch back until all rows done
                JSR     GLINE                   ;Send horizontal line to terminal
;
;DMPGR subroutine: Send address offsets to terminal
;
DMPGR           LDA     #$02                    ;Get msg for "addr:" to terminal
                JSR     M_PROMPT                ;Send to terminal
                JSR     M_SPC                   ;Add two additional spaces
                JSR     M_SPC
                LDX     #$00                    ;Zero index count
MDLOOP          TXA                             ;Send "00" - "0F", separated by 1 Space to terminal
                JSR     M_PRBYTE                ;Print byte value
                JSR     M_SPC                   ;Add a space
                INX                             ;Increment the count
                CPX     #$10                    ;Check for 16
                BNE     MDLOOP                  ;Loop back until done
;
;Print the ASCII text header "0123456789ABCDEF"
;
                JSR     M_SPC                   ;Send a space
                LDX     #$00                    ;Zero X reg for "0"
MTLOOP          TXA                             ;Xfer to A reg
                JSR     M_BIN2ASC               ;Convert Byte to two ASCII digits
                TYA                             ;Xfer the low nibble character to A reg
                JSR     B_CHROUT                ;Send least significant HEX to terminal
                INX                             ;Increment to next HEX character
                CPX     #$10                    ;Check for 16
                BNE     MTLOOP                  ;Branch back till done
                JMP     M_CROUT                 ;Do a CR/LF and return
;
;GLINE subroutine: Send a horizontal line to console used by memory display only.
;
GLINE           LDX     #$4F                    ;Load index for 79 decimal
                LDA     #$7E                    ;Get "~" character
GLINEL          JSR     B_CHROUT                ;Send to terminal (draw a line)
                DEX                             ;Decrement count
                BNE     GLINEL                  ;Branch back until done
                RTS                             ;Return to caller
;
;PRINT Offset subroutine: Prints a $ sign followed by TEMP1L/H
;
PROFFSET        JSR     M_DOLLAR                ;Print a $ sign
                LDA     TEMP1L                  ;Get Index Low byte
                LDY     TEMP1H                  ;Get Index High byte
                JMP     M_PRWORD                ;Print Word, return
;
;Increment Data offset to display
;
INCOFFSET
                INC     TEMP1L                  ;Increment low byte
                BNE     SK_HIOFF                ;If not equal, skip high byte
                INC     TEMP1H                  ;Increment high byte
SK_HIOFF        RTS                             ;Return to caller
;
; MicroDrive Routines
;
IDE_IDENTIFY
; Uses the BIOS call to load drive identity information.
; This routine will display the following information from the ID block:
;
;       Bytes $36 - $5D       Model Number:
;       Bytes $14 - $27       Serial Number:
;       Bytes $2E - $35       Firmware Revision:
;       Bytes $62 - $63       LBA Mode Support:
;       Bytes $78 - $7B       Total LBA Count:
;
; The above data is in Big Endian format or ASCII format
;
                JSR     B_IDE_IDENTIFY          ;Call BIOS routine
                JSR     SWAP_BYTE               ;Swap High/Low Bytes
;
                LDA     #<DRIVE_IDENTITY        ;Get low order offset
                LDY     #>DRIVE_IDENTITY        ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
;ID Block now loaded into buffer
; Next, print the description message, then print the offset to the ID block
; for ASCII text, but will need to do some transforms for Mode Support and
; LBA block count.
;
                LDA     #<MODEL_NUM             ;Get low order offset
                LDY     #>MODEL_NUM             ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
                LDA     #<LBA_BUFFER+$36        ;Get low order offset
                LDY     #>LBA_BUFFER+$36        ;Get high order offset
                LDX     #40                     ;Byte count to display
                JSR     STRING_OUT              ;Use string out routine
;
                LDA     #<SERIAL_NUM            ;Get low order offset
                LDY     #>SERIAL_NUM            ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
                LDA     #<LBA_BUFFER+$14        ;Get low order offset
                LDY     #>LBA_BUFFER+$14        ;Get high order offset
                LDX     #20                     ;Byte count to display
                JSR     STRING_OUT              ;Use string out routine
;
                LDA     #<FIRM_REV              ;Get low order offset
                LDY     #>FIRM_REV              ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
                LDA     #<LBA_BUFFER+$2E        ;Get low order offset
                LDY     #>LBA_BUFFER+$2E        ;Get high order offset
                LDX     #8                      ;Byte count to display
                JSR     STRING_OUT              ;Use string out routine
;
                LDA     #<MODE_SUPPORT          ;Get low order offset
                LDY     #>MODE_SUPPORT          ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
                LDA     LBA_BUFFER+$62          ;Get Capabilities data (bit 9 of Word 49)
                LSR     A                       ;Shift DMA bit to Carry (dont't care)
                LSR     A                       ;Shift LBA bit to Carry (do care)
                BCS     LBA_MODE_Y              ;If active, finish setup
;
                LDA     #$4E                    ;Get "N"
                BRA     LBA_MODE_N              ;Finish sending to console
LBA_MODE_Y
                LDA     #$59                    ;Get "Y"
LBA_MODE_N
                JSR     B_CHROUT                ;Send to console
;
                LDA     #<TOTAL_LBA             ;Get low order offset
                LDY     #>TOTAL_LBA             ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
                LDA     LBA_BUFFER+$78          ;Get LBA count data
                STA     HEX4AND5                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$79          ;Get LBA count data
                STA     HEX6AND7                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$7A          ;Get LBA count data
                STA     HEX0AND1                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$7B          ;Get LBA count data
                STA     HEX2AND3                ;Store in Page Zero work area
;
                JSR     HEXTOBCD                ;Convert 32-bit Hex to ASCII BCD
                JSR     BCDOUT                  ;Print BCD count to console
NO_NEXT_LBA     JMP     USER_INPUT              ;Prompt user for next command
;
IDE_READ_LBA
; This routine will read a User requested LBA and display as Hex/ASCII
; - The "N" key will show the next in sequence
; - Hitting Return will end the LBA Read/Display sequence
;
                STZ     LBA_LOW_WORD            ;Zero out LBA count
                STZ     LBA_LOW_WORD+1
                STZ     LBA_HIGH_WORD
                STZ     LBA_HIGH_WORD+1
;
                LDA     #<LBA_INPUT             ;Get LBA Input message
                LDY     #>LBA_INPUT
                JSR     M_PROMPTR               ;Send to console
                JSR     GET_LBA_NUM             ;Get LBA number to read
;
                STA     LBA_LOW_WORD            ;Save Current LBA number
                STY     LBA_LOW_WORD+1          ;to local variables
                STX     LBA_HIGH_WORD
READ_NEXT_LBA
                LDA     #<LBA_BUFFER            ;Setup LBA Buffer
                LDY     #>LBA_BUFFER            ; address
                LDX     #$01                    ;LBA count = 1
                STA     INDEXL                  ;Save Index L
                STY     INDEXH                  ;Save Index H
                JSR     B_IDE_SET_ADDR          ;Set Buffer address
;
                JSR     B_IDE_READ_LBA          ;Read LBA into Buffer
                LDA     IDE_STATUS_RAM          ;Get IDE Status
                LSR     A                       ;Shift error bit into carry
                BCS     IDE_RW_ERR              ;If carry set, handle read error
;
                LDX     #$20                    ;Set display range for 32 rows
                JSR     DUMP                    ;Display data to console
;
; Prompt user: either display the next LBA or not.
;
                LDA     #<NEXT_LBA              ;Get LBA Output message
                LDY     #>NEXT_LBA
                JSR     M_PROMPTR               ;Send to console
;
LBA_TRY_AGAIN
                JSR     M_RDCHAR                ;Get input from console
                CMP     #$0D                    ;Check for C/R
                BEQ     NO_NEXT_LBA             ;If yes, exit
                CMP     #"N"                    ;Check for "N" for next
                BNE     BAD_ENTRY               ;Bad entry, branch
;
; Need to increase the current LBA number, then loop back and re-display
;
                JSR     LBA_BLK_UPDATE          ;Update LBA Block to read
;
                LDA     LBA_LOW_WORD            ;Get variables
                LDY     LBA_LOW_WORD+1
                LDX     LBA_HIGH_WORD
                JSR     B_IDE_SET_LBA           ;Set LBA to read
;
                LDA     LBA_LOW_WORD+1          ;Get LBA count data
                STA     HEX4AND5                ;Store in Page Zero work area
                LDA     LBA_LOW_WORD            ;Get LBA count data
                STA     HEX6AND7                ;Store in Page Zero work area
                LDA     LBA_HIGH_WORD+1         ;Get LBA count data
                STA     HEX0AND1                ;Store in Page Zero work area
                LDA     LBA_HIGH_WORD           ;Get LBA count data
                STA     HEX2AND3                ;Store in Page Zero work area
;
                JSR     HEXTOBCD                ;Convert and print to ASCII BCD
                LDA     #<SHOW_NEXT_LBA         ;Get LBA Output message
                LDY     #>SHOW_NEXT_LBA
                JSR     M_PROMPTR               ;Send to console
;
                JSR     BCDOUT                  ;Print BCD count to console
                BRA     READ_NEXT_LBA           ;Branch back to show next LBA
;
BAD_ENTRY
                JSR     M_BEEP                  ;Send error beep
                BRA     LBA_TRY_AGAIN           ;Branch and try again
;
IDE_RW_ERR
                JMP     IDE_ERROR_HANDLER       ;Jump to error handler
;
IDE_WRITE_LBA
; This routine will write a User requested LBA from the LBA_BUFFER
; - The Buffer data will be displayed first and then prompted for writing
;
                LDA     #<LBA_OUTPUT            ;Get LBA Output message
                LDY     #>LBA_OUTPUT
                JSR     M_PROMPTR               ;Send to console
                JSR     GET_LBA_NUM             ;Get LBA number to write
                LDA     #<LBA_BUFFER            ;Setup LBA Buffer
                LDY     #>LBA_BUFFER            ; address
                LDX     #$01                    ;LBA count = 1
                STA     INDEXL                  ;Save Index L
                STY     INDEXH                  ;Save Index H
                JSR     B_IDE_SET_ADDR          ;Set Buffer address
;
                LDA     #<LBA_WR_DATA           ;Get LBA Write message
                LDY     #>LBA_WR_DATA
                JSR     M_PROMPTR               ;Send to console
;
                LDX     #$20                    ;Set display range for 32 rows
                JSR     DUMP                    ;Display data to console
;
                LDA     #<LBA_WR_CNFM           ;Get LBA Confirm Write message
                LDY     #>LBA_WR_CNFM
                JSR     M_PROMPTR               ;Send to console
;
                JSR     M_CONTINUE              ;Prompt to confirm LBA write
;
                JSR     B_IDE_WRITE_LBA         ;Write LBA from Buffer
                LDA     IDE_STATUS_RAM          ;Get IDE Status
                LSR     A                       ;Shift error bit into carry
                BCS     IDE_RW_ERR              ;If carry set, handle write error
;
                JSR     B_IDE_VERFY_LBA         ;Verify LBA
                LDA     IDE_STATUS_RAM          ;Get IDE Status
                LSR     A                       ;Shift error bit into carry
                BCS     IDE_RW_ERR              ;If carry set, handle write error
;
                JMP     USER_INPUT              ;Get user input
;
IDE_SEQ_READ
; This routine will read the entire disk data one block at a time,
; - This can take a long time as the current LBA count is displayed.
;
                LDA     #<LBA_SEQ_RD_MSG        ;Get Seq Read Message address
                LDY     #>LBA_SEQ_RD_MSG
                JSR     M_PROMPTR               ;Send to console
;
                LDA     #<LBA_SEQ_TM_MSG        ;Get Time Message address
                LDY     #>LBA_SEQ_TM_MSG
                JSR     M_PROMPTR               ;Send to console
;
                JSR     B_IDE_IDENTIFY          ;Call BIOS routine
                JSR     SWAP_BYTE               ;Swap high/low bytes
;
                LDA     #<TOTAL_LBA             ;Get low order offset
                LDY     #>TOTAL_LBA             ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
                LDA     LBA_BUFFER+$78          ;Get LBA count data
                STA     HEX4AND5                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$79          ;Get LBA count data
                STA     HEX6AND7                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$7A          ;Get LBA count data
                STA     HEX0AND1                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$7B          ;Get LBA count data
                STA     HEX2AND3                ;Store in Page Zero work area
;
                JSR     HEXTOBCD                ;Convert and print to ASCII BCD
                JSR     BCDOUT                  ;Print BCD count to console
;
                LDA     #<LBA_SEQ_CFM           ;Get Message address
                LDY     #>LBA_SEQ_CFM
                JSR     M_PROMPTR               ;Send to console
;
                JSR     M_CONTINUE              ;Prompt user to continue
                JSR     M_CROUT                 ;Send CR/LF
;
                LDA     #<BENCH_BUFFER          ;Setup LBA Buffer
                LDY     #>BENCH_BUFFER          ; address
                LDX     #$01                    ;LBA count = 1
                JSR     B_IDE_SET_ADDR          ;Set Buffer address
;
                STZ     LBA_LOW_WORD            ;Zero out LBA count
                STZ     LBA_LOW_WORD+1
                STZ     LBA_HIGH_WORD
                STZ     LBA_HIGH_WORD+1
;
                LDA     #<LBA_BLKS_RD           ;Get Blocks read msg
                LDY     #>LBA_BLKS_RD
                JSR     M_PROMPTR               ;Send to console
;
SEQ_LBA_READ
                LDA     LBA_LOW_WORD            ;Get variables
                LDY     LBA_LOW_WORD+1
                LDX     LBA_HIGH_WORD
                JSR     B_IDE_SET_LBA           ;Set LBA to read
;
                LDA     #$0D                    ;Get C/R
                JSR     B_CHROUT                ;Send to console
;
                LDA     LBA_LOW_WORD+1          ;Get LBA count data
                STA     HEX4AND5                ;Store in Page Zero work area
                LDA     LBA_LOW_WORD            ;Get LBA count data
                STA     HEX6AND7                ;Store in Page Zero work area
                LDA     LBA_HIGH_WORD+1         ;Get LBA count data
                STA     HEX0AND1                ;Store in Page Zero work area
                LDA     LBA_HIGH_WORD           ;Get LBA count data
                STA     HEX2AND3                ;Store in Page Zero work area
;
                JSR     HEXTOBCD                ;Convert and print to ASCII BCD
                JSR     BCDOUT                  ;Print BCD count to console
;
OC_LOOP         LDA     OCNT_A                  ;Check output buffer count
                BNE     OC_LOOP                 ;Loop back until buffer sent
;
                JSR     B_IDE_READ_LBA          ;Read selected LBA from IDE
                LDA     IDE_STATUS_RAM          ;Get IDE Status
                CMP     #$51                    ;Check for error
                BNE     NO_RD_ERR               ;Branch if no error
;
                JMP     IDE_ERROR_HANDLER       ;Jump to error handler
;
NO_RD_ERR
                JSR     LBA_BLK_UPDATE          ;Update LBA Block to read
;
                JSR     LBA_LIMIT_CHK           ;Check LBA limit
                BCC     SEQ_LBA_READ            ;Loop back to continue (Carry clear)
;
                LDA     #<LBA_BLKS_RD_CMP       ;Get Blocks read msg
                LDY     #>LBA_BLKS_RD_CMP
                JSR     M_PROMPTR               ;Send to console
                JMP     USER_INPUT              ;Prompt User
;
LBA_BLK_UPDATE
; This routine updates the LBA block number being read or written
; - This routine is only used for the full read or write
;
                INC     LBA_LOW_WORD            ;Increment 32-bit word
                BNE     SKIP_BLK_UPDATE
                INC     LBA_LOW_WORD+1
                BNE     SKIP_BLK_UPDATE
                INC     LBA_HIGH_WORD
                BNE     SKIP_BLK_UPDATE
                INC     LBA_HIGH_WORD+1
SKIP_BLK_UPDATE RTS                             ;Return to caller
;
LBA_LIMIT_CHK
                LDA     LBA_HIGH_WORD+1         ;Get high order word
                CMP     $033F                   ;Compare to Limit
                BNE     LIMIT_GOOD              ;If not, exit
                LDA     LBA_HIGH_WORD           ;Get high order word
                CMP     $033E                   ;Compare to limit
                BNE     LIMIT_GOOD              ;If not, exit
                LDA     LBA_LOW_WORD+1          ;Get low order word
                CMP     $033D                   ;Compare to limit
                BNE     LIMIT_GOOD              ;If not, exit
                LDA     LBA_LOW_WORD            ;Get low order word
                CMP     $033C                   ;Compare to limit
                BNE     LIMIT_GOOD
                SEC                             ;Set carry for limit reached
                RTS
LIMIT_GOOD      CLC                             ;Clear carry for limit good
                RTS                             ;Return to caller
;
IDE_SEQ_WRITE
; This routine will write the entire data on the drive!!
; - A 16-bit data pattern is requested from user.
; - This routine can take a long time as the current LBA count is displayed!!
;
                LDA     #<LBA_SEQ_WR_MSG        ;Get Seq Write Message address
                LDY     #>LBA_SEQ_WR_MSG
                JSR     M_PROMPTR               ;Send to console
;
                LDA     #<LBA_SEQ_TM_MSG        ;Get time address
                LDY     #>LBA_SEQ_TM_MSG
                JSR     M_PROMPTR               ;Send to console
;
                JSR     B_IDE_IDENTIFY          ;Call BIOS routine
                JSR     SWAP_BYTE               ;Swap high/low bytes
;
                LDA     #<TOTAL_LBA             ;Get low order offset
                LDY     #>TOTAL_LBA             ;Get high order offset
                JSR     M_PROMPTR               ;Send message to console
;
                LDA     LBA_BUFFER+$78          ;Get LBA count data
                STA     HEX4AND5                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$79          ;Get LBA count data
                STA     HEX6AND7                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$7A          ;Get LBA count data
                STA     HEX0AND1                ;Store in Page Zero work area
                LDA     LBA_BUFFER+$7B          ;Get LBA count data
                STA     HEX2AND3                ;Store in Page Zero work area
;
                JSR     HEXTOBCD                ;Convert Hex to ASCII BCD
                JSR     BCDOUT                  ;Print BCD count to console
;
                LDA     #<LBA_SEQ_CFM           ;Get 1st Message address
                LDY     #>LBA_SEQ_CFM
                JSR     M_PROMPTR               ;Send to console
;
                JSR     M_CONTINUE              ;Prompt user to continue
                JSR     M_CROUT                 ;Send CR/LF
;
                LDA     #<LBA_SEQ_CFM2          ;Get 2nd Message address
                LDY     #>LBA_SEQ_CFM2
                JSR     M_PROMPTR               ;Send to console
;
                JSR     M_CONTINUE              ;Prompt user to continue
                JSR     M_CROUT                 ;Send CR/LF
;
                JSR     GET_PATTERN             ;Prompt for two-byte Hex pattern
;
;                LDA     #$AA                    ;Get Fill Pattern default
;                LDY     #$55                    ; of $AA55
;                STA     INDEXL                  ;Save to Index pointer
;                STY     INDEXH                  ; High/Low
;
                JSR     FILL_PATTERN            ;Fill buffer with pattern
;
                LDA     #<BENCH_BUFFER          ;Setup LBA Buffer
                LDY     #>BENCH_BUFFER          ; address
                LDX     #$01                    ;LBA count = 1
                JSR     B_IDE_SET_ADDR          ;Set Buffer address
;
                STZ     LBA_LOW_WORD            ;Zero out LBA count
                STZ     LBA_LOW_WORD+1
                STZ     LBA_HIGH_WORD
                STZ     LBA_HIGH_WORD+1
;
                LDA     #<LBA_BLKS_WR           ;Get Blocks written msg
                LDY     #>LBA_BLKS_WR
                JSR     M_PROMPTR               ;Send to console
;
SEQ_LBA_WRITE
                LDA     LBA_LOW_WORD            ;Get variables
                LDY     LBA_LOW_WORD+1
                LDX     LBA_HIGH_WORD
                JSR     B_IDE_SET_LBA           ;Set LBA to write
;
                LDA     #$0D                    ;Get C/R
                JSR     B_CHROUT                ;Send to console
;
                LDA     LBA_LOW_WORD+1          ;Get LBA count data
                STA     HEX4AND5                ;Store in Page Zero work area
                LDA     LBA_LOW_WORD            ;Get LBA count data
                STA     HEX6AND7                ;Store in Page Zero work area
                LDA     LBA_HIGH_WORD+1         ;Get LBA count data
                STA     HEX0AND1                ;Store in Page Zero work area
                LDA     LBA_HIGH_WORD           ;Get LBA count data
                STA     HEX2AND3                ;Store in Page Zero work area
;
                JSR     HEXTOBCD                ;Convert Hex to ASCII BCD
                JSR     BCDOUT                  ;Print BCD count to console

OC_LOOP2        LDA     OCNT_A                  ;Check output buffer count
                BNE     OC_LOOP2                ;Loop back until buffer sent
;
                JSR     B_IDE_WRITE_LBA         ;Write selected LBA from buffer
                LDA     IDE_STATUS_RAM          ;Get IDE Status
                CMP     #$51                    ;Check for error
                BNE     NO_WR_ERR               ;Branch if no error
;
                BRA     RD_WR_ERR               ;Branch to handle error
;
NO_WR_ERR
                JSR     LBA_BLK_UPDATE          ;Update LBA Block to write
;
                JSR     LBA_LIMIT_CHK           ;Check LBA limit
                BCC     SEQ_LBA_WRITE           ;Loop back to continue (Carry clear)
;
                LDA     #<LBA_BLKS_WR_CMP       ;Get Blocks written msg
                LDY     #>LBA_BLKS_WR_CMP
                JSR     M_PROMPTR               ;Send to console
                JMP     USER_INPUT              ;Prompt User
;
IDE_BENCHMARK
;
; The Benchmark will read or write a 16MB contiguous block of data.
;  The starting LBA is entered by the user.
;
; - The Benchmark timer in the C02 BIOS/Monitor is used to time the data transfer and shows
; - the number of seconds and hundredths of a second that it takes to complete the transfer.
; -  Note that the benchmark routines use a multiple block transfer of 32 blocks (16KB).
;
; The User is prompted for the Write function, as this overwrites a 16MB block of data on the
; - IDE device, which results in a loss of data. When executing the Write benchmark, the
; - LBA Buffer will be filled with a "55AA" pattern for a 512-byte block.
;
; Error checking is done after each LBA Read or Write function and any error will be
; - displayed and the test aborted after that.
;
                LDA     #<LBA_BENCH_INTRO       ;Get LBA Bench Intro Msg
                LDY     #>LBA_BENCH_INTRO
                JSR     M_PROMPTR               ;Send to console
BENCH_IN_LP
                JSR     M_RDCHAR                ;Get character from user
                JSR     B_CHROUT                ;Send to console
                CMP     #"R"                    ;Test for Read
                BEQ     IDE_BENCH_READ          ;If yes, go set RTC
                CMP     #"W"                    ;Test for Write
                BEQ     IDE_BENCH_WRITE         ;If no, skip RTC set
                JSR     M_BEEP                  ;Else, error, send beep
                BRA     BENCH_IN_LP             ;Branch back and try again
;
IDE_BENCH_READ
;
; Simple test program to transfer multiple sectors - READ
;
                LDA     #<LBA_START             ;Get LBA starting # Msg
                LDY     #>LBA_START
                JSR     M_PROMPTR               ;Send to console
;
                JSR     GET_LBA_NUM             ;Get starting LBA # from user
;
                LDA     #<LBA_RD_BENCH          ;Get LBA Read Bench Msg
                LDY     #>LBA_RD_BENCH
                JSR     M_PROMPTR               ;Send to console
;
OC_LOOP3        LDA     OCNT_A                  ;Check output buffer count
                BNE     OC_LOOP3                ;Loop back until buffer sent
;
                LDA     #<BENCH_BUFFER          ;Setup LBA Buffer
                LDY     #>BENCH_BUFFER          ; address
                LDX     #$20                    ;Sector count of 32 (16KB)
                JSR     B_IDE_SET_ADDR          ;Call BIOS routine to set it
;
; Setup 1024 transfers at 32 blocks per transfer = 16MB
;
                LDX     #$00                    ;Set for 256 blocks (128KB)
                LDY     #$04                    ;Set multiplier of 4 (* 256)
                JSR     B_CNT_INIT              ;Reset and start benchmark counter
;
LBA_RBLK
                JSR     B_IDE_READ_LBA          ;Call BIOS Read Block
                LDA     IDE_STATUS_RAM          ;Get IDE Status (RAM)
                LSR     A                       ;Shift error bit into carry
                BCS     RD_WR_ERR               ;Branch if error
;
                LDA     LBA_LOW_BYTE            ;Get LBA low byte (Carry is clear)
                ADC     LBA_XFER_CNT            ;Add 32 decimal
                STA     LBA_LOW_BYTE            ;Save it back
;
                LDA     LBA_HIGH_BYTE           ;Get LBA high byte
                ADC     #$00                    ;Add carry
                STA     LBA_HIGH_BYTE           ;Save it back
;
                LDA     LBA_EXT_BYTE            ;Get LBA ext byte
                ADC     #$00                    ;Add carry
                STA     LBA_EXT_BYTE            ;Save it back
;
                DEX                             ;Decrement low index
                BNE     LBA_RBLK                ;Loop back until zero
                DEY                             ;Decrement multiplier index
                BNE     LBA_RBLK                ;Loop back until done
                JSR     M_QUITB                 ;Quit Benchmark counter, print results
                JMP     USER_INPUT              ;Prompt User
;
RD_WR_ERR
                JMP     IDE_ERROR_HANDLER       ;Jump to error handler, then return
;
IDE_BENCH_WRITE
;
; Simple test program to transfer multiple sectors - WRITE
;
                JSR     GET_PATTERN             ;Prompt for two-byte Hex pattern
                JSR     FILL_PATTERN            ;Fill 16KB buffer with pattern
;
                LDA     #<LBA_START             ;Get LBA starting # Msg
                LDY     #>LBA_START
                JSR     M_PROMPTR               ;Send to console
;
                JSR     GET_LBA_NUM             ;Get starting LBA # from user
;
                LDA     #<LBA_BENCH_WARN        ;Get LBA Write Bench msg
                LDY     #>LBA_BENCH_WARN
                JSR     M_PROMPTR               ;Send to console
BENCH_WARN_LP
                JSR     M_RDCHAR                ;Get character from user
                JSR     B_CHROUT                ;Send to console
                CMP     #"Y"                    ;Test for yes
                BEQ     BENCH_WRITE_Y           ;If yes, do write benchmark
                CMP     #"N"                    ;Test for no
                BEQ     BENCH_WRITE_N           ;If no, skip write benchmark
                JSR     M_BEEP                  ;Else, error, send beep
                BRA     BENCH_WARN_LP           ;Branch back and try again
BENCH_WRITE_Y
                LDA     #<LBA_WR_BENCH          ;Get LBA Write Bench msg
                LDY     #>LBA_WR_BENCH
                JSR     M_PROMPTR               ;Send to console
;
OC_LOOP4        LDA     OCNT_A                  ;Check output buffer count
                BNE     OC_LOOP4                ;Loop back until buffer sent
;
                LDA     #<BENCH_BUFFER          ;Setup LBA Buffer
                LDY     #>BENCH_BUFFER          ; address
                LDX     #$20                    ;Sector count of 32
                JSR     B_IDE_SET_ADDR          ;Call BIOS routine to set it
;
; Setup 1024 transfers at 32 blocks per transfer = 16MB
;
                LDX     #$00                    ;Set for 256 blocks (128KB)
                LDY     #$04                    ;Set multiplier of 4 (* 256)
                JSR     B_CNT_INIT              ;Reset and start benchmark counter
LBA_WBLK
                JSR     B_IDE_WRITE_LBA         ;Write Block
                LDA     IDE_STATUS_RAM          ;Get IDE Status
                LSR     A                       ;Shift error bit into carry
                BCS     RD_WR_ERR               ;Branch if error
;
                CLC                             ;Clear Carry for add
                LDA     LBA_LOW_BYTE            ;Get LBA low byte
                ADC     LBA_XFER_CNT            ;Add 32 decimal
                STA     LBA_LOW_BYTE            ;Save it back
;
                LDA     LBA_HIGH_BYTE           ;Get LBA high byte
                ADC     #$00                    ;Add carry
                STA     LBA_HIGH_BYTE           ;Save it back
;
                LDA     LBA_EXT_BYTE            ;Get LBA ext byte
                ADC     #$00                    ;Add carry
                STA     LBA_EXT_BYTE            ;Save it back
;
SKP_HI_WR
                DEX                             ;Decrement index
                BNE     LBA_WBLK                ;Loop until done
                DEY                             ;Decrement index high
                BNE     LBA_WBLK                ;Loop back until done
                JSR     M_QUITB                 ;Quit Benchmark counter
                JMP     USER_INPUT              ;Prompt User
;
BENCH_WRITE_N
                LDA     #<LBA_BENCH_ABORT       ;Get LBA Write Bench msg
                LDY     #>LBA_BENCH_ABORT
                JSR     M_PROMPTR               ;Send to console
;
                JMP     USER_INPUT
;
QUIT
                LDA     #<QUIT_MSG              ;Load Message address
                LDY     #>QUIT_MSG              ;into A/Y regs
                JSR     M_PROMPTR               ;Send to console/return
;
                LDA     #<LBA_BUFFER            ;Setup LBA Buffer
                LDY     #>LBA_BUFFER            ; address
                LDX     #$01                    ;LBA count = 1
                JSR     B_IDE_SET_ADDR          ;Set Buffer address
                JSR     B_IDE_RESET             ;Reset MicroDrive
;
                LDX     #$00                    ;Get warm boot function
                JMP     PEM                     ;Jump to PEM to warm boot
;
;Prompt User for what's next
; As we're outputting to a remote console, displaying the menu again could scroll the displayed
; data off the screen, so this routine pauses the program execution and let's the user decide
; on going to the menu display or just continuing on with another function.
;
USER_INPUT
                LDA     #<USER_INMSG            ;Load message address
                LDY     #>USER_INMSG            ;into A/Y reg
                JSR     M_PROMPTR               ;Send to console
                JMP     MAIN_LOOP               ;Goto main input loop
;
GET_LBA_NUM
;Get LBA number from user
                JSR     DECIN                   ;Get Decimal input, convert to BCD, then to HEX
;
                LDA     HEX6AND7                ;Low byte
                LDY     HEX4AND5
                LDX     HEX2AND3
                JMP     B_IDE_SET_LBA           ;Set LBA number
;
STRING_OUT
; String out prints a string based on the A/Y register pointing to the start of the string
; and the X reg containing how many characters to print (not a null terminated string)
;
                STA     PROMPTL                 ;Set low byte address
                STY     PROMPTH                 ;Set high byte address
                LDY     #$00                    ;Zero offet index
STRING_LOOP
                LDA     (PROMPTL),Y             ;Get string data
                CMP     #$20                    ;Check for ASCII space
                BEQ     SKIP_SPC                ;If yes, skip printing it
                JSR     B_CHROUT                ;Send to terminal
SKIP_SPC
                INY                             ;Increment index to string
                DEX                             ;Decrement character count
                BNE     STRING_LOOP             ;Branch back until done
                RTS                             ;Return to caller
;
IDE_ERROR_HANDLER
; This routine gets the error code anytime a command returns with the error
; bit on in the status register. It uses the BIOS routine to get the error register
; and returns with the error code in the X register.
;
; the error code is matched to the list of possible codes per the Hitachi
; MicroDrive documentation, then the matching error message is sent to the console.
; After this, the IDE controller is reset and the User is prompted for the next
; action to be taken.
;
                LDA     #<IDE_CONTROLLER_ERROR  ;Get Base IDE error msg
                LDY     #>IDE_CONTROLLER_ERROR
                JSR     M_PROMPTR               ;Send to console
;
                JSR     B_IDE_GET_STAT          ;Get Status from BIOS
                TXA                             ;Xfer error code to A reg
;
                LDX     #IDE_ERROR_ADDRESS-IDE_ERROR_CODES-1    ;Get error list count
ERROR_LP        CMP     IDE_ERROR_CODES,X       ;Compare to command list
                BNE     ERROR_DEC               ;Check for next error and loop
;
; X Reg now contains error index
;
                TXA                             ;Xfer Error code index to A reg
                ASL     A                       ;Multiply error index value by 2
                TAX                             ;Xfer Error offset address to message table
;
                LDA     IDE_ERROR_ADDRESS,X     ;Get message address
                LDY     IDE_ERROR_ADDRESS+1,X
                BRA     ERROR_MSG               ;Branch below to output message and continue
;
ERROR_DEC       DEX                             ;Decrement index count
                BPL     ERROR_LP                ;If more to check, loop back
;
; No more error codes, so it must be something unknown. So print the unknown message
; to the console and return to the User prompt.
;
                LDA     #<IDE_ERROR_06          ;Get Error message address
                LDY     #>IDE_ERROR_06
ERROR_MSG
                JSR     M_PROMPTR               ;Send to console
                JSR     B_IDE_RESET             ;Reset IDE Controller
                JMP     USER_INPUT              ;Prompt User
;
GET_PATTERN
; Get Pattern: This prompts the User for a 2-byte Hexadecimal fill pattern.
; - This will be used by FILL_PATTERN to load the BENCH_BUFFER.
                LDA     #<PATTERN_MSG           ;Get write pattern msg
                LDY     #>PATTERN_MSG
                JSR     M_PROMPTR               ;Send to console
;
; Monitor routine will get User input and return in A/Y regs
; - Data also saved in INDEXH/INDEXL
                JSR     M_HEXIN4                ;Use Monitor to get input
                JMP     M_CROUT                 ;Send C/R to console and return
;
FILL_PATTERN
;
; Fill Pattern: This fills the LBA buffer with a user specified data pattern.
; - The buffer address is specified by BENCH_BUFFER (at the end of our code).
; - For ease of coding, we default to a 16KB fill buffer.
; - The 16KB buffer is used by the Read and Write Benchmark routines only.
;
                LDA     #<BENCH_BUFFER          ;Setup LBA Buffer
                LDY     #>BENCH_BUFFER          ; address
                STA     TGTL                    ;Setup Page Zero pointer lo
                STY     TGTH                    ;Setup Page Zero pointer hi
;
                LDX     #$00                    ;Set Index for count
                LDY     #$20                    ; of 16KB (8K of words)
FILL_P_LOOP
                LDA     INDEXH                  ;Get High byte fill
                STA     (TGTL)                  ;Save it
                INC     TGTL                    ;Increment pointer
                BNE     SK_FILL_1               ;Skip if no rollover
                INC     TGTH                    ;Increment pointer
SK_FILL_1
                LDA     INDEXL                  ;Get Low byte fill
                STA     (TGTL)                  ;Save it
                INC     TGTL                    ;Increment pointer
                BNE     SK_FILL_2               ;Skip if no rollover
                INC     TGTH                    ;Increment pointer
SK_FILL_2
                DEX                             ;Decrement low count
                BNE     FILL_P_LOOP             ;Loop back till done
                DEY                             ;Decrement high count
                BNE     FILL_P_LOOP             'Loop back until done
                RTS                             ;Return to caller
;
; Routine to swap high and low bytes in the block space
; - used for Identity Data, as the bytes are swapped high and low
;
SWAP_BYTE
                LDA     #<LBA_BUFFER            ;Setup LBA Buffer
                LDY     #>LBA_BUFFER            ; address
                STA     BIOS_XFERL              ;Save it to page zero
                STY     BIOS_XFERH              ;variable
;
                LDX     #$00                    ;Set Index for count of 256
                LDY     #$01                    ;Load Y reg for 1-byte offset
;
SWAP_LOOP
                LDA     (BIOS_XFERL)            ;Get first byte
                PHA                             ;Save to stack
                LDA     (BIOS_XFERL),Y          ;Get second byte
                STA     (BIOS_XFERL)            ;Save it
                PLA                             ;Get second byte back
                STA     (BIOS_XFERL),Y          ;Save it to first byte
;
                INC     BIOS_XFERL              ;Increment index
                BNE     SWAP_SKP1               ;Branch if non-zero
                INC     BIOS_XFERH              ;Increment index
SWAP_SKP1
                INC     BIOS_XFERL              ;Increment index
                BNE     SWAP_SKP2               ;Branch if non-zero
                INC     BIOS_XFERH              ;Increment index
SWAP_SKP2
                DEX                             ;Decrement index
                BNE     SWAP_LOOP               ;Loop back till done
                RTS                             ;Return to caller
;
SYS_XFER
;
;System Transfer routine.
; This routine is used to write a section of memory to a defined set of contiguous blocks
; on the Microdrive. The purpose being to transfer the bootable image from RAM to the disc.
;
; The user is prompted for a few inputs as:
; - Starting LBA on the Microdrive
; - Starting memory address used as the source
; - Number of blocks to be transferred
;
; Once this is entered, the user is prompted to either continue or abort.
; if confirmed, the write executed and the blocks on the disc are overwritten with the
; contens of memory.
;
                LDA     #<SYS_INTRO_MSG         ;Get System Xfer Message
                LDY     #>SYS_INTRO_MSG         ;
                JSR     M_PROMPTR               ;Send to Console
;
                LDA     #<SYS_LBA_MSG           ;Get Starting LBA for xfer
                LDY     #>SYS_LBA_MSG           ;
                JSR     M_PROMPTR               ;Send to Console
                JSR     GET_LBA_NUM             ;Get starting LBA from User
;
; Use C02 Monitor routine to get a 16-bit hex address returned in A/Y
; and stored in INDEXH/INDEXL
;
                LDA     #<RAM_START_MSG         ;Get Starting RAM for xfer
                LDY     #>RAM_START_MSG         ;
                JSR     M_PROMPTR               ;Send to Console
                JSR     M_HEXIN4                ;Call Monitor routine
;
                LDA     #<BLK_SIZE_MSG          ;Get LBA count for xfer
                LDY     #>BLK_SIZE_MSG          ;
                JSR     M_PROMPTR               ;Send to Console
;
                JSR     DECIN                   ;Get Decimal input, convert to BCD, then to HEX
;
                LDX     HEX6AND7                ;Get low byte (number of blocks)
                LDA     INDEXL                  ;Get RAM address to start from
                LDY     INDEXH                  ;
                JSR     B_IDE_SET_ADDR          ;Call BIOS to set address and block count
;
                LDA     #<SYS_CONFIRM_MSG       ;Get Confirm Message for xfer
                LDY     #>SYS_CONFIRM_MSG       ;
                JSR     M_PROMPTR               ;Send to Console
;
SYS_WRT_WARN_LP
                JSR     M_RDCHAR                ;Get character from user
                JSR     B_CHROUT                ;Send to console
                CMP     #"Y"                    ;Test for yes
                BEQ     SYS_WRITE_GO            ;If yes, do write benchmark
                CMP     #"N"                    ;Test for no
                BEQ     SYS_WRITE_ABORT         ;If no, skip write benchmark
                JSR     M_BEEP                  ;Else, error, send beep
                BRA     SYS_WRT_WARN_LP         ;Branch back and try again
;
SYS_WRITE_GO
                LDA     #<SYS_WRITE_MSG         ;Get Confirm Message for xfer
                LDY     #>SYS_WRITE_MSG         ;
                JSR     M_PROMPTR               ;Send to Console
;
WAIT_SYS
                LDA     OCNT_A                  ;Get output count for console
                BNE     WAIT_SYS                ;Wait until done
;
                JSR     B_IDE_WRITE_LBA         ;Call BIOS to write image
                LDA     IDE_STATUS_RAM          ;Get IDE Status (RAM)
                LSR     A                       ;Shift error bit into carry
                BCS     IMG_WR_ERR              ;Branch if error
;
                LDA     #<SYS_COMPLETE_MSG      ;Get Confirm Message for xfer
                LDY     #>SYS_COMPLETE_MSG      ;
                JSR     M_PROMPTR               ;Send to Console
                JMP     USER_INPUT              ;Exit to main
;
IMG_WR_ERR
                JSR     IDE_ERROR_HANDLER       ;Handle Disc error
SYS_WRITE_ABORT
                JMP     USER_INPUT              ;Exit to main
;
;
; The following routines are borrowed from Brian Phelps' SyMON monitor.
; - HEXTOBCD, BCDOUT, BCDTOASC, BCDTOHEX, ASCTODEC
; - I made some coding changes to use CMOS instructions, etc.
;
;HEXTOBCD subroutine: convert a 1-8 digit HEX value to a 1-10 digit BCD value.
; Call with 8 digit (4 byte) HEX value in HEX0AND1(MSB) through HEX6AND7(LSB).
; Returns with 10 digit (5 byte) BCD result in DEC0AND1(MSB) through DEC8AND9(LSB)
;HPHANTOM is a 16 bit address used to reference an 8 bit zero-page address.
; (HEXTOBCD needs LDA $hh,Y (an invalid instruction) so we use LDA $00hh,Y instead)
; This address is not written-to nor read-from in the HEXTOBCD subroutine.
; The address is the zero-page memory location immediatly below the HEX0AND1 variable
;HEX value input buffer:
;HEX0AND1 Two most significant HEX digits
;HEX2AND3
;HEX4AND5
;HEX6AND7 Two least significant HEX digits
;BCD value output buffer (BCD accumulator):
;DEC0AND1 ;Two most significant BCD digits
;DEC2AND3
;DEC4AND5
;DEC6AND7
;DEC8AND9 ;Two least significant BCD digits
;
HEXTOBCD        STZ     DEC0AND1                ;Init (zero) buffer
                STZ     DEC2AND3
                STZ     DEC4AND5
                STZ     DEC6AND7
                STZ     DEC8AND9
                LDY     #$04                    ;Initialize HEX input buffer byte index: point to address minus 1 of LSB
                LDX     #$04                    ;Initialize multiplicand table index: point to LSB of lowest multiplicand
DECLOOP         LDA     HPHANTOM,Y              ;Read indexed byte from input buffer: Y REGISTER index always > 0 here
                AND     #$0F                    ;Zero the high digit
                JSR     MULTIPLY                ;Multiply low digit
                INX                             ;Add 5 to multiplicand table index: point to LSB of next higher multiplicand
                INX
                INX
                INX
                INX
                LDA     HPHANTOM,Y              ;Read indexed byte from input buffer: Y REGISTER index always > 0 here
                LSR     A                       ;Shift high digit to low digit, zero high digit
                LSR     A
                LSR     A
                LSR     A
                JSR     MULTIPLY                ;Multiply digit
                INX                             ;Add 5 to multiplicand table index: point to LSB of next higher multiplicand
                INX
                INX
                INX
                INX
                DEY                             ;Decrement HEX input buffer byte index
                BNE     DECLOOP                 ;LOOP back to DECLOOP IF byte index <> 0: there are more bytes to process
                RTS                             ; ELSE, done HEXTOBCD subroutine, RETURN
;
;Multiply indexed multiplicand by digit in ACCUMULATOR
;
MULTIPLY        PHA
                PHX
                PHY
                SED                             ;Switch processor to BCD arithmatic mode
                TAY                             ;Copy digit to Y REGISTER: multiplier loop counter
HMLTLOOP        CPY     #$00
                BNE     HDOADD                  ;GOTO HDOADD IF multiplier loop counter <> 0
                CLD                             ; ELSE, switch processor to BINARY arithmatic mode
                PLY
                PLX
                PLA
BCD_DONE        RTS                             ;Done MULTIPLY subroutine, RETURN
;
;Add indexed multiplicand to BCD accumulator (output buffer)
;
HDOADD          CLC
                LDA     HMULTAB,X               ;Least significant byte of indexed multiplicand
                ADC     DEC8AND9                ;Least significant byte of BCD accumulator
                STA     DEC8AND9
                LDA     HMULTAB-1,X
                ADC     DEC6AND7
                STA     DEC6AND7
                LDA     HMULTAB-2,X
                ADC     DEC4AND5
                STA     DEC4AND5
                LDA     HMULTAB-3,X
                ADC     DEC2AND3
                STA     DEC2AND3
                LDA     HMULTAB-4,X             ;Most significant byte of indexed multiplicand
                ADC     DEC0AND1                ;Most significant byte of BCD accumulator
                STA     DEC0AND1
                DEY                             ;Decrement multiplier loop counter
                BRA     HMLTLOOP                ;LOOP back to HMLTLOOP
;
;BCDOUT subroutine: convert 10 BCD digits to ASCII DECIMAL digits then send result to terminal.
;Leading zeros are supressed in the displayed result.
;Call with 10 digit (5 byte) BCD value contained in variables DEC0AND1 through DEC8AND9:
;DEC0AND1 ($15) Two most significant BCD digits
;DEC2AND3 ($16)
;DEC4AND5 ($17)
;DEC6AND7 ($18)
;DEC8AND9 ($19) Two least significant BCD digits
;
BCDOUT          LDX     #$00                    ;Initialize BCD output buffer index: point to MSB
                LDY     #$00                    ;Initialize leading zero flag: no non-zero digits have been processed
BCDOUTL         LDA     DEC0AND1,X              ;Read indexed byte from BCD output buffer
                LSR     A                       ;Shift high digit to low digit, zero high digit
                LSR     A
                LSR     A
                LSR     A
                JSR     BCDTOASC                ;Convert BCD digit to ASCII DECIMAL digit, send digit to terminal
                LDA     DEC0AND1,X              ;Read indexed byte from BCD output buffer
                AND     #$0F                    ;Zero the high digit
                JSR     BCDTOASC                ;Convert BCD digit to ASCII DECIMAL digit, send digit to terminal
                INX                             ;Increment BCD output buffer index
                CPX     #$05
                BNE     BCDOUTL                 ;LOOP back to BCDOUTL IF output buffer index <> 5
                CPY     #$00
                BNE     BCD_DONE                ; ELSE, GOTO BCDOUTDN IF any non-zero digits were processed
                LDA     #$30                    ; ELSE, send "0" to terminal
                JMP     B_CHROUT                ;Send to console
;
;BCDTOASC subroutine:
; convert BCD digit to ASCII DECIMAL digit, send digit to terminal IF it's not a leading zero
;
BCDTOASC        BNE     NONZERO                 ;GOTO NONZERO IF BCD digit <> 0
                CPY     #$00                    ; ELSE, GOTO BTADONE IF no non-zero digits have been processed
                BEQ     BCD_DONE                ;  (supress output of leading zeros)
NONZERO         INY                             ; ELSE, indicate that a non-zero digit has been processed (Y REGISTER <> 0)
                CLC                             ;Add ASCII "0" to digit: convert BCD digit to ASCII DECIMAL digit
                ADC     #$30
                JMP     B_CHROUT                ;Send converted digit to terminal
;
;BCDTOHEX subroutine: convert a 1-10 digit BCD value to a 1-8 digit HEX value.
; Call with 10 digit (5 byte) DECIMAL value in DEC0AND1(MSB) through DEC8AND9(LSB).
; Returns with 8 digit (4 byte) HEX result in HEX0AND1(MSB) through HEX6AND7(LSB)
;DPHANTOM is a 16 bit address used to reference an 8 bit zero-page address.
; (BCDTOHEX needs LDA $hh,Y (an invalid instruction) so we use LDA $00hh,Y instead)
; This address is not written-to nor read-from in the BCDTOHEX subroutine.
; The address is the zero-page memory location immediatly below the DEC0AND1 variable
;BCD value input buffer:
;DEC0AND1 ;Two most significant BCD digits
;DEC2AND3
;DEC4AND5
;DEC6AND7
;DEC8AND9 ;Two least significant BCD digits
;HEX value output buffer (HEX accumulator):
;HEX0AND1 Two most significant HEX digits
;HEX2AND3
;HEX4AND5
;HEX6AND7 Two least significant HEX digits
;
BCDTOHEX        STZ     HEX0AND1                ;Init (zero) buffer
                STZ     HEX2AND3
                STZ     HEX4AND5
                STZ     HEX6AND7
                LDY     #$05                    ;Initialize DECIMAL input buffer byte index: point to (address - 1) of LSB
                LDX     #$03                    ;Initialize multiplicand table index: point to LSB of lowest multiplicand
BCDLOOP         LDA     DPHANTOM,Y              ;Read indexed byte from input buffer: Y REGISTER index always > 0 here
                AND     #$0F                    ;Zero the high digit
                JSR     MULTPLI                 ;Multiply low digit
                INX                             ;Add 4 to multiplicand table index: point to LSB of next higher multiplicand
                INX
                INX
                INX
                LDA     DPHANTOM,Y              ;Read indexed byte from input buffer: Y REGISTER index always > 0 here
                LSR     A                       ;Shift high digit to low digit, zero high digit
                LSR     A
                LSR     A
                LSR     A
                JSR     MULTPLI                 ;Multiply digit
                INX                             ;Add 4 to multiplicand table index: point to LSB of next higher multiplicand
                INX
                INX
                INX
                DEY                             ;Decrement DECIMAL input buffer byte index
                BNE     BCDLOOP                 ;LOOP back to BCDLOOP IF byte index <> 0: there are more bytes to process
                RTS                             ; ELSE, done BCDTOHEX subroutine, RETURN
;
;Multiply indexed multiplicand by digit in ACCUMULATOR
;
MULTPLI         PHA                             ;Save registers
                PHX
                PHY
                TAY                             ;Copy digit to Y REGISTER: multiplier loop counter
DMLTLOOP        CPY     #$00
                BNE     DDOADD                  ;GOTO DDOADD IF multiplier loop counter <> 0
                PLY                             ;Restore registers
                PLX
                PLA
                RTS                             ;Done MULTIPLI subroutine, RETURN
;
;Add indexed multiplicand to HEX accumulator (output buffer)
;
DDOADD          CLC
                LDA     DMULTAB,X               ;Least significant byte of indexed multiplicand
                ADC     HEX6AND7                ;Least significant byte of HEX accumulator
                STA     HEX6AND7
                LDA     DMULTAB-1,X
                ADC     HEX4AND5
                STA     HEX4AND5
                LDA     DMULTAB-2,X
                ADC     HEX2AND3
                STA     HEX2AND3
                LDA     DMULTAB-3,X             ;Most significant byte of indexed multiplicand
                ADC     HEX0AND1                ;Most significant byte of HEX accumulator
                STA     HEX0AND1
                DEY                             ;Decrement multiplier loop counter
                BCS     OVERFLOW                ;GOTO OVERFLOW IF the last add produced a CARRY: HEX output buffer has overflowed
                BCC     DMLTLOOP                ; ELSE, LOOP back to DMLTLOOP (always branch)
OVERFLOW        LDA     #$2A                    ;Send "*" to terminal: indicate that an overflow has occured
                JSR     B_CHROUT
                BRA     DMLTLOOP                ;LOOP back to DMLTLOOP
;
;ASCTODEC subroutine: convert ASCII DECIMAL digits to BCD
;
ASCTODEC        STZ     DEC0AND1                ;Init (zero) buffer two most significant BCD digits
                STZ     DEC2AND3
                STZ     DEC4AND5
                STZ     DEC6AND7
                STZ     DEC8AND9                ; two least significant BCD digits
                LDX     BUFIDX                  ;Read number of digits entered: ASCII digit buffer index
                BEQ     A2DDONE                 ;GOTO A2DDONE IF BUFIDX = 0: no digits were entered
                LDY     #$05                    ; ELSE, Initialize BCD input buffer index: process up to 5 BCD bytes (10 digits)
ATODLOOP        JSR     A2DSUB                  ;Read ASCII digit then convert to BCD
                STA     DPHANTOM,Y              ;Write BCD digit to indexed buffer location (index always > 0)
                JSR     A2DSUB                  ;Read ASCII digit then convert to BCD
                ASL     A                       ;Make this BCD digit the more significant in the BCD byte
                ASL     A
                ASL     A
                ASL     A
                ORA     DPHANTOM,Y              ;OR with the less significant digit
                STA     DPHANTOM,Y              ;Write BCD byte to indexed buffer location (index always > 0)
                DEY                             ;Decrement BCD input buffer index
                BNE     ATODLOOP                ;GOTO ATODLOOP IF buffer index <> 0: there is room to process another digit
A2DDONE         RTS                             ; ELSE, done ASCTODEC, RETURN
;
;Read indexed ASCII DECIMAL digit from text buffer then convert digit to 4 bit BCD
;
A2DSUB          TXA                             ;GOTO A2DCONV IF digit buffer index <> 0: there are more digits to process
                BNE     A2DCONV
                PLA                             ; ELSE, pull return address from STACK
                PLA
                RTS                             ;Done ASCTODEC, RETURN
A2DCONV         LDA     IBUFF-1,X               ;Read indexed ASCII DECIMAL digit
                SEC                             ;Subtract ASCII "0" from ASCII DECIMAL digit: convert digit to BCD
                SBC     #$30
                DEX                             ;Decrement ASCII digit buffer index
                RTS                             ;A2DSUB done, RETURN
;
;DECIN subroutine: request 1 - 10 DECIMAL digit input from terminal, followed by [RETURN].
; [ESCAPE] aborts, [BACKSPACE] erases last keystroke.
; Convert input to BCD and HEX then store both results as follows:
; Converted 10 digit (5 byte) BCD value will be contained in variables DEC0AND1 through DEC8AND9:
;  DEC0AND1 ($E5) Two most significant BCD digits
;  DEC2AND3 ($E6)
;  DEC4AND5 ($E7)
;  DEC6AND7 ($E8)
;  DEC8AND9 ($E9) Two least significant BCD digits
; Converted 8 digit (4 byte) HEX value will be contained in variables HEX0AND1 through HEX6AND7:
;  HEX0AND1 ($E1) Two most significant HEX digits
;  HEX2AND3 ($E2)
;  HEX4AND5 ($E3)
;  HEX6AND7 ($E4) Two least significant HEX digits
; NOTE1: If a DECIMAL value greater than 4,294,967,295 ($FFFFFFFF) is entered,
;  1 or 2 asterisks (*) will be sent to the terminal following the inputted digits.
;  This is to indicate that an overflow in the HEX accumulator has occured.
;  (the BCDTOHEX subroutine's HEX accumulator "rolls over" to zero when that value is exceeded)
;  An overflow condition does NOT affect the BCD value stored.
; NOTE2: This subroutine is not used by SyMon; it is here for user purposes, if needed.
;
DECIN           JSR     DECINPUT                ;Request 1 - 8 DECIMAL digit input from terminal
                JSR     ASCTODEC                ;Convert ASCII DECIMAL digits to BCD
                JMP     BCDTOHEX                ;Convert 1-8 digit BCD to a 1-8 digit HEX value
;
;DECINPUT subroutine: request 1 to 8 DECIMAL digits from terminal. Result is
; stored in zero-page address IBUFF through (IBUFF + $08)
;Setup RDLINE subroutine parameters:
;
DECINPUT
                LDX     #$08                    ;  X-REGISTER = maximum number of digits allowed
; Drop into RDLINE routine
;
;RDLINE subroutine: Store keystrokes into buffer until [RETURN] key is struck
; Used for Decimal entry, so only (0-9) are accepted entries.
; On entry, X Reg = buffer length. On exit, X Reg = buffer count
; [BACKSPACE] key removes keystrokes from buffer.
; [ESCAPE] key aborts then returns.
RDLINE          STX     BUFLEN          ;Store buffer length
                STZ     BUFIDX          ;Zero buffer index
RDLOOP          JSR     M_RDCHAR        ;Get character from terminal, convert LC2UC
                CMP     #$1B            ;Check for ESC key
                BEQ     RDNULL          ;If yes, exit back to Monitor
NOTESC          CMP     #$0D            ;Check for C/R
                BEQ     EXITRD          ;Exit if yes
                CMP     #$08            ;Check for Backspace
                BEQ     RDBKSP          ;If yes handle backspace
                CMP     #$30            ;Check for '0' or higher
                BCC     INPERR          ;Branch to error if less than '0'
                CMP     #$3A            ;Check for higher than '9'
                BCS     INPERR          ;Branch to error if more than '9'
                LDX     BUFIDX          ;Get the current buffer index
                CPX     BUFLEN          ;Compare to length for space
                BCC     STRCHR          ;Branch to store in buffer
INPERR          JSR     M_BEEP          ;Else, error, send Bell to terminal
                BRA     RDLOOP          ;Branch back to RDLOOP
STRCHR          STA     IBUFF,X         ;Store keystroke in buffer
                JSR     B_CHROUT        ;Send keystroke to terminal
                INC     BUFIDX          ;Increment buffer index
                BRA     RDLOOP          ;Branch back to RDLOOP
RDBKSP          LDA     BUFIDX          ;Check if buffer is empty
                BEQ     INPERR          ;Branch if yes
                DEC     BUFIDX          ;Else, decrement buffer index
                JSR     M_BSOUT         ;Send Backspace to terminal
                BRA     RDLOOP          ;Loop back and continue
EXITRD          LDX     BUFIDX          ;Get keystroke count (set Z flag)
                BNE     RDL_OK          ;If data entered, normal exit
RDNULL          PLA                     ;Pull return address
                PLA                     ; from stack
                JMP     USER_INPUT      ;Go to main menu
RDL_OK          RTS                     ;Return to caller
; Utility Messages are defined here:
;
INTRO_MSG
        .DB     $0D,$0A
        .DB     " Setup and Diagnostic Utility for:",$0D,$0A
        .DB     " DS15x1 Realtime Clock and MicroDrive PATA Adapter, Version 0.83",$0D,$0A
        .DB     " Copyright 2022 by K.E. Maier",$0D,$0A
        .DB     $00
;
MENU_MSG
        .DB     $0D,$0A
        .DB     " ***************************************************************************** ",$0D,$0A
        .DB     " *                                                                           * ",$0D,$0A
        .DB     " *          DS15x1 Realtime Clock Functions:                                 * ",$0D,$0A
        .DB     " *             1- Set Date and Time (with signature)                         * ",$0D,$0A
        .DB     " *             2- Read NVRAM Data and Display                                * ",$0D,$0A
        .DB     " *             3- Save NVRAM Data (without signature)                        * ",$0D,$0A
        .DB     " *                                                                           * ",$0D,$0A
        .DB     " *          MicroDrive (IDE) Functions:                                      * ",$0D,$0A
        .DB     " *             4- Identify Vendor information                                * ",$0D,$0A
        .DB     " *             5- Read a LBA to Memory and Display                           * ",$0D,$0A
        .DB     " *             6- Write a LBA from Memory and Verify                         * ",$0D,$0A
        .DB     " *             7- Sequential Read all LBA                                    * ",$0D,$0A
        .DB     " *             8- Sequential Write all LBA                                   * ",$0D,$0A
        .DB     " *             9- Benchmark for LBA Read or Write                            * ",$0D,$0A
        .DB     " *             S- System Transfer (Memory to Disc)                           * ",$0D,$0A
        .DB     " *                                                                           * ",$0D,$0A
        .DB     " *             Q- Quit, return to Monitor                                    * ",$0D,$0A
        .DB     " *                                                                           * ",$0D,$0A
        .DB     " ***************************************************************************** ",$0D,$0A,$0A
        .DB     "     Enter Command Number to continue: "
        .DB     $00
;
RTC_INTRO
        .DB     $0D,$0A,$0A
        .DB     " Realtime Clock Setup",$0D,$0A
        .DB     "  NOTE: Entry Data not range checked! Know what you are doing!",$0D,$0A
        .DB     "  Day is 1-7 (1=Saturday, 7=Friday)",$0D,$0A,$0A
        .DB     "  Enter the following to set the Date and Time.",$0D,$0A
        .DB     $00
;
RTC_YEAR_M
        .DB     $0D,$0A
        .DB     "  Enter Year: "
        .DB     $00
;
RTC_MONTH_M
        .DB     $0D,$0A
        .DB     "  Enter Month: "
        .DB     $00
;
RTC_DATE_M
        .DB     $0D,$0A
        .DB     "  Enter Date: "
        .DB     $00
;
RTC_DAY_M
        .DB     $0D,$0A
        .DB     "  Enter Day: "
        .DB     $00
;
RTC_HOURS_M
        .DB     $0D,$0A
        .DB     "  Enter Hours: "
        .DB     $00
;
RTC_MINUTES_M
        .DB     $0D,$0A
        .DB     "  Enter Minutes: "
        .DB     $00
;
RTC_SECONDS_M
        .DB     $0D,$0A
        .DB     "  Enter Seconds: "
        .DB     $00
;
RTC_WRITE
        .DB     $0D,$0A
        .DB     " About to write Data to RTC! Are you sure? "
        .DB     $00
;
RTC_CONFIRM
        .DB     $0D,$0A,$0A
        .DB     " RTC Data Updated!"
        .DB     $0D,$0A
        .DB     $00
;
ALARM_CLEAR
        .DB     $0D,$0A
        .DB     " Clear the Alarm and Interrupt functions? "
        .DB     $00
;
QUIT_MSG
        .DB     $0D,$0A,$0A
        .DB     " Returning to DOS/65."
        .DB     $0D,$0A
        .DB     $00
;
NVRAM_WRMSG
        .DB     $0D,$0A
        .DB     " Enter address for NVRAM data write"
        .DB     $0D,$0A
        .DB     " or hit enter to accept default address: "
        .DB     $00
;
NVRAM_CONF
        .DB     "  About to write NVRAM! Are you sure? (Y/N): "
        .DB     $00
;
NVRAM_DONE
        .DB     $0D,$0A
        .DB     " NVRAM Data written."
        .DB     $00
;
USER_INMSG
        .DB     $0D,$0A,$0A
        .DB     " Enter Command or M for Menu."
        .DB     $0D,$0A
        .DB     $00
;
DRIVE_IDENTITY
        .DB     $0D,$0A,$0A
        .DB     " MicroDrive Information:"
        .DB     $0D,$0A
        .DB     $00
;
MODEL_NUM
        .DB     $0D,$0A
        .DB     " Model Number: "
        .DB     $00
;
SERIAL_NUM
        .DB     $0D,$0A
        .DB     " Serial Number: "
        .DB     $00
;
FIRM_REV
        .DB     $0D,$0A
        .DB     " Firmware Revision: "
        .DB     $00
;
MODE_SUPPORT
        .DB     $0D,$0A
        .DB     " LBA Mode Supported: "
        .DB     $00
TOTAL_LBA
        .DB     $0D,$0A
        .DB     " Total LBA Count: "
        .DB     $00
;
LBA_INPUT
        .DB     $0D,$0A
        .DB     " Enter LBA number to Read from: "
        .DB     $00
;
LBA_OUTPUT
        .DB     $0D,$0A
        .DB     " Enter LBA number to Write to: "
        .DB     $00
;
LBA_START
        .DB     $0D,$0A
        .DB     " Enter starting LBA number: "
        .DB     $00
;
LBA_WR_DATA
        .DB     $0D,$0A
        .DB     " About to write LBA from buffer Data below!"
        .DB     $0D,$0A
        .DB     $00
;
LBA_WR_CNFM
        .DB     $0D,$0A
        .DB     " Are you SURE you want to overwrite the LBA?"
        .DB     $00
;
NEXT_LBA
        .DB     $0D,$0A
        .DB     " Display (N)ext LBA or (R)eturn "
        .DB     $00
;
SHOW_NEXT_LBA
        .DB     $0D,$0A,$0A
        .DB     " Displaying Data for LBA: "
        .DB     $00
;
LBA_SEQ_RD_MSG
        .DB     $0D,$0A,$0A
        .DB     " About to read ALL LBAs from MicroDrive!"
        .DB     $0D,$0A
        .DB     $00
;
LBA_SEQ_WR_MSG
        .DB     $0D,$0A,$0A
        .DB     " About to write ALL LBAs to MicroDrive!"
        .DB     $0D,$0A
        .DB     $00
;
LBA_SEQ_TM_MSG
        .DB     " Completion time based on drive capacity."
        .DB     $0D,$0A
        .DB     $00

LBA_SEQ_CFM
        .DB     $0D,$0A
        .DB     " Are you sure you want to"
        .DB     $00
;
LBA_SEQ_CFM2
        .DB     $0D,$0A
        .DB     " Are you REALLY sure you want to"
        .DB     $00
;
LBA_BLKS_RD
        .DB     $0D,$0A
        .DB     "Blocks Read:"
        .DB     $0D,$0A,$00
;
LBA_BLKS_WR
        .DB     $0D,$0A
        .DB     "Blocks Written:"
        .DB     $0D,$0A,$00
;
LBA_BLKS_RD_CMP
        .DB     $0D,$0A
        .DB     " All LBAs have been successfully read!"
        .DB     $0D,$0A
        .DB     $00
;
LBA_BLKS_WR_CMP
        .DB     $0D,$0A
        .DB     " All LBAs have been successfully written!"
        .DB     $0D,$0A
        .DB     $00
;
PATTERN_MSG
        .DB     $0D,$0A
        .DB     " Enter a 16-bit Hex value for the Fill Pattern: "
        .DB     $00
;
LBA_BENCH_INTRO
        .DB     $0D,$0A,$0A
        .DB     " Benchmark Performance Testing to Read or Write",$0D,$0A
        .DB     " a 16MB contiguous block of data starting from",$0D,$0A
        .DB     " the entered LBA address.",$0D,$0A,$0A
        .DB     " The Write Benchmark requires a 16-bit Hex fill pattern.",$0D,$0A
        .DB     " Note: the Write Benchmark will result in",$0D,$0A
        .DB     " LOSS of DATA on the MicroDrive being tested!",$0D,$0A,$0A
        .DB     " Make sure you know what you are doing!",$0D,$0A,$0A
        .DB     " Enter 'R' for Read or 'W' for Write: "
        .DB     $00
;
LBA_RD_BENCH
        .DB     $0D,$0A,$0A
        .DB     " Reading 16MB of LBA data in: "
        .DB     $00
;
LBA_WR_BENCH
        .DB     $0D,$0A,$0A
        .DB     " Writing 16MB of LBA data in: "
        .DB     $00
;
LBA_BENCH_WARN
        .DB     $0D,$0A,$0A
        .DB     " You are about to Write 32,768 LBAs!",$0D,$0A
        .DB     " All Data from starting LBA will be overwritten!",$0D,$0A
        .DB     " Be sure about this before continuing (Y/N)"
        .DB     $00
;
LBA_BENCH_ABORT
        .DB     $0D,$0A
        .DB     " Write Benchmark test aborted!",$0D,$0A
        .DB     $00
;
IDE_CONTROLLER_ERROR
        .DB     $0D,$0A,$0A
        .DB     " An error occured accessing the MicroDrive!",$0D,$0A,$0A
        .DB     "  * "
        .DB     $00
;
SYS_INTRO_MSG
        .DB     $0D,$0A
        .DB     "This will write an image from memory to the Microdrive!",$0D,$0A
        .DB     "Make sure you know what you are doing before you commit!!",$0D,$0A,$0A
        .DB     $00
;
SYS_LBA_MSG
        .DB     $0D,$0A
        .DB     " Enter the Starting LBA (decimal) to write the Memory Image to: "
        .DB     $00
;
BLK_SIZE_MSG
        .DB     $0D,$0A
        .DB     " Enter the number of 512-byte Blocks (decimal) to transfer: "
        .DB     $00
;
RAM_START_MSG
        .DB     $0D,$0A
        .DB     " Enter the Starting RAM address in Hex: "
        .DB     $00
;
SYS_CONFIRM_MSG
        .DB     $0D,$0A
        .DB     " Are you sure you want to overwrite the Disc data? "
        .DB     $00
;
SYS_WRITE_MSG
        .DB     $0D,$0A
        .DB     " Writing Disc Image..."
        .DB     $0D,$0A,$00
;
SYS_COMPLETE_MSG
        .DB     $0D,$0A
        .DB     " System Image written to Disc."
        .DB     $0A,$0D,$00
;
;
; BCD multiplicand table:
;
HMULTAB .DB $00, $00, $00, $00, $01             ;BCD weight of least significant HEX digit
        .DB $00, $00, $00, $00, $16
        .DB $00, $00, $00, $02, $56
        .DB $00, $00, $00, $40, $96
        .DB $00, $00, $06, $55, $36
        .DB $00, $01, $04, $85, $76
        .DB $00, $16, $77, $72, $16
        .DB $02, $68, $43, $54, $56             ;BCD weight of most significant HEX digit
;
; HEX multiplicand table:
;
DMULTAB .DB  $00, $00, $00, $01                 ;HEX weight of least significant BCD digit
        .DB  $00, $00, $00, $0A
        .DB  $00, $00, $00, $64
        .DB  $00, $00, $03, $E8
        .DB  $00, $00, $27, $10
        .DB  $00, $01, $86, $A0
        .DB  $00, $0F, $42, $40
        .DB  $00, $98, $96, $80
        .DB  $05, $F5, $E1, $00
        .DB  $3B, $9A, $CA, $00                 ;HEX weight of most significant BCD digit
;
; Data variables used
;
ROWS    .DB     #$10                            ;Default to 16 rows of displayed data
;
; LBA word count variables
;
LBA_LOW_WORD    .DW     $0000                   ;Low word for LBA count
LBA_HIGH_WORD   .DW     $0000                   ;High word for LBA count
;
; MicroDrive Error codes
;       These are the error codes per the Hitachi MicroDrive documentation.
;       The codes are read after an error is returned from a command
;       by executing an IDE Get Status command from BIOS.
;
;The X register will contaim the error code as detailed below:
;
; Error Register:
;Bit 7 - CRC Error or Bad Block error
;Bit 6 - Uncorrectable Data Error
;Bit 5 - 0 (not used) MC (used for Removable-Media drives)
;Bit 4 - ID Not Found
;Bit 3 - 0 (not used) MCR (used for Removable-Media drives)
;Bit 2 - Aborted Command error
;Bit 1 - Track Zero not found error
;Bit 0 - Data Address Mark Not Found
;
; The codes are indexed here and as they are received, the appropriate
; error message will be displayed.
;
IDE_ERROR_CODES
;
        .DB     %10000000                       ;CRC or Bad Block
        .DB     %01000000                       ;Uncorrectable Data Error
        .DB     %00010000                       ;ID Not Found
        .DB     %00000100                       ;Aborted Command
        .DB     %00000010                       ;Track Zero not found
        .DB     %00000001                       ;Data Address Mark not found
;
; IDE Error handler addresses
;       These are the addresses for the error messages.
;       These are indexed as above, so once the error message is matched
;       above, the index is multiplied by two and the address is used for the
;       error message text string.
;
IDE_ERROR_ADDRESS
;
        .DW     IDE_ERROR_00                    ;CRC or Bad Block
        .DW     IDE_ERROR_01                    ;Uncorrectable Data Error
        .DW     IDE_ERROR_02                    ;ID Not Found
        .DW     IDE_ERROR_03                    ;Aborted Command
        .DW     IDE_ERROR_04                    ;Track Zero not found
        .DW     IDE_ERROR_05                    ;Data Address Mark not found
;
; Error messages are here:
;
IDE_ERROR_00
        .DB     "CRC or Bad Block Error"
        .DB     $00
;
IDE_ERROR_01
        .DB     "Uncorrectable Data Error"
        .DB     $00
;
IDE_ERROR_02
        .DB     "Block ID Not Found"
        .DB     $00
;
IDE_ERROR_03
        .DB     "Aborted Command"
        .DB     $00
;
IDE_ERROR_04
        .DB     "Track Zero not Found"
        .DB     $00
;
IDE_ERROR_05
        .DB     "Data Address Mark not found"
        .DB     $00
;
IDE_ERROR_06
        .DB     "Unknown Error"
        .DB     $00
;
        .ORG    $/256*256+256                   ;Benchmark Buffer (start on page boundary)
BENCH_BUFFER
;
        .END

