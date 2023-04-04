;**************************************************************************************************
;*                                                                                                *
;*                        C02 BIOS/Monitor JMP Table for Version 4.01                             *
;*                                                                                                *
;*                                                                                                *
;*                                  03/03/2022 (Day/Month/Year)                                   *
;*                                                                                                *
;**************************************************************************************************
; C02BIOS Version 4.01                                                                            *
; - All Jump Table entries are defined in this single source file.                                *
; - Be sure to include this file at the start of any source file that needs it.                   *
;                                                                                                 *
;**************************************************************************************************
;
;**************************************************************************************************
;Monitor JUMP table: 32 JUMP calls are available.
;       - Call 02 is currently Reserved
;**************************************************************************************************
;
M_COLD_MON      .EQU    $E000           ;Call 00        Monitor Cold Start
M_WARM_MON      .EQU    $E003           ;Call 01        Monitor Warm Start
;
M_RESERVE2      .EQU    $E006           ;Call 02        Reserved
;
M_MOVE_RAM      .EQU    $E009           ;Call 03        Move Memory
M_FILL_LP       .EQU    $E00C           ;Call 04        Fill Memory
M_BSOUT         .EQU    $E00F           ;Call 05        Send Backspace
M_XMDM_SAVE     .EQU    $E012           ;Call 06        Xmodem Save Entry
M_XMDM_LOAD     .EQU    $E015           ;Call 07        Xmodem Load Entry
M_BENCH         .EQU    $E018           ;Call 08        Benchmark Start
M_QUITB         .EQU    $E01B           ;Call 09        Benchmark Stop/End
M_TIME          .EQU    $E01E           ;Call 10        System Date/Time
M_PRSTAT1       .EQU    $E021           ;Call 11        CPU Status Display
M_DIS_LINE      .EQU    $E024           ;Call 12        Disassemble Line of Code
M_INCINDEX      .EQU    $E027           ;Call 13        Increment Index by 1
M_DECINDEX      .EQU    $E02A           ;Call 14        Decrement Index by 1
M_RDLINE        .EQU    $E02D           ;Call 15        Read Line from Terminal
M_RDCHAR        .EQU    $E030           ;Call 16        Read Character from Terminal
M_HEXIN2        .EQU    $E033           ;Call 17        Hex input 2 characters
M_HEXIN4        .EQU    $E036           ;Call 18        Hex input 4 characters
M_HEX2ASC       .EQU    $E039           ;Call 19        Convert Hex to ASCII
M_BIN2ASC       .EQU    $E03C           ;Call 20        Convert Binary to ASCII
M_ASC2BIN       .EQU    $E03F           ;Call 21        Convert ASCII to Binary
M_BEEP          .EQU    $E042           ;Call 22        Send BEEP to Terminal
M_DOLLAR        .EQU    $E045           ;Call 23        Send $ to Terminal
M_CROUT         .EQU    $E048           ;Call 24        Send C/R to Terminal
M_SPC           .EQU    $E04B           ;Call 25        Send ASCII Space to Terminal
M_PRBYTE        .EQU    $E04E           ;Call 26        Print Byte to Terminal
M_PRWORD        .EQU    $E051           ;Call 27        Print Word to Terminal
M_PRASC         .EQU    $E054           ;Call 28        Print ASCII to Terminal
M_PROMPT        .EQU    $E057           ;Call 29        Send Message by number to Terminal
M_PROMPTR       .EQU    $E05A           ;Call 30        Send Message by address to Terminal
M_CONTINUE      .EQU    $E05D           ;Call 31        Y/N Prompt to Continue Command
;**************************************************************************************************
;
;**************************************************************************************************
;BIOS JUMP Table starts here:
;       - BIOS calls are listed below - total of 32
;       - Reserved calls are for future hardware support
;
B_IDE_RESET             .EQU    $FF00   ;Call 00
B_IDE_GET_STAT          .EQU    $FF03   ;Call 01
B_IDE_IDENTIFY          .EQU    $FF06   ;Call 02
B_IDE_READ_LBA          .EQU    $FF09   ;Call 03
B_IDE_WRITE_LBA         .EQU    $FF0C   ;Call 04
B_IDE_VERFY_LBA         .EQU    $FF0F   ;Call 05
B_IDE_SET_LBA           .EQU    $FF12   ;Call 06
B_IDE_SET_ADDR          .EQU    $FF15   ;Call 07
B_IDE_EN_CACHE          .EQU    $FF18   ;Call 08
B_IDE_DIS_CACHE         .EQU    $FF1B   ;Call 09
;
B_RTC_NVRD              .EQU    $FF1E   ;Call 10
B_RTC_NVWR              .EQU    $FF21   ;Call 11
B_RTC_INIT              .EQU    $FF24   ;Call 12
;
B_CHRIN2                .EQU    $FF27   ;Call 13
B_CHROUT2               .EQU    $FF2A   ;Call 14
;
B_CNT_INIT              .EQU    $FF2D   ;Call 15
B_CNT_STRT              .EQU    $FF30   ;Call 16
B_CNT_STOP              .EQU    $FF33   ;Call 17
;
B_CHRIN_NW              .EQU    $FF36   ;Call 18
B_CHRIN                 .EQU    $FF39   ;Call 19
B_CHROUT                .EQU    $FF3C   ;Call 20
;
B_SET_DLY               .EQU    $FF3F   ;Call 21
B_EXE_MSDLY             .EQU    $FF42   ;Call 22
B_EXE_LGDLY             .EQU    $FF45   ;Call 23
;
B_RESERVE               .EQU    $FF48   ;Call 24
;
B_INIT_VEC              .EQU    $FF4B   ;Call 25
B_INIT_CFG              .EQU    $FF4E   ;Call 26
B_INIT_28L92            .EQU    $FF51   ;Call 27
B_RESET_28L92           .EQU    $FF54   ;Call 28
;
B_WRMMNVEC0             .EQU    $FF57   ;Call 29
B_CLDMNVEC0             .EQU    $FF5A   ;Call 30
B_COLDSTRT              .EQU    $FF5D   ;Call 31
;
BIOS_MSG                .EQU    $FFD0   ;BIOS Startup Message is hard-coded here
;**************************************************************************************************
        .END