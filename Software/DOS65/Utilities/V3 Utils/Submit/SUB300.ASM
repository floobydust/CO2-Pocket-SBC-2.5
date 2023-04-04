;*************************************************************************************************
        GLOBALS ON
        CASE    OFF
        CHIP    W65C02S
        PW      132
        PL      66
;*************************************************************************************************
;SUBMIT V3.03
;DOS/65 program to create $$$.SUB file from source file of type SUB and with
;parameter substitution. This program is based on the CP/M program of the same
;name.
;SUBMIT will only work with the CCM of DOS/65 V3 or later.
;released:              23 February 2015 (V3.00)
;
;Updated 24th May 2021 - KM
; - 3.03 Version. Changes for:
; - WDC Assembler/Linker, DOS/65 Version 3.03
; - CMOS opcodes and addressing modes (requires 65C02 CPU)
; - Code size reduction
; - Some source cleanup, commenting
;*************************************************************************************************
;DOS/65 constants
PZSTRT  .EQU    $30                     ;page zero start
PEM     .EQU    $103
DFLFCB  .EQU    $107
DFLBUF  .EQU    $128
TEA     .EQU    $800
BUFLNG  .EQU    8196                    ;output buffer (rbuff) length
;ASCII definitions
LF      .EQU    $0A                     ;linefeed
CR      .EQU    $0D                     ;carriage return
EOF     .EQU    $1A                     ;end of file
;*************************************************************************************************
;
        PAGE0
;Page Zero variables
        .ORG    PZSTRT                  ;start after HW registers
ErrMs                                   ;address of error message
        DEFS    2
;variables for MOVE
from                                    ;from pointer in MOVE
        DEFS    2
to                                      ;to pointer in MOVE
        DEFS    2
bbyte                                   ;b byte value in GetSrc
        DEFS    1
sbp                                     ;SUB buffer pointer
        DEFS    1
rbp                                     ;pointer into rbuff
        DEFS    2
rlen                                    ;record length
        DEFS    1
ss                                      ;substitue string
        DEFS    1
ssbp                                    ;substitute string pointer
        DEFS    1
PutRIn                                  ;In/Out character
        DEFS    1
readng                                  ;flag = bit 7 set or cleared
        DEFS    1
b                                       ;character active in fillrbuff
        DEFS    1
ichar                                   ;single char and index in makefile
        DEFS    1
;
        ENDS
;
        CODE
;
;code section
        .ORG    TEA                     ;Start at TEA
;
;main code - simple and straightforward
        lda     #<openms                ;send opening message
        ldy     #>openms
        jsr     MsgOut
        jsr     SETUP                   ;get everything ready
        jsr     FillRB                  ;get and process source
        jsr     MkFile                  ;prepare and save $$$.sub
boot    ldx     #0                      ;must end with warm boot
        jmp     pem                     ;using PEM
;
;messages and strings
OpenMs  .DB     'DOS/65 SUBMIT V3.03$'
ERRLMs  .DB     cr,lf,'Error on Line $'
SUBStr  .DB     'SUB'
NSUBMs  .DB     'No .SUB File Present$'
WrErMs  .DB     'Disk Write Error$'
BfOvMs  .DB     'Command Buffer Overflow$'
ToLnMs  .DB     'Command Too Long$'
PrmEMs  .DB     'Parameter Error$'
IvClMs  .DB     'Invalid Control Character$'
DrFlMs  .DB     'Directory Full$'
NoClMs  .DB     'Cannot Close, Read-only?$'
;
;send message pointed to by AY to console
MsgOut  ldx     #9                      ;send message out command
        jmp     PEM
;
;the following PEM commands are only used once, so the usage in code
; below just does a LDX #cmd, then JSR PEM. Saves some space.
;
;open file with FCB at AY
;OPEN    ldx     #15
;        jmp     PEM
;
;close file with FCB at AY
;CLOSE   ldx     #16
;        jmp     PEM
;
;delete file with FCB at AY
;DELETE  ldx     #19
;        jmp     PEM
;
;read from file
;READ    ldx     #20
;        jmp     PEM
;
;write to file
;WRITE   ldx     #21
;        jmp     PEM
;
;create file
;MAKE    ldx     #22
;        jmp     PEM
;
;move(from,to,n)
;source address (from)
;destination address (to)
;count (n)
;Parameters are passed as follows:
;x=n
;a=low of to
;y=high of to
;from must be set by calling routine
move    sta     to                      ;save to
        sty     to+1
        ldy     #0                      ;clear index
movelp  lda     (from),y                ;get byte
        sta     (to),y                  ;put byte
        iny                             ;increment index
        dex                             ;drop count
        bne     movelp                  ;loop if more
        rts
;
;general error handling routine
;INPUT: Specific message address = AY
;OUTPUT: none
ERROR   sta     ErrMs                   ;save error line
        sty     ErrMs+1
        lda     #<ERRLMs                ;send main error message
        ldy     #>ERRLMs
        jsr     MsgOut                  ;send to console
        lda     #<LneNum                ;point to line # structure
        ldy     #>LneNum
        jsr     MsgOut                  ;send to console
        lda     ErrMs                   ;point to specific message
        ldy     ErrMs+1
        jsr     MsgOut                  ;send to console
        jmp     boot                    ;back to CCM
;
;set up the various pointers, counters, buffers, etc.
SETUP   lda     #<dflbuf+1              ;point to default buffer + 1
        ldy     #>dflbuf                ;second position
        sta     from                    ;store to from location
        sty     from+1                  ;store high byte
        ldx     #127                    ;load 127 as count to move
        lda     #<sstrng                ;get address for destination
        ldy     #>sstrng                ;get address high byte
        jsr     move                    ;call move routine
        ldx     DflBuf                  ;get first byte = length
        stz     sstrng,x                ;insert 0 to mark end
        lda     #<SUBStr                ;get address for SUB string
        ldy     #>SUBStr                ;get high byte address
        sta     from                    ;store to from location
        sty     from+1                  ;store high byte
        ldx     #3                      ;load 3 as count to move
        lda     #<dflfcb+9              ;get address for destination
        ldy     #>dflfcb                ;get address high byte
        jsr     move                    ;call move routine
        lda     #<DskBuf                ;set disk buffer
        ldy     #>DskBuf                ;address to local
        ldx     #26                     ;load PEM command for set disk buffer
        jsr     PEM                     ;Call PEM
        lda     #<dflfcb                ;Get address for default FCB
        ldy     #>dflfcb                ;Get address high byte
        ldx     #15                     ;load PEM command for file open
        jsr     PEM                     ;Call PEM
        cmp     #255                    ;check for error
        bne     STUPOK                  ;jump ahead if none
        lda     #<nsubms                ;get address SUB file error msg
        ldy     #>nsubms                ;get address high byte
        bra     ERROR                   ;branch to error handler
STUPOK  lda     #128                    ;else, set buffer pointer to
        sta     sbp                     ;to force read later
        rts                             ;return to caller
;
;GetSrc - get single byte from source file
GetSrc  ldx     sbp                     ;get buffer index
        bpl     USESBP                  ;use if 0-127
        lda     #<DflFCB                ;else point to FCB
        ldy     #>DflFCB                ;get high byte FCB
        ldx     #20                     ;get PEM command for read file
        jsr     PEM                     ;call PEM
        cmp     #0                      ;check return code
        beq     CLRSBP                  ;if OK clear pointer
        lda     #eof                    ;else return end of file
        rts                             ;return to caller
;start record read at start of buffer
CLRSBP  stz     sbp                     ;clear sbp
;sbp is now 0 to 127 - we are ready to read byte
        ldx     sbp                     ;get index
USESBP  inc     sbp                     ;bump for next
        lda     DskBuf,x                ;get char from buffer
        sta     bbyte                   ;and save
        cmp     #cr                     ;see if carriage return
        bne     BNOTCR                  ;if not skip ahead
        inc     LN3                     ;if is bump LSD of line number
        lda     LN3                     ;get digit
        cmp     #'9'+1                  ;see if over '9'
        bcc     bnotcr                  ;not so use as is
        lda     #'0'                    ;but if is reset to '0'
        sta     ln3                     ;store to line number
        inc     ln2                     ;bump middle digit
        lda     ln2                     ;then get it
        cmp     #'9'+1                  ;and check it
        bcc     bnotcr                  ;skip ahead if 0 to 9
        lda     #'0'                    ;else reset middle digit
        sta     ln2                     ;store to line number
        inc     ln1                     ;then bump LSD
BNOTCR  lda     bbyte                   ;get char back
;convert to upper case
        cmp     #'a'                    ;not lower if a <
        bcc     notlc
        cmp     #'z'+1                  ;not lower if a >=
        bcs     notlc
        and     #$5f                    ;convert to upper
        sta     bbyte                   ;save it
        rts                             ;save some clock cycles
NOTLC   lda     bbyte                   ;return it
WRBFOK  rts                             ;return to caller
;
;WrtBuf - write disk buffer to disk file
WrtBuf  lda     #<OutFCB                ;point to FCB
        ldy     #>OutFCB                ;get high byte FCB
        ldx     #21                     ;load PEM command for write file
        jsr     PEM                     ;call PEM
        cmp     #0                      ;check for OK return
        beq     WRBFOK                  ;if 0 OK
        lda     #<WrErMs                ;else get write error msg
        ldy     #>WrErMs                ;Get high byte msg
        jmp     ERROR                   ;jump to error handler
;
;Routine that reads source file and does parameter replacement
;b is the character being processed
FillRB  stz     rbuff                   ;put "stopper(0)" in buffer
        lda     #<rbuff
        ldy     #>rbuff                 ;set rbp to start of rbuff
        sta     rbp                     ;rbp <-- rbuff
        sty     rbp+1
        SMB7    readng                  ;set flag
;do while reading TRUE
DOLp00  BBS7    readng,L01              ;Branch if readng set
        bra     WRBFOK                  ;if FALSE exit
L01     stz     rlen                    ;reset cmd length
;do while (b<--getsource) <> eof and b <> cr
DOLp01  jsr     GetSrc                  ;get character from source
        sta     b                       ;save it
;now check if EOF or CR
        cmp     #eof                    ;see if EOF
        bne     L02
        bra     XLp01                   ;if so done
L02     cmp     #cr                     ;now check for CR
        bne     IfLp02
        bra     Xlp01                   ;if so done
;if b<>lf then
IFLP02  lda     b                       ;get saved
        cmp     #lf                     ;see if LF
        beq     DoLp01                  ;if so start again
;see if $
        cmp     #'$'
        bne     Else3a                  ;is not so test ^
;if (b<--GetSrc) = $
        jsr     GetSrc                  ;get next
        sta     b                       ;save it
        cmp     #'$'                    ;see if double
        bne     Else3b                  ;jump if not
        jsr     PutRBf                  ;insert single $
        bra     DoLp01
;not double $
Else3b  lda     b                       ;prepare for subtract
;if (b <-- b-'0')
        sec                             ;use subtraction
        sbc     #'0'                    ;subtract ASCII 0
;the previous line made b a parameter counter
        sta     b                       ;save result (1 to 9)
        cmp     #9+1                    ;carry clear if a < 10
;if result <= 9 then OK else a parameter error
        bcc     Else3c                  ;then continue
        lda     #<PrmEMs                ;else parameter error
        ldy     #>PrmEMs
        jmp     ERROR
;looks like parameter so find string b in sstrng
Else3c  stz     ssbp                    ;start at string start
Else3G  jsr     DeBPrm                  ;find next non-blank
;do while b<>0
Else3d  lda     b                       ;get char
        beq     Else3e                  ;end if zero
        dec     b                       ;b <-- b-1
Else3f  jsr     notend                  ;scan
        bne     Else3f                  ;until end
        bra     Else3g
;ready to copy substitute string to RBuff
Else3e  jsr     notend                  ;check for end
        beq     DoLp01                  ;done if END
        lda     ss                      ;get char
        jsr     PUTRBF                  ;insert in RBuff
        bra     Else3e                  ;loop for Do While
;
;else if b=^
Else3a  cmp     #'^'                    ;see if control
        bne     Else3j                  ;if not just copy
        jsr     GetSrc                  ;get next char
        sec                             ;b <-- b-'A'
        sbc     #'A'
        sta     b                       ;and save
        cmp     #27                     ;make sure 0 to Z
        bcc     Else3h                  ;OK if < 26
        lda     #<IvClMs                ;else invalid control char error
        ldy     #>IvClMs
        jmp     error                   ;error exit
Else3h  lda     b                       ;get char back
        inc     a                       ;bump by one
        jsr     PutRBf                  ;put in output buffer
        bra     DoLp01                  ;and loop
;was neither $ nor ^
Else3j  lda     b
        jsr     PutRBf                  ;so just transfer char
        bra     DOLp01                  ;then loop
;
XLp01   lda     b                       ;get character
        cmp     #cr                     ;see if cr
        beq     L05                     ;if so go ahead
        RMB7    readng                  ;clear flag bit
        BRA     L06                     ;branch to finish
L05     SMB7    readng                  ;set flag bit
L06     lda     rlen                    ;store length
        jsr     PutRBf
        jmp     DOLp00                  ;loop
;
;notend
;Return true if next char in substitute string not at end of string
;Also returns character in ss and if NOTEND returns tru pointer (ssbp)
;is advanced by one.
NOTEND  ldx     ssbp                    ;get pointer or index
        lda     sstrng,x                ;get character
        sta     ss                      ;save
        cmp     #' '                    ;see if space
        beq     NotEnF                  ;if space is end so notend false
        cmp     #0                      ;see if zero
        beq     NBChar                  ;if 0 is end so notend false
;NOTEND is true
        inc     ssbp                    ;ssbp <-- ssbp+1
        lda     #255                    ;return true
        rts
NotEnF  lda     #0                      ;return false
NBChar  rts
;
;DeBPrm - move to next non-blank substitute string char
DeBPrm  ldx     ssbp
        lda     sstrng,x                ;get char
        cmp     #' '                    ;see if space
        bne     NBChar                  ;done if not
        inc     ssbp                    ;else bump index
        bra     DeBPrm                  ;and loop
;
;PutRBf
;INPUT: byte in A
PutRBf  sta     PutRIn                  ;save inout character
        inc     rbp                     ;bump pointer
        bne     rbpok                   ;skip high
        inc     rbp+1                   ;else bump high
rbpok   sec
        lda     rbp                     ;check for too big
        sbc     #<lastrb
        lda     rbp+1
        sbc     #>lastrb
        bcc     RBPLEn                  ;OK if less than end + 1
        lda     #<BfOvMs                ;else buffer overflow
        ldy     #>BfOvMs
        jmp     ERROR
RBPLEn  lda     PutRIn                  ;get character back
        sta     (rbp)                   ;put in buffer
        inc     rlen
        lda     rlen                    ;bump and get record length
        cmp     #126                    ;compare to max
        bcc     NBChar                  ;OK if <126
        lda     #<ToLnMs                ;else too long error
        ldy     #>ToLnMs
        jmp     ERROR
;
;MkFile creates the active command file.
MkFile  lda     #<OutFCB                ;point to FCB
        ldy     #>OutFCB
        ldx     #19                     ;PEM command for delete file
        jsr     PEM                     ;delete it
        stz     drec                    ;clear record number
        stz     dext                    ;and extent
        lda     #<OutFCB                ;point to FCB
        ldy     #>OutFCB
        ldx     #22                     ;PEM command for create file
        jsr     PEM
        cmp     #255                    ;check for MAKE error
        bne     MAKEOK
        lda     #<DrFlMs                ;error - drive full
        ldy     #>DrFlMs
        jmp     ERROR
MAKEOK  jsr     GetRBf                  ;get character
        beq     LSTCHR                  ;close out if zero
        sta     ichar                   ;save
        sta     DskBuf                  ;also put in disk buffer
; buff(i+1) <-- 0 and buff(i+2) <-- '$'
        tax                             ;make i an index
        stz     DskBuf+1,x              ;insert 0 into buffer
        lda     #'$'                    ;get $
        sta     DskBuf+2,x
;do while i>0
DWIGT0  ldx     ichar                   ;see if > 0
        beq     IEq0                    ;i = 0
        jsr     GetRBf                  ;else keep getting
        sta     DskBuf,x                ;and save
        dec     ichar
        bne     DWIGT0                  ;and loop for more
IEQ0    jsr     WrtBuf
        bra     MAKEOK
LSTCHR  lda     #<OutFCB                ;point to FCB
        ldy     #>OutFCB
        ldx     #16                     ;PEM command for close file
        jsr     PEM
        cmp     #255
        bne     OKCLS
        lda     #<NoClMs                ;close error
        ldy     #>NoClMs
        jmp     ERROR
;
;GetRBf after rbp <-- rbp-1
GetRBf  lda     rbp                     ;get low byte
        bne     skp_rbp                 ;skip if zero
        dec     rbp+1                   ;decrement high byte
skp_rbp dec     rbp                     ;decrement low byte
        lda     (rbp)                   ;get character
OKCLS   rts
;
;line number string
LneNum  .DB     '001 $'
LN1     .EQU    LneNum
LN2     .EQU    LneNum+1
LN3     .EQU    LneNum+2
;Output FCB
OutFCB  .DB     0
        .DB     '$$$     SUB'
;extent
dext    .DB     0
        .DB     0,0,0
        DEFS    16
;record number for OutFCB
drec    .DB     0
;substitute string
sstrng
        DEFS    128
;disk buffer - replaces default
DskBuf
        DEFS    128
;read buffer
;baseline allows 32 command lines at 4096 bytes total
;If desired this can be increased.
;
rbuff   .EQU    *
;
lastrb  .EQU    *+BufLng
;
        ENDS
;
        END
