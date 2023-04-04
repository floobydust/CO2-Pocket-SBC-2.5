;
;----------------------------------------
;dos/65 primitive execution module (pem)
;----------------------------------------
;
; Note: The checksum routine and all associated code
; to support the checksum operations have been removed
; from PEM. This is no longer required with standard
; IDE or other nonremovable block devices.
;  - clcchk and clcckp routines are also removed.
;
; Other routines which are specific to diskette media
; has also been removed. Many of these routines are
; specific to checking for a diskette being removed,
; which is considered removable media.
;
; As this new version of DOS/65 works specifically with
; newer Block level devices (IDE, Compact Flash, SD-Card,
; SCSI, etc.) there's no longer a need to keep these
; routines in play.
;
pemram
;
;main program
; input:x=command,a=value,a&y=address
; returns:a=value,a&y=address
; alters:all
        cld                     ;ensure binary mode
        sta     lowin           ;save value and
        sty     lowin+1         ;address to pointers
        cpx     #numcmd         ;check for valid cmd number
        bcs     noswot          ;exit without switch, if over
        phx                     ;else, save command to stack
        jsr     switch          ;switch data to/from page zero
        pla                     ;then restore command to A reg
        stz     tmpdrv          ;clear drive switch flag
        asl     a               ;multiply command by two
        tax                     ;xfer back for index pointer
        lda     addinp          ;get input value
        ldy     addinp+1        ;and address value
        jsr     cmd_exe         ;call execute PEM command
;
;The next line is the exit entry location for
;all PEM function calls.
extexq  sta     bytout          ;save value
        sty     addout+1        ;and address
;First step is to restore original drive
        lda     tmpdrv          ;see if temp drive flag set
        beq     notmpd          ;no so skip following
        lda     olddrv          ;get original drive number
        sta     drvcmd          ;set up like input
        jsr     chgdrv          ;change drive back
;
;At this point OLDDRV is mapped with USRCDE in
;byte 0 of FCB. Now clear that byte and restore
;original drive designator.
notmpd  bbr5    d65flg,skptmp   ;test flag, branch if clear
        rmb5    d65flg          ;clear drive flag bit
;
        lda     tmpdrv          ;get temp drive
        sta     (addinp)        ;put back into fcb
        stz     tmpdrv          ;clear flag
skptmp  jsr     switch          ;switch memory to/from page zero
noswot  ldy     addout+1        ;get address
        lda     bytout          ;and value (address low)
        rts                     ;return to caller
;
cmd_exe jmp     (cmdtbl,x)      ;execute PEM command via table
;
;command vector table
cmdtbl
        .DW     xwboot          ;warm boot (x=0)
        .DW     xcnsin          ;console input with echo (x=1)
        .DW     sndchr          ;console output (x=2)
        .DW     simram+21       ;tape reader (x=3)
        .DW     simram+18       ;tape punch (x=4)
        .DW     simram+15       ;printer output (x=5)
        .DW     getcon          ;console input w/o echo (x=6)
        .DW     xgtios          ;read i/o status (x=7)
        .DW     xstios          ;set i/o status (x=8)
        .DW     sndstr          ;print buffer (x=9)
        .DW     bufinp          ;read buffer (x=10)
        .DW     kbdsts          ;test console ready (x=11)
        .DW     simram+45       ;read list status (x=12)
        .DW     xintds          ;initialize system (x=13)
        .DW     xchgdr          ;log in drive (x=14)
        .DW     xopen           ;open file (x=15)
        .DW     xclose          ;close file (x=16)
        .DW     xfndfr          ;find first match (x=17)
        .DW     xfndnx          ;find next match (x=18)
        .DW     xdltfl          ;delete file (x=19)
        .DW     xread           ;read record (x=20)
        .DW     xwrite          ;write record (x=21)
        .DW     xmake           ;create file (x=22)
        .DW     xrenme          ;rename file (x=23)
        .DW     xintlg          ;interrogate log in status (x=24)
        .DW     xintdr          ;interrogate current drive (x=25)
        .DW     chgdma          ;set buffer address (x=26)
        .DW     xrdalv          ;read allocation map start (x=27)
        .DW     setron          ;set r/w status (x=28)
        .DW     xrdros          ;read r/w status (x=29)
        .DW     setlst          ;set list echo status (x=30)
        .DW     lststs          ;read list echo status (x=31)
        .DW     xrtclo          ;read low clock (x=32)
        .DW     xrtchi          ;read high clock (x=33)
        .DW     xrddcb          ;read dcb address (x=34)
        .DW     simram+51       ;translate sector (x=35) - obsolete
        .DW     xgsusr          ;get or set user code (x=36)
;
;set list echo status
setlst  sta     lstflg          ;set flag
        rts
;
;read list echo status
lststs  lda     lstflg          ;read flag
        rts
;
;execute get or set user code
; when A reg is #255, it's a request to get the user code
; otherwise, A reg is #0-15 to set the user code
xgsusr  cmp     #255            ;check if request
        beq     rtnusr          ;if #255, get user code
        sta     usrcde          ;else, set user code
        rts                     ;then exit
rtnusr  lda     usrcde          ;get user code
        rts                     ;then exit
;
;execute warm boot
; ***this command does not return to pem***
; SIM handles reloading of CCM and PEM,
; then initializes Page zero, the CPU stack
; the PEM vectors at $0100, then jumps to CCM.
xwboot  jsr     switch          ;switch memory
        lda     usrcde          ;get user code
        asl     a               ;shift to high nibble
        asl     a
        asl     a
        asl     a
        ora     curdrv          ;or in current drive
        sta     iostat          ;save for SIM
        jmp     simram+3        ;go to sim to execute
;
;execute read real time clock
xrtclo  jsr     simram+48       ;read clock
        sta     rtclk           ;save low
        sty     rtclk+1         ;middle
        stx     rtclk+2         ;high
        txa
        and     #%10000000      ;test for valid
        tay
        lda     rtclk           ;get low
        rts
;
;execute read real time clock high
xrtchi  lda     rtclk+2         ;get high
        and     #%01111111      ;clear status
        tay
        lda     rtclk+1         ;get middle
        rts
;
;execute read dcb address
xrddcb  lda     cptdcb+1        ;get address
        ldy     cptdcb+2
        rts
;
;execute read i/o status
xgtios  lda     iostat          ;get status byte
        rts
;
;execute read allocation map starting address
xrdalv  lda     alcmap          ;get
        ldy     alcmap+1        ;starting address
        rts                     ;then return
;
;execute set i/o status
xstios  sta     iostat          ;set status
        rts
;
;execute interrogate current drive
xintdr  lda     curdrv          ;get number
        rts
;
;execute read log in status
xintlg  lda     lginvc          ;get status
        rts
;
;execute read read/write status
xrdros  lda     ronlst          ;get status
        rts
;
;execute find next match
xfndnx  jsr     autodr          ;auto drive select
        lda     #13             ;match including
        jmp     search          ;extent
;
;execute read next record
xread   jsr     autodr          ;auto drive select
        jsr     inrcct          ;initialize record counters
        lda     nxtrec          ;if next record less
        cmp     numrec          ;then number records
        bcc     tryrde          ;then try to read
        cmp     #128            ;but if not and
        beq     tryext          ;is max try to extend
rdeeof  lda     #1              ;else set eof
        rts                     ;code and return
tryext  smb6    d65flg          ;set bit for read
        jsr     extend          ;file extension
        bne     rdeeof          ;with jump back
        sta     nxtrec          ;clear next record
tryrde  jsr     getblk          ;and get block
        lda     blknum          ;if block not zero
        ora     blknum+1
        bne     dorde           ;do the read
        lda     #2              ;else set unwritten
        rts                     ;code and exit
dorde   jsr     calrcn          ;calculate record number
        jsr     setrec          ;set record number
        jsr     rderec          ;do record read
        jsr     updtrc          ;update counters
        lda     #0              ;return w/o error
extrde  rts
;
;execute file rename
xrenme  jsr     autodr          ;automatic drive select
        jsr     tstron          ;test for r/o
        lda     #12             ;match name
        jsr     frstsr          ;and type
        bmi     extren          ;exit if not found
doren   lda     (addinp)
        ldy     #16             ;in new name
        sta     (addinp),y      ;part of fcb
        lda     #12             ;then move
        ldx     #16             ;new name
        jsr     dirchg          ;to directory
        lda     #12             ;see if
        jsr     search          ;another match
        bpl     doren           ;loop if found
extren  rts                     ;number
;
;execute delete file
xdltfl  jsr     autodr          ;automatic drive select
        jsr     tstron          ;test for r/o
        lda     #12             ;match name
        jsr     frstsr          ;and type
        bmi     extdlt          ;done if not found
dodlt   ldx     #0              ;else do a
        jsr     mapdir          ;delete
        ldy     subrec          ;then change
        lda     empty           ;byte zero to
        sta     (bufadd),y      ;empty code
        jsr     wrtrec
        lda     #12             ;search
        jsr     search          ;for next
        bpl     dodlt           ;loop if found
extdlt  rts                     ;exit
;
;execute write next record
xwrite  jsr     autodr          ;automatic drive select
        jsr     tstron          ;test for r/o
        jsr     inrcct          ;initialize counters
        lda     nxtrec          ;get next record
        cmp     #128            ;compare to max
        bcc     ntexte          ;continue if less
        lda     #1              ;else flag as extend
        rts                     ;error and exit
ntexte  jsr     getblk          ;calculate block num
        lda     blknum          ;get it and if
        ora     blknum+1
        beq     zwrt            ;zero then get number
        jmp     blkopn          ;else go write
zwrt    lda     fcbind          ;get index from prior
        pha                     ;and save
        cmp     #16             ;if first block
        beq     isfrst          ;then don't change
        tay                     ;else make an index
        dey                     ;point to last
        bbr7    d65flg,blm0     ;test mode, branch if byte
        dey                     ;else drop for word
blm0    lda     (addinp),y      ;and get that number
        sta     blknum          ;store as starting
        iny                     ;bump for word
        lda     #0              ;preset for byte
        bbr7    d65flg,blm1     ;test mode, branch if byte
        lda     (addinp),y      ;else get high
blm1    sta     blknum+1        ;then set high
isfrst  lda     blknum          ;get block
        sta     lkdown          ;set lower and
        sta     lookup          ;upper pointers
        lda     blknum+1        ;now set high part
        sta     lkdown+1
        sta     lookup+1
srblag  lda     lookup          ;if upper pointer
        cmp     maxblk          ;not at max
        bne     upnemx          ;then keep going
        ldy     lookup+1        ;now check high
        cpy     maxblk+1
        beq     tstdwn          ;go test down
upnemx  inc     lookup          ;else bump upper
        bne     skpup
        inc     lookup+1
skpup   lda     lkdown          ;then if lower
        ora     lkdown+1
        beq     dotest          ;is zero start test
        bne     dcdown          ;else drop lower
tstdwn  lda     lkdown          ;if upper & lower at
        ora     lkdown+1
        bne     dcdown          ;drop if not zero
        tay                     ;also set y
        beq     extsbl          ;then exit
dcdown  lda     lkdown          ;drop lower
        bne     skpdn
        dec     lkdown+1
skpdn   dec     lkdown
dotest  lda     lkdown          ;get lower
        ldy     lkdown+1
        jsr     tstblk          ;and test
        bne     trylup          ;if full try upper
        lda     lkdown          ;else use lower
        ldy     lkdown+1
        bra     extsbl          ;as result
trylup  lda     lookup          ;get upper
        ldy     lookup+1
        jsr     tstblk          ;if full
        bne     srblag          ;then loop
        lda     lookup          ;else use upper
        ldy     lookup+1
extsbl  sta     blknum          ;then save number
        sty     blknum+1
        ora     blknum+1        ;see if zero
        bne     gtgood          ;then proceed
        pla                     ;clear stack
        lda     #2              ;set end
        rts                     ;of data return
gtgood  lda     blknum          ;get low again
        jsr     setblk          ;set allocation map
        ply                     ;get block pointer back
        lda     blknum          ;get number
        sta     (addinp),y      ;and put in fcb
        lda     blknum+1        ;get high
        iny
        bbr7    d65flg,blkopn   ;test mode, branch if byte
        sta     (addinp),y      ;else set high
blkopn  jsr     calrcn          ;calculate record num
        jsr     setrec          ;set record number
        jsr     wrtrec          ;do write
        ldx     nxtrec          ;get next record
        cpx     numrec          ;if less than max
        bcc     notful          ;then ok
        inx                     ;else bump count
        stx     numrec          ;and save
        dex                     ;back down
notful  cpx     #127            ;if not at max
        bne     noawex          ;skip extending
        jsr     updtrc          ;update record counters
        rmb6    d65flg          ;set bit to zero for write
        jsr     extend          ;file extension
        bne     extwrt          ;exit
        lda     #255            ;else start counter
        sta     nxtrec          ;at one short
noawex  jsr     updtrc          ;then update counters
        lda     #0              ;good return
extwrt  rts                     ;exit
;
;execute console input
xcnsin  jsr     getcon          ;get input
        jsr     tstchr          ;test it and
        bcc     extxci          ;if control exit
        pha                     ;else save
        jsr     sndchr          ;echo
        pla                     ;restore
extxci  rts                     ;return
;
;switch memory
;page zero block begins at pzstrt and is numvar bytes long.
switch  ldx     #numvar-1       ;get number to switch
nxtswh  lda     pzstrt,x        ;get zero page
        ldy     varblk,x        ;and save area
        sta     varblk,x        ;save zero
        sty     pzstrt,x        ;and high
        dex                     ;count down
        bpl     nxtswh          ;and loop until done
        rts                     ;then return
;
;change dma address
; input:addinp=address
; returns:none
; alters:all
chgdma  lda     addinp          ;get low
        ldy     addinp+1        ;and high address
        sta     bufadd          ;then store
        sty     bufadd+1        ;in address
        jmp     simram+36       ;then go to sim
;
;automatic drive select
; input:(addinp) 0=no change
; 1-8 or 'A'-'H' = change
; returns:none
; alters:all
autodr  lda     (addinp)        ;first byte of fcb
        beq     qtatdr          ;if 0 skip drive set
        dec     a               ;subtract one
        and     #%00000111      ;look at lower 3 bits
        sta     drvcmd          ;make parameter for CHGDRV
        lda     curdrv          ;get current
        sta     olddrv          ;and save
        lda     (addinp)        ;get fcb entry
        sta     tmpdrv          ;and save as flag
qtatdr  lda     usrcde          ;get user code
        sta     (addinp)        ;save in byte zero in fcb
        jsr     chgdrv          ;change the drive
        smb5    d65flg          ;set drive flag bit
drvsme  rts
;
;change drive
; input:addinp
; returns:none
; alters:all
xchgdr  lda     addinp          ;get inout
        cmp     #maxdrv         ;check valid maxdrv range
        bcs     drnger          ;if over, send range error
        sta     drvcmd          ;and save
chgdrv  lda     drvcmd          ;get input
        cmp     curdrv          ;if same as current
        beq     drvsme          ;do nothing
        sta     curdrv          ;else change current
        bra     mapdrv          ;then log it in
;
drnger  lda     #<rngmsg        ;point to
        ldy     #>rngmsg        ;select message
        jsr     sndstr          ;and send it
        jmp     xwboot          ;and abort
;
;execute initialize system
; input:none
; returns:none
; alters:all,lginvc,bufadd
xintds  stz     lginvc          ;clear log in status
        lda     #<dflbuf        ;get default buffer
        ldy     #>dflbuf        ;address
        sta     addinp          ;and set up
        sty     addinp+1        ;parameters
        jsr     chgdma          ;change address
;
;log in drive, set pointers and maps
; input:curdrv
; returns:none
; alters:all
mapdrv  lda     curdrv          ;if current drive
        cmp     #maxdrv         ;check valid maxdrv range
        bcc     vlddrv          ;then log it in
drserr  jsr     errout          ;send error message
        lda     #<sltmsg        ;point to
        ldy     #>sltmsg        ;select message
        jsr     sndstr          ;and send it
        jmp     xwboot          ;and abort
vlddrv  jsr     simram+27       ;go to sim to set
        sta     cptdcb+1        ;save
        sty     cptdcb+2
        ora     cptdcb+2        ;see if invalid
        beq     drserr          ;error if is
;capture dcb
        ldy     #11-1           ;do 11 bytes (new DCB)
cptdcb  lda     $FFFF,y         ;get value from sim
        sta     dcb,y           ;store it
        dey
        bpl     cptdcb          ;loop for more
        lda     blkscd          ;get block size code
        tay                     ;save in y
        clc
        adc     #3              ;convert to sxb
        sta     sxb             ;and save
        lda     sabtbl,y        ;get sab
        sta     sab             ;and set
        lda     maxdir          ;get max dir
        sta     maxdrc          ;set max dir record
        lda     maxdir+1
        lsr     a               ;divide by 4
        ror     maxdrc
        lsr     a
        ror     maxdrc
        sta     maxdrc+1        ;then save high
        rmb7    d65flg          ;reset mode to byte
        lda     maxblk+1        ;branch if max block
        beq     ntm255          ;not over 255
; Y reg contains the block size code, which can be 0,
; so the initial comment about it being an error is not
; correct, so nothing to handle here as an error.
;
        dey                     ;back up index
        smb7    d65flg          ;set mode to word
ntm255  lda     exmtbl,y        ;get extent mask
        sta     exm
        sec                     ;now calculate cexm1f
        lda     #31
        sbc     exmtbl,y
        sta     cexm1f
        ldx     curdrv          ;get drive as pointer
        lda     bitmap,x        ;get bit
        and     lginvc          ;if logged in
        bne     extstm          ;then exit
        lda     bitmap,x        ;get bit back
        ora     lginvc          ;set in log-in
        sta     lginvc          ;and update
        bra     flinal          ;then fill in maps
extstm  rts                     ;and return
;
;fill in allocation map
flinal  jsr     setrw           ;set to read/write
        lda     maxblk          ;divide max block by
        sta     gpcnt           ;eight to get max
        lda     maxblk+1
;
        lsr     a               ;use a for speed
        ror     gpcnt
        lsr     a               ;use a for speed
        ror     gpcnt
        lsr     a               ;use a for speed
        ror     gpcnt
;
        sta     gpcnt+1         ;save high
        inc     gpcnt           ;bump by one
        bne     skpam
        inc     gpcnt+1         ;with carry
skpam   lda     alcmap          ;get map start
        ldy     alcmap+1
        sta     alcpnt          ;set pointer to start
        sty     alcpnt+1
clraml  lda     #0              ;clear byte
        sta     (alcpnt)        ;put in map
        inc     alcpnt          ;bump pointer
        bne     skpal
        inc     alcpnt+1        ;with carry
skpal   lda     gpcnt           ;get low of count
        bne     skpgp           ;skip if not zero
        dec     gpcnt+1         ;else drop high
skpgp   dec     gpcnt           ;always drop low
        lda     gpcnt           ;test for zero
        ora     gpcnt+1
        bne     clraml          ;loop if more
;at this point complete map is cleared
        lda     maxdrc          ;get low of max dir record
        sta     gpcnt
        lda     maxdrc+1        ;high in a
        ldx     sxb             ;set x according to block size
clcmdb  lsr     a               ;do division
        ror     gpcnt
        dex
        bne     clcmdb          ;until x is zero
        sta     gpcnt+1         ;set high
        inc     gpcnt           ;then bump by one
        bne     skpgp2
        inc     gpcnt+1
skpgp2  stx     blknum          ;clear block number
        stx     blknum+1
fildal  lda     blknum          ;get block number
        ldy     blknum+1
        jsr     setblk          ;set bit
        inc     blknum          ;bump block number
        bne     skpblk
        inc     blknum+1
skpblk  lda     gpcnt           ;get low of count
        bne     skpgp3          ;skip if not zero
        dec     gpcnt+1         ;else drop high
skpgp3  dec     gpcnt           ;always do low
        lda     gpcnt           ;test for zero
        ora     gpcnt+1
        bne     fildal          ;loop if more
;at this point directory space is mapped
        jsr     cldrnm          ;clear directory number
fillpe  ldx     #1              ;parameter for fill
        jsr     nxtdir          ;execute for next directory
        bmi     extfil          ;done if invalid
        ldy     subrec          ;get offset
        lda     (bufadd),y      ;get empty/valid flag
        cmp     #$E5            ;if empty
        beq     fillpe          ;try next
        ldx     #1              ;parameter for fill in
        jsr     mapdir          ;do directory map
        bra     fillpe          ;then loop
extfil  rts                     ;return
;
;directory record set up
; input:dirnum
; returns:none
; alters:all,recnum,dirrec
drrcsu  lda     dirnum+1        ;move high dir number
        sta     recnum+1        ;to record number
        lda     dirnum          ;divide
        lsr     recnum+1        ;directory by four
        ror     a
        lsr     recnum+1
        ror     a
        sta     dirrec          ;and save
        sta     recnum          ;set low record number
        lda     recnum+1        ;get high
        sta     dirrec+1        ;and set
        stz     recnum+2
;
;new routine here for using Records
; note that the SIM call has changed to xfer the requested Record
;
; input:recnum, resrec
; returns:none
; alters:all
;
; first check to see if there are reserved records for the drive
; if yes, these need to be added into the calculated record number
; and then sent to SIM.
;
setrec
        lda     resrec          ;get the reserved record lo count
        ora     resrec+1        ;or in hi count
        bne     addres          ;if non-zero, add in reserved records
;
; no reserved records are in the current DCB
; just send the current record number to SIM
;
        lda     recnum          ;get record number lo
        ldy     recnum+1        ;get record number hi
        jmp     simram+33       ;call sim to set requested record, return
;
addres  clc                     ;clear carry for add
        lda     recnum          ;get lo record number
        adc     resrec          ;add reserved record lo number
        sta     simrec          ;save it to sim record
        lda     recnum+1        ;get hi record number
        adc     resrec+1        ;add reserved record hi number
        sta     simrec+1        ;save it to sim record
;
        tay                     ;xfer record hi to Y reg
        lda     simrec          ;get sim record lo
        jmp     simram+33       ;call sim to set requested record, return
;
;get block bit mask and index
; input:a&y=block number
; returns:a=bit mask
; alters:all and alcpnt
blkmsk  pha                     ;save block number
        sty     alcpnt+1        ;including high
;
        lsr     alcpnt+1        ;shift high
        ror     a
        lsr     alcpnt+1
        ror     a
        lsr     alcpnt+1
        ror     a
;
        clc                     ;now add map start
        adc     alcmap
        sta     alcpnt
        lda     alcpnt+1
        adc     alcmap+1
        sta     alcpnt+1
        pla                     ;get number back
        and     #%00000111      ;look at 3 lsbs
        tax                     ;get
        lda     bitmsk,x        ;mask
        rts                     ;and return
;
;test block
; input:a&y=block number
; returns:z=1 if unassigned
;          =0 if assigned and bit in a is set
; alters:all
tstblk  jsr     blkmsk          ;get mask and index
        and     (alcpnt)        ;mask with map entry
        rts                     ;then return
;
;alter block status
; input:a&y=block number,x=1 if set
;                         =0 if reset
; returns:none
; alters:all and allocation map
altalc  cpx     #1              ;if set
        beq     setblk          ;go do it
clrblk  jsr     blkmsk          ;else get mask
        eor     #$FF            ;and complement
        and     (alcpnt)        ;preserve others
        sta     (alcpnt)        ;and save
        rts                     ;then return
;
setblk  jsr     blkmsk          ;get mask
        ora     (alcpnt)        ;set bit
        sta     (alcpnt)        ;and put back
        rts                     ;then return
;
;set current drive to read only
; input:curdrv,ronlst,bitmap
; returns:none
; alters:a,x,p and ronlst
setron  ldx     curdrv          ;get number
        lda     bitmap,x        ;and get mask
        tsb     ronlst          ;set drive bit
        rts                     ;then return
;
;check read/write status
; input:curdrv,ronlst
; returns:z=0 if r/o or z=1 if r/w
; alters:a,x,p
chkron  ldx     curdrv          ;get current drive
        lda     bitmap,x        ;get mask
        and     ronlst          ;and test status
        rts
;
;initialize record counters from fcb
; input:fcb@(addinp)
; returns:none
; alters:a,y,p,nxtrec,numrec
inrcct  ldy     #32             ;next record offset
        lda     (addinp),y      ;get next record
        sta     nxtrec          ;and save
        ldy     #15             ;number records offset
        lda     (addinp),y      ;get number
        sta     numrec          ;and save
        rts                     ;and return
;
;update record counters in fcb
; input:nxtrec,numrec
; returns:none
; alters:all,fcb@(addinp)
updtrc  ldx     nxtrec          ;get next record
        inx                     ;bump it
        txa                     ;transfer
        ldy     #32             ;set offset
        sta     (addinp),y      ;and store in fcb
        lda     numrec          ;get number
        ldy     #15             ;and its offset
        sta     (addinp),y      ;and store
        rts                     ;then return
;
;execute open file
xopen   jsr     autodr          ;auto drive select
;
;open file
; input:fcb @ (addinp)
; returns:n=1 if not found,a=dirmod (255 if not found)
; alters:all
opnfle  jsr     fndf13          ;match all including extent
        bmi     extopn          ;done if not found
        lda     #12             ;point to first char
        ora     subrec          ;add offset
        tay                     ;make it a pointer
nxopmv  lda     (bufadd),y      ;get buffer contents
        tax                     ;and save
        tya                     ;save index
        and     #%00011111      ;remove offset
        tay                     ;back to index
        txa                     ;get byte back
        sta     (addinp),y      ;and store in fcb
        tya                     ;get index
        ora     subrec          ;add offset back
        tay                     ;and make index again
        iny                     ;next position
        tya                     ;if index
        and     #%00011111      ;not gone past
        bne     nxopmv          ;end then loop
;now correct extent and max records
        ldy     #12             ;point at extent in fcb
        lda     savext          ;get save from search
        cmp     (addinp),y      ;compare
        beq     extsme          ;jump ahead if same
        sta     (addinp),y      ;else change extent
        lda     #128            ;assume fcb ext < dir ext
        bcc     fcbxls          ;jump ahead if true
        asl     a               ;clear a
fcbxls  ldy     #15             ;point to max
        sta     (addinp),y      ;and set
extsme  lda     dirmod          ;else get number
extopn  rts                     ;and return
;
;execute close file
xclose  jsr     autodr          ;auto drive select
;
;close file
;if file is r/o then no actual close
;operation is performed.
; input:fcb @ (addinp)
; returns:n=1 if not valid,a=dirmod (255 if invalid)
; alters:all
clsfle  jsr     fndf13          ;match including extent
        bmi     extcls          ;exit if not found
        jsr     chkron          ;see if r/o
        bne     noclse          ;branch if is
;now set flag to ensure directory extent and number of
;records fields are only changed if necessary.
;The decision to change is determined by whether or
;not the system is closing an extent less than the
;maximum extent in the directory entry. If that is
;the case, the directory extent and number of record
;fields are not changed.
        ldy     #12             ;get extent
        lda     (addinp),y
        pha                     ;save it
        tya                     ;now look in directory
        ora     subrec
        tay
        pla                     ;get extent back
        cmp     (bufadd),y
        ror     skpdir          ;save result
;now go do it
        jsr     updtdr          ;update directory
noclse  lda     dirmod          ;get directory number
extcls  rts                     ;and return
;
;execute find first match
xfndfr  jsr     autodr          ;auto drive select
fndf13  lda     #13             ;match including extent
;search for first match
; input:a=number char to match
; returns:n=1 if invalid,a=dirmod (255 if invalid)
; alters:all
frstsr  pha                     ;save number to match
        jsr     cldrnm          ;clear directory number to -1
        pla                     ;get number to match
;search for directory match
; input:a=number char to match
; returns:n=1 if not found,a=dirmod (255 if invalid)
; alters:all
search  sta     chrcnt          ;save number
newtry  ldx     #0              ;set for search
        stz     cmppnt          ;clear pointer
        jsr     nxtdir          ;get next entry
        bmi     exitsr          ;exit if not found
        ldx     chrcnt          ;get count
mremch  ldy     cmppnt          ;get pointer
        inc     cmppnt          ;and bump
        lda     (addinp),y      ;get fcb entry
        cpy     #12             ;see if at extent
        bne     notaex          ;jump if not
        sta     savext          ;save for later use
        pha                     ;save extent
        tya                     ;change to directory
        ora     subrec          ;coordinates
        tay
        pla                     ;get extent back
        eor     (bufadd),y      ;exclusive or with dir
        and     cexm1f          ;and with exm complement + 1f
        beq     trynxt          ;ok if zero
        bra     newtry          ;else get next directory
notaex  cmp     #'?'            ;if a ? then
        beq     trynxt          ;is a match
        pha                     ;save char
        tya                     ;then add
        ora     subrec          ;offset to
        tay                     ;make pointer
        pla                     ;get char back
        cmp     (bufadd),y      ;if not same
        bne     newtry          ;try next directory
trynxt  dex                     ;else count number down
        bne     mremch          ;and loop if more
        lda     dirmod          ;return with directory
exitsr  rts                     ;number mod 4
;
;calculate logical record number
; input:blknum
; returns:none
; alters:a,x,p,recnum
calrcn  ldx     sxb             ;set x according to blkscd
mulmre  asl     blknum          ;multiply block
        rol     blknum+1        ;number
        rol     blknum+2
        dex                     ;by code
        bne     mulmre
        lda     sab             ;set mask in a
        and     nxtrec          ;and with next record
        ora     recnum          ;or with number
        sta     recnum          ;and save
        rts
;
;update directory
updtdr  lda     #32             ;change all
        ldx     #0              ;from start
;
;change directory entry
; input:a=number char to change,x=starting position,fcb@(addinp)
; returns:none
; alters:all,directory,checksums
dirchg  sta     chrcnt          ;save count
        dec     chrcnt          ;back up
mrechg  clc                     ;clear carry
        txa                     ;get offset
        adc     chrcnt          ;compute index
        tay                     ;and set
        cpy     #12             ;see if at extent
        beq     docare          ;if so do special
        cpy     #15             ;see if at number rec
        bne     dntcar          ;if not skip
docare  bit     skpdir          ;check flag
        bpl     nochng          ;skip if ok
dntcar  lda     (addinp),y      ;get char
        pha                     ;save it
        lda     chrcnt          ;get count
        ora     subrec          ;add offset
        tay                     ;make an index
        pla                     ;get char back
        sta     (bufadd),y      ;and put in buffer
nochng  dec     chrcnt          ;count down
        bpl     mrechg          ;and loop
        jsr     drrcsu          ;set it up
        jmp     wrtrec          ;write record
;
;execute create file
xmake   jsr     autodr          ;auto drive select
;
;create file
; input:fcb@(addinp)
; returns:n=1 if not valid,a=dirmod (255 if not valid)
; alters:dirnum,dirmod,fcb@(addinp)
mkefle  jsr     tstron          ;test for r/o
        lda     addinp          ;save fcb
        pha                     ;address
        lda     addinp+1        ;on
        pha                     ;stack
        lda     #<empty         ;then point
        ldy     #>empty         ;to empty
        sta     addinp          ;dummy
        sty     addinp+1        ;fcb
        lda     #1              ;match only
        jsr     frstsr          ;first char
        pla                     ;restore
        sta     addinp+1        ;fcb
        pla                     ;address
        sta     addinp          ;from stack
        lda     dirmod          ;get number mod 4
        bmi     extmke          ;quit if not found
        ldy     #13             ;else set up
        lda     #0              ;to clear
mkeagn  sta     (addinp),y      ;rest of
        iny                     ;fcb
        cpy     #33             ;including next
        bne     mkeagn          ;record
;the next two lines ensure that the extent and
;number of records fields are updated
        sec
        ror     skpdir
        jsr     updtdr          ;update directory
        lda     dirmod          ;get number
extmke  rts                     ;and quit
;
;extend file
; input:fcb@(addinp), x=1 read
;                       0 write
; d65flg bit 6 used instead of X reg.
; calling routine sets flag bit before calling here.
extend  jsr     clsfle          ;close current extent
        bmi     extext          ;exit if not found
        ldy     #12             ;else
        lda     (addinp),y      ;get extent
        inc     a               ;add one ;-)
        and     #%00011111      ;see if overflow
        beq     exteof          ;eof if so
        sta     (addinp),y      ;and save
        jsr     fndf13          ;see if next extent exists
        bpl     opnext          ;if so open
        bbs6    d65flg,extext   ;test extend flag, branch if write
dwrtex  jsr     mkefle          ;create file
        bra     tstext          ;and test
opnext  jsr     opnfle          ;open
tstext  bpl     extnok          ;continue if ok
exteof  lda     #1              ;else set eof
        rts                     ;and return
extnok  jsr     inrcct          ;initialize counters
        lda     #0              ;good
extext  rts                     ;return
;
;set up next directory block
; input:dirnum,x=1 for update checksum
; returns:a=dirmod (255 if invalid),n=1 if invalid
; alters:alll,dirnum,dirmod
nxtdir  inc     dirnum          ;bump directory
        bne     skpdr
        inc     dirnum+1
skpdr   lda     maxdir          ;if not at limit continue
        cmp     dirnum
        lda     maxdir+1
        sbc     dirnum+1
        bcs     gtnxdr          ;then continue
;
;clear directory number to $FFFF
; input:none
; returns:a=$ff,n=1,z=0
; alters:a,p,dirnum,dirmod
cldrnm  lda     #$FF            ;set to $FF
        sta     dirnum
        sta     dirnum+1
        sta     dirmod
        rts
;
gtnxdr  lda     dirnum          ;get low again
        and     #%00000011      ;look at 2 lsbs
        sta     dirmod          ;save mod 4
        asl     a               ;multiply
        asl     a               ;by
        asl     a               ;32 to
        asl     a               ;get pointer
        asl     a               ;offset
        sta     subrec          ;and save
        beq     getdir          ;if zero read new
        bra     gotdir          ;and exit
getdir  jsr     drrcsu          ;set up to read
        jsr     rderec          ;do read
gotdir  lda     dirmod          ;and return
exnxdr  rts                     ;with number
;
;get block number
; input:nxtrec,blmode,fcb@(addinp),sxb,exm
; returns:none
; alters:all,blknum
getblk  ldx     sxb             ;set x according to blkscd
        lda     nxtrec          ;get next record
gblp    lsr     a               ;divide by 2 x times
        dex
        bne     gblp
        sta     blknum          ;save previous as temp
;use extent as offset but first use sxb to create param
        sec
        lda     #8
        sbc     sxb
        tax
;now get extent from fcb
        ldy     #12
        lda     (addinp),y
        and     exm             ;and with mask
        lsr     a               ;shift with lsb to c
gbxlp   rol     a               ;now go other way
        dex
        bne     gbxlp
        clc                     ;now add saved value
        adc     blknum          ;back
        bbr7    d65flg,blm2     ;test mode, branch if byte
        asl     a               ;else times two
blm2    clc                     ;then add
        adc     #16             ;offset into fcb
        sta     fcbind          ;save for later
        tay                     ;set index
        lda     (addinp),y      ;get number
        sta     blknum          ;store
        iny
        lda     #0              ;clear
        bbr7    d65flg,blm3     ;test mode, branch if byte
        lda     (addinp),y      ;else get high
blm3    sta     blknum+1        ;high byte
        rts                     ;and return
;
;change allocation map
; input:subrec,directory record @ (bufadd)
; returns:none
; alters:all,allocation map
;
; Note: the "gthibn" routine has been integrated
; into this routine. While it does take a few
; extra bytes of code space, it eliminates a pair
; of JSR/RTS sequences, improving performance.
mapdir  clc                     ;add 16 to
        lda     subrec          ;to subrec to point
        adc     #16             ;to block number field
        tay                     ;make index
lpmpdr  sty     mpdrsy          ;save index
        phx                     ;save x operation
        lda     (bufadd),y      ;get block number
        sta     mpdrtm          ;save in temp
        iny                     ;bump index for word
;
;get high part of block number if word (zero if byte)
; input:y=index to high,blmode,directory@(bufadd)+subrec
; returns:a=high part of block number
; alters:a,p,mpdrsy if word
;gthibn
        lda     #0              ;preset for byte
        bbr7    d65flg,mpdrnw   ;test mode, branch if byte
        lda     (bufadd),y      ;get high
        sty     mpdrsy          ;alter y
;
mpdrnw  ora     mpdrtm          ;see if zero
        beq     skpedr          ;skip if zero
;
;get high part of block number if word (zero if byte)
; input:y=index to high,blmode,directory@(bufadd)+subrec
; returns:a=high part of block number
; alters:a,p,mpdrsy if word
;gthibn
        lda     #0              ;preset for byte
        bbr7    d65flg,gthiex   ;test mode, branch if byte
        lda     (bufadd),y      ;get high
        sty     mpdrsy          ;alter y
;
gthiex  tay                     ;move to y
        lda     mpdrtm          ;get low again
        jsr     altalc          ;else alter map
skpedr  plx                     ;get operation back
        ldy     mpdrsy          ;get index back
        iny                     ;bump it
        tya                     ;if still
        and     #%00001111      ;in field
        bne     lpmpdr          ;then loop
        rts                     ;else quit
;
;test r/w status
;does warm boot if r/o
; input:curdrv,ronlst
; returns:none
; alters:a,x,p
tstron  jsr     chkron          ;test bit
        beq     exttro          ;exit if r/w
        jsr     errout          ;else send error
        lda     #<romsg         ;point to r/o
        ldy     #>romsg         ;message
        jsr     sndstr          ;send it
wrerr   jmp     xwboot          ;then abort
;
;set current drive to r/w
; input:curdrv,ronlst
; returns:ronlst
; alters:a,x,p,ronlst
setrw   ldx     curdrv          ;get drive
        lda     bitmap,x        ;and mask
        trb     ronlst          ;reset drive bit
exttro  rts                     ;return
;
;read record
rderec  jsr     simram+39       ;do read
        beq     exttro          ;done if A= zero
        bra     rwerrt          ;else handle error
;
;write record
wrtrec  jsr     simram+42       ;do write
chkrwe  beq     exttro          ;done if A= zero
;
;read/write error
rwerrt  jsr     errout          ;send error message
        lda     #<bdsmsg        ;point to
        ldy     #>bdsmsg        ;bad record message
        jsr     sndstr          ;and send
        jsr     getcon          ;get input
        cmp     #cr             ;if a cr
        bne     wrerr           ;if not cr, exit
ignerr  jmp     pcrlf           ;crlf and return
;
;error output routine
; input:curdrv,pemmvc
; returns:none
; alters:all
errout  lda     #<pemmsg        ;point to
        ldy     #>pemmsg        ;error message
        jsr     sndstr          ;send it
        lda     curdrv          ;get drive number
        clc                     ;add
        adc     #'A'            ;ascii a
        jmp     sndchr          ;and send it
;
;get console input
; input:pndkey
; returns:a=character
; alters:all,pndkey
getcon  lda     pndkey          ;get pending key
        stz     pndkey          ;clear pending key
        bne     extkbd          ;exit if not null
        jmp     simram+9        ;else get new key, return
;
;check keyboard status
;handles <ctl-s> for freeze and <ctl-c> for boot
; input:pndkey
; returns:a=0 if no input or <>0 if input
; alters:all,pndkey
kbdsts  lda     pndkey          ;get pending key
        bne     extkbd          ;if there is one, exit
        jsr     simram+6        ;else check console status
        beq     extkbd          ;if no character in console, exit
        jsr     simram+9        ;else get input from console
        cmp     #ctls           ;check for freeze
        bne     newpnd          ;if not, save input
        jsr     simram+9        ;else wait for next input
        cmp     #ctlc           ;check for cancel
        bne     nowarm          ;if not, then finish
        jmp     xwboot          ;else do a warm boot
nowarm  lda     #0              ;clear
        rts                     ;and return
newpnd  sta     pndkey          ;save input
        lda     #$FF            ;set ready
extkbd  rts                     ;and return
;
;test character
; input:a=character
; returns:c=0 if control or c=1 if printing
; alters:p
tstchr  cmp     #cr             ;if cr
        beq     chtext          ;quit
        cmp     #lf             ;if linefeed
        beq     chtext          ;quit
        cmp     #ctli           ;if tab
        beq     chtext          ;quit
        cmp     #' '            ;see if control
chtext  rts                     ;and return
;
;send string ending in $
; input:ay=string address
; returns:none
; alters:all,index,sndlpe+1 and +2
sndstr  sta     sndlpe+1        ;set pointer
        sty     sndlpe+2
        ldy     #0
sndlpe  lda     $FFFF,y         ;get char
        cmp     #'$'            ;if terminator
        beq     sndext          ;then exit
        iny                     ;else bump
        phy                     ;save index
        jsr     sndchr          ;send char
        ply                     ;restore index
        bne     sndlpe          ;and loop
sndext  rts                     ;return
;
;send char to printer if enabled
; input:a=character,lstflg
; returns:a=character
; alters:x,y,p
; ensure A reg is preserved by SIM routine!
lstout  bit     lstflg          ;test flag
        bpl     sndext          ;exit if off
        bbs4    d65flg,sndext   ;test out flag, branch if set
        jmp     simram+15       ;send (dummy routine), return
;
;output a character
; input:a=character,console definition block in sim
; returns:none
; alters:all,positn
output  jsr     tstchr          ;test it
        bcs     sndchr          ;if not control jump
        pha                     ;else save
        lda     sysdef+4        ;get invert
        jsr     nolist          ;send to console
        lda     #'^'            ;get arrow
        jsr     lstout          ;send to printer
        pla                     ;get character
        ora     #'A'-1          ;convert to ascii
        jsr     pchrot          ;send to all
        lda     sysdef+3        ;get normal
        bra     nolist          ;to console
sndchr  cmp     #ctli           ;if not tab
        bne     pchrot          ;send
tabspc  lda     #' '            ;else get space
        jsr     pchrot          ;send
        lda     positn          ;get count
        and     #7              ;if not mod 8
        bne     tabspc          ;loop
        rts                     ;else exit
pchrot  pha                     ;save char
        jsr     kbdsts          ;test input
        pla                     ;restore
        jsr     lstout          ;to printer if on
nolist  pha                     ;save again
        bbs4    d65flg,no_oflg  ;test out flag, branch if set
        jsr     simram+12       ;send to console
no_oflg pla                     ;restore
        inc     positn          ;bump col
        cmp     #' '            ;if space or more
        bcs     extchr          ;is ok
        cmp     sysdef+2        ;also ok
        beq     extchr          ;if forward
        dec     positn          ;else drop back
        cmp     sysdef+0        ;see if bs
        bne     tryotr          ;branch if not
        dec     positn          ;else drop again
        bmi     zrocol          ;zero if <0
        rts                     ;else ok, return
tryotr  cmp     #cr             ;if a cr
        beq     zrocol          ;clear col
        cmp     sysdef+7        ;if a formfeed
        beq     zrocol          ;also clear
        cmp     sysdef+8        ;if not home
        bne     extchr          ;then done
zrocol  stz     positn          ;column
extchr  rts                     ;and exit
;
;go to left and space past prompt
; input:frscol,positn
; returns:none
; alters:positn
spcovr  lda     #cr             ;get cr
        jsr     pchrot          ;send to all
        lda     #lf             ;send lf
        jsr     lstout          ;only to printer
mreovr  lda     frscol          ;get first
        cmp     positn          ;see if there
        beq     extchr          ;done if is
        lda     sysdef+2        ;get forward
        jsr     nolist          ;send it
        bra     mreovr          ;and loop
;
;buffered read
; input:buffer@(addinp)
; returns:none
; alters:all,buffer@(addinp)
bufinp  lda     #0              ;clear
        ldy     #1              ;length
        sta     (addinp),y      ;position in buffer
        sty     bufpsn          ;set point to 1
        lda     positn          ;get current
        sta     frscol          ;and save
nxtinp  jsr     getcon          ;get input
        ldy     bufpsn          ;get index
        cmp     #cr             ;if not a cr
        bne     notcr           ;then jump
        jmp     endlin          ;else done
notcr   cmp     #delete         ;if not delete
        bne     ntdelt          ;then jump
        cpy     #1              ;else if start
        beq     nxtinp          ;then loop
        lda     (addinp),y      ;get last
        pha                     ;save char
        ldy     #1              ;point to count
        sec                     ;set carry
        lda     (addinp),y      ;get count
        sbc     #1              ;decrement
        sta     (addinp),y      ;then save
        pla                     ;restore char
        dec     bufpsn          ;backup pointer
        cmp     #' '            ;if space or more
        bcs     nrmbs           ;just backspace
        cmp     #ctli           ;see if tab
        bne     ctlbs           ;if not is control
        smb4    d65flg          ;set out flag bit
        lda     positn          ;get position and save
        sta     lstcol
        jsr     spcovr          ;else go back
        jsr     rptlne          ;and retype
        lda     positn          ;get new last position
        pha                     ;save on stack
        sec                     ;subtract to get delta
        lda     lstcol
        sbc     positn
        sta     lstcol          ;and save
        rmb4    d65flg          ;clear out flag bit
bstab   jsr     dobs            ;do one
        dec     lstcol          ;drop count
        bne     bstab           ;loop if more
        pla                     ;get position
        sta     positn          ;and set
        bra     nxtinp          ;then loop
ctlbs   lda     sysdef+3        ;get normal
        jsr     chkbs           ;bs if printing
        lda     sysdef+4        ;same for invert
        jsr     chkbs           ;then delete char itself
nrmbs   jsr     dobs            ;do a backspace
        bra     nxtinp          ;and loop
ntdelt  cmp     #ctlp           ;if not ctl-p
        bne     ntctlp          ;then jump
        lda     lstflg          ;else get printer flag
        eor     #$FF            ;complement
        sta     lstflg          ;save
        bra     nxtinp          ;and loop
ntctlp  cmp     #ctlx           ;if not ctl-x
        bne     ntctlx          ;then jump
        jsr     spcovr          ;restart
        lda     sysdef+1        ;get clear to eol
        jsr     nolist          ;send it
        jmp     bufinp          ;and start over
ntctlx  cmp     #ctlr           ;if not ctl-r
        bne     ntctlr          ;then jump
        jsr     spcovr          ;restart
        jsr     rptlne          ;retype line
        jmp     nxtinp          ;and start over
ntctlr  iny                     ;next position
        sta     (addinp),y      ;store char
        pha                     ;and save
        sty     bufpsn          ;index
        ldy     #1              ;point to count
        tya                     ;set a to 1
        clc                     ;then
        adc     (addinp),y      ;add count
        sta     (addinp),y      ;and save
        pla                     ;restore char
dontsv  jsr     output          ;send char
        ldy     bufpsn          ;get index
        lda     (addinp),y      ;get char
        cmp     #ctlc           ;if not ctl-c
        bne     ignrcc          ;ignore
        ldy     #1              ;get count
        lda     (addinp),y      ;from buffer
        cmp     #1              ;if not at start
        bne     ignrcc          ;ignore
        jmp     xwboot          ;else do warm boot
ignrcc  ldy     #1              ;get
        lda     (addinp),y      ;count
        dey                     ;point to max
        cmp     (addinp),y      ;if length
        bcs     lineen          ;at max jump
        jmp     nxtinp          ;else loop
;
;lf and cr
pcrlf   lda     #lf             ;then a
        jsr     pchrot          ;lf
lineen  lda     #cr             ;and a
endlin  jmp     pchrot          ;cr
;
;retype line
rptlne  lda     bufpsn          ;get pointer
        sta     numcnt          ;save as count
        lda     #1              ;set start position
        pha                     ;save to stack
mrerpt  pla                     ;get position (from stack)
        dec     numcnt          ;count down
        bne     retym           ;continue if more
        rts                     ;else done
retym   tay                     ;else make index
        iny                     ;and bump
        phy                     ;save on stack
        lda     (addinp),y      ;get char
        jsr     output          ;send
        bra     mrerpt          ;and loop
;check for printing and backspace if needed
chkbs   cmp     #' '            ;compare to space
        bcc     extdec          ;not printing so done
;do a backspace
dobs    lda     sysdef+0        ;get backspace
        pha                     ;save it
        jsr     nolist          ;send
        lda     #' '            ;get space
        jsr     nolist          ;send
        pla                     ;get backspace
        jmp     nolist          ;send it
;
;test for decimal digit
;if decimal then c=0 else c=1
tstdec  cmp     #'0'            ;if under 0
        bcc     notdec          ;then not decimal
        cmp     #'9'+1          ;if 9 or under is ok
        bcc     extdec
notdec  sec                     ;else not a match
extdec  rts
;
;test for hexadecimal digit
;if hex then c=0 else c=1
tsthex
        cmp     #'0'            ;if under 0
        bcc     notdec          ;then not hexidecimal
        cmp     #'A'            ;if undr A
        bcc     notdec          ;then not hex
        cmp     #'F'+1          ;if F or under is ok
        rts                     ;return
;
;bump load address by 128 and return in ay
adjdb   lda     dskbuf          ;get old
        ldy     dskbuf+1        ;address
        clc                     ;and bump
        adc     #128            ;by 128
        sta     dskbuf          ;save low
        bcc     adjend          ;then bump
        iny                     ;and save
        sty     dskbuf+1        ;high as needed
adjend  rts
;
;move record from disk buffer to default buffer
mv128   lda     dskbuf          ;get address
        ldy     dskbuf+1
        sta     mvfrom+1        ;and set pointer
        sty     mvfrom+2
        ldx     #0              ;clear index
mvfrom  lda     $FFFF,x         ;get byte
        sta     dflbuf,x        ;move it
        inx
        bpl     mvfrom          ;loop until done
        rts
;
;messages
romsg   .DB     ":, READ-ONLY$"
bdsmsg  .DB     ":, BAD BLOCK, <Ret> IGNORE, <Other> ABORT$"
pemmsg  .DB     cr,lf,"PEM ERROR on $"
sltmsg  .DB      ":, INVALID DRIVE$"
rngmsg  .DB     cr,lf,"UNKNOWN DRIVE$"
;
;dummy fcb
empty   .DB     $E5
;
;bit mask table
bitmsk  .DB     128,64,32,16,8,4,2,1
;
;bit map table
bitmap  .DB    1,2,4,8,16,32,64,128
;
;extent mask table (also uses 3 bytes in sabtbl)
exmtbl  .DB     0,1,3
;
;sab table
sabtbl  .DB     7,15,31,63,127
;
;variable storage
drvcmd  .DB     0               ;new commanded drive
usrcde  .DB     0               ;user code
skpdir  .DB     0               ;positive if no change
fcbind  .DB     0               ;index to block number
savext  .DB     0               ;save extent
frscol  .DB     0               ;first col
pndkey  .DB     0               ;pending input
lstflg  .DB     0               ;printer flag
positn  .DB     0               ;print position
addout  .DW     0               ;output address
bytout  .EQU    addout          ;output value
bufpsn  .DB     0               ;input buffer position
tmpdrv  .DB     0               ;temporary drive flag & number
simrec  .DW     0               ;record number sent to SIM
lkdown  .DW     0               ;down pnt. for block search
lookup  .DW     0               ;up pnt. for block search
olddrv  .DB     0               ;old drive number
curdrv  .DB     0               ;current drive
lginvc  .DB     0               ;log in status
ronlst  .DB     0               ;read write status
dirnum  .DW     0               ;directory number
subrec  .DB     0               ;directory offset
recnum  .DW     0               ;record number
blknum  .EQU    recnum          ;block number
        .DB     0               ;overflow
chrcnt  .DB     0               ;character count
cmppnt  .DB     0               ;comparison pointer
nxtrec  .DB     0               ;next record
numrec  .DB     0               ;number records
dirrec  .DW     0               ;directory record
dirmod  .DB     0               ;directory mod 4
numcnt  .DB     0               ;counter
lstcol  .DB     0               ;last column
mpdrsy  .DB     0               ;save for y in mapdir
mpdrtm  .DB     0               ;temp in mapdir
maxdrc  .DW     0               ;max directory record
sab     .DB     0               ;mask for block
sxb     .DB     0               ;shift for block
rtclk   .DB     0,0,0           ;real time clock
gpcnt   .DW     0               ;gp counter
;
;following region is used to capture DCB
; Note: The Disk Control Block has been changed!
; As diskette and other track/sector devices are no longer
; used/supported, there's no need to use checksums. This
; saves 3 bytes per DCB and execution time.
;
dcb
maxblk  .DW     0               ;maximum block number
recblk  .DW     0               ;records per block
resrec  .DW     0               ;reserved records
blkscd  .DB     0               ;block size code
maxdir  .DW     0               ;maximum directory number
alcmap  .DW     0               ;address of allocation map
;
exm     .DB     0               ;extent mask
cexm1f  .DB     0               ;exm complemented and 1f
;
;zero page save block
; Note: Per above, no checksum support is required, so we
; save two bytes of Page Zero space from the byte swap
; routine.
;
varblk
lowin   .DW     0               ;save addinp
        .DW     0               ;save bufadd
        .DW     0               ;save alcpnt
;
        end
