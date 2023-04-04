;
;-------------------------------
;DOS/65 Console Command Module
;-------------------------------
;CCM unique definitions
nbuilt  equ     8               ;number built in commands
dirpln  equ     4               ;number dir listings per line
;
ccmram
;
;entry point
        bra     CCMStr          ;branch to start
;
;$$$.sub related subroutines
;prepare drive A and FCB pointer for $$$.sub operation
PrpSUB  lda     dfldsk          ;get default drive
        sta     dsksav          ;save it
        stz     dfldsk          ;set default to A
        jsr     slctds          ;and select A
;set up submit FCB pointer
setsub  lda     #<subfcb        ;low byte address
        ldy     #>subfcb        ;high byte address
        rts                     ;return to caller
;recover disk assignments after $$$.sub ops
RcvDsk  lda     dsksav          ;get saved drive,
        sta     dfldsk          ;make it default,
        jmp     slctds          ;and select it
;delete $$$.SUB file and clear flag
delsub  jsr     setsub          ;set up .SUB FCB address
        jsr     dltfil          ;and delete the $$$.SUB file
        jsr     rcvdsk          ;restore drives
        rmb0    d65flg          ;clear submit flag
        rts                     ;return to caller
;
;main program
CCMStr  cld                     ;ensure binary mode
        lda     iostat          ;else get drive and user numbers
        lsr     a               ;move user to low nibble
        lsr     a
        lsr     a
        lsr     a
        jsr     setusr          ;tell PEM user number
        lda     iostat          ;get data again
        and     #%00000111      ;mask for drive only
        sta     dfldsk          ;save it
        jsr     slctds          ;select drive
        lda     #<DflBuf        ;set default buffer
        ldy     #>DflBuf        ;address
        jsr     setbuf          ;and set it,
        jsr     rstdsk          ;then reset the disk system
;now see if $$$.sub file on A
        rmb0    d65flg          ;clear submit flag
        jsr     prpsub          ;get ready for $$$.sub
        jsr     srchfr          ;search for file
;if $$$.sub file present set subflg to use file
        bmi     nosub           ;flag ok if a=255
        smb0    d65flg          ;set submit flag
        bra     ccmlpe          ;and go to main loop
nosub   jsr     rcvdsk          ;return to original default
ccmlpe  ldx     #$ff            ;set
        txs                     ;stack
        lda     #<DflBuf        ;get default buffer
        ldy     #>DflBuf        ;address
        jsr     setbuf          ;and set it
        jsr     hdrout          ;send header
        lda     #'>'            ;then print
        jsr     chrout          ;prompt
;at this point we check flag for use of $$$.SUB file
        bbs0    d65flg,L01      ;test submit flag, branch if set
        bra     usecns          ;else clear use console
;we must use $$$.SUB file
L01     jsr     setsub          ;get FCB pointer
        jsr     openfl          ;try to open
        bpl     L02             ;use if OK
        jsr     delsub          ;get rid of file
        bra     usecns          ;use console if no open
L02     ldx     subrc           ;get record count
        dex
        stx     subcr           ;and read last record
        jsr     setsub          ;get ready for sub
        jsr     rdrcrd          ;read record
        beq     L03             ;if OK get data
        jsr     delsub          ;else clean up from EOF
        bra     usecns          ;and use console
;read was OK so move record to command buffer
L03     ldx     #0
movsub  lda     dflbuf,x        ;get char
        sta     cnslng,x        ;move it
        inx                     ;bump index
        bpl     movsub          ;loop until index is 128
;now delete record
        dec     subrc           ;drop number records by one
        jsr     setsub          ;get ready for sub
        jsr     clsefl
        bpl     typbuf          ;if OK go type line
        jsr     delsub          ;then delete $$$.SUB
        bra     usecns          ;switch to console if bad
;send command line to console
typbuf  ldx     #0              ;start one past length
print0  lda     cnstxt,x        ;get char
        beq     gotend          ;done if zero
        jsr     cotsxy          ;save x and send to console
        inx
        bpl     print0          ;do next
gotend  jsr     chkcst          ;check for key pressed
        beq     nobrak          ;no break if no key
        jsr     cnsrdx          ;get key without echo
        jsr     delsub          ;stop $$$.SUB use
        jmp     CCMStr          ;and go back to start
;next line is the entry that stops $$$.SUB processing
usecns  lda     #<cnsbuf        ;point to start
        ldy     #>cnsbuf        ;of buffer
        jsr     rdebuf          ;do buffer read
;next line is the entry point from $$$.SUB read
nobrak  ldy     #0              ;clear index
        ldx     cnslng          ;get length
        bne     lwrupr          ;continue
        bra     ccmlp2          ;loop if empty
lwrupr  lda     cnstxt,y        ;else get char
        cmp     #'a'            ;if less than a
        bcc     nxtchg          ;skip convert
        cmp     #'z'+1          ;if over z
        bcs     nxtchg          ;skip convert
        and     #%01011111      ;else convert
        sta     cnstxt,y        ;put back in buffer
nxtchg  iny                     ;bump index
        dex                     ;count down
        bne     lwrupr          ;loop if more
        txa                     ;set a to zero
        sta     cnstxt,y        ;insert stopper
        sta     cnbfpt          ;initialize pointer
mrecmd  jsr     stdflb          ;set default buffer
        jsr     intdsk          ;get drive number
        sta     dfldsk          ;store as default
        jsr     prslin          ;parse command
        bne     cmderr          ;error if afn
        lda     tmpdsk          ;get temporary
        beq     nodrch          ;if zero ok
        lda     #nbuilt*2       ;else set and
        bra     dotrns          ;do transient
nodrch  ldx     #0              ;zero table index
        stz     count           ;and clear count
tsnxfn  ldy     #1              ;set fcb index
tsnxch  lda     fnctbl,x        ;get table entry
        beq     gotcmd          ;if zero got it
        cmp     fcbone,y        ;else test
        bne     nocmpr          ;no good if not =
        inx                     ;bump
        iny                     ;pointers
        bne     tsnxch          ;and loop
nocmpr  inx                     ;go to end
        lda     fnctbl,x        ;of table
        bne     nocmpr          ;entry
        inx                     ;point to next
        inc     count           ;bump count
        lda     count           ;and if not
        cmp     #nbuilt         ;limit then
        bne     tsnxfn          ;try next
gotcmd  lda     count           ;get count
        asl     a               ;multiply by two
dotrns  tax                     ;use as index
        jsr     ccm_cmd         ;call execute command
extcmd  jsr     rstddr          ;restore default
extwod  jsr     prslin          ;parse more
        lda     fcbone+1        ;if first
        sec                     ;character
        sbc     #' '            ;not a space
        ora     tmpdsk          ;or if temporary
        bne     cmderr          ;then an error
ccmlp2  jmp     ccmlpe          ;else loop
ccm_cmd jmp     (xqfntb,x)      ;execute command
;
;command error syntax handler
cmderr  jsr     ccrlf           ;do a cr lf
        ldy     curpnt          ;get command pointer
cmerlp  lda     cnstxt,y        ;get char
        beq     cmerex          ;if null then done
        cmp     #' '            ;if space
        beq     cmerex          ;then done
        jsr     cotsxy          ;output with save
        iny                     ;point to next
        bne     cmerlp          ;and loop
cmerex  lda     #'?'            ;send a
        jsr     chrout          ;question mark
        bra     ccmlp2          ;and start over
;
;execute dir command
; dir <afn>
dir     jsr     prslin          ;find object file
        jsr     clrslt          ;clear auto and select
        lda     #' '            ;if name
        cmp     fcbone+1        ;and
        bne     findfr          ;type are
        cmp     fcbone+9        ;not empty
        bne     findfr          ;then use it
        lda     #'?'            ;else
        ldy     #11             ;fill
fillqu  sta     fcbone,y        ;fcb
        dey                     ;with
        bne     fillqu          ;question marks
findfr  jsr     srchf1          ;search for first match
        bmi     notfnd          ;error if none
fnddir  lda     #dirpln         ;set across
        sta     across          ;to entries per line
        jsr     ccrlf           ;do a crlf
dirl    jsr     hdr             ;do header
        lda     #':'            ;then a
        jsr     chrout          ;colon
        lda     diradd          ;get number
        asl     a               ;and
        asl     a               ;multiply
        asl     a               ;by
        asl     a               ;thirty two
        asl     a               ;then
        and     #%01100000      ;mask out insignificant
        tay                     ;make a pointer
        iny                     ;and bump
        ldx     #1              ;set counter
nmelpe  lda     dflbuf,y        ;get name
        jsr     cotsxy          ;else send to console
noname  iny                     ;bump index
        inx                     ;and count
        cpx     #12             ;if count is 12
        beq     endnme          ;then done
        cpx     #9              ;or if not 9
        bne     nmelpe          ;then loop
        lda     #'.'            ;else send
        jsr     cotsxy          ;a period
        bra     nmelpe          ;and loop
endnme  jsr     chkcst          ;check console status
        bne     extdir          ;if key down quit
        jsr     setone          ;else point to fcb
        jsr     srchnx          ;else search for next
        sta     diradd          ;save number
        bmi     extdir          ;quit if not found
        dec     across          ;drop count
        beq     fnddir          ;new line if 0
        jsr     spcout          ;send two spaces
        jsr     spcout          ; between names
        bra     dirl            ;loop for more filenames
extdir  rts                     ;else done
;
;not found error handler
notfnd  ldy     #ntfnms         ;point to message
        bra     remsg           ;send and exit
;
;execute ren command
; ren <ufn> <ufn>
;where first ufn is from name and second is to name
ren     jsr     prslin          ;get old name
        bne     renerr          ;error if afn
        jsr     tmpold          ;save drive
        jsr     clrslt          ;clear auto and select
        jsr     srchf1          ;if file does not exist
        bmi     fntfnd          ;then branch for error
        jsr     frssec          ;move name to second half
        ldy     cnbfpt          ;get pointer
        lda     cnstxt,y        ;and then char
        cmp     #' '            ;if not a space
        bne     rensyn          ;then error
        jsr     prslin          ;get new name
        bne     rensyn          ;error if afn
        lda     tmpdsk          ;get new drive
        beq     nonwdr          ;if zero ok
        cmp     oldtmp          ;compare to old
        beq     nonwdr          ;ok if same
        dec     a               ;subtract one ;-)
        cmp     dfldsk          ;if not default
        bne     rensyn          ;is error
        lda     oldtmp          ;and then if from not
        bne     rensyn          ;default is error
nonwdr  lda     oldtmp          ;else get old
        sta     tmpdsk          ;and save
        stz     fcbone+16       ;clear fcb offset
        jsr     clrslt          ;reselect
        jsr     srchf1          ;search for old
        bpl     filexs          ;error if found
        jsr     frssec          ;swap names
        jsr     setone          ;point to fcb
        jmp     renmfl          ;and do it
;ren file exists error handler
filexs  ldy     #flexms         ;point to
        bra     remsg           ;and send
;ren syntax error handler
rensyn  jsr     rstddr          ;restore default
renerr  jmp     cmderr          ;do error
;ren source file not found error handler
fntfnd  ldy     #ntfnms         ;point to
remsg   jmp     sndmsg          ;and send
;
;execute save command
; save <length> <drive:>ufn (<address>)
save    lda     #<tea           ;set start address to tea start
        ldy     #>tea
        sta     sadr
        sty     sadr+1
        jsr     bldnum          ;calculate length
        bne     saverr          ;error if > 255 pages
        sta     length          ;else save low
        ora     length          ;test for zero length
        beq     saverr          ;error if is
        jsr     prslin          ;get file name
        bne     saverr          ;error if afn
        jsr     frssec          ;move name to second half
        jsr     tmpold          ;save drive
        jsr     bldnum          ;it is so find value
        ldx     fcbone+1        ;see if there
        cpx     #' '
        beq     nosadr          ;it is not
        sta     sadr            ;and set address
        sty     sadr+1
        cpy     #2              ;make sure is high enough
        bcc     saverr          ;error if not
nosadr  jsr     secfrs          ;move name back
        jsr     clrslt          ;clear auto and select
        jsr     dlt1            ;delete
        jsr     setone          ;then create
        jsr     cratfl          ;new file
        bmi     noroom          ;say no room if error
        jsr     open1           ;open file
        bmi     noopen          ;if error
        clc                     ;now find end page
        lda     sadr+1
        adc     length
        sta     length
        lda     sadr            ;get start address
        ldy     sadr+1
wrtmre  sta     dskbuf          ;set
        sty     dskbuf+1        ;buffer
        jsr     mv128           ;move data to buffer
        jsr     setone          ;point to fcb
        jsr     wrrcrd          ;write record
        bne     wrterr          ;exit if error
        clc                     ;else
        lda     dskbuf          ;get old
        ldy     dskbuf+1        ;buffer
        adc     #128            ;add 128
        bcc     donotc          ;if carry
        iny                     ;bump upper
        beq     clssav          ;done if page zero
donotc  cpy     length          ;loop if upper
        bne     wrtmre          ;not at limit
clssav  jsr     setone          ;else point to
        jsr     clsefl          ;and close file
        bmi     cantcl          ;say so if error
extera  rts                     ;return to caller
;save error handler
saverr  bra     typafn          ;do error
;save no room error handler
noroom  ldy     #nospms         ;point to
        bra     temsg           ;and send
;save can not open error handler
noopen  ldy     #cnnoms         ;point to
        bra     temsg           ;and send
;save write error handler
wrterr  ldy     #wrerms         ;point
        bra     temsg           ;to and send
;save can not close error
cantcl  ldy     #ntclms         ;point to
        bra     temsg           ;and send
;
;execute era command
; era <afn>
era     jsr     prslin          ;get file name
        cmp     #11             ;if not all ?
        bne     nteral          ;then skip verify
        ldy     #alflms         ;else point to
        jsr     sndmsg          ;and send all files
        jsr     cnsrde          ;get an input
        pha                     ;and save
        jsr     ccrlf           ;do a cr lf
        pla                     ;restore char
        and     #$5f            ;convert to upper case
        cmp     #'Y'            ;if not y
        bne     extera          ;then quit
nteral  jsr     clrslt          ;clear auto and select
        jmp     dlt1            ;delete first, return
;
;execute type command
; type d:ufn
type    jsr     prslin          ;get file
        bne     typafn          ;error if afn
        jsr     casdo1          ;clear - select - open file 1
        bmi     notype          ;error if not found
        jsr     ccrlf           ;else do cr lf
typmre  jsr     read1           ;read record
        beq     okread          ;if ok continue
        bpl     extera          ;exit if just end
        bmi     typerr          ;else error
okread  ldx     #0              ;clear index
typlpe  lda     dflbuf,x        ;get char
        cmp     #eof            ;if eof
        beq     extera          ;then exit
        jsr     cotsxy          ;else send
        jsr     chkcst          ;if key down
        bne     exttok          ;then exit
        ldx     savx            ;else get index
        inx                     ;bump it
        bpl     typlpe          ;and loop if < 128
        bmi     typmre          ;else read more
exttok  jmp     cnsrdx          ;clear console, no echo
;type syntax error handler
typafn  jmp     cmderr          ;error return
;type read error handler
typerr  ldy     #rderms         ;point to
        bra     temsg           ;and send
;type not found error handler
notype  ldy     #ntfnms         ;point to
temsg   jmp     sndmsg          ;and send
;
;execute load command
; load <ufn> (<address>)
load    jsr     setddb          ;set load start to tea start
        jsr     prslin          ;build fcb
        bne     typafn          ;error if afn
        jsr     frssec          ;save in second half
        jsr     tmpold          ;save file
        jsr     bldnum          ;find start address
        ldx     fcbone+1        ;see if number
        cpx     #' '
        beq     usedfl          ;if none use default
        jsr     setdb           ;and set
usedfl  jsr     secfrs          ;move name back
        jsr     casdo1          ;clear - select - open file 1
        bmi     notype          ;exit if not found
        lda     dskbuf          ;get load start
        ldy     dskbuf+1
lcmdlp  jsr     setbuf          ;set as disk buffer
        jsr     read1           ;read record
        bne     loadnd          ;done if not zero
        jsr     adjdb           ;bump address
        bra     lcmdlp          ;and loop
loadnd  bmi     typerr          ;if error say so
        jmp     stdflb          ;back to default buffer
;
;execute GO command
; go (<address>)
go      jsr     bldnum          ;get address
        ldx     fcbone+1        ;get first char
        cpx     #' '            ;see if nothing
        beq     nolnum          ;nothing so use tea
        jmp     godoit          ;then execute
nolnum  jmp     gotea
;
;execute USER command
; user (<number>)
user    jsr     bldnum          ;build number
        ldx     fcbone+1        ;get first character
        cpx     #' '            ;see if nothing
        beq     usrext          ;done if nothing
        lda     num+1           ;check high
        bne     usrerr          ;error if too big
        lda     num             ;get low
        cmp     #16             ;see if too big
        bcs     usrerr          ;error if is
        bra     setusr          ;set if 0-15
usrext  rts                     ;return to caller
;bad user number entered
usrerr  ldy     #userms         ;get error message
        bra     temsg
;user code (usrcde) related routines
;get user code from PEM
getusr  lda     #255            ;query parameter
;set user code to A
setusr  ldx     #36             ;37th function
        jmp     pemjmp          ;execute
;Save user code in A in case of action that may
;destroy or alter it.
savusr  jsr     getusr          ;get usrcde from PEM
        asl     a               ;shift code to high nibble
        asl     a
        asl     a
        asl     a
        ora     dfldsk          ;paste default disk into
        sta     iostat          ;iostat and save
        rts                     ;return to caller
;
;execute trns (transient) command
; <ufn> (<fn>) (<fn>)
trns    lda     fcbone+1        ;get first char
        cmp     #' '            ;if not space
        bne     chktyp          ;check type
        lda     tmpdsk          ;else test temp
        beq     skpdrv          ;if none skip
        dec     a               ;subtract one ;-)
        sta     dfldsk          ;set default
        jsr     slctds          ;and select
skpdrv  jmp     extwod          ;return
chktyp  lda     fcbone+9        ;get type
        cmp     #' '            ;if space
        beq     typemp          ;then ok
        jmp     trnerr          ;else error
typemp  ldx     #2              ;make
setcom  lda     typcom,x        ;type
        sta     fcbone+9,x      ;com
        dex                     ;then
        bpl     setcom          ;continue
        jsr     casdo1          ;clear - select - open file one
        bpl     gottrn          ;jump if ok
        jmp     topner          ;else do error
gottrn  jsr     setddb          ;set start to tea
nxtrcr  jsr     setbuf          ;set address
        jsr     read1           ;read fcb 1
        bne     endlde          ;branch if error or eof
        jsr     adjdb           ;adjust address up by 128
        cmp     #<ccmram        ;if new low
        bcc     tryhig          ;ok try high
        cpy     #>ccmram        ;else if high
        bcc     nxtrcr          ;ok do more
        bcs     chklnd          ;else check for end
tryhig  cpy     #>ccmram        ;if high less
        bcc     nxtrcr          ;then ok
        beq     nxtrcr          ;or ok if same
chklnd  jsr     stdflb          ;set buffer to default
        jsr     read1           ;read (should be eof)
        beq     tlderr          ;error if more
endlde  bmi     tlderr          ;do error
gotea   lda     #<tea           ;set go address to tea start
        ldy     #>tea
godoit  sta     do+1            ;set to ay
        sty     do+2
        jsr     rstddr          ;else restore default
        jsr     prslin          ;parse file
        lda     tmpdsk          ;set auto
        sta     fcbone          ;select position
        ldx     #16             ;do a second
        jsr     prsmre          ;fcb if there
        lda     tmpdsk          ;set auto select
        sta     fcbone+16       ;again
        ldy     #0              ;clear record
        sty     fcbone+32       ;counter
        ldx     #32             ;move
movfcb  lda     fcbone,x        ;all
        sta     dflfcb,x        ;to
        dex                     ;default
        bpl     movfcb          ;fcb
        inx                     ;set x to zero
tstbuf  lda     cnstxt,y        ;get char
        beq     isnull          ;if null jump
        cmp     #' '            ;or if space
        beq     isnull          ;jump
        iny                     ;else bump pointer
        bne     tstbuf          ;and loop
isnull  stz     dflbuf          ;clear count
movbuf  lda     cnstxt,y        ;get char
        sta     dflbuf+1,x      ;and move
        beq     fillov          ;exit if null
        inx                     ;increment
        iny                     ;counters
        inc     dflbuf          ;and length
        bne     movbuf          ;and loop
fillov  jsr     ccrlf           ;do a cr lf
        jsr     stdflb          ;set default
        jsr     savusr          ;save user code
do      jsr     tea             ;call execute jmp
        lda     dfldsk          ;get default
        jsr     slctds          ;and set
        jmp     ccmlpe          ;then loop
;trns syntax error handler
topner  jsr     rstddr          ;restore default
trnerr  jmp     cmderr          ;do error
;trns load error
tlderr  ldy     #lderms         ;point to
        jmp     sndmsg          ;and send
;
;carriage return and linefeed
ccrlf   lda     #cr             ;get a cr
        jsr     chrout          ;send
        lda     #lf             ;get a lf
        bra     chrout          ;and send
;
;general PEM calls
cnsrde  ldx     #1              ;console read
        bra     pemgo
spcout  lda     #' '            ;output space
chrout  ldx     #2              ;console output
        bra     pemgo
cnsrdx  ldx     #6              ;console read without echo
        bra     pemgo
rdebuf  ldx     #10             ;buffered input
        bra     pemgo
chkcst  ldx     #11             ;check console
        bra     pemgo
rstdsk  ldx     #13             ;initialize system
        bra     pemgo
slctds  ldx     #14             ;select drive
        bra     pemgo
openfl  ldx     #15             ;open file
        bra     pemgo
clsefl  ldx     #16             ;close file
        bra     pemgo
srchfr  ldx     #17             ;first match
        bra     pemgo
srchnx  ldx     #18             ;next match
        bra     pemgo
;delete file one
dlt1    jsr     setone          ;point to fcb
dltfil  ldx     #19             ;delete file
        bra     pemgo
rdrcrd  ldx     #20             ;read record
        bra     pemgo
wrrcrd  ldx     #21             ;write record
        bra     pemgo
cratfl  ldx     #22             ;create file
        bra     pemgo
renmfl  ldx     #23             ;rename file
        bra     pemgo
intdsk  ldx     #25             ;read drive num
        bra     pemgo
setbuf  ldx     #26             ;set buffer add
pemgo   jmp     pemjmp          ;go to pem
;
;restore default drive
rstddr  lda     tmpdsk          ;get temp drive,
        beq     extddr          ;if zero, exit
        dec     a               ;else, subtract one
        cmp     dfldsk          ;compare to default
        beq     extddr          ;exit if same
seldfl  lda     dfldsk          ;else, get default
        bra     slctds          ;and select
;
;clear auto - select disk - open file at fcbone
casdo1  jsr     clrslt          ;clear auto and select
;open file one
; returns:n=1 if not found
open1   stz     fcbone+32       ;clear record number
        jsr     setone          ;point to fcb
        jsr     openfl          ;open it
        sta     diradd          ;save number
extddr  rts                     ;return to caller
;read file one
read1   jsr     setone          ;point to fcb
        bra     rdrcrd          ;do read
;search for first file one
; returns:n=1 if not found
srchf1  jsr     setone          ;point to fcb
        jsr     srchfr          ;search for first
        sta     diradd          ;save number
        rts                     ;return to caller
;header output
hdrout  jsr     ccrlf           ;do cr lf
hdr     jsr     intdsk          ;get drive number
        clc                     ;add "A" to make it
        adc     #'A'            ;a drive letter
        jsr     cotsxy          ;send drive letter
;drive letter is written now do user number
        jsr     getusr          ;get user number
        cmp     #10             ;see if < 10
        bcc     usr09           ;if so use as is
        pha                     ;else, save number,
        lda     #'1'            ;send an ASCII 1
        jsr     cotsxy          ;to the console
        pla                     ;get user number back
        sec                     ;subtract 10,
        sbc     #10             ;then get remainder
usr09   ora     #'0'            ;convert to ASCII
;
;drop in to cotsxy
;
;output with save of x and y
;this routine calls the pem resident routine
;tstchr to see if a char is a printing char.
;if it is then c=1 upon return from tstchr.
cotsxy  stx     savx            ;save x
        sty     savy            ;and y
        jsr     tstchr          ;see if printing
        bcs     isprnt          ;jump if is
        pha                     ;else save
        lda     sysdef+4        ;get invert
        jsr     chrout          ;send it
        pla                     ;get char
        ora     #'@'            ;make printing
        jsr     chrout          ;send it
        lda     sysdef+3        ;get normal
isprnt  jsr     chrout          ;do output
        ldy     savy            ;get y
        ldx     savx            ;and x
        rts                     ;return to caller
;
;set up fcb one pointer
setone  lda     #<fcbone        ;low
        ldy     #>fcbone        ;and high
        rts                     ;return to caller
;
;save tmpdsk in oldtmp
tmpold  lda     tmpdsk
        sta     oldtmp
        rts                     ;return to caller
;
;parse command line
prslin  ldx     #0              ;clear index
prsmre  phx                     ;save index
        stz     tmpdsk          ;clear temp drive flag
        ldy     cnbfpt          ;get buffer pointer
        jsr     skpspc          ;find first non-space
        sty     curpnt          ;save index
        beq     nulchr          ;branch if null
;
;The "and" instruction below is to strip off ascii bits that are drive letter related!
; by doing so, A - H becomes %0001 - %1000 (1-8), which can be a real problem...
; if the next character is a semicolon (:). that 4-bit masked value becomes the
; TMPDSK variable, which is spliced into the FCB and becomes the selected drive.
; As a result, any ascii character can be entered and you wind up with a list of
; phantom drive letters, depending on what was entered, alpha or not.
;
; The fix here is to mask off the lower 7 bits for true ascii, then let this
; routine below starting at "drvinp" complete the check for a valid drive letter.
;
        and     #%01111111      ;strip off bit 7 for ascii only
        pha                     ;and save to stack
        iny                     ;point to next char
        lda     cnstxt,y        ;and get it
        cmp     #':'            ;if it's a colon
        beq     drvinp          ;branch and set drive
        pla                     ;else clear stack
        dey                     ;backup the index
nulchr  lda     dfldsk          ;set automatic
        sta     fcbone,x        ;to default drive
        bra     trynme          ;then branch to parse name
;
;at this stage, we get the ascii character back from the stack, which is the
; assumed drive letter. Now we need to range check this and ensure it's within
; the range of a-z or A-Z. First, we'll convert lower case to upper case, then
; check for the proper range. If the character is outside of the range, we can
; simply set it for the acceptable range plus one. This will be caught by the
; PEM routine and flagged as a drive out of range.
;
drvinp  pla                     ;get the "drive letter" back
        cmp     #$61            ;check for lower case ascii/control characters
        bcc     ucok            ;if lower, upper case is okay
        sbc     #$20            ;else subtract $20 to convert to upper case
ucok    cmp     #'A'            ;compare for ascii "A" or higher
        bcc     baddrv          ;branch if too low
        cmp     #maxdrv+$41     ;check for ascii of maxdrv or higher
        bcc     drvok           ;if okay, branch and set drive
baddrv  lda     #maxdrv+$41     ;else, get first out of range drive
;
drvok   and     #%00001111      ;mask off upper 4 bits
        sta     tmpdsk          ;set as temp disk
        sta     fcbone,x        ;and into fcb
        iny                     ;increment pointer past colon
trynme  lda     #8              ;set name count
        sta     count           ;to eight
tstnme  jsr     tstlgl          ;test for illegal
        beq     flnmsp          ;if illegal jump
        inx                     ;bump pointer
        cmp     #'*'            ;if not *
        bne     notafn          ;skip ? fill
        lda     #'?'            ;get a ?
        sta     fcbone,x        ;store it,
        bra     nxtout          ;then branch
notafn  sta     fcbone,x        ;store char
        iny                     ;bump source
nxtout  dec     count           ;drop counter down
        bne     tstnme          ;and loop
skpmre  jsr     tstlgl          ;if illegal
        beq     trytyp          ;try type
        iny                     ;else bump index
        bne     skpmre          ;and loop
flnmsp  inx                     ;next position
        lda     #' '            ;get a space
        sta     fcbone,x        ;store
        dec     count           ;count down
        bne     flnmsp          ;and loop
        lda     cnstxt,y        ;get char again
trytyp  pha                     ;save char
        lda     #3              ;set count
        sta     count           ;to three
        pla                     ;get char
        cmp     #'.'            ;if not .
        bne     fltysp          ;then fill spaces
        iny                     ;else bump source
tsttyp  jsr     tstlgl          ;if not legal
        beq     fltysp          ;then fill spaces
        inx                     ;else bump index
        cmp     #'*'            ;if not *
        bne     notaft          ;then skip fill
        lda     #'?'            ;else get ?
        sta     fcbone,x        ;move to fcb
        bne     mrtyfl          ;and loop
notaft  sta     fcbone,x        ;move char to fcb
        iny                     ;bump source
mrtyfl  dec     count           ;count down
        bne     tsttyp          ;and loop
nttype  jsr     tstlgl          ;if illegal
        beq     filnul          ;fill out nulls
        iny                     ;else bump source
        bne     nttype          ;and loop
fltysp  inx                     ;bump fcb pointer
        lda     #' '            ;get space
        sta     fcbone,x        ;put in fcb
        dec     count           ;count down
        bne     fltysp          ;and continue
filnul  lda     #3              ;clear
        sta     count           ;three
mrnlty  inx                     ;bump fcb pointer
        stz     fcbone,x        ;clear fcb
        dec     count           ;count down
        bne     mrnlty          ;loop until done
        sty     cnbfpt          ;update start pointer
        stz     count           ;clear count
        ply                     ;restore stack
        ldx     #11             ;counter to 11
tstqus  iny                     ;point to
        lda     fcbone,y        ;and get char
        cmp     #'?'            ;if not a ?
        bne     nxtqus          ;then jump
        inc     count           ;else bump count
nxtqus  dex                     ;loop until
        bne     tstqus          ;11 tested
        lda     count           ;get count
        rts                     ;and return
;skip space
skpspc  lda     cnstxt,y        ;get char
        beq     extskp          ;jump if null
        cmp     #' '            ;if not space
        bne     extskp          ;then exit
        iny                     ;else go to next
        bne     skpspc          ;and test
extskp  rts                     ;return to caller
;test legal
tstlgl  lda     cnstxt,y        ;get char
        beq     extlgl
        cmp     #' '            ;if space
        bcc     badinp          ;ok - less is error
        beq     extlgl
        cmp     #'='
        beq     extlgl
        cmp     #'.'
        beq     extlgl
        cmp     #':'
        beq     extlgl
        cmp     #semico
        beq     extlgl
        cmp     #'<'
        beq     extlgl
        cmp     #'>'
extlgl  rts                     ;return to caller
badinp  jmp     cmderr
;
;set default buffer address
stdflb  lda     #<dflbuf        ;point to
        ldy     #>dflbuf        ;location
        jmp     setbuf          ;and set
;
;clear automatic and select
clrslt  stz     fcbone          ;clear auto select
slttmp  lda     tmpdsk          ;get temp
        beq     extsnd          ;if zero exit
        dec     a               ;subtract one ;-)
        cmp     dfldsk          ;if same as default
        beq     extsnd          ;then quit
        jmp     slctds          ;else set
;
;send message
sndmsg  sty     savy            ;save index
        jsr     ccrlf           ;do cr and lf
        ldy     savy            ;get index
lpesnd  lda     msgtbl,y        ;get char
        beq     extsnd          ;if null quit
        jsr     cotsxy          ;else send
        iny                     ;point to next
        bne     lpesnd          ;and loop
extsnd  rts                     ;return to caller
;
;set load start address
setddb  lda     #<tea           ;to tea start
        ldy     #>tea
setdb   sta     dskbuf          ;to ay
        sty     dskbuf+1
        rts                     ;return to caller
;
;move first part of fcb to second half
;and move second part to first part.
;only ren depends upon this routine to actually
;swap fcb halves.
;alters:a,x,p
;returns:x=$ff
frssec  ldx     #15             ;set index to last
frsclp  lda     fcbone,x        ;get byte
        ldy     fcbone+16,x
        sta     fcbone+16,x     ;move it
        tya
        sta     fcbone,x
        dex
        bpl     frsclp          ;until 16 moved
        rts                     ;return to caller
;
;move second part of fcb to first half
;restore tmpdsk from save location
;alters:a,x,p
;returns:x=$ff
secfrs  ldx     #15             ;set index to last
scfrlp  lda     fcbone+16,x     ;get byte
        sta     fcbone,x        ;move it
        dex
        bpl     scfrlp          ;loop until done
        lda     oldtmp          ;get old flag
        sta     tmpdsk          ;and set
        rts                     ;return to caller
;
;build a number from command line and leave in num
;will handle either decimal (e.g., 578) or hexadecimal (e.g., $3f2)
;move parse index past number
;modifies:all
;returns:a,y set to low and high of number
; flags set according to y which is high part of number
bldnum  lda     #9              ;preset to decimal
        sta     moduls
        jsr     prslin          ;put in fcb
        lda     tmpdsk          ;if temp
        bne     numerr          ;is error
        sta     num             ;else clear num
        sta     num+1
        tay                     ;and index
        lda     fcbone+1,y      ;get first char
        cmp     #'$'            ;see if hex signal
        bne     bnumlp          ;skip if not
        lda     #15             ;set for modulus
        sta     moduls
        iny                     ;bump past $
bnumlp  iny                     ;next char
        lda     fcbone,y        ;get digit
        cmp     #' '            ;see if space
        beq     endnlp          ;done if is
        jsr     tstdec          ;see if decimal
        bcc     usedig          ;use if is
        jsr     tsthex          ;see if hex
        bcs     numerr          ;error if not
        sbc     #6              ;remove offset
        ldx     #15             ;test for hex modulus
        cpx     moduls
        bne     numerr          ;error if not
usedig  and     #%00001111      ;eliminate ascii bias
        pha                     ;save result
        lda     num             ;move number to temp
        sta     tnum
        lda     num+1
        sta     tnum+1
        ldx     moduls          ;get modulus
        clc
addlpe  lda     num             ;add number to itself
        adc     tnum
        sta     num
        lda     num+1           ;modulus times
        adc     tnum+1
        sta     num+1
        bcs     numerr          ;error if carry out
        dex
        bne     addlpe          ;loop if more
        pla                     ;get input back
        adc     num             ;add in
        sta     num
        bcc     bnumlp          ;loop if no carry
        inc     num+1           ;else bump high
        bne     bnumlp          ;and loop for more
;error in bldnum - handle and quit
numerr  jmp     cmderr          ;do general error
;end of use of name part - check type
endnlp  lda     fcbone+9        ;get first
        cmp     #' '            ;see if space
        bne     numerr          ;error if not
        lda     num             ;return with result
        ldy     num+1
        rts                     ;return to caller
;
;function entry vector table
xqfntb  .DW     dir
        .DW     save
        .DW     era
        .DW     type
        .DW     ren
        .DW     go
        .DW     load
        .DW     user
        .DW     trns
;
;Messages
msgtbl
;
nospms  .EQU    *-msgtbl
        .DB     "NO SPACE",0
ntclms  .EQU    *-msgtbl
        .DB     "CAN NOT CLOSE",0
alflms  .EQU    *-msgtbl
        .DB     "ALL FILES (Y/N)?",0
rderms  .EQU    *-msgtbl
        .DB     "READ ERROR",0
ntfnms  .EQU    *-msgtbl
        .DB     "NOT FOUND",0
flexms  .EQU    *-msgtbl
        .DB     "FILE EXISTS",0
lderms  .EQU    *-msgtbl
        .DB     "LOAD ERROR",0
cnnoms  .EQU    *-msgtbl
        .DB     "CAN NOT OPEN",0
wrerms  .EQU    *-msgtbl
        .DB     "WRITE ERROR",0
userms  .EQU    *-msgtbl
        .DB     "INVALID USER",0
;
;Function name table
fnctbl
        .DB     "DIR ",0
        .DB     "SAVE ",0
        .DB     "ERA ",0
        .DB     "TYPE ",0
        .DB     "REN ",0
        .DB     "GO ",0
        .DB     "LOAD ",0
        .DB     "USER ",0
;
;ccm variable storage
;
;input buffer
cnsbuf
cnsmax  .DB     127
cnslng  .DB     0
cnstxt  DEFS    128             ;set to 128 for safety
;fcb one
fcbone  DEFS    33
;
;submit FCB
subfcb  .DB     0               ;will be drive A
        .DB     '$$$     '      ;name field
        .DB     'SUB'           ;type field
        .DB     0               ;extent
        .DB     0,0             ;unused
SUBRC   .DB     0               ;number of records
        DEFS    16              ;block number area
SUBCR   .DB     0               ;current record to read
;
;com type field
typcom  .DB     "COM"
;
;variable storage
cnbfpt  .DB     0               ;buffer index
dfldsk  .DB     0               ;default drive
tmpdsk  .DB     0               ;temporary drive
dsksav  .DB     0               ;save default during $$$.sub ops
count   .DB     0               ;counter
curpnt  .DB     0               ;input parse start
diradd  .DB     0               ;directory number
savx    .DB     0               ;save x location
savy    .DB     0               ;save y location
length  .DB     0               ;number pages to save
oldtmp  .DB     0               ;temp drive in ren
dskbuf  .DW     tea             ;disk buffer start
across  .DB     0               ;dir count on line
moduls  .DB     9               ;modulus-1 for number input
num     .DW     0               ;input number
tnum    .DW     0               ;temp in bldnum
sadr    .DW     tea             ;save start address
;
        END

