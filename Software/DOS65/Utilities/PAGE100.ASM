;page
;********************************************************
;*							*
;*	    FILE SCREEN PAGING UTILITY			*
;*							*
;********************************************************
;
;released:	14 August 2014 (1.00)
;last revision:
;
;DOS/65 definitions
pzstrt  =       0		;start of page zero storage
WBOOT	=	$100		;REBOOT ENTRY POINT
PEM	=	$103		;CPM ENTRY POINT
TFCB	=	$107		;TRANSIENT PROGRAM FCB
tea	=	$800		:TEA start
;DOS/65 PEM commands
charout	=	2		;send character to console
charin	=	6		;get input key without echo
STRING	=	9		;STRING PRINT FUNCTION
STATUS	=	11		;CONSOLE STATUS FUNCTION
OPEN	=	15		;OPEN FUNCTION CODE
READ	=	20		;READ FUNCTION CODE
SetBuf	=	26		;set buffer address
;display definitions
PAGE	=	23		;LINES/PAGE
WIDTH	=	79		;CHARACTERS/LINE
;character definitions
ctlc	=	3		;control-c
TAB	=	9		;ASCII TAB
ALF	=	$a		;ASCII LINE FEED
ACR	=	$d		;ASCII CARRIAGE RETURN
eof	=	$1a		;end of file
ESC	=	$1b		;ASCII ESCAPE
SPACE	=	$20		;ASCII SPACE
;
	page0
;page zero variable storage
	*=	pzstrt
line			        ;screen row
        *=	*+1
col			        ;screen column
        *=	*+1
bufpos			        ;disk buffer (0-127 valid - 128=wrap)
        *=	*+1
;
;code section
	*=	tea
	ldx	#$ff		;set stack so must
	txs			;exit with warm boot
	lda	#<opnmsg	;point to welcome message
	ldy	#>opnmsg
	jsr	message		;send it
	lda	#<buffer	;change to local buffer
	ldy	#>buffer
	ldx	#SetBuf
	jsr	pem
	lda	#<tfcb		;point to FCB
	ldy	#>tfcb
	ldx	#OPEN		;FILE OPEN FUNCTION
	jsr	PEM
	bpl	openok		;if OK
	jmp	ERROR1		;IF ERROR, EXIT
openok	jsr	RESET
	lda	#128		;set disk buffer pointer
	sta	bufpos		;to wrapped value
LOOP	jsr	FetchByt	;GET NEXT BYTE FROM FILE
	bcc	fetchok		;ok if c=0
	jmp	ERROR2		;error if c=1
fetchok	cmp	#eof		;see if text eof
	bne	noteof		;ok if not eof
	jmp	EXIT		;but if is done
noteof	cmp	#ACR		;see if cr
	bne	notacr		;ok if not
	jmp	CRET		;if is UPDATE COLUMN COUNT
notacr	cmp	#ALF		;see if lf
	bne	notalf		;if not OK
	jmp	LFEED		;but if is uPDATE LINE COUNT
notalf	cmp	#TAB		;see if tab
	beq	TABloop		;if is expand
	cmp	#SPACE		;ANOTHER CONTROL CHARACTER?
	bcs	DISPLAY		;NO, PRINT CHARACTER
	jmp	LOOP		;YES, IGNORE IT
;
;expand tab modulo 8
TABLOOP	lda	#space		;GET A SPACE
	jsr	FeedDisp	;DISPLAY IT
	lda	col		;GET CURRENT COLUMN
	AND	#%00000111	;MOD 8, AT TAB STOP?
	bne	TABLOOP		;NO, ANOTHER SPACE
	jmp	LOOP
;
DISPLAY	jsr	FeedDisp	;DISPLAY THE CHARACTER
	jmp	LOOP
;
;zero column and echo cr
CRET	lda	#0
	sta	COL		;RESET COLUMN COUNT
	lda	#ACR		;GET A CARRIAGE RETURN
	jmp	DISPLAY
;
;bump line count and pause if page
LFEED	lda	LINE		;GET LINE COUNT
	cmp	#PAGE		;SCREEN FULL?
	beq	NEXTPAGE	;YES, WAIT FOR KEYPRESS
	INC	line		;NO, BUMP LINE COUNT
	lda	#ALF		;GET A LINE FEED
	jmp	DISPLAY
;
NEXTPAGE
	jsr	KEYLOOP		;WAIT FOR KEYPRESS
	lda	#ALF		;GET A LINE FEED
	jmp	DISPLAY
;
;At page end wait for console key entry.
;Clear pending char without echo but if
;control-c exit the program.
KeyLoop	ldx	#status		;check status
	jsr	PEM
	beq	keyloop		;loop if no key
	ldx	#charin		;else get key
	jsr	PEM
	cmp	#ctlc		;see if ctl-c
	bne	reset		;ignore if not
	jmp	wboot		;else do warm boot
;Initialize display position
RESET	lda	#0
	sta	LINE		;RESET LINE COUNT
	sta	COL		;RESET COLUMN COUNT
	rts
;
;Send characters to feed display and check for line wrap
;and adjust as needed.
FeedDisp
	pha			;save char
	jsr	chartpem	;send to console
	pla			;get character back
	cmp	#space		;see if cr or lf
	bcc	cont		;if so do not change position
	inc	col		;else bump column
	lda	col		;get it
	cmp	#width		;see if at EOL
	bne	cont		;if not continue
	lda	#acr		;else send cr
	jsr	chartpem	;to console
	lda	#0		;now clear col
	sta	col
	lda	line		;now get line
	cmp	#page		;see if page end page
	bne	cont2		;if not OK
	jsr	keyloop		else wait for response
cont2	lda	#alf		;then get lf
	jsr	chartpem	;send it
	inc	line		;and bump line
cont	rts			;else done
;
;send character to PEM for console
chartpem
	ldx	#charout	;send to console
	jsr	pem		;send to console
	rts
;
EXIT	lda	#<MESSG4	;POINT TO EOF MESSAGE
	ldy	#>messg4
error	jsr	MESSAGE		;send it
done	jmp	wboot		;then do warm boot
;file read error
ERROR2	lda	#<MESSG2	;POINT TO READ MESSAGE
	ldy	#>messg2
	jmp	error
;error opening file
ERROR1	lda	#<MESSG1	;POINT TO OPEN MESSAGE
	ldy	#>messg1
	jmp	error
;normal message output for message at ay
MESSAGE	ldx	#STRING		;DISPLAY THE MESSAGE
	jsr	PEM
	rts			;and return
;
;ROUTINE TO READ A BYTE
FetchByt
	ldy	bufpos		;get index
	bmi	NextBlk	        ;if invalid read new block
	lda	buffer,y	;else get character
	and	#%01111111	;clear msb in case
	inc	bufpos		;bump index
	clc			;clear carry
	rts			;and return with char
NextBlk
	lda	#<tfcb		;point to fcb
	ldy	#>tfcb
	ldx	#read		;do read
	jsr	pem		;record
;return codes are as follows:
;a=0 OK
;a=1 physical EOF
;a=2 read from unwritten block
;a=255 error
	cmp	#255		;check for error
	beq	inerror		;error = error of course
	cmp	#2		;text file should never
	beq	inerror		;have unwritten block
;next two conditions are good so reset index
	ldy	#0		;point index to start
	sty	bufpos
	cmp	#0		;check for normal read
	beq	FetchByt	;if so go to start
	lda	#eof		;only condition left is eof
	sta	buffer		;so stuff EOF into buffer
	jmp	FetchByt	;and start over
;error during read
INERROR	sec			;SET CARRY
	rts
;
;messages
opnmsg	.byte	'DOS/65 TYPE TEXT FILE BY PAGE V1.00',acr,alf,'$'
MESSG1	.byte	acr,alf,'**** File Open Error ****',ACR,ALF,'$'
MESSG2	.byte	acr,alf,'**** File Read Error ****',ACR,ALF,'$'
MESSG4	.byte	acr,alf,'**** End of Input File Reached ****',acr,alf,'$'
;
;disk buffer
buffer
        *=      *+128
;
	.END
