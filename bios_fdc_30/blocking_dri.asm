;*********************************************************************** 
; PATCHED
;*********************************************************************** 
;					 Following the comment on line 111 of Appendix G:
;						   "read the selected CP/M sector"
;
;					 Insert the following two lines:
;						   xra	   a			;accum = 0
;						   sta	   unacnt		;unacnt = 0
;
;					 The next source lines remain as shown:
;						   mvi	   a,1
;						   sta	   readop		;read operation
;
;					 Insert the following code in your BIOS Home routine:
;					  home:
;						   lda	   hstwrt		;check for pending write
;						   ora	   a
;						   jnz	   homed
;						   sta	   hstact		;clear host active flag
;					  homed:
;
;					 Continue with the rest of the home routine.
;*********************************************************************** 
 
;*********************************************************************** 
;*																	   *
;*		SECTOR DEBLOCKING ALGORITHMS FOR CP/M 2.0					   *
;*																	   *
;*********************************************************************** 
;
;
;*********************************************************************** 
;*													 
;*		   CP/M TO HOST DISK CONSTANTS				 
;*													 
;*********************************************************************** 
BLKSIZ	EQU	2048		;CP/M ALLOCATION SIZE
			if (Floppy==360)
HSTSPT	EQU	2*18		;HOST DISK SECTORS/TRK
			elseif (Floppy==720)
HSTSPT	EQU	2*18		;HOST DISK SECTORS/TRK
			elseif (Extra==50)
HSTSPT	EQU	2*26		;HOST DISK SECTORS/TRK
			elseif (Floppy==120)
HSTSPT	EQU	2*26		;HOST DISK SECTORS/TRK
			elseif (Floppy==144)
HSTSPT	EQU	2*32		;HOST DISK SECTORS/TRK
			elseif (Floppy==100)
HSTSPT	EQU	2*26		;HOST DISK SECTORS/TRK
			endif
HSTBLK	EQU	HSTSIZ/128	;CP/M SECTS/HOST BUFF
CPMSPT	EQU	HSTBLK * HSTSPT	;CP/M SECTORS/TRACK
SECMSK	EQU	HSTBLK-1	;SECTOR MASK
;	SMASK	HSTBLK		;COMPUTE SECTOR MASK
SECSHF	EQU	1		;LOG2(HSTBLK)
;
;*********************************************************************** 
;*													 
;*		  BDOS CONSTANTS ON ENTRY TO WRITE			 
;*													 
;*********************************************************************** 
WRALL	EQU	0		;WRITE TO ALLOCATED
WRDIR	EQU	1		;WRITE TO DIRECTORY
WRUAL	EQU	2		;WRITE TO UNALLOCATED
;
;*********************************************************************** 
;*													 
;*	THE BDOS ENTRY POINTS GIVEN BELOW SHOW THE
;*		CODE WHICH IS RELEVANT TO DEBLOCKING ONLY.	 
;*													 
;*********************************************************************** 
;
;	DISKDEF MACRO, OR HAND CODED TABLES GO HERE
;DPBASE	EQU	$		;DISK PARAM BLOCK BASE
;
;
;------------------------------------------------------------------------------
; * Disk services *
;------------------------------------------------------------------------------
;
;------------------------------------------------------------------------------
;SELDSK:
	;SELECT DISK
;	MOV	A,C		;SELECTED DISK NUMBER
;	STA	SEKDSK		;SEEK DISK NUMBER
;	MOV	L,A		;DISK NUMBER TO HL
;	MVI	H,0
;	REPT	4		;MULTIPLY BY 16
;	DAD	H
;	ENDM
;	DAD	H
;	DAD	H
;	DAD	H
;	DAD	H
;	LXI	D,DPBASE	;BASE OF PARM BLOCK
;	DAD	D		;HL=.DPB(CURDSK)
;	RET
;
; * SELDSK - Select disk
; I: C=disk nr
; O: HL=address of table DPH, 0 when wrong disk nr
; M: all
SELDSK:			lxi	h,0					; return 0 on error
				mov	a,c
				cpi	NumFlps				; disks A: B:
				rnc						; error - HL = 0
				sta	SEKDSK				; drive - save it
				mov	l,c					; HL=disk nr, compute DPH address
				dad	h					; *2
				dad	h					; *4
				dad	h					; *8
				dad	h					; *16
				lxi	d,DPH				; disk. param. address base
				dad	d					; HL=chosen disk params address
				ret
;HOME THE SELECTED DISK
;	LDA	HSTWRT	;CHECK FOR PENDING WRITE
;	ORA	A
;	JNZ	HOMED
;	STA	HSTACT	;CLEAR HOST ACTIVE FLAG
;HOMED:
;	RET
;------------------------------------------------------------------------------
; * HOME - Disk Home
; I: -
; O: -
; M: A
HOME:			mvi	c,0
				;DRI blocking alg.code
				LDA		HSTWRT			;CHECK FOR PENDING WRITE
				ORA		A
				JNZ		HOMED
				STA		HSTACT			;CLEAR HOST ACTIVE FLAG
HOMED:
;	RET
;SETTRK:
	;SET TRACK GIVEN BY REGISTERS BC
;	MOV	H,B
;	MOV	L,C
;	SHLD	SEKTRK		;TRACK TO SEEK
;	RET
;
;------------------------------------------------------------------------------
; * SETTRK - Set track
; I: C=track nr
; O: -
; M: A
SETTRK:			mov		a,c				; 8-bit version is enough for floppy
				sta		SEKTRK
				ret
;SETSEC:
	;SET SECTOR GIVEN BY REGISTER C 
;	MOV	A,C
;	STA	SEKSEC		;SECTOR TO SEEK
;	RET
;
;------------------------------------------------------------------------------
; * SETSEC - Set sector
; I: BC=sektor nr (B=0)
; O: -
; M: AF
SETSEC:			mov		a,c				; sectors 1 .. 64
				;dcr	a				; change to 0 .. 63
				sta		SEKSEC			; save
				ret
;SETDMA:
	;SET DMA ADDRESS GIVEN BY BC
;	MOV	H,B
;	MOV	L,C
;	SHLD	DMAADR
;	RET
;
;------------------------------------------------------------------------------
; * SETDMA - Set DMA
; I: BC=DMA address
; O: -
; M: HL
SETDMA:			mov		h,b
				mov		l,c
				shld	DMAADR
				ret
;SECTRAN:
	;TRANSLATE SECTOR NUMBER BC
;	MOV	H,B
;	MOV	L,C
;	RET
;
;------------------------------------------------------------------------------
; * SECTRN - Sector translation
; I: BC=logical sector nr, DE=tran table address
; O: HL=physical sector nr
; M: HL
SECTRN:			mov		h,b					; no translation
				mov		l,c
				ret
;*********************************************************************** 
;*													 
;*	THE READ ENTRY POINT TAKES THE PLACE OF							   *
;*	THE PREVIOUS BIOS DEFINTION FOR READ.							   *
;*													 
;*********************************************************************** 
;READ:
read_flp:
	;READ THE SELECTED CP/M SECTOR
	XRA	A								;buggy DRI code PATCHED :-)
	STA	UNACNT
	MVI	A,1
	STA	READOP							;READ OPERATION
	STA	RSFLAG							;MUST READ DATA
	MVI	A,WRUAL
	STA	WRTYPE							;TREAT AS UNALLOC
	JMP	RWOPER							;TO PERFORM THE READ
;
;*********************************************************************** 
;*													 
;*	THE WRITE ENTRY POINT TAKES THE PLACE OF						   *
;*	THE PREVIOUS BIOS DEFINTION FOR WRITE.							   *
;*													 
;*********************************************************************** 
;WRITE:
write_flp:
	;WRITE THE SELECTED CP/M SECTOR
	XRA	A								;0 TO ACCUMULATOR
	STA	READOP							;NOT A READ OPERATION
	MOV	A,C								;WRITE TYPE IN C
	STA	WRTYPE
	CPI	WRUAL							;WRITE UNALLOCATED?
	JNZ	CHKUNA							;CHECK FOR UNALLOC
;
;	WRITE TO UNALLOCATED, SET PARAMETERS
	MVI	A,BLKSIZ/128					;NEXT UNALLOC RECS
	STA	UNACNT
	LDA	SEKDSK							;DISK TO SEEK
	STA	UNADSK							;UNADSK = SEKDSK
	LHLD	SEKTRK
	SHLD	UNATRK						;UNATRK = SECTRK
	LDA	SEKSEC
	STA	UNASEC							;UNASEC = SEKSEC
;
CHKUNA:
	;CHECK FOR WRITE TO UNALLOCATED SECTOR
	LDA	UNACNT							;ANY UNALLOC REMAIN?
	ORA	A
	JZ	ALLOC							;SKIP IF NOT
;
;	MORE UNALLOCATED RECORDS REMAIN
	DCR	A								;UNACNT = UNACNT-1
	STA	UNACNT
	LDA	SEKDSK							;SAME DISK?
	LXI	H,UNADSK
	CMP	M								;SEKDSK = UNADSK?
	JNZ	ALLOC							;SKIP IF NOT
;
;	DISKS ARE THE SAME
	LXI		H,UNATRK
	CALL	SEKTRKCMP					;SEKTRK = UNATRK?
	JNZ		ALLOC						;SKIP IF NOT
;
;	TRACKS ARE THE SAME
	LDA	SEKSEC							;SAME SECTOR?
	LXI	H,UNASEC
	CMP	M								;SEKSEC = UNASEC?
	JNZ	ALLOC							;SKIP IF NOT
;
;	MATCH, MOVE TO NEXT SECTOR FOR FUTURE REF
	INR	M								;UNASEC = UNASEC+1
	MOV	A,M								;END OF TRACK?
	CPI	CPMSPT							;COUNT CP/M SECTORS
	JC	NOOVF							;SKIP IF NO OVERFLOW
;
;	OVERFLOW TO NEXT TRACK
	MVI	M,0								;UNASEC = 0
	LHLD	UNATRK
	INX	H
	SHLD	UNATRK						;UNATRK = UNATRK+1
;
NOOVF:
	;MATCH FOUND, MARK AS UNNECESSARY READ
	XRA	A								;0 TO ACCUMULATOR
	STA	RSFLAG							;RSFLAG = 0
	JMP	RWOPER							;TO PERFORM THE WRITE
;
ALLOC:
	;NOT AN UNALLOCATED RECORD, REQUIRES PRE-READ
	XRA	A								;0 TO ACCUM
	STA	UNACNT							;UNACNT = 0
	INR	A								;1 TO ACCUM
	STA	RSFLAG							;RSFLAG = 1
;
;*********************************************************************** 
;*													 
;*	COMMON CODE FOR READ AND WRITE FOLLOWS							   *
;*													 
;*********************************************************************** 
RWOPER:
	;ENTER HERE TO PERFORM THE READ/WRITE
	XRA	A								;ZERO TO ACCUM
	STA	ERFLAG							;NO ERRORS (YET)
	LDA	SEKSEC							;COMPUTE HOST SECTOR
	;REPT	SECSHF
	;ORA	A							;CARRY = 0
	;RAR								;SHIFT RIGHT
	;ENDM
	ORA	A								;CARRY = 0
	RAR									;SHIFT RIGHT
	STA	SEKHST							;HOST SECTOR TO SEEK
;
;	ACTIVE HOST SECTOR?
	LXI	H,HSTACT						;HOST ACTIVE FLAG
	MOV	A,M
	MVI	M,1								;ALWAYS BECOMES 1
	ORA	A								;WAS IT ALREADY?
	JZ	FILHST							;FILL HOST IF NOT
;
;	HOST BUFFER ACTIVE, SAME AS SEEK BUFFER?
	LDA	SEKDSK
	LXI	H,drive_nr						;SAME DISK?
	CMP	M								;SEKDSK = HSTDSK?
	JNZ	NOMATCH
;
;	SAME DISK, SAME TRACK?
	LXI	H,track_nr
	CALL	SEKTRKCMP					;SEKTRK = HSTTRK?
	JNZ	NOMATCH
;
;	SAME DISK, SAME TRACK, SAME BUFFER?
	LDA	SEKHST
	LXI	H,unadj_sctr_nr						;SEKHST = HSTSEC?
	CMP	M
	JZ	MATCH							;SKIP IF MATCH
;
NOMATCH:
	;PROPER DISK, BUT NOT CORRECT SECTOR
	LDA	HSTWRT							;HOST WRITTEN?
	ORA	A
	CNZ	WRITEHST						;CLEAR HOST BUFF
;
FILHST:
	;MAY HAVE TO FILL THE HOST BUFFER
	LDA	SEKDSK
	STA	drive_nr
	LHLD SEKTRK
	SHLD track_nr
	LDA	SEKHST
	STA	unadj_sctr_nr
	LDA	RSFLAG							;NEED TO READ?
	ORA	A
	CNZ	READHST							;YES, IF 1
	XRA	A								;0 TO ACCUM
	STA	HSTWRT							;NO PENDING WRITE
;
MATCH:
	;COPY DATA TO OR FROM BUFFER
	LDA	SEKSEC							;MASK BUFFER NUMBER
	ANI	SECMSK							;LEAST SIGNIF BITS
	MOV	L,A								;READY TO SHIFT
	MVI	H,0								;DOUBLE COUNT
	;REPT	7							;SHIFT LEFT 7
	;DAD	H
	;ENDM
	DAD	H
	DAD	H
	DAD	H
	DAD	H
	DAD	H
	DAD	H
	DAD	H
;	HL HAS RELATIVE HOST BUFFER ADDRESS
	LXI	D,HSTBUF
	DAD	D								;HL = HOST ADDRESS
	XCHG								;NOW IN DE
	LHLD	DMAADR						;GET/PUT CP/M DATA
	MVI	C,128							;LENGTH OF MOVE
	LDA	READOP							;WHICH WAY?
	ORA	A
	JNZ	RWMOVE							;SKIP IF READ
;
;	WRITE OPERATION, MARK AND SWITCH DIRECTION
	MVI	A,1
	STA	HSTWRT							;HSTWRT = 1
	XCHG								;SOURCE/DEST SWAP
;
RWMOVE:
	;C INITIALLY 128, DE IS SOURCE, HL IS DEST
	LDAX	D							;SOURCE CHARACTER
	INX	D
	MOV	M,A								;TO DEST
	INX	H
	DCR	C								;LOOP 128 TIMES
	JNZ	RWMOVE
;
;	DATA HAS BEEN MOVED TO/FROM HOST BUFFER
	LDA	WRTYPE							;WRITE TYPE
	CPI	WRDIR							;TO DIRECTORY?
	LDA	ERFLAG							;IN CASE OF ERRORS
		RNZ								;NO FURTHER PROCESSING
;
;	CLEAR HOST BUFFER FOR DIRECTORY WRITE
	ORA	A								;ERRORS?
	RNZ									;SKIP IF SO
	XRA	A								;0 TO ACCUM
	STA	HSTWRT							;BUFFER WRITTEN
	CALL	WRITEHST
	LDA	ERFLAG
	RET
;
;*********************************************************************** 
;*													 
;*	UTILITY SUBROUTINE FOR 16-BIT COMPARE							   *
;*													 
;*********************************************************************** 
SEKTRKCMP:
	;HL = .UNATRK OR .HSTTRK, COMPARE WITH SEKTRK
	XCHG
	LXI	H,SEKTRK
	LDAX	D							;LOW BYTE COMPARE
	CMP	M								;SAME?
	RNZ									;RETURN IF NOT
;	LOW BYTES EQUAL, TEST HIGH 1S
	INX	D
	INX	H
	LDAX	D
	CMP	M								;SETS FLAGS
	RET
;
;*********************************************************************** 
;*													 
;*	WRITEHST PERFORMS THE PHYSICAL WRITE TO							   *
;*	THE HOST DISK, READHST READS THE PHYSICAL						   *
;*	DISK.															   *
;*													 
;*********************************************************************** 
WRITEHST:
	;HSTDSK = HOST DISK #, HSTTRK = HOST TRACK #,
	;HSTSEC = HOST SECT #. WRITE "HSTSIZ" BYTES
	;FROM HSTBUF AND RETURN ERROR FLAG IN ERFLAG.
	;RETURN ERFLAG NON-ZERO IF ERROR
		mvi		a,FDC_WRITE
		call	fd_exec_cmd
phyret: mvi		a,1						; error
		sta		ERFLAG
		rc
.l1		xra		a						; success
		sta		ERFLAG
		ret
;	RET
;
READHST:
	;HSTDSK = HOST DISK #, HSTTRK = HOST TRACK #,
	;HSTSEC = HOST SECT #. READ "HSTSIZ" BYTES
	;INTO HSTBUF AND RETURN ERROR FLAG IN ERFLAG.
		mvi		a,FDC_READ
		call	fd_exec_cmd
		jmp		phyret					;return
;	RET
		;
;*********************************************************************** 
;*													 
;*	UNINITIALIZED RAM DATA AREAS									   *
;*													 
;*********************************************************************** 
;
SEKDSK:	DS	1		;SEEK DISK 
SEKTRK:	DS	2		;SEEK TRACK 
SEKSEC:	DS	1		;SEEK SECTOR 
;
;HSTDSK:	DS	1		;HOST DISK 
;HSTTRK:	DS	2		;HOST TRACK 
;HSTSEC:	DS	1		;HOST SECTOR 
;
SEKHST:	DS	1		;SEEK SHR SECSHF
HSTACT:	DS	1		;HOST ACTIVE 
HSTWRT:	DS	1		;HOST WRITTEN 
;
UNACNT:	DS	1		;UNALLOC REC CNT
UNADSK:	DS	1		;LAST UNALLOC 
UNATRK:	DS	2		;LAST UNALLOC 
UNASEC:	DS	1		;LAST UNALLOC 
;
ERFLAG:	DS	1		;ERROR REPORTING
RSFLAG:	DS	1		;READ SECTOR 
READOP:	DS	1		;1 IF READ 
WRTYPE:	DS	1		;WRITE OPERATION 
DMAADR:	DS	2		;LAST DMA
;HSTBUF:	DS	HSTSIZ		;HOST BUFFER
;
;*********************************************************************** 
;*													 
;*	THE ENDEF MACRO INVOCATION GOES HERE							   *
;*													 
;*********************************************************************** 
