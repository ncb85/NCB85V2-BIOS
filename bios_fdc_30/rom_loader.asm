;------------------------------------------------------------------------------
; ROM loader - loads BIOS from EPROM (0000H) to RAM (e.g. F500H, depends on BIOS size)
; this is the very first routine executed after POWER ON
;------------------------------------------------------------------------------
;				 
		di								;disable all interrupts
		lxi	sp,8010h					;stack to 8010
										;source, destination, length
		lxi	h,QBaseBIOS					;eprom address
		lxi	d,BaseBIOS					;copy BIOS to RAM top
		lxi		b,XBIOSLength			;counter
St5:	mov	a,m							;read from EPROM
		stax	d						;store at RAM top
.f1		inx	h							;increment EPROM address
		inx	d							;increment destination RAM address
		dcx		b						;decrement counter
		mov		a,c						;counter lower byte
		ora		b						;upper byte
		jnz	St5							;if not zero continue
										;BIOS copied
		jmp	BaseBIOS					;run BIOS from target RAM now

