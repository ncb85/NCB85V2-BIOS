; minimalistic BIOS serial routines
; using UART port of 8256 MUART
;------------------------------------------------------------------------------
; * CONST - Console status
; Key press (if any) is stored in front
; I: -
; O: Z=0, A=255 - key press
;    Z=1, A=0   - no key press
; M: HL, AF
QueueRdAdr:
CONST:  in  MUART_ADDR+0Fh		; 8256 status
		ani 40h					; char received?
        rz
        mvi a,255				; key pressed
		ret
		;
;------------------------------------------------------------------------------
; * CONIN - Console input - wait for keypress
; I: -
; O: A=key code
; M: HL, BC, AF
CONIN:	in	MUART_ADDR+0Fh		; 8256 status
		ani 40h	                ; char received?
		jz	CONIN				; no wait
        in  MUART_ADDR+07		; get the char
		ret
		;
;------------------------------------------------------------------------------
; * CONOUT - Console output 
; I: A or C=char to output
; O: -
; M: AF
ConsoleOutputA:	mov	c,a
ConsoleOutputC:
CONOUT: in	MUART_ADDR+0Fh		; 8256 status
		ani 20h					; out buffer free ?
		jz	ConsoleOutputC		; no wait
		mov	a,c					; char to A
		out	MUART_ADDR+07		; send
		ret
