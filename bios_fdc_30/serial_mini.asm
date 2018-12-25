; minimalistic BIOS serial routines
;------------------------------------------------------------------------------
; * CONST - Console status
; Key press (if any) is stored in front
; I: -
; O: Z=0, A=255 - key press
;    Z=1, A=0   - no key press
; M: HL, AF
CONST:
QueueRdAdr:	in	USART_STAT	; 8251 status
		ani     02              ; char received?
        rz
        mvi     a,255
		ret

;------------------------------------------------------------------------------
; * CONIN - Console input - wait for keypress
; I: -
; O: A=kod klavesu
; M: HL, BC, AF
CONIN:	in	USART_STAT	; 8251 status
		ani     02              ; char received?
		jz	CONIN           ; no wait
        in      USART_DATA      ; get the char
		ret

;------------------------------------------------------------------------------
; * CONOUT - Console output
; I: A alebo C=char to output
; O: -
; M: AF
ConsoleOutputA:	mov	c,a
CONOUT:
ConsoleOutputC:	in	USART_STAT	; 8251 status
		rrc			; out buff free ?
		jnc	ConsoleOutputC	; no wait
		mov	a,c		; char to A
		out	USART_DATA	; send
		ret
