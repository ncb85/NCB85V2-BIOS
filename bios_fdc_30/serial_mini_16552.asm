; minimalistic BIOS serial routines
; for PC16522 UART - channel 1
;------------------------------------------------------------------------------
; * CONST - Console status
; Key press (if any) is stored in front
; I: -
; O: Z=0, A=255 - key press
;    Z=1, A=0   - no key press
; M: HL, AF
CONST:
QueueRdAdr:	in	LINE_STATUS_REG_1   ;16552 status channel 1
		ani     01                  ; char received?
                rz
                mvi     a,255
		ret

;------------------------------------------------------------------------------
; * CONIN - Console input - wait for keypress *
; I: -
; O: A=kod klavesu
; M: HL, BC, AF
CONIN:		in	LINE_STATUS_REG_1   ;16552 status channel 1
		ani     01                  ; char received?
                jz	CONIN               ; no wait
                in      RECEIVER_BUFFER_REG_1   ; get the char
		ret

;------------------------------------------------------------------------------
; * CONOUT - Console output - Vystup na konzolu *
; I: A alebo C=znak posielany na vystup
; O: -
; M: AF
ConsoleOutputA:	mov	c,a
CONOUT:
ConsoleOutputC: in      LINE_STATUS_REG_1   ; 16552 channel 1
		ani     20h
		jz	ConsoleOutputC      ; wait
		mov	a,c                 ; char to A
		out	TRANSMITTER_BUFFER_REG_1	; send  
		ret

