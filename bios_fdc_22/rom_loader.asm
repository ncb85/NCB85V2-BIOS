; rom loader - loads BIOS from ROM (0000H) to RAM (e.g. F300H, depends on BIOS size)

		di
		lxi	sp,8000h		; stack to 8000

                ifdef DEBUG
		; init USART 8251
		xra	a		; be sure to be in Command mode
		out	USART_CWR	; 3x 00
		out	USART_CWR
		out	USART_CWR
		mvi	a,USART_RESET	; Reset 8251
		out	USART_CWR
		mvi	a,USART_CMD1	; 8 data, 1 stop, x16
		out	USART_CWR	; RxC a TxC = 307,2 kHz -> 19200 Bd
		mvi	a,USART_CMD2	; RTS, DTR, Enable RX, TX
		out	USART_CWR

                lxi	h,Sign		; print msg
		call	Prt0
                endif

		lxi	h,QBaseBIOS	; copy BIOS to top RAM
		lxi	d,BaseBIOS
St5:		mov	a,m
		stax	d

                ifdef DEBUG
                mov     a,e
                ora     a               ; each 256 bytes one dot
                jnz     .f1
                mvi     c,"."
                call    ConOutput
                endif

.f1             inx	h
		inx	d
                mov     a,d
                ora     e
		jnz	St5
		jmp	BaseBIOS        ;run BIOS from target RAM now

;------------------------------------------------------------------------------
; Check if RAM/ROM switching works
; Ruotine moves itself to top RAM, not to be disabled together with ROM where it is now
; I: -
; O: Z=1 - switching works
;    Z=0 - mem paging not working
; M: HL, DE, B, AF
CheckMem:	lxi	d,CheckMemR+CheckMemL	; move routine to the ram end
		lxi	h,CheckMemAdr+CheckMemL
		mvi	b,CheckMemL
CheckMem2:	dcx	d
		dcx	h
		ldax	d
		mov	m,a
		dcr	b
		jnz	CheckMem2
		pchl

CheckMemR:	mvi	a,XRAM		; disable ROM
		sim
		lxi	h,0		; RAM now from address 0
		mov	a,m		; invert byte at address 0
		cma
		mov	m,a
		cmp	m		; compare, really changed?
		cma			;   - ROM vs. RAM
		mov	m,a		; restor value
		mvi	a,XROM		; switch ROM on again
		sim
		ret
CheckMemL	equ	$-CheckMemR

                ifdef DEBUG
;------------------------------------------------------------------------------
; Input from console without waiting
; I: -
; O: Z=1 - no char at uart, A=0
;    Z=0, A=input char
; M: AF
; T: 29 / 47
ConInNW:	in	USART_STAT	; get 8251 state
		ani	00000010b	; is char at input ?
		rz			; no, return
		in	USART_DATA	; yes, read it
		ora	a
		ret

;------------------------------------------------------------------------------
; Output to console
; I: C=char to send
; O: -
; M: AF
ConOutput:	in	USART_STAT	; get 8251 state
		rrc			; output ready ?
		jnc	ConOutput	; no, wait
		mov	a,c		; char to A
		out	USART_DATA	; send to UART
		ret

;------------------------------------------------------------------------------
; Print zero terminated text
; I: HL=text address
; O:
; M: HL, AF, C
Prt0:		mov	a,m
		inx	h
		ora	a
		rz
		mov	c,a
		call	ConOutput
		jmp	Prt0

Sign:		;db	ESC,"[2J"	; clear console
		;db	ESC,"[24;1H"	; set cursor to botom line
		db	"Loading BIOS from ROM", 0

                endif
