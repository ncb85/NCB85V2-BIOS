;------------------------------------------------------------------------------
; * Console services *
;------------------------------------------------------------------------------
; * Input *
; - terminals VT-100, ANSI
; - multi-byte key codes get transformed do one-byte - to make it easier for user
;------------------------------------------------------------------------------
; Key        | Scancode | Code |
;------------|----------|------|------------------------------------------------
; Up         | 1B 5B 41 | 0B   | ESC [ A
; Down       | 1B 5B 42 | 0C   | ESC [ B
; Right      | 1B 5B 43 | 0E   | ESC [ C
; Left       | 1B 5B 44 | 0F   | ESC [ D
; Home       | 1B 5B 48 | 06   | ESC [ H - ANSI mode
; End        | 1B 5B 4B | 05   | ESC [ K - ANSI mode
; Num Enter  | 1B 5B 4D | 07   | ESC [ M - VT100 application mode
; F1         | 1B 4F 50 | 01   | ESC O P
; F2         | 1B 4F 51 | 02   | ESC O Q
; F3         | 1B 4F 52 | 03   | ESC O R
; F4         | 1B 4F 53 | 04   | ESC O S
; Num +      | 1B 5B 6C | 1C   | ESC [ l - VT100 application mode
; Num -      | 1B 5B 6D | 1D   | ESC [ m - VT100 application mode
; Num .      | 1B 5B 6E | 1E   | ESC [ n - VT100 application mode
; Num 0      | 1B 5B 70 | 10   | ESC [ p - VT100 application mode
; Num 1      | 1B 5B 71 | 11   | ESC [ q - VT100 application mode
; Num 2      | 1B 5B 72 | 12   | ESC [ r - VT100 application mode
; Num 3      | 1B 5B 73 | 13   | ESC [ s - VT100 application mode
; Num 4      | 1B 5B 74 | 14   | ESC [ t - VT100 application mode
; Num 5      | 1B 5B 75 | 15   | ESC [ u - VT100 application mode
; Num 6      | 1B 5B 76 | 16   | ESC [ v - VT100 application mode
; Num 7      | 1B 5B 77 | 17   | ESC [ w - VT100 application mode
; Num 8      | 1B 5B 78 | 18   | ESC [ x - VT100 application mode
; Num 9      | 1B 5B 79 | 19   | ESC [ y - VT100 application mode
;------------------------------------------------------------------------------
; Backspace  | 08       | 08   | Backspace
; Tab        | 09       | 09   | Tab
; Ctrl+Enter | 0A       | 0A   | Ctrl+Enter / Ctrl+Num Enter
; Enter      | 0D       | 0D   | Enter
; Esc        | 1B       | 1B   | Esc
; Ctrl+BS    | 7F       | 7F   | Delete
;------------------------------------------------------------------------------
; * CONST - Console status
; Key press (if any) is stored in front
; I: -
; O: Z=0, A=255 - key press
;    Z=1, A=0   - no key press
; M: HL, AF
CONST:
QueueRdAdr:	lxi	h,0		; pointer to front
		mov	a,m		; valid char in front ?
		ora	a
		rz			; no, get back A=0
		mvi	a,255		; yes, A=255
		ret

;------------------------------------------------------------------------------
; * CONIN - Console input - cakanie na stlacenie klavesu *
; I: -
; O: A=kod klavesu
; M: HL, BC, AF
CONIN:		lhld	QueueRdAdr+1	; ukazatel do fronty
		mov	a,m		; je platny znak vo fronte ?
		ora	a
		jz	CONIN		; nie, cakaj
		mov	b,a		; uloz kod klavesu do B
		mvi	m,0		; vynuluj aktualnu polozku
		call	IncQueueAdr	; posun ukazatel
		mov	a,b		; kod klavesu nazad do A
		cpi	ESC		; je to ESC ?
		jnz	ConInE		; nie, vrat sa s danym kodom
		call	ConInWait	; pockaj na dalsi byte z konzoly
		jz	ConInE		; neprisiel, vrat sa s kodom ESC
		sui	'O'		; je to 'O' ?
		jz	ConInA		; ano, skoc dalej
		sui	'['-'O'		; je to '[' ?
		jnz	ConInE		; nie, vrat sa s kodom ESC
ConInA:		mov	m,a		; vynuluj aktualnu polozku
		call	IncQueueAdr	; posun ukazatel
		call	ConInWait	; pockaj na dalsi byte z konzoly
		jz	ConInE		; neprisiel, vrat sa s kodom ESC
		mov	b,a		; uloz si prijaty kod
		mvi	m,0		; vynuluj aktualnu polozku
		call	IncQueueAdr	; posun ukazatel
		mov	a,b
		call	DecodeEsc	; dekoduj Esc kod
		mov	b,a
		jnc	ConInE		; skoc, ak je kod v poriadku
		mvi	b,ESC		; pre neznamy ESC kod vrat samotne ESC
ConInE:		shld	QueueRdAdr+1	; uloz novy ukazatel
		mov	a,b		; kod znaku do A
		ret

;------------------------------------------------------------------------------
; Pocka na byte z konzoly stanoveny cas - asi 2ms.
; I: HL=adresa vo fronte
; O: Z=0, A=byte z konzoly
;    Z=1, A=0, C=0 - neprisiel dalsi byte
; M: B, AF
ConInWait:	mvi	c,255		; pockaj asi 2ms na dalsi byte
ConInWaitL:	mov	a,m		; precitaj byte z fronty
		ora	a		; je tam dalsi znak ?
		rnz			; ano, vrat sa
		dcr	c		; zniz pocitadlo timeoutu
		jnz	ConInWaitL
		ret

;------------------------------------------------------------------------------
; I: A=kod, ktory sa ma prekodovat
; O: CY=0 - A=novy kod
;    CY=1 - chybny vstupny kod
; M: A, C
DecodeEsc:	cpi	'A'		; je to kod >= 'A' ?
		rc			; nie, vrat sa s CY=1
		cpi	'D'+1		; je to kod 'A' az 'D' ?
		jnc	DecodeEsc2	; nie, skoc dalej
		sui	'A'-0Bh		; 'A' -> 0Bh, 'B' -> 0Ch
		cpi	0Dh		; ODh je CR, tak ho vynechame
		cmc
		rnc
		inr	a		; 'C' -> 0Eh, 'D' -> 0Fh
		ora	a
		ret

DecodeEsc2:	cpi	'P'		; je to kod >= 'P' ?
		jc	DecodeEsc3	; nie, skoc dalej
		cpi	'S'+1		; je to kod 'P' az 'S' ?
		jnc	DecodeEsc4	; nie, skoc dalej
		sui	'P'-1		; 'P' az 'S' -> 01h az 04h
		ret

DecodeEsc3:	mvi	c,5		; 'K' -> 05h
		cpi	'K'		; je to kod 'K' ?
		jz	DecodeEsc3A	; ano skoc, dalej
		inr	c		; 'H' -> 06h
		cpi	'H'		; je to kod 'H' ?
		jz	DecodeEsc3A	; ano skoc, dalej
		cpi	'M'		; je to kod 'M' ?
		stc
		rnz			; nie, vrat sa s CY=1
		inr	c		; 'M' -> 07h
DecodeEsc3A:	mov	a,c
		ora	a
		ret

DecodeEsc4:	cpi	'l'		; je to kod >= 'l' ?
		rc			; nie, vrat sa s CY=1
		cpi	'o'		; je to 'o' ?
		cmc
		rz			; ano, vrat sa s CY=1
		jc	DecodeEsc5	; kod >= 'p', skoc
		sui	'l'-1Ch		; 'l' az 'n' -> 1Ch az 1Eh
		ret

DecodeEsc5:	cpi	'y'+1		; je to kod 'p' az 'y' ?
		cmc
		rc			; nie, vrat sa s CY=1
		sui	'p'-10h		; 'p' az 'y' -> 10h az 19h
		ret

;------------------------------------------------------------------------------
; Posun ukazatela do klavesnicovej fronty na dalsiu polozku
; I: HL=adresa do fronty
; O: HL=nova adresa do fronty
; M: AF, HL
IncQueueAdr:	inx	h		; posun ukazatel
		mov	a,l		; dosiahol si koniec fronty ?
		cpi	(KeyQueue+KeyQueSiz) & 255
		rnz			; nie, navrat
		lxi	h,KeyQueue	; nastav sa na zaciatok fronty
		ret

;------------------------------------------------------------------------------
; Inicializacia prerusenia od USARTu.
; I: -
; O: -
; M: AF, HL, B
InitSerialInt:	di			; zakaz prerusenia
		mvi	a,11110b	; povolenie prerusenia RST5.5
		sim			; zapis masku preruseni
		mvi	a,0C3h		; vytvor skokovu instrukciu na rutinu
		sta	Rst55Vect	; pre prijatie bytu z konzoly (USARTu)
		lxi	h,SerialInt
		shld	Rst55Vect+1
		lxi	h,KeyQueue	; inicializuj klavesnicovu frontu
		shld	QueueWrAdr+1	; ukazatel pre zapis do fronty
		shld	QueueRdAdr+1	; ukazatel pre citanie z fronty
		xra	a		; vynuluj frontu
		mvi	b,KeyQueSiz
InitSerialIntL:	mov	m,a		;  aktualny znak vo fronte
		inx	h
		dcr	b
		jnz	InitSerialIntL
		ei			; povol prerusenie
		ret

;------------------------------------------------------------------------------
; Prerusenie od USARTu - prijem znaku z konzoly
; Potrebuje na zasobniku 4 byty - volanie prerusenia a AF.
; HL sa odpamatava osobitne, aby samotne prerusenie nezaberalo v zasobniku
; volajucej strany vela miesta.
; I: -
; O: -
; M: -
SerialInt:	push	psw		; odpamataj AF
		shld	OldHL		; odpamataj HL
		in	USART_DATA	; precitaj znak zo vstupu
		sta	UsartByte
QueueWrAdr:	lxi	h,0		; ukazatel zapisu
		mov	a,m		; je volna polozka ?
		ora	a
		jnz	SerialIntE	; nie, vrat sa z prerusenia
		lda	UsartByte
		mov	m,a		; uloz prijaty kod do fronty
		call	IncQueueAdr	; posun ukazatel
		shld	QueueWrAdr+1	; a uloz
SerialIntE:	lhld	OldHL
		pop	psw
		ei			; povol prerusenie
		ret			; a navrat

;------------------------------------------------------------------------------
; * CONOUT - Console output - Vystup na konzolu *
; I: A alebo C=znak posielany na vystup
; O: -
; M: AF
ConsoleOutputA:	mov	c,a
CONOUT:
ConsoleOutputC:	in	USART_STAT	; vezmi stav 8251
		rrc			; je vystup volny ?
		jnc	ConsoleOutputC	; nie, cakaj
		mov	a,c		; znak do A
		out	USART_DATA	; posli na vystup
		ret

;------------------------------------------------------------------------------
KeyQueSiz	equ	96
KeyQueue:	ds	KeyQueSiz	; keyboard buffer

