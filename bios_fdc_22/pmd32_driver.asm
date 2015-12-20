;
;PMD32SD is a SD card emulator of 1980's diskette drive for 8080 based PMD85 computers
;http://pmd85.borik.net/wiki/PMD_32
;http://pmd85.borik.net/wiki/Blog:Postavte_si_PMD_32-SD
;
; interface uses Mode2 of 8255 PIO
;
                if NumPmd32>0
; init PIO, Group A mode 2, Group B input
PIO_CMD		equ	11000011b	; init PIO 

; bit mask INTRA
INTRA		equ	8

; enable/disable INTRA for I/o
INTRA_IN_OFF	equ	08h
INTRA_IN_ON	equ	09h
INTRA_OUT_OFF	equ	0Ch
INTRA_OUT_ON	equ	0Dh

;------------------------------------------------------------------------------
; Command "Q" - read sector
; I: disk, sector and track at adresses Drive, Sector, Track
;    source addresa at address Dma
; O: CY = 1 - chyba timeout, CRC
;    CY = 0 - OK
; M: all
ReadSector:	mvi	a,'Q'		; povel "Q" - precitaj sektor
		call	SendCommand	; odosli na vystup
		rc			; vrat sa, ak doslo ku chybe
		call	SendSecTrk	; posli stopu a sektor
		rc			; vrat sa, ak doslo ku chybe
		call	SndCrcRdAck	; posli CRC a pockaj na potvrdenie
		rc			; vrat sa, ak doslo ku chybe
                mvi	b,0		; vynuluj CRC
		mvi	d,128		; nastav pocitadlo
		lhld	Dma		; do HL adresa pamati
ReadSector2:	call	ReadByte	; precitaj byte
		rc			; vrat sa, ak doslo ku chybe
		mov	m,a		; uloz byte do pamati
		inx	h		; posun ukazatel
		dcr	d		; zniz pocitadlo prijimanych bytov
		jnz	ReadSector2	; ak neboli prijate vsetky, vrat sa do slucky
		jmp	ReadCheckCrc	; skoc precitat CRC

;------------------------------------------------------------------------------
; Command "T" - write sector
; I: disk, sector and track at adresses Drive, Sector, Track
;    source addresa at address Dma
; O: CY = 1 - chyba timeout, CRC
;    CY = 0 - OK
; M: all
WriteSector:	mvi	a,'T'		; povel "T" - zapis sektor
		call	SendCommand	; odosli na vystup
		rc			; vrat sa, ak doslo ku chybe
		call	SendSecTrk	; posli stopu a sektor
		rc			; vrat sa, ak doslo ku chybe
		mvi	d,128		; nastav pocitadlo
		lhld	Dma		; do HL adresa pamati
WriteSector2:	mov	a,m		; byte z pamati
		call	SendByte	; odosli
		rc			; vrat sa, ak doslo ku chybe
		inx	h		; posun ukazatel
		dcr	d		; zniz pocitadlo bytov
		jnz	WriteSector2	; opakuj pre vsetkych 128 bytov
		jmp	SndCrcRdAck	; odosli CRC a cakaj na potvrdenie

;------------------------------------------------------------------------------
; Send disk number, track number and sector number to PMD32SD
; I: disk, sector and track at adresses Drive, Sector, Track
;    B=running CRC
; O: CY = 1 - err
;    CY = 0 - OK, B = A = modifikovane CRC
; M: AF, BC
SendSecTrk:	lda	Drive		; cislo disku do 7. a 6. bitu
		ora	a
		jz	SendSecTrk2
       		mvi	c,0C0h
		xri	3
		jz	SendSecTrk3
		rrc
		rrc
SendSecTrk2:	mov	c,a		; C => 00h=A: 80h=B: 40h=C: 0C0h=D:
SendSecTrk3:	lda	Sector		; vezmi sektor
		ora	c		; pridaj disk
		call	SendByte	; a odosli
		rc			; navrat pri chybe
		lda	Track		; vezmi stopu
       		jmp	SendByte	; a odosli

;------------------------------------------------------------------------------
; Send CRC and wait for ACK and result.
; I: B=current CRC
; O: CY=1 - err
;    CY=0 - OK, A=result
; M: AF, BC, DE
SndCrcRdAckErr:	mov	a,b		; posielane CRC do A
		call	SendByte	; posli na vystup
		rc			; vrat sa, ak doslo ku chybe
ReadAckErr:	call	EnaByteIn	; povol prerusenie na vstupe
		call	ReadByte	; precitaj byte zo vstupu
		rc			; vrat sa, ak doslo ku chybe
		cpi	33h		; bol prijaty byte ACK?
		stc			; nastav chybovy priznak
		rnz			; vrat sa s chybou, ak to nebolo ACK
WaitErrT15:	lxi	d,1500*(CRYSTAL/2) ; timeout asi 15,5 sec
WaitErrT:	push	d		; DE na zasobnik
		call	IsIntr		; je platny byte na vstupe?
		pop	d		; obnov DE
		jnc	ReadByte	; ak ano, skoc ho precitat
		dcx	d		; zniz pocitadlo timeoutu
		jnk	WaitErrT	; ak nepretiekol, skoc do slucky
       		stc			; nastav priznak chyby
		ret

;------------------------------------------------------------------------------
; Send CRC and wait for acknowledge. Test result code for zero.
; I: B=to be sent CRC
; O: CY=1 - err
;    CY=0 - OK
; M: AF, BC, DE
SndCrcRdAck:	call	SndCrcRdAckErr	; posli CRC a cakaj ACK a vysledok povelu
		rc			; vrat sa, ak doslo ku chybe
		jmp	ReadCheckCrc2	; ak bol druhy byte skutocne 0, je to OK

;------------------------------------------------------------------------------
; Read byte form input, it is CRC
; I: -
; O: CY=1 - chyba CRC alebo timeout
;    CY=0 - CRC OK
; M: AF, BC
ReadCheckCrc:	call	ReadByte	; precitaj CRC zo vstupu
		rc			; vrat sa, ak doslo ku chybe
		mov	a,b		; skontroluj CRC
ReadCheckCrc2:	ora	a
		rz			; OK
		stc			; nastav priznak chyby
                ret

;------------------------------------------------------------------------------
; Check PMD 32 for presence
; I: -
; O: CY=1 - err, timeout, PMD 32 does not answer with presentaion byte
; M: AF, DE, BC
CheckPMD32:	call	EnaByteIn	; povol prerusenie pre vstup
		lxi     d,6500*(CRYSTAL/2) ; timeout asi 300ms
		call    IsIntrD		; cakaj na byte
		rc			; timeout, vrat sa
		in      PIO_PA		; precitaj prijaty byte
		cpi	0AAh		; je to prezentacny byte 0AAh ?
		stc			; nastav CY ako priznak chyby
		rnz			; nie je to prezent. byte 0AAh, vrat sa s CY
		call	EnaByteOut	; povol prerusenie pre vystup
		mvi	a,0AAh		; posli ako odpoved prezent. byte 0AAh
		jmp	SendByte

;------------------------------------------------------------------------------
; Read byte from PMD 32 and modify CRC.
; I: B=running CRC
; O: CY=1 - chyba
;    CY=0 - A=C=precitany byte, B=modifikovane CRC
; M: AF, BC
ReadByte:	push	d		; DE na zasobnik
		call	IsIntr		; je platny byte na vstupe?
		pop	d		; obnov DE
		rc			; nie, vrat sa s chybou
		in	PIO_PA		; precitaj byte
		mov	c,a		; odloz si ho do C
		xra	b		; modifikuj CRC
		mov	b,a		; a uloz ho naspat do B
		mov	a,c		; vrat nacitany byte do A
		ret

;------------------------------------------------------------------------------
; Send command to PMD 32. Checks for prezentation byte before (should not happen)
; I: A=cmd
; O: CY=1 - err
;    CY=0 - OK, B=A=modified CRC
; M: AF, BC
SendCommand:	mov	b,a		; uloz posielany byte do B
		push	b		; BC, DE na zasobnik
		push	d
		call    IsPresent	; zisti, ci je na zbernici prezentacny
					; byte 0AAh, ak ano, odosli naspat
					; prezentacny byte
		pop	d		; obnov DE, BC
		pop	b
		rc			; vrat sa, ak doslo ku chybe
		mov     a,b		; vrat posielany byte do A
		mvi	b,0		; vynuluj CRC
; fall through
 
;------------------------------------------------------------------------------
; Send byte to PMD 32 and modify running CRC.
; I: A=byte to send, B=runing CRC
; O: CY=1 - err
;    CY=0 - OK, B=A=modified CRC
; M: AF, B
SendByte:	push	d		; DE on stack
		call	IsIntr		; zisti, ci je volny vystup
		pop	d		; obnov DE
		rc			; vrat sa, ak doslo ku timeoutu
		out	PIO_PA		; posli byte na vystup
		xra	b		; modifikuj CRC
		mov	b,a		; a uloz ho naspat do B
		ret

;------------------------------------------------------------------------------
; Check for prezentation byte 0AAh
; I: -
; O: CY=1 - chyba
;    CY=0 - OK
; M: AF, DE
IsPresent:	call	EnaByteIn	; povol prerusenie pre vstup
		call	WaitQ		; male zdrzanie asi 90 us
		in	PIO_PC
		ani	INTRA		; je platny byte na vstupe?
		jz	EnaByteOut	; ak nie, vrat sa cez povolenie prerusenia pre vystup
IsPresent5:	call	EnaByteIn	; povol prerusenie pre vstup
IsPresent3:	in	PIO_PA		; precitaj byte zo vstupu
		cpi	0AAh		; ak je to prezentacny byte 0AAh
		jz	IsPresent2	; skoc poslat prezentacny byte 0AAh na vystup
		lxi	d,2500*(CRYSTAL/2) ; timeout asi 125 ms
		call	IsIntrD		; cakaj na byte
		rc			; timeout, vrat sa
		jmp	IsPresent3	; ano, skoc precitat byte zo vstupu

IsPresent2:	call	WaitQ		; male zdrzanie asi 90 us
		call	EnaByteOut	; povol prerusenie pre vystup
		mvi	a,0AAh		; posli na vystup prezentacny byte 0AAh
		out	PIO_PA
		lxi     d,2500*(CRYSTAL/2) ; timeout asi 125 ms
		call    IsIntrD		; cakaj na prijatie prezetacneho bytu
		jc	IsPresent5	; ak nebol prijaty, skoc otestovat vstup
		ret			; prezentacny byte bol prijaty,
					; vrat sa s vynulovanym CY

;------------------------------------------------------------------------------
; Check for INTRA. Wait some time
; INTRA=1, if valid byte at input, or byte was read from output.
; I: when called from IsIntrD DE=timeout
; O: CY=1 - timeout
;    CY=0 - OK
; M: F, DE
IsIntr:		lxi	d,102*(CRYSTAL/2) ; timeout 5ms
IsIntrD:	push	psw		; save AF
IsIntr2:	in	PIO_PC  	; check INTRA
		ani	INTRA
		jnz	IsIntr3		; if INTR=1, jump forward
		dcx	d
		jnk	IsIntr2		; if not zero do loop
		pop	psw		; timeoutu, restore AF
		stc			; set CY as error flag
		ret
IsIntr3:	pop	psw
		ora	a		; clear CY
		ret

;------------------------------------------------------------------------------
; Enable interrupt from input on port A
; I: -
; O: enables interrupt for byte input
; M: A
;------------------------------------------------------------------------------
EnaByteIn:	mvi	a,INTRA_IN_ON	; 1->C4 enable interrupt signal INTRA for input A
                out	PIO_CWR
		mvi	a,INTRA_OUT_OFF	; 0->C6 disable interrupt signal INTRA for output A
		out	PIO_CWR		;
		ret

;------------------------------------------------------------------------------
; Enable interrupt output of port A
; I: -
; O: enables interrupt for byte output
; M: A
;------------------------------------------------------------------------------
EnaByteOut:	mvi	a,INTRA_IN_OFF	; 0->C4 disable interrupt signal INTRA for input A
		out	PIO_CWR		;
		mvi	a,INTRA_OUT_ON	; 1->C6 enable interrupt signal INTRA for output A
		out	PIO_CWR		;
		ret

                endif