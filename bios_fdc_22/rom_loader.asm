; rom loader - loads BIOS from ROM (0000H) to RAM (e.g. F300H, depends on BIOS size)

		di
		lxi	sp,8000h		; stack to 8000

		lxi	h,QBaseBIOS	; copy BIOS to top RAM
		lxi	d,BaseBIOS
St5:		mov	a,m
		stax	d

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

