;------------------------------------------------------------------------------
; NCB85 CP/M BIOS
; Roman Borik/RM-Team 2/2012, 2/2013, 1/2014
; archeocomp 2014-2015
; NCB85 is a SBC based on 8085 and 64k SRAM. There is also 8kB ROM from 0000H
; that can be shadowed by outputting 1 to SOD pin of 8085
; BIOS is in the ROM and moved to target RAM after reset. CP/M is loaded from diskette drive
;------------------------------------------------------------------------------
;DEBUG           equ     1               ; floppy driver debug on uart 2.2
            
		cpu	8085undoc

jnk		macro	adr
		jnx5	adr
		endm

jk		macro	adr
		jx5	adr
		endm

;------------------------------------------------------------------------------

CPMVer		equ	22		; CP/M ver 2.2
BIOSVer		equ	21		; BIOS ver 2.1
CRYSTAL		equ	12		; MHz

CPMSource       equ     "ROM"           ; when defined - CP/M in ROM, otherwise
;CPMSource       equ     "FLP"           ; traditionally loaded from system track
; step rate, head settle time
FloppySpeed     equ     "FAST"          ;for TEAC drives
;FloppySpeed     equ     "MEDIUM"        ;slower drives MITSUMI?
; drive capacity
;Floppy          equ     360             ; drive type - 360kB/5.25"
;Floppy          equ     720             ; drive type - 720kB/5.25"
;Floppy          equ     120             ; drive type - 1.2MB/5.25"
Floppy          equ     144             ; drive type - 1.44MB/3.5"
; drives used
NumPmd32        equ     0               ; number of PMD32 drives
NumFlps         equ     2               ; number of floppy drives
NumDisks	equ	NumPmd32+NumFlps       ; drives num (PMD32SD+floppies)

CCPLength	equ	0800h		; CCP len
BDOSLength	equ	0E00h		; BDOS len
BIOSLengthMax	equ	1000h		; BIOS (Max) len
CcpBdosSec	equ	(CCPLength+BDOSLength)/128 ; CCP + BDOS len in sectors
; be sure to include (or not include) correct cp/m image at the end of file
            if (CPMSource <> "ROM")
BaseBIOS	equ	0EE00h		; BIOS address (with PMD32 driver, debug routines..)
            else
                if (Floppy == 360)
BaseBIOS	equ	0F500h		; BIOS address (with CPM bundled in 8k ROM) for 2x360
                elseif (Floppy==144) 
BaseBIOS	equ	0F400h		; BIOS address (with CPM bundled in 8k ROM) for 2x1.44
                endif
            endif
BaseCCP		equ	BaseBIOS-(CCPLength+BDOSLength) ; CCP address
BaseBDOS	equ	BaseCCP+CCPLength+6 ; BDOS address
                message "BIOS begins at \{BaseBIOS}"
                message "CCP begins at \{BaseCCP}"
IOByte		equ	0003h		; IO byte (not used)
CurrentDisk	equ	0004h		; default disk
Buffer		equ	0080h		; default DMA address
Retry		equ	5		; disk retries
Rst55Vect	equ	002Ch		; RST5.5 (USART)
Rst65Vect	equ	0034h		; RST6.5 (TIMER)

BEL		equ	7
CR		equ	0Dh
LF		equ	0Ah
EOF		equ	1Ah
ESC		equ	1Bh
SPACE		equ	20h
;------------------------------------------------------------------------------
; IO ports
PIO_PA		equ	10h
PIO_PB		equ	11h
PIO_PC		equ	12h
PIO_CWR		equ	13h

USART_DATA	equ	8
USART_CWR	equ	9
USART_STAT	equ	9

; paging ROM/RAM - values for SIM  instruction
XROM		equ	40h
XRAM		equ	0C0h

; init USART
USART_RESET	equ	01000000b
USART_CMD1	equ	01001110b
USART_CMD2	equ	00010101b

; USART 16552
FIFO_CTRL_REG_1         equ     2Ah
FIFO_CTRL_REG_2         equ     22h
LINE_CTRL_REG_1         equ     2Bh
LINE_CTRL_REG_2         equ     23h
DIVISOR_LOW_BYTE_1      equ     28h
DIVISOR_HIGH_BYTE_1     equ     29h
DIVISOR_LOW_BYTE_2      equ     20h
DIVISOR_HIGH_BYTE_2     equ     21h
LINE_STATUS_REG_1       equ     2Dh
TRANSMITTER_BUFFER_REG_1 equ    28h
RECEIVER_BUFFER_REG_1    equ    28h
LINE_STATUS_REG_2        equ    25h
TRANSMITTER_BUFFER_REG_2 equ    20h
;------------------------------------------------------------------------------
Mon85Entry	equ	010Bh		; MON85 from Dave Dunfield start point
CheckMemAdr	equ	0FFD0h		; ram paging test routine runtime address

;------------------------------------------------------------------------------
;******************************************************************************
;------------------------------------------------------------------------------
		org	0
Boot:
                include "rom_loader.asm"
;------------------------------------------------------------------------------
;******************************************************************************
;------------------------------------------------------------------------------

QBaseBIOS:	equ	$

                phase	BaseBIOS
; BIOS entry points
CBOOTE:		jmp	CBOOT		; Cold boot
WBOOTE:		jmp	WBOOT		; Warm Boot
		jmp	CONST		; Console status
		jmp	CONIN		; Console input
		jmp	CONOUT		; Console output
		jmp	LIST		; List (printer) output
		jmp	PUNCH		; Character output
		jmp	READER		; Character input
		jmp	HOME		; Go to track 0
		jmp	SELDSK		; Select disk
		jmp	SETTRK		; Set track
		jmp	SETSEC		; Set sector
		jmp	SETDMA		; Set DMA
		jmp	READ		; Read sector
		jmp	WRITE		; Write sector
		jmp	LISTST		; List (printer) status
		jmp	SECTRN		; Sector translation

            if NumPmd32>0
		jmp	ReadByte	; read byte from PMD 32-SD
		jmp	SendCommand	; send cmd
		jmp	SendByte	; send byte
		jmp	SndCrcRdAckErr	; send CRC and receive ACK and ERR
		jmp	ReadAckErr	; receive ACK and ERR
		jmp	WaitErrT15	; wait for error code 15s
		jmp	WaitErrT	; wait for error code selected time
		jmp	SndCrcRdAck	; send CRC and receive ACK
		jmp	ReadCheckCrc	; receive and check CRC
            endif
;------------------------------------------------------------------------------
; Disk parameters
DPH:
            if NumPmd32>0
DPH0:		dw	0,0		; no translation table
		dw	0,0
		dw	DIRBUF,DPB	; buff.adr., disk param adr.
		dw	CSV0,ALV0	; checksum zone adr., aloc bit map adr.
DPH1:		dw	0,0		; no translation table
		dw	0,0
		dw	DIRBUF,DPB	; buff.adr., disk param adr.
		dw	CSV1,ALV1	; checksum zone adr., aloc bit map adr.
            endif
DPH4:		dw	0,0		; no translation table
		dw	0,0
		dw	DIRBUF,DPB4	; buff.adr., disk param adr.
		dw	CSV4,ALV4	; checksum zone adr., aloc bit map adr.
DPH5:		dw	0,0		; no translation table
		dw	0,0
		dw	DIRBUF,DPB4     ; buff.adr., disk param adr.
		dw	CSV5,ALV5	; checksum zone adr., aloc bit map adr.

; PMD32SD
; size 2MB
; 256 tracks, 64 sectors/track
; 1020 allocation 2kb blocks
; 256 dir size
; 1 system track
            if NumPmd32>0
DPB:            dw 64                   ; SPT - logical sectors per track
                db 4                    ; BSH - block shift
                db 15                   ; BLM - block mask
                db 0                    ; EXM - ext.mask
                dw 1019                 ; DSM - capacity-1
                dw 255                  ; DRM - dir size-1
                db 240                  ; AL0 - dir allocation mask
                db 0                    ; AL1
                dw 64                   ; CKS - checksum array size
                dw 1                    ; OFF - system tracks
            endif

; Diskette 5,25" DD, 360kB(DOS and CP/M)
; 40 tracks(two side), 18 (256 byte) sectors per track/side, 1440 sectors totally
; 175 allocation 2kB blocks (first track reserved for system)
; 64 dir size (1x16x4) - dir is saved in 1 allocation blocks
; 1 system track
            if (Floppy==360) && (CPMSource <> "ROM")
DPB4:           dw  72                  ; SPT - logical sectors per track
                db  4                   ; BSH - posun bloku
                db  15                  ; BLM - block mask
                db  1                   ; EXM - ext.mask, 32kB per extent
                dw  174                 ; DSM - capacity-1
                dw  63                  ; DRM - dir size-1
                db  128                 ; AL0 - dir allocation mask
                db  0                   ; AL1
                dw  16                  ; CKS - checksum array size
                dw  1                   ; OFF - system tracks
            endif
            if (Floppy==360) && (CPMSource == "ROM")
DPB4:           dw  72                  ; SPT - logical sectors per track
                db  4                   ; BSH - posun bloku
                db  15                  ; BLM - block mask
                db  1                   ; EXM - ext.mask, 32kB per extent
                dw  179                 ; DSM - capacity-1 - full 360kB
                dw  63                  ; DRM - dir size-1
                db  128                 ; AL0 - dir allocation mask
                db  0                   ; AL1
                dw  16                  ; CKS - checksum array size
                dw  0                   ; OFF - system tracks - no system track
            endif

; Diskette 3,5" HD
; 3.5" / 1.44MB(DOS) / 1.28MB(CP/M)
; 80 tracks(two side), 32 (256 byte) sectors per track/side, 5120 sectors totally
; 632 allocation 2kB blocks (first track reserved for system)
; 256 dir size (4x16x4) - dir is saved in 4 allocation blocks
; 1 system track
            if (Floppy==144) && (CPMSource <> "ROM")
DPB4:           dw 128                  ; SPT - logical sectors per track
                db 4                    ; BSH - block shift
                db 15                   ; BLM - block mask
                db 0                    ; EXM - ext.mask
                dw 631                  ; DSM - capacity-1
                dw 255                  ; DRM - dir size-1
                db 240                  ; AL0 - dir allocation mask
                db 0                    ; AL1
                dw 64                   ; CKS - checksum array size
                dw 1                    ; OFF - system tracks 
            endif
            if (Floppy==144) && (CPMSource == "ROM")
DPB4:           dw 128                  ; SPT - logical sectors per track
                db 4                    ; BSH - block shift
                db 15                   ; BLM - block mask
                db 0                    ; EXM - ext.mask
                dw 639                  ; DSM - capacity-1
                dw 255                  ; DRM - dir size-1
                db 240                  ; AL0 - dir allocation mask
                db 0                    ; AL1
                dw 64                   ; CKS - checksum array size
                dw 0                    ; OFF - system tracks 
            endif
;------------------------------------------------------------------------------
Signature:	db	ESC,"[0m"	; reset terminal attributes
		db	ESC,"[2J"	; clear console
		db	CR,LF
		db	"NCB85 64k CP/M v"
		db	CPMVer/10+'0','.',CPMVer#10+'0'
		db	", BIOS v"
		db	BIOSVer/10+'0','.',BIOSVer#10+'0'
		db	CR,LF
		db	"RomBor, Archeocomp 07/2014"
TCRLF:		db	CR,LF,0

                if NumPmd32>0
ErrPMD32:	db	CR,LF
		db	"PMD 32-SD Error!"
		db	CR,LF,BEL,0
                endif

ErrBoot:	db	"BOOT Error!"
		db	CR,LF,CR,LF,BEL,0
ErrRead:	db	CR,LF,"Read",0
ErrWrite:	db	CR,LF,"Write",0
ErrError:	db	" Error! - ",0
ErrRetry:	db	" [R]etry [I]gnore Re[B]oot"
		db	CR,LF,BEL,0

		db	0		; 0=warm start, 1=cold start
CCPCommand:	db	4,"cd *",0
CCPCommandLen:	equ	$ - CCPCommand

;------------------------------------------------------------------------------
; * CBOOT * Cold boot
CBOOT:		di
		lxi	sp,0		; stack to ram end

		mvi	a,XRAM		; disable ROM
		sim

		call	HardwareInit	; HW init

		lxi	h,Signature
		call	Print0

		xra	a		; zero
		sta	IOByte		;  IOByte
		sta	CurrentDisk	;  current disk and user
                ;DRI bloking alg.
                STA	HSTACT          ;CLEAR HOST ACTIVE FLAG
                STA     UNACNT

		inr	a
		sta	CCPCommand-1	; flag: cold start

;------------------------------------------------------------------------------
; * WBOOT * Warm boot
WBOOT:		di
		lxi	sp,0		; stack to ram end

		lda	CCPCommand-1
		ora	a
		cz	HardwareInit	; initialize HW at warm start

            if NumPmd32>0
		mvi	l,5		; 5 retries
WBoot1:		call	CheckPMD32	; is PMD 32-SD connected?
		jnc	WBoot2		; yes, jump forward
		call	WaitL
		dcr	l
		jnz	WBoot1

		lxi	h,ErrPMD32	; show error msg
		call	Print0
		call	CONIN		; wait for keypress
		di			; disable interrupts
		mvi	a,XROM+1Fh	; connect ROM tu bus and disable interrupts
		sim
		hlt
            endif

WBoot2:		xra a
                sta     drive_nr        ;drive 0, track 0, sector 0, head 0
                sta     track_nr        ;FDC driver variables
                sta     motor_0_state   ;
                sta     motor_1_state   ; floppy motors
                sta     HSTSEC          ;host sector in DRI blocking alg.
                inr a                   ;256 bytes per sector
                sta NUMBER_OF_BYTES     ;FDC driver variables

            if Floppy==360
                call set_drv_type0      ;5.25" 360kB, 18x256byte sectors/track
            elseif Floppy==144
                call set_drv_type3      ;3.5" 1.44MB, 32x256byte sectors/track
            endif
                call fd_init
                call long_delay
                
                jnc .f1
                lxi     h,ErrBoot       ;print boot error
                call	Print0
                hlt
.f1
            if CPMSource == "ROM"       ;load CPM from ROM
		mvi	a,XROM+1Fh	;connect ROM to bus and disable interrupts
		sim
                lxi h,CPMBIN
                lxi d,BaseCCP
                lxi b,CCPLength+BDOSLength
                call CopyBlock
		mvi	a,XRAM		;disable ROM
		sim
            else                    
                lxi h,BaseCCP           ;load CPM from floppy here (dest.address)
                lda HSTSEC              ;sector counter(it is safe to use this var at boot)
.f2             inr a                   ;move to next sector
                sta HSTSEC              ;save it
                push h                  ;backup dest.address
                call set_sector         ;compute correct sec.nr - if we are already on head 1
.f3             mvi a,FDC_READ          ;read command
                call fd_exec_cmd        ;execute
                pop h                   ;restore dest.address
                mvi c,0                 ;transfer 256 bytes to address
                lxi d,dbuffer
.f4             ldax d
                mov M,a
                inx d
                inx h
                dcr c
                jnz .f4
                lda HSTSEC              ;CPM loaded?
                cpi CcpBdosSec/2        ;5.5kB CPM (3.5kB BDOS + 2kB CCP)
                jnz .f2
            endif

WBootEnd:	lxi	h,Buffer	; DMA address
		shld	Dma
		mvi	a,0C3h		; jmp instruction to WBOOT to address 0
		sta	0
		lxi	h,WBOOTE
		shld	1
		sta	5		; jmp instruction to BDOS to address 5
		lxi	h,BaseBDOS
		shld	6
		lda	CurrentDisk	; last used drive
		mov	c,a		; send to CCP in C register
		lxi	h,CCPCommand-1	; cold/warm start ?
		mov	a,m		; flag to A
		mvi	m,0		; clear flag
            if NumPmd32>0
		ora	a		; warm start ?
		jz	BaseCCP+3	; jump to CCP with empty edit buffer
		inx	h		; move to beg of "CD *"
		lxi	d,BaseCCP+7	; copy to edit buffer
		lxi	b,CCPCommandLen
		call	CopyBlock
		jmp	BaseCCP		; jump to CCP
            elseif
		jmp     BaseCCP+3	; jump to CCP with empty edit buffer
            endif
                ;
;------------------------------------------------------------------------------
CopyBlock:	mov	a,m
		stax	d
		inx	h
		inx	d
		dcx	b
                mov     a,b
                ora     c
		jnz	CopyBlock
		ret

;------------------------------------------------------------------------------
; * Unused services *
;------------------------------------------------------------------------------
; printer ready - unused
LISTST:		mvi	a,255		; always ready

;------------------------------------------------------------------------------
; Char out to printer - unused
LIST:

;------------------------------------------------------------------------------
; Char out to char output - unused
PUNCH:
		ret

;------------------------------------------------------------------------------
; Char in from char input - unused
READER:		mvi	a,EOF		; vrat vzdy znak konca suboru
		ret

;------------------------------------------------------------------------------
; Print zero terminated text
; I: HL=text address
; O:
; M: HL, AF, C
Print0:		mov	a,m		; get char
		inx	h		; move to next
		ora	a		; end of text?
		rz			; yes, return
		call	ConsoleOutputA	; send char
		jmp	Print0		; repeat

;------------------------------------------------------------------------------
; Show error
; Read Error - D/T/S
; Write Error - D/T/S
; D - drive
; T - track
; S - sector
PrintError:	call	Print0		; text Read/Write Error
		lxi	h,ErrError
		call	Print0
		lda	Drive		; show Drive
		adi	'A'
		call	ConsoleOutputA
		mvi	c,'/'
		call	ConsoleOutputC
		lda	Track		; show Track
		call	Dec8
		mvi	c,'/'
		call	ConsoleOutputC
		lda	Sector		; show Sector
		call	Dec8
		lxi	h,ErrRetry
		call	Print0
		call	CONIN		; wait
		ani	05Fh
		cpi	'B'		; is it 'B'?
		jz	WBOOT		; warm start
		ret

;------------------------------------------------------------------------------
; Print 8 bit number on 3 digits.
; Leading zeroes are suppresed
; I: A=cislo - 0 .. 255
; O: -
; M: AF, B, DE, HL
Dec8:		mov	h,a		; cislo do H
		mvi	b,8		; 8 bitove cislo
		lxi	d,0		; vynuluj vysledok v DE
Dec8L:		dad	h		; najvyssi bit do CY
		mov	a,e		; jednotky a desiatky
		adc	a		; zdvojnasob a pripocitaj prenos z DAD H
		daa			; preved do BCD
		mov	e,a
		mov	a,d		; stovky
		adc	a		; zdvojnasob a pripocitaj prenos z DAA
		mov	d,a
		dcr	b		; opakuj pre 8 bitov
		jnz	Dec8L
		lxi	h,PrtBCDX+1	; flag - neplatne nuly
		mov	m,b		; vynuluj ho - uvodne nuly sa netlacia
		mov	a,d		; stovky do A
		call	PrtBCDchar	; zobraz
		mov	a,e		; desiatky a jednotky do A

;------------------------------------------------------------------------------
; Print BCD number
; I: A=BCD number, HL=PrtBCDX+1
; O: -
; M: AF, B
PrtBCD:		mov	b,a		; odpamataj cislo
		rrc			; presun desiatky do dolnych 4 bitov
		rrc
		rrc
		rrc
		call	PrtBCDchar	; zobraz cislo
		mov	m,h		; nastav flag - neplatne nuly  - pri jednotkach sa tlaci aj 0
		mov	a,b		; vezmi cislo
PrtBCDchar:	ani	15		; maska na dolne 4 bity
		jnz	PrtBCDchar2	; ak je to cislica > 0, skoc
PrtBCDX:	mvi	a,0		; uz bola vytlacena nejaka platna cislica ?
		ora	a
		rz			; nie, vrat sa - uvodne nuly sa netlacia
		xra	a		; tlacime aj nulu
PrtBCDchar2:	adi	'0'		; uprav na znaky '0' - '9'
		mov	m,a		; nastav flag - mame platne cislice
		push	b
		push	d
		call	ConsoleOutputA	; vytlac znak
		pop	d
		pop	b
		ret

;------------------------------------------------------------------------------
; * Diskove services *
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; * SELDSK - Select disk
; I: C=disku nr
; O: HL=address of table DPH, 0 when wrong disk nr
; M: vsetky
SELDSK:		lxi	h,0		; return 0 on error
		mov	a,c
		cpi	NumDisks	; disks A: .. D: (and E: floppy)
		rnc			; error - HL = 0
		sta	Drive		; drive - save it
		mov	l,c		; HL=disk nr, compute DPH address
		dad	h		; *2
		dad	h		; *4
		dad	h		; *8
		dad	h		; *16
		lxi	d,DPH		; disk. param. address base
		dad	d		; HL=chosen disk params address
		ret

;------------------------------------------------------------------------------
; * HOME - Disk Home
; I: -
; O: -
; M: A
HOME:		mvi	c,0
                ;DRI blocking alg.code
                LDA     HSTWRT          ;CHECK FOR PENDING WRITE
                ORA     A
                JNZ     HOMED
                STA     HSTACT          ;CLEAR HOST ACTIVE FLAG
HOMED:
;	RET

;------------------------------------------------------------------------------
; * SETTRK - Set track
; I: C=track nr
; O: -
; M: A
SETTRK:		mov	a,c
		sta	Track
		ret

;------------------------------------------------------------------------------
; * SETSEC - Set sector
; I: BC=sektora nr (B=0)
; O: -
; M: AF
SETSEC:		mov	a,c		; sectors 1 .. 64
		dcr	a		; change to 0 .. 63
		sta	Sector		; save
		ret

;------------------------------------------------------------------------------
; * SETDMA - Set DMA
; I: BC=DMA address
; O: -
; M: HL
SETDMA:		mov	h,b
		mov	l,c
		shld	Dma
		ret

;------------------------------------------------------------------------------
; * SECTRN - Sector translation
; I: BC=logical sector nr, DE=tran table address
; O: HL=physical sector nr
; M: HL
SECTRN:		mov	h,b		; no translation
		mov	l,c
		inx	h		; 1 .. n
		ret

;------------------------------------------------------------------------------
; * READ - Read sector
; I: disk drive, sector and track at addresses Drive, Sector, Track
;    source address at address Dma
; O: A=0 - OK
;    A=1 - error
; M: all
READ:		call	LocalSP		; set local stack
            if NumPmd32>0
                lda     Drive           ; last drive is floppy(s)
                cpi     NumDisks-NumFlps
                jc      READ3
            endif
                call    read_flp        ; handle floppy differently
                ana     a               ; check success
                jz      READ2
		lxi	h,ErrRead	; text Read Error
		call	PrintError	; show err
		mvi	a,1		; A=1 - err
                jmp	READ2		; ret -> A=0 - OK
            if NumPmd32>0
READ3:		mvi	b,Retry		; number of retries
READ1:		push	b		; save
		call	ReadSector	; read sector
		pop	b		; restore
		ana	a		; success ?
		jz	READ2		; yes, ret -> A=0 - OK
		dcr	b		; decrement retries
		jnz	READ1		; again
		lxi	h,ErrRead	; text Read Error
		call	PrintError	; show err
		cpi	'R'		; Retry ?
		jz	READ3		; yes, again
		sui	'I'		; Ignore ?
		jz	READ2		; ret A=0
		mvi	a,1		; A=1 - error
            endif
READ2:		call	RestoreSP	; restore original stack
		ret

;------------------------------------------------------------------------------
; * WRITE - Write sector
; I: disk drive, sector and track at addresses Drive, Sector, Track
;    source address at address Dma
; O: A=0 - OK
;    A=1 - chyba
; M: vsetky
WRITE:		call	LocalSP		; set local stack
            if NumPmd32>0
                lda     Drive           ; last drive is floppy(s)
                cpi     NumDisks-NumFlps
                jc      WRITE3
            endif
                call    write_flp       ; handle floppy differently
                ana     a               ; check success
                jz      WRITE2
		lxi	h,ErrWrite	; text Read Error
		call	PrintError	; show err
		mvi	a,1		; A=1 - err
                jmp	WRITE2		; ret -> A=0 - OK
            if NumPmd32>0
WRITE3:		mvi	b,Retry		; number of retries
WRITE1:		push	b		; save
		call	WriteSector	; write sector
		pop	b		; restore
		ana	a		; success ?
		jz	WRITE2		; yes, ret -> A=0 - OK
		dcr	b		; decrement retries
		jnz	WRITE1		; again
		lxi	h,ErrWrite	; text Write Error
		call	PrintError	; show chybu
		cpi	'R'		; Retry ?
		jz	WRITE3		; yes, again
		sui	'I'		; Ignore ?
		jz	WRITE2		; ret A=0
		mvi	a,1		; A=1 - err
            endif
WRITE2:		call	RestoreSP	; restore original stack
		ret

;------------------------------------------------------------------------------
; Set local stack
; I: -
; O: -
; M: DE, HL, SP
LocalSP:	pop	d		; ret address to DE
		lxi	h,0		; stack address to HL
		dad	sp
		lxi	sp,Stack	; new stack address
		push	h		; old stack address on new stack
		xchg			; return indirectly
		pchl

;------------------------------------------------------------------------------
; Restors original stack
; I: -
; O: -
; M: DE, HL, SP
RestoreSP:	pop	d		; ret adresa to DE
		lhld 	Stack-2		; original stack to HL
		sphl			; set original stack address
		xchg			; return indirectly
		pchl

;------------------------------------------------------------------------------
; Short delay
; I: -
; O: cca 90 us
; M: AF
WaitQ:		mvi	a,10*(CRYSTAL/2)
WaitQ1:		dcr	a
		jnz	WaitQ1
		ret

;------------------------------------------------------------------------------
; Long delay
; I: -
; O: cca 1s
; M: AF, BC
WaitL:		lxi	b,3700*(CRYSTAL/2)
WaitL1:		call	WaitQ
		dcx	b
		jnk	WaitL1
		ret

;------------------------------------------------------------------------------
; Delay cca 1ms
;WaitM:		mvi	b,11
;WaitM1:		call	WaitQ
;		dcr	b
;		jnz	WaitM1
;		ret

;------------------------------------------------------------------------------
; Init USART and PIO
; I: -
; O: -
; M: AF
HardwareInit:
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
                ;
                ; init PIO 8255
            if NumPmd32>0
PioInit:	mvi	a,PIO_CMD
		out	PIO_CWR
            endif
		;
uart_init_16552:
                mvi a, 0C7h              ;06-no FIFO, 07-enable FIFO, bits 6,7 - fifo trigger
                out FIFO_CTRL_REG_1
                out FIFO_CTRL_REG_2
                mvi a, 83h
                out LINE_CTRL_REG_1
                out LINE_CTRL_REG_2
                mvi a, 02h              ;8-4800,4-9600,2-19200,1-38400
                out DIVISOR_LOW_BYTE_1
                out DIVISOR_LOW_BYTE_2
                xra a
                out DIVISOR_HIGH_BYTE_1
                out DIVISOR_HIGH_BYTE_2
                mvi a, 03h
                out LINE_CTRL_REG_1
                out LINE_CTRL_REG_2
                ret
                ;
                ; print char in C reg on UART2.2
            ifdef DEBUG
CONOUT2:        in      LINE_STATUS_REG_2   ; 16552
		ani     20h
		jz	CONOUT2         ; wait
		mov	a,c		; char to A
		out	TRANSMITTER_BUFFER_REG_2    ; send  
		ret
                ;
                ;print hl in hex format
hl_to_hex:      mov a,h
                call tohexh
                call CONOUT2            ;output
                mov a,h
                call tohexl
                call CONOUT2            ;output
l_to_buff:      mov a,l
                call tohexh
                call CONOUT2            ;output
l2_to_buff:     mov a,l
                call tohexl
                call CONOUT2            ;output
                ret
tohexh:         rlc                     ;high 4-bits
                rlc
                rlc
                rlc
tohexl:         ani 0Fh                 ;low 4-bits
                adi 30h                
                cpi 3Ah                 ;9 is 0x39
                jm nohex                ;0-9 ?
                adi 07h                 ;no, it is A-F
nohex:          mov c,a                 ;return in c
                ret
            endif

;------------------------------------------------------------------------------
; Variables in BIOS
SEKDSK:                                 ; DRI name
Drive:		db	0		; current disk

SEKTRK:                                 ; DRI name
Track:		dw	0		; current track

SEKSEC:                                 ; DRI name
Sector:		db	0		; current sector a disk

DMAADR:                                 ; DRI name
Dma:		dw	0		; adresa DMA

StackX:		ds	32		; stack for BIOS
Stack		equ	$

OldHL:		ds	2		; backup HL in interrupt
UsartByte	ds	1		; received byte from USART

;------------------------------------------------------------------------------
; choose one of the implementations of serial routinnes
                ; 8251 uart pulling mode, no special char decode
                ;include "serial_mini.asm"
                ;
                ; 8251 uart, interrupt mode, with special char decoding
                ;include "serial_int.asm"
                ;
                ; 16552 uart, channel 1, pulling mode, no special char decode
                include "serial_mini_16552.asm"
;------------------------------------------------------------------------------
                include "pmd32_driver.asm"
;------------------------------------------------------------------------------
                include "fdc_driver.asm"
;------------------------------------------------------------------------------
                include "blocking_dri.asm"
;------------------------------------------------------------------------------
                ; BIOS data areas
            if NumPmd32>0
ALV0:		ds	128		; disk A: allocation vector
CSV0:		ds	64		; disk A: directory checksum
ALV1:		ds	128		; disk B: allocation vector
CSV1:		ds	64		; disk B: directory checksum
            endif
                    ; for EPROM located CCP/BDOS we need to save space
                    ; buffers are not allocated so that BIOS+CCP/BDOS fits in 8k
        if (CPMSource == "ROM")         ; saving rom space
HSTBUF:                                 ; used by DRI blocking algorithm
DIRBUF          equ     HSTBUF + HSTSIZ ; not allocated, only defined
            if (Floppy==360)
;ALV4:		ds	23		; disk E: allocation vector
;CSV4:		ds	16		; disk E: directory checksum
;ALV5:		ds	23		; disk F: allocation vector
;CSV5:		ds	16		; disk F: directory checksum
ALV4            equ     DIRBUF + 128    ; not allocated, only defined
CSV4            equ     ALV4 + 23       ; not allocated, only defined
ALV5            equ     CSV4 + 16       ; not allocated, only defined
CSV5            equ     ALV5 + 23       ; not allocated, only defined
                message "END ADDRESS : \{CSV5+16}"    ;16 bytes for CSV5
            elseif (Floppy==144)
;ALV4:		ds	81		; disk E: allocation vector
;CSV4:		ds	64		; disk E: directory checksum
;ALV5:		ds	81		; disk F: allocation vector
;CSV5:		ds	64		; disk F: directory checksum
ALV4            equ     DIRBUF + 128    ; not allocated, only defined
CSV4            equ     ALV4 + 81       ; not allocated, only defined
ALV5            equ     CSV4 + 64       ; not allocated, only defined
CSV5            equ     ALV5 + 81       ; not allocated, only defined
                message "END ADDRESS : \{CSV5+81}"    ;81 bytes for CSV5
            endif
        else        ; CCP/BDOS loaded from first track, buffers allocated
DIRBUF:		ds	128		; dir buffer
            if (Floppy==360)
ALV4:		ds	22		; disk E: allocation vector
CSV4:		ds	16		; disk E: directory checksum
ALV5:		ds	22		; disk F: allocation vector
CSV5:		ds	16		; disk F: directory checksum
            elseif (Floppy==144)
ALV4:		ds	80		; disk E: allocation vector
CSV4:		ds	64		; disk E: directory checksum
ALV5:		ds	80		; disk F: allocation vector
CSV5:		ds	64		; disk F: directory checksum
            endif
                message "END ADDRESS : \{$}"    ;
        endif       ; CPMSource

XBIOSLength:	equ	$ - BaseBIOS

		if	XBIOSLength > BIOSLengthMax
		  warning "\aBIOS is too long"
		endif
;------------------------------------------------------------------------------
		dephase
;------------------------------------------------------------------------------
CPMBIN:                
            if (CPMSource <> "ROM")
                message "DO NOT INCLUDE CP/M IMAGE"
            else
                binclude "../CPM/cpmDE00.bin"
                message "../CPM/cpm\{BaseCCP}.bin DOUBLE CHECK binclude file!!!"
                message "AND FIX!!! binclude(d) CPM image if NECESSARY!!!"
            endif
    
		end
;------------------------------------------------------------------------------
