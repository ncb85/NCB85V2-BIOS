;------------------------------------------------------------------------------
; NCB85 CP/M BIOS
; Roman Borik/RM-Team 2/2012, 2/2013, 1/2014
; archeocomp 2014-2016
; NCB85 is a SBC based on 8085 and 64k SRAM. There is also 8kB ROM from 0000H
; that can be shadowed by outputting 1 to SOD pin of 8085
; BIOS is in the ROM and moved to target RAM after reset. CP/M is loaded from
; diskette drive or from ROM
;------------------------------------------------------------------------------
;DEBUG			 equ	 1				 ; floppy driver debug on uart 2.2
				cpu	8085undoc

jnk				macro	adr
				jnx5	adr
				endm

jk				macro	adr
				jx5	adr
				endm
;------------------------------------------------------------------------------
CPMVer			equ	22		; CP/M ver 2.2
BIOSVer			equ	30		; BIOS ver 3.0
CRYSTAL			equ	10		; MHz

; physical floppy sectors
HSTSIZ			equ		256				; HOST DISK SECTOR SIZE
; step rate, head settle time
;FloppySpeed	 equ	 "FAST"			 ;for TEAC drives
FloppySpeed		equ		"MEDIUM"		;slower drives MITSUMI?
			ifndef Floppy
				message "Floppy type not defined!!!!!!"
			endif
; drives used
			if (Extra==0)
NumFlps			equ		2				; number of floppy drives
			else
NumFlps			equ		3				; number of floppy drives
			endif

CCPLength	equ	0800h			; CCP len
BDOSLength	equ	0E00h			; BDOS len
BIOSLengthMax	equ	1000h		; BIOS (Max) len
CcpBdosSec	equ	(CCPLength+BDOSLength)/128 ; CCP + BDOS len in sectors
; be sure to include correct cp/m image at the end of file
			if (Floppy == 360)
BaseBIOS		equ	0F500h		; BIOS address (with CPM bundled in 8k ROM) for 2x360k
			elseif (Floppy==720) 
BaseBIOS		equ	0F500h		; BIOS address (with CPM bundled in 8k ROM) for 2x720k
			elseif (Floppy==120)
				if (Extra==120)
BaseBIOS		equ	0F400h		; BIOS address (with CPM bundled in 8k ROM) for 2x1.2MB + 1x 1.2MB
				else
BaseBIOS		equ	0F500h		; BIOS address (with CPM bundled in 8k ROM) for 2x1.2MB + 500kB 8"(or just 2 drives)
				endif
			elseif (Floppy==144) 
				if (Extra==0)
BaseBIOS		equ	0F500h		; BIOS address (with CPM bundled in 8k ROM) for 2x1.44MB
				else
BaseBIOS		equ	0F400h		; BIOS address (with CPM bundled in 8k ROM) for 2x1.44MB + 500kB 8"(or 3 1.44MB drives)
				endif
			elseif (Floppy==100) 
BaseBIOS		equ	0F500h		; BIOS address (with CPM bundled in 8k ROM) for 2x1.0MB, IBM floppy
			endif
BaseCCP			equ	BaseBIOS-(CCPLength+BDOSLength) ; CCP address
BaseBDOS		equ	BaseCCP+CCPLength+6 ; BDOS address
				message "BIOS begins at \{BaseBIOS}"
				message "CCP begins at \{BaseCCP}"
IOByte			equ	0003h				; IO byte (not used)
CurrentDisk		equ	0004h				; default disk
Buffer			equ	0080h				; default DMA address
Rst55Vect		equ	002Ch				; RST5.5 (USART)
Rst65Vect		equ	0034h				; RST6.5 (TIMER)

BEL				equ	7
CR				equ	0Dh
LF				equ	0Ah
EOF				equ	1Ah
ESC				equ	1Bh
SPACE			equ	20h
;------------------------------------------------------------------------------
; IO ports
PIO_PA			equ	10h					; 8255
PIO_PB			equ	11h
PIO_PC			equ	12h
PIO_CWR			equ	13h

USART_DATA		equ	8					; 8251
USART_CWR		equ	9
USART_STAT		equ	9

PCF8584_ADDR	equ		58h				; I2C Controller
MUART_ADDR		equ		60h				; 8256

; paging ROM/RAM - values for SIM  instruction
XROM			equ	40h
XRAM			equ	0C0h
; init USART
USART_RESET		equ	01000000b
USART_CMD1		equ	01001110b
USART_CMD2		equ	00010101b
; USART 16552
FIFO_CTRL_REG_1			equ		2Ah
FIFO_CTRL_REG_2			equ		22h
LINE_CTRL_REG_1			equ		2Bh
LINE_CTRL_REG_2			equ		23h
DIVISOR_LOW_BYTE_1		equ		28h
DIVISOR_HIGH_BYTE_1		equ		29h
DIVISOR_LOW_BYTE_2		equ		20h
DIVISOR_HIGH_BYTE_2		equ		21h
LINE_STATUS_REG_1		equ		2Dh
TRANSMITTER_BUFFER_REG_1 equ	28h
RECEIVER_BUFFER_REG_1	 equ	28h
LINE_STATUS_REG_2		 equ	25h
TRANSMITTER_BUFFER_REG_2 equ	20h
;------------------------------------------------------------------------------
				org	0
Boot:
				include "rom_loader.asm"
;------------------------------------------------------------------------------
QBaseBIOS:		equ	$
				phase	BaseBIOS
; BIOS entry points
CBOOTE:			jmp	CBOOT		; Cold boot
WBOOTE:			jmp	WBOOT		; Warm Boot
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
;------------------------------------------------------------------------------
Signature:		db	ESC,"[0m"	; reset terminal attributes
				db	ESC,"[2J"		; clear console
				db	CR,LF
				db	"NCB85 64k CP/M v"
				db	CPMVer/10+'0','.',CPMVer#10+'0'
				db	", BIOS v"
				db	BIOSVer/10+'0','.',BIOSVer#10+'0'
				db	", ",BaseBIOS/4096+'A'-10,(BaseBIOS#4096)/256+'0',"00-FFFF"
		if ((NumFlps==3) && (Extra==Floppy))
			if (Floppy==360)
				db	", 3x 360kB 5.25\""
			elseif (Floppy==720)
				db	", 3x 720kB 5.25\""
			elseif (Floppy==120)
				db	", 3x 1.2MB 5.25\""
			elseif (Floppy==144)
				db	", 3x 1.44MB 3.5\""
			elseif (Floppy==100)
				db	", 3x 1.0MB 8\" IBM"
			endif
		elseif
			if (Floppy==360)
				db	", 2x 360kB 5.25\""
			elseif (Floppy==720)
				db	", 2x 720kB 5.25\""
			elseif (Floppy==120)
				db	", 2x 1.2MB 5.25\""
			elseif (Floppy==144)
				db	", 2x 1.44MB 3.5\""
			elseif (Floppy==100)
				db	", 2x 1.0MB 8\" IBM"
			endif
			if (Extra==50)
				db	" + 1x 500kB 8\""
			elseif (Extra==120)
				db	" + 1x 1.2MB 5.25\""
			elseif (Extra==144)
				db	" + 1x 1.44MB 3.5\""
			endif
		endif
				db	CR,LF
				db	"RomBor, Archeocomp 07/2014, 12/2018"
TCRLF:			db	CR,LF,0

ErrBoot:		db	"BOOT Error!"
				db	CR,LF,CR,LF,BEL,0
ErrRead:		db	CR,LF,"Read",0
ErrWrite:		db	CR,LF,"Write",0
ErrError:		db	" Error! - ",0
ErrRetry:		db	" [R]etry [I]gnore Re[B]oot"
				db	CR,LF,BEL,0
				;						; flag, do not move
				db	0					; 0=warm start, 1=cold start
CCPCommand:
				db	3,"DIR",0
CCPCommandLen:	equ	$ - CCPCommand
;------------------------------------------------------------------------------
; * CBOOT * Cold boot
CBOOT:			di
				lxi	sp,0				; stack to ram end
				;
				mvi	a,XRAM				; disable ROM
				sim
				;
				call	HardwareInit	; HW init

				lxi	h,Signature
				call	Print0
				;
				xra	a					; zero
				sta	IOByte				; IOByte
				sta	CurrentDisk			; current disk and user
				;DRI blocking alg.
				;STA		HSTACT			; CLEAR HOST ACTIVE FLAG
				;STA		UNACNT
				;
				inr	a
				sta	CCPCommand-1		; flag: cold start
;------------------------------------------------------------------------------
; * WBOOT * Warm boot
WBOOT:			di
				lxi	sp,0				; stack to ram end
				;
				lda	CCPCommand-1
				ora	a
				cz	HardwareInit		; initialize HW at warm start
				;
				xra a					; clear A
				dcr a					; decrement
				sta drive_nr			; drive -1, track -1, sector -1, head N.A.
				sta track_nr			; FDC driver variables, avoid false detection of 
				sta sector_nr			; cached sector in deblocking algorithm
				;
WBoot2:			xra a
				sta motor_0_state		; floppy motor A:
				sta motor_1_state		; floppy motor B:
			if (Extra==50)
				sta motor_2_state		; floppy motor C:
			endif
				inr a					; 256 bytes per sector
				sta NUMBER_OF_BYTES		; FDC driver variables
				call set_drv_type		; set disk drive type (param form build)
				call fd_init			; initialize FDC
				call long_delay			; wait
				jnc .f1					; load CPM from ROM
				lxi	 h,ErrBoot			; print boot error
				call Print0
				hlt
.f1				mvi a,XROM+1Fh			; connect ROM to bus and disable interrupts
				sim
				lxi h,CPMBIN			; source ROM address
				lxi d,BaseCCP			; destination RAM
				lxi b,CCPLength+BDOSLength	; length
				call CopyBlock			; copy
				mvi a,XRAM				; disable ROM
				sim
WBootEnd:		lxi	h,Buffer			; DMA address
				shld	DMAADR
				mvi	a,0C3h				; jmp instruction to WBOOT to address 0
				sta	0
				lxi	h,WBOOTE
				shld	1
				sta	5					; jmp instruction to BDOS to address 5
				lxi	h,BaseBDOS
				shld	6
				lda	CurrentDisk			; last used drive
				mov	c,a					; send to CCP in C register
				lxi	h,CCPCommand-1		; cold/warm start ?
				mov	a,m					; flag to A
				mvi	m,0					; clear flag
				ora	a					; warm start ?
				jz	BaseCCP+3			; jump to CCP with empty edit buffer
				inx	h					; move to beg of "DIR"
				;jmp	 BaseCCP+3		; jump to CCP with empty edit buffer
				lxi	d,BaseCCP+7			; copy to edit buffer
				lxi	b,CCPCommandLen
				call	CopyBlock
				jmp	BaseCCP				; jump to CCP
				;
;------------------------------------------------------------------------------
CopyBlock:		mov		a,m
				stax	d
				inx		h
				inx		d
				dcx		b
				mov		a,b
				ora		c
				jnz	CopyBlock
				ret
;------------------------------------------------------------------------------
; * Unused services *
;------------------------------------------------------------------------------
; printer ready - unused
LISTST:			mvi	a,255				; always ready
;------------------------------------------------------------------------------
; Char out to printer - unused
LIST:
;------------------------------------------------------------------------------
; Char out to char output - unused
PUNCH:
				ret
;------------------------------------------------------------------------------
; Char in from char input - unused
READER:			mvi	a,EOF				; always return EOF
				ret
;------------------------------------------------------------------------------
; Print zero terminated text
; I: HL=text address
; O:
; M: HL, AF, C
Print0:			mov	a,m					; get char
				inx	h					; move to next
				ora	a					; end of text?
				rz						; yes, return
				call	ConsoleOutputA	; send char
				jmp	Print0				; repeat
;------------------------------------------------------------------------------
; Show error
; Read Error - D/T/S
; Write Error - D/T/S
; D - drive
; T - track
; S - sector
PrintError:		call	Print0			; text Read/Write Error
				lxi	h,ErrError
				call	Print0
				lda		SEKDSK			; show Drive
				adi	'A'
				call	ConsoleOutputA
				mvi	c,'/'
				call	ConsoleOutputC
				lda		SEKTRK			; show Track
				call	Dec8
				mvi	c,'/'
				call	ConsoleOutputC
				lda		SEKSEC			; show Sector
				call	Dec8
				lxi	h,ErrRetry
				call	Print0
				call	CONIN			; wait
				ani	05Fh
				cpi	'B'					; is it 'B'?
				jz	WBOOT				; warm start
				ret
;------------------------------------------------------------------------------
; Print 8 bit number on 3 digits.
; Leading zeros are suppresed
; I: A= 0 .. 255
; O: -
; M: AF, B, DE, HL
Dec8:			mov	h,a					; number to H
				mvi	b,8					; 8 bit number
				lxi	d,0					; clear result in DE
Dec8L:			dad	h					; highest bit to CY
				mov	a,e					; lower and higher digits
				adc	a					; double it and add carry from DAD H
				daa						; make it decimal
				mov	e,a
				mov	a,d					; hundreds - third digit
				adc	a					; double it and add carry from DAA
				mov	d,a
				dcr	b					; repeat for 8 bits
				jnz	Dec8L
				lxi	h,PrtBCDX+1			; flag - leading zeroes
				mov	m,b					; clear flag - do not print leading zero
				mov	a,d					; higher digit to A
				call	PrtBCDchar		; print
				mov	a,e					; lower and higher digits to A
;------------------------------------------------------------------------------
; Print BCD number
; I: A=BCD number, HL=PrtBCDX+1
; O: -
; M: AF, B
PrtBCD:			mov	b,a					; back up
				rrc						; swap higher digit to lower
				rrc
				rrc
				rrc
				call	PrtBCDchar		; print digit
				mov	m,h					; set flag - leading zero
				mov	a,b					; restore
PrtBCDchar:		ani	15					; low 4 bits
				jnz	PrtBCDchar2			; digit > 0
PrtBCDX:		mvi	a,0					; any digit printed ?
				ora	a
				rz						; no, return - do not print leading zero
				xra	a					; print also zero
PrtBCDchar2:	adi	'0'					; convert to char '0' - '9'
				mov	m,a					; set flag - valid digits
				push	b
				push	d
				call	ConsoleOutputA	; print char
				pop	d
				pop	b
				ret
;------------------------------------------------------------------------------
; * READ - Read sector
; I: disk drive, sector and track at addresses Drive, Sector, Track
;	 source address at address Dma
; O: A=0 - OK
;	 A=1 - error
; M: all
READ:			di
				call	LocalSP			; set local stack
				call	read_flp		; handle floppy
				ana		a				; check success
				jz		READ2
				lxi		h,ErrRead		; text Read Error
				call	PrintError		; show err
				mvi		a,1				; A=1 - err
READ2:			call	RestoreSP		; restore original stack
				ei
				ret
;------------------------------------------------------------------------------
; * WRITE - Write sector
; I: disk drive, sector and track at addresses Drive, Sector, Track
;	 source address at address Dma
; O: A=0 - OK
;	 A=1 - error
; M: all
WRITE:			di
				call	LocalSP			; set local stack
				call	write_flp		; handle floppy
				ana		a				; check success
				jz		WRITE2
				lxi		h,ErrWrite		; text Read Error
				call	PrintError		; show err
				mvi		a,1				; A=1 - err
WRITE2:			call	RestoreSP		; restore original stack
				ei
				ret
;------------------------------------------------------------------------------
; Set local stack
; I: -
; O: -
; M: DE, HL, SP
LocalSP:		pop		d				; ret address to DE
				lxi		h,0				; stack address to HL
				dad		sp
				lxi		sp,Stack		; new stack address
				push	h				; old stack address on new stack
				xchg					; return indirectly
				pchl
;------------------------------------------------------------------------------
; Restores original stack
; I: -
; O: -
; M: DE, HL, SP
RestoreSP:		pop		d				; ret address to DE
				lhld	Stack-2			; original stack to HL
				sphl					; set original stack address
				xchg					; return indirectly
				pchl
;------------------------------------------------------------------------------
; Init USART and PIO
; I: -
; O: -
; M: AF
HardwareInit:
				; init USART 8251
				xra		a				; be sure to be in Command mode
				out		USART_CWR		; 3x 00
				out		USART_CWR
				out		USART_CWR
				mvi		a,USART_RESET	; Reset 8251
				out		USART_CWR
				mvi		a,USART_CMD1	; 8 data, 1 stop, x16
				out		USART_CWR		; RxC a TxC = 307,2 kHz -> 19200 Bd
				mvi		a,USART_CMD2	; RTS, DTR, Enable RX, TX
				out		USART_CWR
				; I2C Controller PCF8584
				mvi		a,80h			; RESET,  will also choose register S0_OWN i.e. next byte will be
				out		PCF8584_ADDR+1	; loaded into reg S0^ (own address reg); serial interface off.
				mvi		a,55h			; loads byte 55H into reg S0^ effective own address becomes AAH.
				out		PCF8584_ADDR	; pcf8584 shifts this value left one bit
				mvi		a,0A0h			; loads byte A0H into reg S1, i.e. next byte will
				out		PCF8584_ADDR+1	; be loaded into the clock control reg S2.
				mvi		a,10h			; loads byte 10H into reg S2;system clock is 4.43 MHz; SCL = 90 kHz
				out		PCF8584_ADDR
				mvi		a,0C1h			; loads byte C1H into reg S1; reg enable serial interface
				out		PCF8584_ADDR+1	; next write or read operation will be to/from data transfer reg S0
				; 8256 MUART
				mvi		a,01h			; 8256 in 8085 mode, command1
				out		MUART_ADDR
				mvi		a,03h			; PARITY | SYSTEM_CLK | BAUD_RATE_19, command2
				out		MUART_ADDR+1
				mvi		a,0C1h			; SET_BIT | RXE | RST_BIT, command3
				out		MUART_ADDR+2
				mvi		a,0C0h			; T35 | T24, P2C2=P2C1=P2C0=input, mode
				out		MUART_ADDR+3
				mvi		a,00h			; port1, input
				out		MUART_ADDR+4
				mvi		a,0FFh			; reset interrupts
				out		MUART_ADDR+6
				;
uart_init_16552:
				mvi		a, 0C6h			;06-no FIFO, 07-enable FIFO, bits 6,7 - fifo trigger
				out		FIFO_CTRL_REG_1
				out		FIFO_CTRL_REG_2
				mvi		a, 83h
				out		LINE_CTRL_REG_1
				out		LINE_CTRL_REG_2
				mvi		a, 02h			;8-4800,4-9600,2-19200,1-38400
				out		DIVISOR_LOW_BYTE_1
				out		DIVISOR_LOW_BYTE_2
				xra		a
				out		DIVISOR_HIGH_BYTE_1
				out		DIVISOR_HIGH_BYTE_2
				mvi		a, 03h
				out		LINE_CTRL_REG_1
				out		LINE_CTRL_REG_2
				ret
				;
                ; print char in C reg on UART2.2
            ifdef DEBUG
CONOUT2:        in LINE_STATUS_REG_2	; 16552
				ani 20h
				jz	CONOUT2				; wait
				mov	a,c					; char to A
				out	TRANSMITTER_BUFFER_REG_2	; send  
				ret
                ;
                ;print hl in hex format
hl_to_hex:      mov a,h
                call tohexh
                call CONOUT2			;output
                mov a,h
                call tohexl
                call CONOUT2			;output
l_to_hex:		mov a,l
                call tohexh
                call CONOUT2			;output
l2_to_buff:     mov a,l
                call tohexl
                call CONOUT2			;output
                ret
tohexh:         rlc						;high 4-bits
                rlc
                rlc
                rlc
tohexl:         ani 0Fh					;low 4-bits
                adi 30h                
                cpi 3Ah					;9 is 0x39
                jm nohex				;0-9 ?
                adi 07h					;no, it is A-F
nohex:          mov c,a					;return in c
                ret
newline:        mvi c,13
                call CONOUT2
                mvi c,10
                call CONOUT2
                ret
            endif
;------------------------------------------------------------------------------
; Variables in BIOS
StackX:			ds		32				; stack for BIOS
Stack			equ		$

;------------------------------------------------------------------------------
; choose one of the implementations of serial routines
				; 8251 uart polling mode, no special char decode
				;include "serial_mini.asm"
				;
				; 8251 uart, interrupt mode, with special char decoding
				;include "serial_int.asm"
				;
				; 16552 uart, channel 1, pulling mode, no special char decode
				include "serial_mini_16552.asm"
				;
				; 8256 muart, polling mode, no special char decode
				;include "serial_8256.asm"
;------------------------------------------------------------------------------
				include "fdc_driver.asm"
;------------------------------------------------------------------------------
; choose one of the implementations of blocking algorithm
				;include "blocking_dri.asm"
				; with dri uncomment also 2 lines following comment 'DRI blocking alg.' in CBOOT
				;
				include "blocking_chuck.asm"
;------------------------------------------------------------------------------
				include "disk_params.asm"
;------------------------------------------------------------------------------

XBIOSBuffers:	equ		$
XBIOSLength:	equ		$ - BaseBIOS

			if	XBIOSLength > BIOSLengthMax
				warning "\aBIOS is too long"
			endif
;------------------------------------------------------------------------------
		dephase
;------------------------------------------------------------------------------
CPMBIN:				   
			if (BaseBIOS==0F300h) 
				binclude "../CPM/cpmDD00.bin"
			elseif (BaseBIOS==0F400h) 
				binclude "../CPM/cpmDE00.bin"
			elseif (BaseBIOS==0F500h)
				binclude "../CPM/cpmDF00.bin"
			elseif (BaseBIOS==0F600h) 
				binclude "../CPM/cpmE000.bin"
			else
				message "MISSING CPM image. FIX IT!!!"
			endif
;------------------------------------------------------------------------------
				phase	XBIOSBuffers
;------------------------------------------------------------------------------
				include "disk_buffers.asm"
				message "END ADDRESS : \{$}"
;------------------------------------------------------------------------------
				dephase
				end
;------------------------------------------------------------------------------
