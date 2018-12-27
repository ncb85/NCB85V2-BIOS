;------------------------------------------------------------------------------
				; floppy routines for 8085 CPU, PC8477B FDC
				; uses no DMAs, no IRQs and TC pin is not serviced
				; all data flow control is done via FDC internal registers
;------------------------------------------------------------------------------
				; symbolic constants
FDC_RECALIB		equ 0
FDC_SEEK		equ 1
FDC_READ		equ 2
FDC_WRITE		equ 3
				; driver constants
READ_RETRY		equ 3					; when read sector fails, retry it n times
RST65_ADDR		equ 0034h				; 6.5 interrupt vector
MOTOR_TIMEOUT	equ 4					; seconds motor timeout

FDC_BASE		equ 50h
REG_DOR			equ FDC_BASE+02h		; digital output register bits (MTR3 MTR2 MTR1 MTR0 DMAEN RESET DRIVE_SEL_1 DRIVE_SEL_0)

REG_MSR			equ FDC_BASE+04h		; address of main status register
REG_DSR			equ FDC_BASE+04h		; address of data rate select register
REG_DATA		equ FDC_BASE+05h		; floppy data register
REG_CCR			equ FDC_BASE+07h		; configuration control register

CNF_250			equ 02h					; 250kbps data rate (360kB, 720kB)
CNF_300			equ 01h					; 300kbps data rate (360kB disk in 1.2MB drive)
CNF_500			equ 00h					; 500kbps data rate (1.2MB, 1.44MB)
;MODE_BYTE1		 equ 0x86				 ;mode2, no NSC imp.seek (better use 82077 method), ISO, auto low pwr
MODE_BYTE1		equ 06h					; mode1, no imp.seek, IBM, auto low pwr
MODE_BYTE2		equ 00h					; FIFO enabled, few tracks

			if FloppySpeed == "FAST"
MODE_BYTE3		equ 0C1h				; default densel, 1x8ms head settle time
SPECIFY_BYTE1	equ 0EAh				; step rate 4ms, motor off 10s
			elseif FloppySpeed == "MEDIUM"
MODE_BYTE3		equ 0C2h				; default densel, 2x8ms head settle time
SPECIFY_BYTE1	equ 0CAh				; step rate 8ms, motor off 10s
			endif

MODE_BYTE4		equ 00h					; dskchg default
SPECIFY_BYTE2	equ 03h					; 1ms motor on delay, no DMA
				; fdc commands
CMD_RESET		equ 80h					; reset fdc by pulling bit 7 high
CMD_INIT		equ 04h					; unset reset bit, no DMA
CMD_RECALIB		equ 07h					; recalibrate
CMD_READ		equ 46h					; read command, bit 6 is MFM
CMD_WRITE		equ 45h					; write command, bit 6 is MFM
CMD_SEEK		equ 0Fh					; absolute track seek
CMD_SENS_INTR	equ 08h					; sense interrupt command
CMD_FORMAT		equ 4Dh					; format command, bit 6 is MFM
CMD_CONFIG1		equ 13h					; configure command
CMD_CONFIG24	equ 00h
;CMD_CONFIG3	 equ 0x14				 ;disable polling mode (765), no implied seeks, FIFO thresh 4
CMD_CONFIG3		equ 54h					; disable polling mode (765), implied seeks 82077 method, FIFO thresh 4
CMD_NSC			equ 18h					; National PC8477 identifes itself as 73h
CMD_MODE		equ 01h					; set motor timer mode, implied seek, index address, low power
CMD_SPECIFY		equ 03h					; set internal timers
CMD_SENSE_STAT	equ 04h					; sense drive status

				; do not change order of following lines
				; read/write param table (set at runtime)
drive_nr:		db 0					; drive
track_nr:		db 0					; track
head_nr:		db 0					; head
sector_nr:		db 1					; sector
NUMBER_OF_BYTES:db 1					; 256 bytes per sector
eot_sec_nr:		db 0					; end of track sector number
				; config values (changed at runtime when needed)
gap_length:		db 0Ah					; intersector gap length
data_length:	db 80h					; data length - don't care (end of table above)
number_of_sctrs:db 18					; 18 sectors
gap3_length:	db 0Ch					; recommended value
ccr_dsr_value:	db CNF_250
mode3_value:	db MODE_BYTE3
				; data buffer
;dbuffer:		 ds 256					; HSTBUF, host buffer, sector data buffer, defined in blocking_dri.asm
				; results area
status_reg_0:	db 0					; status register 0
status_reg_1:	db 0					; status register 1
status_reg_2:	db 0					; status register 2
status_reg_3:	db 0					; status register 3, bit 6 - write protect
										; bit 4 - track0, bit 2 - head select
										; bits 0,1 - drive
				ds 3					; placeholder for last 3 bytes
				;
				; variables area
unadj_sctr_nr:	db 0					; unadjusted sector - head 1 side appeded after head 0 sectors
fdc_extraloop:	db 0					; used in some time loops
ticks:			db 0					; 8155/8253 timer ticks
seconds:		db 0					; motor time off
motor_0_state:	db 0					; motor on flag
motor_1_state:	db 0					; motor on flag
		if (NumFlps==3)
motor_2_state:	db 0					; motor on flag
		endif
num_of_tracks:	db 0					; drive param
				;
				; table with drive type params(density,sectors,tracks,GAP,GAP3,densel)
			if (Floppy==360)
		message	"drive type: 2x 360kB 5.25\""
table_drv_typ:	db CNF_250,18,40,0Ah,0Ch,MODE_BYTE3		; 360kB 5.25" DD/40tracks standard drive
			elseif (Floppy==720)
		message	"drive type: 2x 720kB 5.25\""
;table_drv_typ:	 db CNF_250, 18, 80, 0Ah, 0Ch, 0C2h		 ; 720kB 5.25" DD/80tracks special drive (TEAC FD-55F)
table_drv_typ:	db CNF_300,18,80,0Ah,0Ch,MODE_BYTE3		; 720kB 5.25" HD drive, 360rpm
			elseif (Floppy==120)
		message	"drive type: 2x 1.2MB 5.25\""
table_drv_typ:	db CNF_500,26,80,0Eh,36h,MODE_BYTE3		; 1.2MB 5.25" HD drive
			elseif (Floppy==144)
		message	"drive type: 2x 1.44MB 3.5\""
table_drv_typ:	db CNF_500,32,80,0Eh,36h,02h			; 1.44MB 3.5" HD drive
			elseif (Floppy==100)
		message	"drive type: 2x 1.0MB 8\" IBM"
table_drv_typ:	db CNF_500,26,77,0Ah,0Ch,0C2h			; 500(SS)/1MB(DS) 8" DD drive
			endif
			if (Extra==50)
		message	" + 1x 500kB 8\""
			elseif (Extra==120)
		message	" + 1x 1.2MB 5.25\""
			elseif (Extra==144)
		message	" + 1x 1.44MB 3.5\""
			endif
				; set drive type
set_drv_type:	lxi h, table_drv_typ
				mov a,m
				sta ccr_dsr_value		; data rate
				out REG_CCR
				out REG_DSR
				inx h					; number of sectors
				mov a,m
				sta number_of_sctrs
				inx h					; tracks
				mov a,m
				sta num_of_tracks
				inx h					; read/write gap value
				mov a,m
				sta gap_length
				inx h					; format gap3 value
				mov a,m
				sta gap3_length
				inx h					; densel pin polarity (3.5 vs 5.25 drives)
				mov a,m
				sta mode3_value
				call fd_mode			; use new value
				ret
				;
				; long delay, cca 3ms
long_delay:		push b
				lxi b,0100h
ld1:			call fd_delay
				dcx b
				mov a,b
				ora c
				jnz ld1
				pop b
				ret
				;
				; wait aprox. 25us (42t fixed + 24t x LOOP)
				; 5MHz CPU 125 cycles (act.162)
				; 8MHz CPU 200 cycles (act.234)
fd_delay:		push b					;[12]
				lxi b,CRYSTAL/2			;[10]
dela1:			dcx b					;[6]
				mov a,b					;[4]
				ora c					;[4]
				jnz dela1				;[10]
				pop b					;[10]
				ret						;[10]
				;
				; wait until FDC is ready for new command, C flag set on timeout
busy_check:		push b
				mvi b,0FFh
busy_check2:	dcr b
				jz busy_err				; something is wrong
				in REG_MSR				; get FDC status
				ral						; look for busy bit
				jnc busy_wait			; wait
				rar						; remake status byte
				pop b
				stc
				cmc						; ok
				ret
busy_wait:		call fd_delay
				jmp busy_check2
busy_err:		pop b
				stc
				ret
				;
				; read one byte from data reg
read_byte:		call busy_check
				rc						; timeout
				in REG_MSR				; wants to give us result?
				ani 40h
				cpi 40h
				jz read_byte2
				stc
				; lxi h,DATA_NOT_READY
				ret
read_byte2:		in REG_DATA				; read the byte
				ora a					; set flags, clear carry
				ret
				;
				; write one byte to data reg
write_byte:		push psw
				call busy_check
				jnc .l1
				pop psw
				stc
				ret						; timeout
.l1				ani 0C0h
				cpi 80h					; ready to accept byte?
				jz .l2
				pop psw
				stc
				; lxi h,FDC_NOT_READY
				ret
.l2				pop psw
				out REG_DATA			; write the byte
				stc
				cmc						; clear carry
				ret
				;
				; initialise FDC
fd_init:		mvi a,CMD_RESET
				out REG_DSR
				call long_delay
				xra a
				out REG_DOR				; software reset
				call long_delay
				mvi a,CMD_INIT			; unset reset bit, no DMA, (stops motor)
				out REG_DOR
				xra a					; save 0
				call fd_delay
				lda ccr_dsr_value		; 250kbps/300kbps/500kbps
				out REG_DSR
				call fd_delay
				lda ccr_dsr_value		; 250kbps/300kbps/500kbps
				out REG_CCR
				call fd_configure
				call fd_specify
				call fd_mode
				ret
				;
				; National Semiconductor PC8477B identifies itself as 73h
fd_nsc:			;mvi a,CMD_NSC
				;call write_byte
				;call read_byte			 ;should be 73h
				;ret
				; configure command - implied seek, disable polling, fifo enable
fd_configure:	mvi a,CMD_CONFIG1
				call write_byte
				mvi a,CMD_CONFIG24
				call write_byte
				mvi a,CMD_CONFIG3
				call write_byte
				mvi a,CMD_CONFIG24
				call write_byte
				ret
				;
				; mode command - sets special features of fdc (FIFO, densel, low pwr, ..)
fd_mode:		mvi a,CMD_MODE
				call write_byte
				mvi a,MODE_BYTE1
				call write_byte
				mvi a,MODE_BYTE2
				call write_byte
				lda mode3_value			; DENSEL polarity
				call write_byte
				mvi a,MODE_BYTE4
				call write_byte
				ret
				;
				; mode specify - sets internal timers of fdc (step rate, motor on/of)
fd_specify:		mvi a,CMD_SPECIFY
				call write_byte
				mvi a,SPECIFY_BYTE1
				call write_byte
				mvi a,SPECIFY_BYTE2
				call write_byte
				ret
				;
				; sense drive status - read status register 3
fd_sensestat:	mvi a,CMD_SENSE_STAT
				call write_byte
				rc						; without diskette it hangs (TODO init FDC?)
				lda drive_nr			; 2nd byte is drive number
				call write_byte
				call read_byte			; result phase, status 3
				sta status_reg_3		; store it
				ani 10h					; check track 0 flag
				stc						; not track 0
				rz
				cmc						; track 0 detected
				ret
				;
				; recalibrate - move to track 0
fd_recalib:		mvi a,CMD_RECALIB		; output recalibrate command
				call write_byte
				lda drive_nr			; 2nd byte is drive number
				call write_byte
				lxi b,0FFFh				; wait limit
.l1				call fd_sensestat		; check for track 0
				jnc .l2					; track 0 detected
				dcx b
				mov a,b
				ora c
				jnz .l1
				;jmp sense_intrpt		 ;timeout, try sensing now
.l2				;fall through
				;
				; sense interrupt - clears busy bit in MSR after seek and recalibrate commands
sense_intrpt:	call long_delay			; give the drive some time
				in REG_MSR				; sense drive seek bits
				ani 0Fh					; all four drive bits
				lxi h,0					; for C
				rz
				call long_delay			; no hurry, mechanics is slow
				mvi a,CMD_SENS_INTR
				out REG_DATA
sense_stat:		call read_byte			; result phase, status0
				ani 0F0h				; isolate state bits
				mov b,a					; backup
				ani 0C0h				; D7,6 must be 00 for normal termination
				; lxi h,SENSE_FAILED		 ;for C program
				stc						; error
				rnz
				mov a,b
				ani 10h
				;lxi h,EQUIPMENT_CHECK	 ;track 0 signal failed after recalibrate
				stc
				rnz
sense_track:	call read_byte			; read track_nr (recalibrate sets to 0)
				sta track_nr			; drive is calibrated
sense_exit:		;lxi h,RESULT_OK		 ;for C programm
				;stc					optimized
				;cmc					 ;clear carry
				ret
				;
				; seek track
;fd_seek:		 mvi a,CMD_SEEK
;				 call write_byte		 ;output seek command
;				 call send_head_drv		 ;output head and drive number
;				 call send_track		 ;output track number
;				 lxi d,40h				 ;sense interrupt retries
;.l1			 call sense_intrpt
;				 jnc .l2
;				 dcr d
;				 jnz .l1
				;lxi h,SEEK_FAILED		 ;for C
;				 stc					 ;error
;				 ret
.l2				;lxi h,RESULT_OK		 ;for C
;				 ret					 ;optimized carry is cleared
				;
				; status - read result of a command
read_status:	lxi d,0					; bytes read
read_stl1:		lxi b,0					; timeout loop
read_stl2:		call fd_delay			; wait
				in REG_MSR
				ani 0F0h				; RQM=1, DIO=1, EXEC=0, BUSY=1 
				cpi 0D0h
				jz next_status
				cpi 80h					; command finished
				jz eval_status
				dcx b
				mov a,b
				ora c
				jnz read_stl2
				;lxi h,TIMEOUT_WATING	 ;timeout
				stc
				ret
next_status:	mov a,d
				cpi 07h					; there are 7 result bytes
				jz read_sterr			; too many status bytes
				lxi h,status_reg_0		; beginning of the status byte area
				dad d					; array offset
				inx d					; move to next array item
				in REG_DATA
				mov M,a
				jmp read_stl1			; loop
read_sterr:		;lxi h,ST_TOO_MANY		 ;should never happen
				stc
				ret
				;
				; evaluate status bytes
eval_status:	;lda status_reg_0		 ;process Status 0
				lda status_reg_1		; process Status 1
				mov b,a					; backup
				;ani 0x80				 ;end of track error
				;jz 1$					in non DMA transfers without using TC pin
				;lxi h,ST1_EOTER		it is expected condition. There is no other
				;stc					way to stop FDC to process next sector
				;ret
.l1				;mov a,b
				ani 37h
				jz eval_st2
				stc
				ret
				;ani 20h				 ;CRC error
				;jz .l2
				;lxi h,ST1_CRCER
				;stc
				;ret
.l2				;mov a,b
				;ani 10h				 ;overrun, CPU too slow
				;jz .l3
				;lxi h,ST1_OVERN
				;stc
				;ret
.l3				;mov a,b
				;ani 04h				 ;no data
				;jz .l4
				;lxi h,ST1_NODAT
				;stc
				;ret
.l4				;mov a,b
				;ani 02h				 ;write protect
				;jz .l5
				;lxi h,ST1_WRTPRT
				;stc
				;ret
.l5				;mov a,b
				;ani 01h				 ;mising address mark
				;jz eval_st2
				;lxi h,ST1_MSADR
				;stc
				;ret
eval_st2:		lda status_reg_2		; process Status 2
				mov b,a
				ani 73h
				jz read_stok
				stc
				ret
				;ani 12h				 ;wrong track detected
				;jz .l1
				;lxi h,ST2_BADTRK		 
				;stc
				;ret
.l1				;mov a,b
				;ani 40h				 ;scan not satisfied
				;jz .l2
				;lxi h,ST2_CRCERR
				;stc
				;ret
.l2				;mov a,b
				;ani 20h				 ;CRC error
				;jz .l3
				;lxi h,ST2_CRCERR
				;stc
				;ret
.l3				;mov a,b
				;ani 01h				 ;missing address mark in data field
				;jz read_stok
				;lxi h,ST2_MISADR
				;stc
				;ret
read_stok:		;lxi h,RESULT_OK		 ;all OK
				stc
				cmc
				ret
				;
				; output head and drive number
send_head_drv:	lda head_nr				; head number
				ral
				ral
				mov b,a					; backup
				lda drive_nr			; drive number
				ora b					; combine them
				call write_byte			; output head and drive number
				ret
				;
				; output track number
send_track:		lda track_nr			; get physical track
				call write_byte
				ret
				;
sector_adjust:	push	psw
				push	h
				lda		unadj_sctr_nr	; sector number (for both floppy sides appended)
				sta		sector_nr		; store to controller value
				lxi		h,number_of_sctrs		; side 1 of the diskette?
				cmp		M				; sector number less than 26 ?
				jc		.l2				; yes, jump over
				sub		M				; side 1 sectors (number_of_sctrs .. number_of_sctrs*2-1)
				sta		sector_nr		; store real sector number and select head 1
				mvi		a,1				; yes, side 1, 8" SS third drive ignores this value ***???
				jmp		.l3
.l2				xra		a				; no, side 0
.l3				sta		head_nr
				pop		h
				pop		psw
				ret
				;
				; command phase, send 7 data fields
fd_cmd_pha:		lda sector_nr			; load current sector number
				sta eot_sec_nr			; store it in EOT to stop processing more secs
				mvi c,7
				lxi d,track_nr			; table begin (track, head, sector, bytes, EOT, GAP, len)
fd_cmd_l1:		call busy_check
				ani 0F0h				; RQM/DIO
				cpi 90h					; RQM=1, DIO=0 (fdc ready for a byte), CMD in progress
				jnz fd_cmd_err			; error ..
				ldax d					; load data
				out REG_DATA			; send to fdc
				inx d
				dcr c
				jnz fd_cmd_l1			; next param
				ret						; command sent
fd_cmd_err:		jmp read_status			; jump and return to caller from there
				;
				; write one sector to diskette
fd_write:		mvi a,CMD_WRITE			; command phase
				call write_byte
				call send_head_drv		; send head << 2 | drive byte
				call fd_cmd_pha			; send 7 param bytes
				mvi e,0					; sector size 256 bytes
				lxi h,HSTBUF			; execution phase
fd_write_start: mvi a,(CRYSTAL/2+8)/4	; start write
				sta fdc_extraloop
fd_write_outlo: mvi c,0					; outer loop 256x
fd_write_inrlo: mvi b,0					; inner loop 256x
fd_write_l1:	in REG_MSR
				cpi 0B0h				; RQM=1,DIO=1,NDM=1,BUSY=1 (ready to receive byte)
				jnz .l1					; wait
				mov a,m					; transfer data to FDC
				out REG_DATA
				inx h
				dcr e
				jnz fd_write_l1			; next byte
				jmp read_status			; result phase
.l1				dcr b
				jnz fd_write_l1			; quickly poll for next byte
				cpi 0C0h				; waiting for very first byte to write (or abort)
				jz fd_cmd_abort			; RQM=1, DIO=1, EXEC=0, BUSY=0 (execution aborted)
				dcr c
				jnz fd_write_inrlo		; try again
				cpi 0D0h				; exec aborted (FDC hanged)?
				jnz .l2
				out REG_DATA			; take FDC out of hung
				jmp read_status			; and jump to status phase
.l2				lda fdc_extraloop
				dcr a
				sta fdc_extraloop
				jnz fd_write_outlo
				;lxi h,TIMEOUT_WATING	 ;timeout
				stc
				ret
				;
				; read one sector from diskette
fd_read:		mvi a,CMD_READ			; command phase
				call write_byte
				call send_head_drv		; send head << 2 | drive byte
				call fd_cmd_pha			; send 7 param bytes
				lxi h,HSTBUF			; execution phase
				mvi a,(CRYSTAL/2+8)/4	;
				sta fdc_extraloop
				mvi e,0					; sector size 256 bytes
fd_read_outlo:	mvi c,0					; outer loop 256x
fd_read_inrlo:	mvi b,0					; inner loop 256x
fd_read_l1:		in REG_MSR
				cpi 0F0h				; RQM/DIO/EXEC/BUSY
				jnz .l1					; data from floppy?
				in REG_DATA				; get it
				mov m,a					; transfer data to buff
				inx h
				dcr e
				jnz fd_read_l1			; next byte
				call read_status		; result phase
				ret
.l1				dcr b
				jnz fd_read_l1			; quickly poll for next byte
				cpi 0C0h				; waiting for first byte (or abort) takes longer
				jz fd_cmd_abort			; RQM=1, DIO=1, EXEC=0, BUSY=0 (execution aborted)
				dcr c
				jnz fd_read_inrlo		; try again
				cpi 0D0h				; exec aborted (FDC hanged)?
				jnz .l2
				;out REG_DATA			; take FDC out of hung - actually a bug..!!!
				jmp read_status			; and jump to status phase
.l2				lda fdc_extraloop
				dcr a
				sta fdc_extraloop
				jnz fd_read_outlo
				;lxi h,TIMEOUT_WATING	 ;timeout
				stc
				ret
fd_cmd_abort:	;lxi h,CMD_ABORT
				stc
				ret
				;
				; report status register to C
fd_msr:			;mvi h,0
				;in REG_MSR
				;mov l,a
				;ret
				;
				; execute command - all disk operations should be called from here
				; A - operation number (0 - recalibrate, 1 - seek, 2 - read sector, 3 - read sector)
fd_exec_cmd:	push psw
				call fd_motor_on		; motor started?
				pop psw
;				 cpi FDC_RECALIB		 ;recalibrate
;				 jnz .l1
;				 call fd_recalib
;				 jmp .l8
;.l1			 cpi FDC_SEEK			 ;seek track
;				 jnz .l2
;				 call fd_seek
;				 jmp .l8
.l2				cpi FDC_READ			; read sector
				jz .l3
				cpi FDC_WRITE			; write sector
				jnz .l8					; unknown operation
.l3				call sector_adjust		; adjust sector if it is behing last sector on first side
				mvi d,READ_RETRY		; 3x retry
				mov e,a					; backup operation
.l4				push d
				mov a,e					; restore operation (what to do, read or write)
				cpi FDC_READ			; READ op?
				jz .l5
				call fd_write			; call WRITE op
				jmp .l6
.l5				call fd_read			; call READ op
.l6				pop d
				jnc .l8					; success, return
				dcr d					; no luck retrying
				jz .l7
				lda track_nr			; backup track_nr to read
				mov c,a
				push b
				; push h					 ;backup error code (for C program)
				call fd_recalib			; recalibrate heads (sets track_nr to 0)
				; pop h					 ;restore error code
				pop b
				mov a,c					; restore track_nr
				sta track_nr			; set it to param table
				jmp .l4					; try again
.l7				stc						; no luck
.l8				push psw				; backup
				call fd_motor_off		; begin stop motor count
				pop psw					; restore
				ret
				;
				; start motor (if not already started), disable interrupt from timer
fd_motor_on:	lda drive_nr			; which drive?
				cpi 1					; drive 1?
				jz .l1					; yes jump
		if (NumFlps==3)
				cpi 2					; drive 2?
				jz .l2					; yes jump
		endif
				lda motor_0_state		; get motor 0 state
				ora a					; is it already spinning?
				jz .l3					; no. start it and wait for spin up
				jmp motor_on			; yes, only update drive select bits
.l1				lda motor_1_state		; get motor 1 state
				ora a					; is it already spinning?
				jz .l3					; no. start it and wait for spin up
				jmp motor_on			; yes, only update drive select bits
		if (NumFlps==3)
.l2				lda motor_2_state		; get motor 2 state
				ora a					; is it already spinning?
				jz .l3					; no. start it and wait for spin up
				jmp motor_on			; yes, only update drive select bits
		endif
.l3				call motor_on			; start motor now
				mvi b,20h				; wait till it starts, cca 100ms
.l4				call long_delay			; delay
				dcr b					; loop counter
				jnz .l4					; again
				ret
				;
				; motor on - start motor (when not already started and selects drive)
motor_on:		lda drive_nr
				mov b,a					; move to B
				ora b					; update Z flag
				mvi a,MOTOR_TIMEOUT		; timeout
				jz .l1					; drive 0
				dcr b					; decrement drive_nr
				jz .l2					; drive 1
		if (NumFlps==3)
				dcr b					; decrement drive_nr
				jz .l3					; drive 2
		endif
				;lxi h,BAD_DRIVE_NR		; for C programm
				stc						; error
				ret
.l1				sta motor_0_state		; set motor 0 started
				jmp motor_do			; jump further
.l2				sta motor_1_state		; set motor 1 started
		if (NumFlps==3)
				jmp motor_do			; jump
.l3				sta motor_2_state		; set motor 2 started
		endif
				; fall through
				; sets DOR register accordingly to motor flags
motor_do:		push h					; back up
				push b					; back up
				lxi h,motor_0_state		; point to motor 0
				xra a					; compute result DOR value
				mov b,a					; clear a,b regs
				ora M					; add motor bit for drive 0
				jz .l1					; zero means motor stopped
				mvi b,10h				; motor 0 is bit 4
.l1				inx h					; move to drive 1
				xra a
				ora M					; add motor bit for drive 1
				jz .l2					; motor 1 stopped
				mov a,b					; restore motor 0
				ori 20h					; motor 1 is bit 5
				mov b,a					; backup motors 0,1
		if (NumFlps==3)
.l2				inx h					; move to drive 2
				xra a
				ora M					; add motor bit for drive 2
				jz .l3					; motor 2 stopped
				mov a,b					; restore motors 0,1
				ori 40h					; motor 2 is bit 6
				mov b,a					; backup motors 0,1,2
		else
.l2				;
		endif
.l3				lda drive_nr			; compute result DOR value (drive bits 0,1)
				ora b					; add motor bits (7..4)
				ori 04h					; do not reset FDC :-)
				out REG_DOR				; set FDC register
				in REG_MSR				; needed (to get FDC in correct state), also flash LED
				pop b					; restore
				pop h					; restore
				ret
				;
				; stop motor (if not needed anymore)
fd_motor_off:	xra a					; clear acc
				sta ticks				; clear interval
				sta seconds				;
				; start timer - when it expires motor off will be called
				mvi a,00h				; 8155 timer
				out 04h					; LSB register
				mvi a,0FCh				; single pulse, frequency 10Hz
				out 05h					; MSB register
				mvi a,0CEh				; automatic reload mode, one shot
				out 00h					; command register
				mvi a,34h				; 8253 timer 0, mode 2
				out 01Bh				; control register
				mvi a,00h				; LSB value
				out 18h					; followed by MSB
				mvi a,3Ch				; pulse frequency 10Hz
				out 18h					; timer 0 register
				; hook up interrupt handler
				mvi a,0C3h				; JUMP instruction
				sta RST65_ADDR			; timer is connected to 6.5 interrupt
				lxi h,interpt_65		; routine's address
				shld RST65_ADDR+1
				ei						; enable interrupts
				rim						; get interrupt status
				ori 00001000b			; mask out none
				ani 00001101b			; set interrupt mask, enable 6.5 (timer)
				sim						; set mask
				ret
				;
				; interrupt 6.5 - the timer
interpt_65:		di
				push psw				; backup
				push b					; backup
				push h					; backup
				lda ticks				; load ticks
				inr a					; increment
				sta ticks				; store
				cpi 10					; 10hz signal
				jc .l4					; less then second, return
				xra a					; clear ticks
				sta ticks				; store
				lxi h,motor_0_state		; motor 0 time out
				mov a,M					; get count
				ora a					; seconds to stop
				jz .l1					; time out?
				dcr M					; no. only decrement time
.l1				inx h					; motor 1 time out
				mov a,M					; get count
				ora a					; seconds to stop
				jz .l2					; time out?
				dcr M					; no. only decrement time
		if (NumFlps==3)
.l2				inx h					; motor 2 time out
				mov a,M					; get count
				ora a					; seconds to stop
				jz .l3					; time out?
				dcr M					; no. only decrement time
		else
.l2				;
		endif
.l3				call motor_do			; set updated motor state to DOR register
				lxi h,motor_0_state		; motor 0
				mov a,M					; get count
				inx h					; motor 1
				ora M					; both motors stopped?
		if (NumFlps==3)
				inx h					; motor 2
				ora M					; all three motors stopped?
		endif
				jnz .l4					; yes,mask out 6.5 (timer)
				rim						; get interrupt status
				ori 00001010b			; mask out 6.5 (timer)
				ani 00001111b			; set interrupt mask, disable 6.5 (timer)
				sim						; set mask
				mvi a,4Eh				; stop timer immediately
				out 00h					; command register (8155)
				mvi a,38h				; 8253 timer 0, mode 4(stop)
				out 01Bh				; control register
.l4				pop h					; restore
				pop b					; restore
				pop psw					; restore
				ei						; done, enable interrupt again
				ret
				;

