;------------------------------------------------------------------------------
; Algorithm adapted from sample code supplied by Chuck(G) on vcf.com
; sector deblocking routines for 256 byte sectors on floppy disk
; Similar (for 512byte sectors) was used in Durango F-85
; Type of function (BDOS CONSTANTS ON ENTRY TO WRITE):
; 0	(normal sector write)
; 1	(write to directory sector)
; 2	(write to the first sector of a new data block)
;------------------------------------------------------------------------------
; disk request parameters.
DMAADR:			dw	080h				; LAST DMA
SEKDSK:									; SEEK DISK 
seek_disk:		db	-1					;
SEKTRK:									; SEEK TRACK 
seek_track:		dw	-1					;
SEKSEC:									; SEEK SECTOR 
seek_sector:	db	-1					;
; variables
offset:			dw	-1					; beginning of partition inside host sector
must_flush:		db	0					; must flush first
				;
				; set transfer address
setdma:			mov		l,c
				mov		h,b
				shld	DMAADR
				ret
				;
				; translate sector address
sectrn:			mov		l,c				; no translation
				mov		h,b				; 
				ret						; assume 1::1
				;
				; set disk unit
seldsk:			mov		a,c
				sta		seek_disk		; save for request
				lxi		h,0				; on error return 0
				cpi		NumFlps			; number of floppy drives
				rnc						; if out of range
				mov		b,a				; back up
				lda		drive_nr		; host_disk
				cmp		b				; check unit/last unit
				cnz		chkflush_buff	; flush buffer if different unit
				lda		seek_disk		; compute DPB address
				mov		l,a				; disk number
				mvi		h,0				; higher byte
				dad		h				; multiply 2x
				dad		h				; multiply 4x
				dad		h				; multiply 8x
				dad		h				; 16*ordinal
				lxi		d,DPH			; disk parameter table
				dad		d				; add together
				ret						; exit
				;
				; home
home:			lxi		h,0				; track 0
				shld	seek_track		; store track
				ret
				;
				; set track
settrk:			mov		h,b				; track input in BC
				mov		l,c				; move to HL
				shld	seek_track		; store track
				ret						; exit
				;
				; set sector, reduce to 256 byte host sector/128 byte offset
setsec:			mov		a,c				; get lower byte
				rar						; D0 to CY, partition 0 or 1
				mvi		a,0				; keep CY
				mov		h,a				; always 0
				rar						; CY to D7, gains offset 0 or 128
				mov		l,a				; real position inside host sector
				shld	offset			; store partition
				mov		a,c				; get lower byte
				ani		0FEh			; ignore last bit (clear D0)
				rrc						; sector/2, D0 to D7 and CY
				sta		seek_sector		; store sector
				ret
				;
				; read logical disk sector(128byte)
read_flp:		call	chkflush_buff	; is in buffer
				jnc		.l2				; if not in buffer
.l1:			lhld	offset			; offset from beginning of host sector
				lxi		d,HSTBUF		; address of cached physical sector's data
				dad		d				; add together
				xchg					; put to DE
				lhld	DMAADR			; CP/M buffer address to HL
				xchg					; swap DE<->HL
				call	move_block		; move 128 byte of data from HL to DEstination
				xra		a				; clear A and CPU flags **** optimized out
				ret						; done...
.l2:			mvi		a,FDC_READ		; READ command
				call	host_action		; also set new drive/track/sector
				jnc		.l1				; if okay
				ret						; error exit...
				;
				; write logical sector (128byte)
write_flp:		mov		a,c				; 0	(normal sector write), 1 (write to directory sector), 2..
				sta		writa			; save type of function
				call	chkflush_buff	; flush buffer
				jc		.l2				; if buffer okay
				lda		writa
				cpi		2				; write to the first sector of a new data block
				jnz		.l1				; if not a new block write then do read physical sector into buffer
				lda		seek_disk		; request drive
				sta		drive_nr		; save to FDC driver
				lda		seek_track		; request track
				sta		track_nr		; save to FDC driver
				lda		seek_sector		; request sector
				sta		unadj_sctr_nr	; save to FDC driver
				jmp		.l2				; go join write code
.l1:			mvi		a,FDC_READ		; READ command
				call	host_action		; also set new drive/track/sector
				rc						; error
.l2:			lhld	offset			; offset from beginning of physical sector
				lxi		d,HSTBUF		; address of cached physical sector's data
				dad		d				; add together
				xchg					; put to DE
				lhld	DMAADR			; CP/M buffer address to HL
				call	move_block		; move 128 byte of data from HL to DEstination
				lda		must_flush		; load write flag ***needed? mvi a,1
				ori		1				; set write flag
				sta		must_flush		; save it
				mvi		a,$-$			; move to A
writa			equ		$-1				; write type flag
				sui		1				; 1 (write to directory sector)
				jnz		.l3				; if not directory write
				sta		must_flush		; clear cache flag
				mvi		a,FDC_WRITE		; WRITE command
				call	host_action		; also sets set new drive/track/sector
				rc						; error
.l3:			xra		a				; success, clear CY and A
				ret
				;
				; flush disk buffer. Only set CY on the same unit/track/sector
chkflush_buff:	lda		drive_nr		; host disk
				mov		b,a				; back up
				lda		seek_disk		; seek disk
				cmp		b				; compare
				jnz		.l1				; not same unit
				lda		track_nr		; host track
				mov		b,a				; back up
				lda		seek_track		; seek track
				cmp		b				; compare
				jnz		.l1				; not same track
				lda		unadj_sctr_nr	; host sector
				mov		b,a				; back up
				lda		seek_sector		; seek sector
				cmp		b				; compare
				jnz		.l1				; not same host sector buffer
				stc
				ret
.l1:			lda		must_flush		; get cache flag
				ora		a				; update CPU zero flag(s)
				mvi		a,FDC_WRITE		; WRITE command, keep CPU flags
				cnz		host_exec		; do not set new drive/track/sector
				rc						; return on error
				xra		a				; clear CY and cache flag
				sta		must_flush		; save it
				ret
				;
				; move 128 bytes from (hl) to (de)
move_block:		mvi		c,128/4			; unrolled loop count
.l1:			mov		a,m				; get source byte 1
				stax	d				; save in destination
				inx		h				; increment source address
				inx		d				; increment destination address
				mov		a,m				; get source byte 2
				stax	d				; save in destination
				inx		h				; increment source address
				inx		d				; increment destination address
				mov		a,m				; get source byte 3
				stax	d				; save in destination
				inx		h				; increment source address
				inx		d				; increment destination address
				mov		a,m				; get source byte 4
				stax	d				; save in destination
				inx		h				; increment source address
				inx		d				; increment destination address
				dcr		c				; decrement loop count
				jnz		.l1				; loop again
				ret
				;
				; update sector address - disk/track/sector
host_action:	mov		b,a				; backup FDC_READ/FDC_WRITE command
				lda		seek_disk		; request drive
				sta		drive_nr		; save to FDC driver
				lda		seek_track		; request track
				sta		track_nr		; save to FDC driver
				lda		seek_sector		; request sector
				sta		unadj_sctr_nr	; save to FDC driver
				mov		a,b				; restore command
				; fall through
				; performs the read or write to the physical disk
host_exec:		call	fd_exec_cmd		; execute
				mvi		a,1				; error
				rc						; both CY and A are set
.l1				xra		a				; success, clear both CY and A
				ret
				;
