;------------------------------------------------------------------------------
; DEFW	spt		;Number of 128-byte records per track
; DEFB	bsh		;Block shift. 3 => 1k, 4 => 2k, 5 => 4k....
; DEFB	blm		;Block mask. 7 => 1k, 0Fh => 2k, 1Fh => 4k...
; DEFB	exm		;Extent mask, see later
; DEFW	dsm		;(no. of blocks on the disc)-1
; DEFW	drm		;(no. of directory entries)-1
; DEFB	al0		;Directory allocation bitmap, first byte
; DEFB	al1		;Directory allocation bitmap, second byte
; DEFW	cks		;Checksum vector size, 0 for a fixed disc
;				;No. directory entries/4, rounded up.
; DEFW	off		;Offset, number of reserved tracks
;
; The directory allocation bitmap is interpreted as:
;	   al0				al1
; b7b6b5b4b3b2b1b0 b7b6b5b4b3b2b1b0
; 1 1 1 1 0 0 0 0  0 0 0 0 0 0 0 0
; ie, in this example, the first 4 blocks of the disc contain the directory.
;------------------------------------------------------------------------------
; Disk parameters
DPH:
				dw	0,0					; no translation table
				dw	0,0
				dw	DIRBUF,DPB0			; buff.adr., disk param adr.
				dw	CSV0,ALV0			; checksum zone adr., alloc.bit map adr.
				dw	0,0					; no translation table
				dw	0,0
				dw	DIRBUF,DPB0			; buff.adr., disk param adr.
				dw	CSV1,ALV1			; checksum zone adr., alloc.bit map adr.
			if (NumFlps==3)
				dw	0,0					; no translation table
				dw	0,0
				dw	DIRBUF,DPB2			; buff.adr., disk param adr.
				dw	CSV2,ALV2			; checksum zone adr., alloc.bit map adr.
			endif
; Diskette 5,25" DD, 360kB(DOS and CP/M)
; 40 tracks(two side), 18 (256 byte) sectors per track/side, 1440 sectors totally
; 180(175) allocation 2kB blocks (first track reserved for system)
; 64 dir size (1x16x4) - dir is saved in 1 allocation blocks
; 0 system track
			if (Floppy==360)
DPB0:			dw	72					; SPT - logical sectors per track
				db	4					; BSH - posun bloku
				db	15					; BLM - block mask
				db	1					; EXM - ext.mask, 32kB per extent
				dw	179					; DSM - capacity-1 - full 360kB
				dw	63					; DRM - dir size-1
				db	128					; AL0 - dir allocation mask
				db	0					; AL1
				dw	16					; CKS - checksum array size
				dw	0					; OFF - system tracks - no system track
			endif

; Diskette 5,25" DD, 720kB(DOS and CP/M)
; 80 tracks(two side), 18 (256 byte) sectors per track/side, 2880 sectors totally
; 360(350) allocation 2kB blocks (first track reserved for system)
; 64 dir size (1x16x4) - dir is saved in 1 allocation blocks
; 0 system track
			if (Floppy==720)
DPB0:			dw	72					; SPT - logical sectors per track
				db	4					; BSH - posun bloku
				db	15					; BLM - block mask
				db	0					; EXM - ext.mask, 16kB per extent
				dw	359					; DSM - capacity-1 - full 720kB
				dw	63					; DRM - dir size-1
				db	128					; AL0 - dir allocation mask
				db	0					; AL1
				dw	16					; CKS - checksum array size
				dw	0					; OFF - system tracks - no system track
			endif

; Diskette 5,25" HD
; 5.25" / 1.2MB(DOS) / 1.04MB(CP/M)
; 80 tracks(two side), 26 (256 byte) sectors per track/side, 4160 sectors totally
; 520(513) allocation 2kB blocks (first track reserved for system)
; 128 dir size (2x16x4) - dir is saved in 2 allocation blocks (16log.secs per block, 4dir entries per sec.)
; 0 system track(s)
			if (Floppy==120)
DPB0:			dw 104					; SPT - logical sectors per track
				db 4					; BSH - block shift
				db 15					; BLM - block mask
				db 0					; EXM - ext.mask
				dw 519					; DSM - capacity-1
				dw 127					; DRM - dir size-1
				db 192					; AL0 - dir allocation mask
				db 0					; AL1
				dw 32					; CKS - checksum array size
				dw 0					; OFF - system tracks 
			endif
; Diskette 3,5" HD
; 3.5" / 1.44MB(DOS) / 1.28MB(CP/M)
; 80 tracks(two side), 32 (256 byte) sectors per track/side, 5120 sectors totally
; 640(632) allocation 2kB blocks (first track reserved for system)
; 256 dir size (4x16x4) - dir is saved in 4 allocation blocks
; 0 system track
			if (Floppy==144)
DPB0:			dw 128					; SPT - logical sectors per track
				db 4					; BSH - block shift
				db 15					; BLM - block mask
				db 0					; EXM - ext.mask
				dw 639					; DSM - capacity-1
				dw 255					; DRM - dir size-1
				db 240					; AL0 - dir allocation mask
				db 0					; AL1
				dw 64					; CKS - checksum array size
				dw 0					; OFF - system tracks 
			endif
; Diskette 8" DS/DD, 1.0MB
; 77 tracks(two side), 26 (256 byte) sectors per track/side, 4004 sectors totally
; 500(492) allocation 2kB blocks (first track reserved for system)
; 128 dir size (2x16x4) - dir is saved in 2 allocation blocks
; 0 system track
			if (Floppy==100)
DPB0:			dw 104					; SPT - logical sectors per track
				db 4					; BSH - block shift
				db 15					; BLM - block mask
				db 0					; EXM - ext.mask
				dw 499					; DSM - capacity-1
				dw 255					; DRM - dir size-1
				db 240					; AL0 - dir allocation mask
				db 0					; AL1
				dw 32					; CKS - checksum array size
				dw 0					; OFF - system tracks 
			endif
; Extra diskette 8" SS/DD, 500kB, only as C: drive (A: B: is 1.2MB 5.25")
; 77 tracks(two side), 26 (256 byte) sectors per track/ only one side, 2002 sectors totally
; 250 allocation 2kB blocks (2000 sectors), 2 sectors unused
; 64 dir size (1x16x4) - dir is saved in 1 allocation blocks
; 0 system track
; QUICK and DIRTY hack - make it pretend it is double sided to avoid blocking/deblocking bug
; virtual track sector -> trck = (track*2 + sector%26), sec = (sector % 26)
			if (Extra==50)
DPB2:			dw 52					; SPT - logical sectors per track
				db 4					; BSH - block shift
				db 15					; BLM - block mask
				db 1					; EXM - ext.mask
				dw 249					; DSM - capacity-1
				dw 63					; DRM - dir size-1
				db 128					; AL0 - dir allocation mask
				db 0					; AL1
				dw 16					; CKS - checksum array size
				dw 0					; OFF - system tracks 
			endif
