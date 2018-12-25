;------------------------------------------------------------------------------
; BIOS disk buffer areas
; CSV scratch pad area used for software check for changed disks.
; CKS = (DRM + 1)/4, where DRM is the last directory entry number
; ALV scratch pad area used by the BDOS to keep disk storage allocation information
; (DSM/8) + 1
;------------------------------------------------------------------------------
;
HSTBUF:         ds		HSTSIZ			; HOST BUFFER
DIRBUF:         ds      128             ; DIR BUFFER
                ;
        if (Floppy==360)
ALV0:		ds	23		; disk A: allocation vector
CSV0:		ds	16		; disk A: directory checksum
ALV1:		ds	23		; disk B: allocation vector
CSV1:		ds	16		; disk B: directory checksum
        elseif (Floppy==720)
ALV0:		ds	46		; disk A: allocation vector
CSV0:		ds	16		; disk A: directory checksum
ALV1:		ds	46		; disk B: allocation vector
CSV1:		ds	16		; disk B: directory checksum
        elseif (Floppy==120)
ALV0:		ds	65		; disk A: allocation vector
CSV0:		ds	32		; disk A: directory checksum
ALV1:		ds	65		; disk B: allocation vector
CSV1:		ds	32		; disk B: directory checksum
        elseif (Floppy==144)
ALV0:		ds	80		; disk A: allocation vector
CSV0:		ds	64		; disk A: directory checksum
ALV1:		ds	80		; disk B: allocation vector
CSV1:		ds	64		; disk B: directory checksum
        elseif (Floppy==100)
ALV0:		ds	63		; disk A: allocation vector
CSV0:		ds	32		; disk A: directory checksum
ALV1:		ds	63		; disk B: allocation vector
CSV1:		ds	32		; disk B: directory checksum
        endif
        if (Extra==50)	; extra drive C: 1x 500kB 8"
ALV2:		ds	32		; disk C: allocation vector
CSV2:		ds	16		; disk C: directory checksum
        endif
