;----------------------------------------------------------------------
; Hello World Operating System Boot Program
;
; Joel Gompert 2001
;
; Disclaimer: I am not responsible for any results of the use of the contents
;   of this file
;----------------------------------------------------------------------
	org 0x7c00	; This is where BIOS loads the bootloader


; Execution begins here
entry:
	jmp short begin ; jump over the DOS boot record data


; ----------------------------------------------------------------------
; data portion of the "DOS BOOT RECORD"
; ----------------------------------------------------------------------
brINT13Flag     DB      90H             ; 0002h - 0EH for INT13 AH=42 READ
brOEM           DB      'MSDOS5.0'      ; 0003h - OEM name & DOS version (8 chars)
brBPS           DW      512             ; 000Bh - Bytes/sector
brSPC           DB      1               ; 000Dh - Sectors/cluster
brResCount      DW      1               ; 000Eh - Reserved (boot) sectors
brFATs          DB      2               ; 0010h - FAT copies
brRootEntries   DW      0E0H		; 0011h - Root directory entries
brSectorCount   DW      2880		; 0013h - Sectors in volume, < 32MB
brMedia         DB      240		; 0015h - Media descriptor
brSPF           DW      9               ; 0016h - Sectors per FAT
brSPH           DW      18              ; 0018h - Sectors per track
brHPC           DW      2		; 001Ah - Number of Heads
brHidden        DD      0               ; 001Ch - Hidden sectors
brSectors       DD      0	        ; 0020h - Total number of sectors
		DB      0               ; 0024h - Physical drive no.
		DB      0               ; 0025h - Reserved (FAT32)
		DB      29H             ; 0026h - Extended boot record sig 
brSerialNum     DD      404418EAH       ; 0027h - Volume serial number (random)
brLabel         DB      'Joels disk '   ; 002Bh - Volume label  (11 chars)
brFSID          DB      'FAT12   '      ; 0036h - File System ID (8 chars)
;------------------------------------------------------------------------


; --------------------------------------------
;  Boot program code begins here
; --------------------------------------------
; boot code begins at 0x003E
begin:
	xor	ax, ax		; zero out ax
	mov	ds, ax		; set data segment to base of RAM
	mov	si, msg		; load address of our message
	call	putstr		; print the message

hang:
	jmp	hang		; just loop forever.

; --------------------------------------------
; data for our program

msg	db	'Hello, World!', 0

; ---------------------------------------------
; Print a null-terminated string on the screen
; ---------------------------------------------
putstr:
	lodsb		; AL = [DS:SI]
	or al, al	; Set zero flag if al=0
	jz putstrd	; jump to putstrd if zero flag is set
	mov ah, 0x0e	; video function 0Eh (print char)
	mov bx, 0x0007	; color
	int 0x10
	jmp putstr
putstrd:
	retn
;---------------------------------------------

size	equ	$ - entry
%if size+2 > 512
  %error "code is too large for boot sector"
%endif
	times	(512 - size - 2) db 0

	db	0x55, 0xAA		;2  byte boot signature