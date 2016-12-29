;----------------------------------------------------------------------
; Hello World Operating System
;
; Joel Gompert 2001
;
; Disclaimer: I am not responsible for any results of the use of the contents
;   of this file
;----------------------------------------------------------------------

; --------------------------------------------
;  Here is the operating system entry point
; --------------------------------------------
begin:
	mov	ax, cs		; Get the current segment
	mov	ds, ax		; The data is in this segment
	cli			; disable interrupts while changing stack
	mov	ss, ax		; We'll use this segment for the stack too
	mov	sp, 0xfffe	; Start the stack at the top of the segment
	sti			; Reenable interrupts

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