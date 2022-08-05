;
; Title:	BBC Basic for AGON
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/08/2022
;
; Modinfo:
; 24/07/2022:	OSWRCH and OSRDCH now execute code in MOS
; 03/08/2022:	OSLOAD and OSSAVE now execute code in MOS, added * commands, implemented some file I/O commands
			
			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"
			INCLUDE "mos_api.inc"	; In MOS/src
		
			SEGMENT CODE
				
			XDEF	OSWRCH
			XDEF	OSLINE
			XDEF	ESCSET
			XDEF 	CLRSCN
			XDEF	MODE
			XDEF	PUTIME
			XDEF	GETIME
			XDEF	PUTCSR
			XDEF 	GETCSR
			XDEF	OSRDCH
			XDEF	PROMPT
			XDEF	OSKEY
			XDEF	TRAP
			XDEF	LTRAP
			XDEF	OSINIT
			XDEF	OSCLI
			XDEF	GETSCHR
			
			XDEF	GCOL
			XDEF	MOVE
			XDEF	PLOT
			XDEF	POINT
			XDEF	DRAW
			XDEF	SOUND
			XDEF	OSBPUT
			XDEF	OSBGET
			XDEF	OSSTAT
			XDEF	OSSHUT
			XDEF	OSOPEN
			XDEF	OSCALL
			XDEF	GETPTR
			XDEF	PUTPTR
			XDEF	GETEXT
			XDEF	RESET
			XDEF	OSLOAD
			XDEF	OSSAVE

			XREF	ASC_TO_NUMBER
			XREF	RAM_START
			XREF	RAM_END
			XREF	FLAGS
			XREF	ESCAPE
			XREF	USER
			XREF	RAM_Top
			XREF	EXTERR
			XREF	COUNT0
			XREF	EXPRI
			XREF	COMMA
			XREF	XEQ
			XREF	CRTONULL
			XREF	NULLTOCR
			XREF	CRLF

; OSWRCH: Write a character out to the ESP32 VDU handler via the MOS
; A: Character to write
;
OSWRCH:			RST.LIS	10h		; This is at odds with the manual - assembles to RST.L
			RET

; OSRDCH: Read a character in from the ESP32 keyboard handler
;
OSRDCH:			MOSCALL	mos_getkey
			OR	A 		
			JR	Z, OSRDCH	; Loop until key is pressed
			RET

; OSLINE: Invoke the line editor
;
OSLINE:			PUSH	IY
			PUSH	HL			; Buffer address
			LD	BC, 255			; Buffer length
			MOSCALL	mos_editline		; Call the MOS line editor
			POP	HL			; Pop the address
			POP	IY
			PUSH	AF			; Stack the return value (key pressed)
			CALL	NULLTOCR		; Turn the 0 character to a CR
			CALL	CRLF			; Display CRLF
			POP	AF
			CP	1Bh			; Check for Escape
			JR	NZ, $F
			CALL	ESCSET
			CALL	LTRAP			
$$:			XOR	A			; Return A = 0			
			RET
;			JP	Edit_Line 			

; CLRSCN: clears the screen.
;
CLRSCN:			LD	A, 0CH
			JP	OSWRCH
				
; MODE n: Set video mode
;
MODE:			CALL    EXPRI
			EXX
			VDU	16H
			VDU	L
			CALL	CLRSCN				
			JP	XEQ

; PUTIME: set current time to DE:HL, in centiseconds.
;
PUTIME:			PUSH 	IX
			LD	C, sysvar_time
			MOSCALL	mos_sysvars
			LD.LIL	(IX + 0), L
			LD.LIL	(IX + 1), H
			LD.LIL	(IX + 2), E
			LD.LIL	(IX + 3), D
			POP	IX
			RET

; GETIME: return current time in DE:HL, in centiseconds
;
GETIME:			PUSH 	IX
			MOSCALL	mos_sysvars
			LD.LIL	L, (IX + 0)
			LD.LIL	H, (IX + 1)
			LD.LIL	E, (IX + 2)
			LD.LIL	D, (IX + 3)
			POP	IX
			RET

; PUTCSR: move to cursor to x=DE, y=HL
;
PUTCSR:			LD	A, 1Fh				; TAB
			RST.LIS	10h
			LD	A, E				; X
			RST.LIS 10h
			LD	A, L				; Y
			RST.LIS 10h
			RET

; GETCSR: return cursor position in x=DE, y=HL
; TODO: Need to get value from MOS/VDP
;
GETCSR:			LD 	DE, 0
			LD	HL, 0
			RET

; PROMPT: output the input prompt
;
PROMPT: 		LD	A,'>'
			JP	OSWRCH

;OSKEY - Read key with time-limit, test for ESCape.
;Main function is carried out in user patch.
;   Inputs: HL = time limit (centiseconds)
;  Outputs: Carry reset if time-out
;           If carry set A = character
; Destroys: A,H,L,F
;
OSKEY: 			MOSCALL	mos_getkey	; Read keyboard
			OR	A		; If we have a character
			JR	NZ, $F		; Then process it
			LD	A,H		; Check if HL is 0 (this is passed by INKEY() function
			OR	L
			RET	Z 		; If it is then ret
			HALT			; Bit of a bodge so this is timed in ms
			DEC	HL 		; Decrement the counter and 
			JR	OSKEY 		; loop
$$:			CP	1BH		; If we are not pressing ESC, 
			SCF 			; then flag we've got a character
			RET	NZ		
;
ESCSET: 		PUSH    HL
        		LD      HL,FLAGS
        		BIT     6,(HL)          ; ESC DISABLED?
        		JR      NZ,ESCDIS
        		SET     7,(HL)     	; SET ESCAPE FLAG
ESCDIS: 		POP     HL
        		RET	
;
ESCTEST:		MOSCALL	mos_getkey
			CP	1BH		; ESC	
			JR	Z,ESCSET
			RET
;
TRAP:			CALL	ESCTEST
LTRAP:			LD	A,(FLAGS)
			OR	A
			RET	P
			LD	HL,FLAGS 
			RES	7,(HL)
			JP	ESCAPE

;OSINIT - Initialise RAM mapping etc.
;If BASIC is entered by BBCBASIC FILENAME then file
;FILENAME.BBC is automatically CHAINed.
;   Outputs: DE = initial value of HIMEM (top of RAM)
;            HL = initial value of PAGE (user program)
;            Z-flag reset indicates AUTO-RUN.
;  Destroys: A,D,E,H,L,F
;
OSINIT:			XOR	A
			LD	(FLAGS), A		; Clear flags and set F = Z
			LD 	HL, USER
			LD	DE, RAM_Top
			LD	E, A			; Page boundary
			RET	

;
;OSCLI - Process an "operating system" command
;
OSCLI: 			CALL    SKIPSP
			CP      CR
			RET     Z
			CP      '|'
			RET     Z
			CP      '.'
			JP      Z,STAR_DOT		; *.
			EX      DE,HL
			LD      HL,COMDS
OSCLI0:			LD      A,(DE)
			CALL    UPPRC
			CP      (HL)
			JR      Z,OSCLI2
			JR      C,HUH
OSCLI1:			BIT     7,(HL)
			INC     HL
			JR      Z,OSCLI1
			INC     HL
			INC     HL
			JR      OSCLI0
;
OSCLI2:			PUSH    DE
OSCLI3:			INC     DE
			INC     HL
			LD      A,(DE)
			CALL    UPPRC
			CP      '.'			; ABBREVIATED?
			JR      Z,OSCLI4
			XOR     (HL)
			JR      Z,OSCLI3
			CP      80H
			JR      Z,OSCLI4
			POP     DE
			JR      OSCLI1
;
OSCLI4:			POP     AF
		        INC     DE
OSCLI5:			BIT     7,(HL)
			INC     HL
			JR      Z,OSCLI5
			LD      A,(HL)
			INC     HL
			LD      H,(HL)
			LD      L,A
			PUSH    HL
			EX      DE,HL
			JP      SKIPSP

HUH:    		LD      A,254
        		CALL    EXTERR
        		DB    	'Bad command'
        		DEFB    0			

SKIPSP:			LD      A,(HL)
        		CP      ' '
        		RET     NZ
        		INC     HL
        		JR      SKIPSP	

UPPRC:  		AND     7FH
			CP      '`'
			RET     C
			AND     5FH			; CONVERT TO UPPER CASE
			RET					

; Each command has bit 7 of the last character set, and is followed by the address of the handler
; These must be in alphabetical order
;
COMDS:  		DB	'BY','E'+80h		; BYE
			DW	STAR_BYE
			DB	'CA','T'+80h		; CAT
			DW	STAR_CAT
			DB	'C', 'D'+80h		; CD
			DW	STAR_CD
			DB	'ERAS','E'+80h		; ERASE
			DW	STAR_ERASE
			DB	'F','X'+80h		; FX
			DW	STAR_FX
			DB	0FFH	
			
; *BYE
;
STAR_BYE:		RST.LIS	00h			; Reset MOS

; *CAT / *.
;
STAR_DOT:
STAR_CAT:		PUSH	IY
			MOSCALL	mos_dir
			POP	IY
			RET
			
; *CD path
;
STAR_CD:		PUSH 	IY
			CALL	SKIPSP
			CALL	CRTONULL
			MOSCALL	mos_cd
			CALL	NULLTOCR	
			POP	IY
			RET
			
; *ERASE filename
;
STAR_ERASE:		PUSH	IY
			CALL	SKIPSP
			CALL	CRTONULL
			MOSCALL	mos_del
			CALL	NULLTOCR
			POP	IY
			RET
			
; OSCLI FX n
;
STAR_FX:		CALL	ASC_TO_NUMBER	; C: FX #
			LD	C, E
			CALL	ASC_TO_NUMBER	; B: First parameter
			LD	B, E
			CALL	ASC_TO_NUMBER	; E: Second parameter
			LD	L, B 		; L: First parameter
			LD	H, E 		; H: Second parameter
			LD	A, C 		; A: FX #, and fall through to OSBYTE	
;
; OSBYTE
;  A: FX #
;  L: First parameter
;  H: Second parameter
;
OSBYTE:			CP	13H
			JR	Z, OSBYTE_13
			JP	HUH

; OSBYTE 0x13 (FX 19): Wait 1/50th of a second
;
OSBYTE_13:		HALT	
			LD	L, 0
			JP	COUNT0

;OSLOAD - Load an area of memory from a file.
;   Inputs: HL addresses filename (CR terminated)
;           DE = address at which to load
;           BC = maximum allowed size (bytes)
;  Outputs: Carry reset indicates no room for file.
; Destroys: A,B,C,D,E,H,L,F
;
OSLOAD:			CALL	CRTONULL
			MOSCALL	mos_load
			PUSH	AF
			CALL	NULLTOCR
			POP	AF
			RET

;OSSAVE - Save an area of memory to a file.
;   Inputs: HL addresses filename (term CR)
;           DE = start address of data to save
;           BC = length of data to save (bytes)
; Destroys: A,B,C,D,E,H,L,F
;
OSSAVE:			CALL	CRTONULL
			MOSCALL	mos_save
			PUSH	AF
			CALL 	NULLTOCR
			POP	AF
			RET
			
;OSCALL - Intercept page &FF calls and provide an alternative address
;
;&FFF7:	OSCLI	Execute *command.
;&FFF4:	OSBYTE	Various byte-wide functions.
;&FFF1:	OSWORD	Various control block functions.
;&FFEE:	OSWRCH	Write character to output stream.
;&FFE7:	OSNEWL	Write NewLine to output stream.
;&FFE3:	OSASCI	Write character or NewLine to output stream.
;&FFE0:	OSRDCH	Wait for character from input stream.
;&FFDD:	OSFILE	Perform actions on whole files or directories.
;&FFDA:	OSARGS	Read and write information on open files or filing systems.
;&FFD7:	OSBGET	Read a byte from an a channel.
;&FFD4:	OSBPUT	Write a byte to a channel.
;&FFD1:	OSGBPB	Read and write blocks of data.
;&FFCE:	OSFIND	Open or close a file.
;
OSCALL:			LD	HL, OSCALL_TABLE
OSCALL_1:		LD	A, (HL)
			INC	HL
			CP	FFh
			RET	Z 
			CP	A, IYL
			JR	Z, OSCALL_2
			RET	NC
			INC	HL 
			INC	HL 
			JR	OSCALL_1
OSCALL_2:		LD	A, (HL)
			LD	IYL, A 
			INC	HL 
			LD	A, (HL) 
			LD	IYH, A 
			RET
OSCALL_TABLE:		DB 	D4h
			DW 	OSBPUT
			DB 	D7h
			DW 	OSBGET
			DB 	EEh
			DW 	OSWRCH
			DB	F4h
			DW 	OSBYTE
			DB	FFh	

; OSOPEN
; HL: Pointer to path
;  F: C Z
;     x x OPENIN
; 	  OPENOUT
;     x	  OPENUP
; Returns:
;  A: Filehandle, 0 if cannot open
;
OSOPEN:			LD	C, fa_read
			JR	Z, $F
			LD	C, fa_write | fa_open_append
			JR	C, $F
			LD	C, fa_write | fa_create_always
$$:			PUSH	IY
			MOSCALL	mos_fopen
			POP	IY
			RET

;OSSHUT - Close disk file(s).
; E = file channel
;  If E=0 all files are closed (except SPOOL)
; Destroys: A,B,C,D,E,H,L,F
;
OSSHUT:			LD	C, E
			PUSH	IY
			MOSCALL	mos_fclose
			POP	IY
			RET
	
; OSBGET - Read a byte from a random disk file.
;  E = file channel
; Returns
;  A = byte read
;  Carry set if LAST BYTE of file
; Destroys: A,B,C,F
;
OSBGET:			LD	C, E
			PUSH	IY
			MOSCALL	mos_fgetc
			POP	IY
			RET
	
; OSBPUT - Write a byte to a random disk file.
;  E = file channel
;  A = byte to write
; Destroys: A,B,C,F
;	
OSBPUT:			LD	C, E
			LD	B, A
			PUSH	IY
			MOSCALL	mos_fputc
			POP	IY
			RET

; OSSTAT - Read file status
;  E = file channel
; Returns
;  F: Z flag set - EOF
;  A: If Z then A = 0
; Destroys: A,D,E,H,L,F
;
OSSTAT:			RET
	
; GETPTR - Return file pointer.
;    E = file channel
; Returns:
; DEHL = pointer (0-&7FFFFF)
; Destroys: A,B,C,D,E,H,L,F
;
GETPTR:			RET

; PUTPTR - Update file pointer.
;    A = file channel
; DEHL = new pointer (0-&7FFFFF)
; Destroys: A,B,C,D,E,H,L,F
;
PUTPTR:			RET
	
; GETEXT - Find file size.
;    E = file channel
; Returns:
; DEHL = file size (0-&800000)
; Destroys: A,B,C,D,E,H,L,F
;
GETEXT:			RET	

; GCOL mode,R,G,B
;
GCOL:			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+0), A	; The mode
			CALL	COMMA

			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+1), A	; Red
			CALL	COMMA

			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+2), A	; Green
			CALL	COMMA

			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+3), A	; Blue				
			
			VDU	12H			; VDU code for GCOL
			VDU	(VDU_BUFFER+0)		; Mode
			VDU	(VDU_BUFFER+1)		; Red
			VDU	(VDU_BUFFER+2)		; Green
			VDU	(VDU_BUFFER+3)		; Blue
			
			JP	XEQ
				

	
; PLOT mode,x,y
;
PLOT:			CALL	EXPRI		; Parse mode
			EXX					
			PUSH	HL		; Push mode (L) onto stack
			CALL	COMMA 	
			CALL	EXPR_W2		; Parse X and Y
			POP	BC		; Pop mode (C) off stack
PLOT_1:			VDU	19H		; VDU code for PLOT				
			VDU	C		;  C: Mode
			VDU	E		; DE: X
			VDU	D
			VDU	L		; HL: Y
			VDU	H
			JP	XEQ

; MOVE x,y
;
MOVE:			CALL	EXPR_W2		; Parse X and Y
			LD	C, 04H		; Plot mode 04H (Move)
			JR	PLOT_1		; Plot
				

; Get two word values from EXPR in DE, HL
; IY: Pointer to expression string
; Returns:
; DE: P1
; HL: P2
;
EXPR_W2:		CALL	EXPRI			; Get first parameter	
			EXX
			PUSH	HL
			CALL	COMMA 
			CALL	EXPRI			; Get second parameter
			EXX
			POP	DE
			RET

; Stuff not implemented yet
;
GETSCHR:
POINT:
DRAW:
SOUND:
RESET:			RET
