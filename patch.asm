;
; Title:	BBC Basic for AGON
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/10/2022
;
; Modinfo:
; 24/07/2022:	OSWRCH and OSRDCH now execute code in MOS
; 03/08/2022:	OSLOAD and OSSAVE now execute code in MOS, added * commands, implemented some file I/O commands
; 05/08/2022:	Implemented OSSTAT, assumes MOS will save registers in file I/O commands
; 07/08/2022:	Added CLG, COLOUR, and emulated palette mode for COLOUR and GCOL, fixed GETCSR
; 19/08/2022:	Moved GETSCHR, POINT to agon_graphics.asm, optimised GETCSR, added SOUND
; 19/09/2022:	Added STAR_REN, improved filename parsing for star commands, moved SOUND to agon_sound.asm
; 24/09/2022:	Added STAR_MKDIR, STAR_EDIT; file errors for MOS commands LOAD, SAVE, CD, ERASE, REN, DIR
; 03/10/2022:	Fixed OSBYTE_13 command
			
			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"
			INCLUDE "mos_api.inc"	; In MOS/src
		
			SEGMENT CODE
				
			XDEF	OSWRCH
			XDEF	OSLINE
			XDEF	ESCSET
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
			XDEF	EXPR_W2

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
			XREF	NXT
			XREF	NULLTOCR
			XREF	CRLF
			XREF	CSTR_FNAME
			XREF	FINDL
			XREF	OUT_
			XREF	ERROR_
			XREF	DECODE
			XREF	BUF_DETOKEN
			XREF	BUF_PBCDL
			XREF	ONEDIT

; OSWRCH: Write a character out to the ESP32 VDU handler via the MOS
; A: Character to write
;
OSWRCH:			RST.LIS	10h			; This is at odds with the manual - assembles to RST.L
			RET

; OSRDCH: Read a character in from the ESP32 keyboard handler
;
OSRDCH:			MOSCALL	mos_getkey
			OR	A 		
			JR	Z, OSRDCH		; Loop until key is pressed
			RET

; OSLINE: Invoke the line editor
;
OSLINE:			LD 	E, 1			; Default is to clear the buffer
;
OSLINE1:		PUSH	IY			; Entry point to not clear buffer
			PUSH	HL			; Buffer address
			LD	BC, 256			; Buffer length
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

; PUTIME: set current time to DE:HL, in centiseconds.
;
PUTIME:			PUSH 	IX
			MOSCALL	mos_sysvars
			LD.LIL	(IX + sysvar_time + 0), L
			LD.LIL	(IX + sysvar_time + 1), H
			LD.LIL	(IX + sysvar_time + 2), E
			LD.LIL	(IX + sysvar_time + 3), D
			POP	IX
			RET

; GETIME: return current time in DE:HL, in centiseconds
;
GETIME:			PUSH 	IX
			MOSCALL	mos_sysvars
			LD.LIL	L, (IX + sysvar_time + 0)
			LD.LIL	H, (IX + sysvar_time + 1)
			LD.LIL	E, (IX + sysvar_time + 2)
			LD.LIL	D, (IX + sysvar_time + 3)
			POP	IX
			RET

; PUTCSR: move to cursor to x=DE, y=HL
;
PUTCSR:			LD	A, 1Fh			; TAB
			RST.LIS	10h
			LD	A, E			; X
			RST.LIS 10h
			LD	A, L			; Y
			RST.LIS 10h
			RET

; GETCSR: return cursor position in x=DE, y=HL
;
GETCSR:			PUSH	IX			; Get the system vars in IX
			MOSCALL	mos_sysvars		; Reset the semaphore
			RES.LIL	0, (IX+sysvar_vpd_pflags)
			VDU	23			; Do VDU 23,0,2
			VDU	0
			VDU	2
$$:			BIT.LIL	0, (IX+sysvar_vpd_pflags)
			JR	Z, $B			; Wait for the result
			LD 	D, 0
			LD	H, D
			LD.LIL	E, (IX + sysvar_cursorX)
			LD.LIL	L, (IX + sysvar_cursorY)			
			POP	IX			
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
OSKEY: 			MOSCALL	mos_getkey		; Read keyboard
			OR	A			; If we have a character
			JR	NZ, $F			; Then process it
			LD	A,H			; Check if HL is 0 (this is passed by INKEY() function
			OR	L
			RET	Z 			; If it is then ret
			HALT				; Bit of a bodge so this is timed in ms
			DEC	HL 			; Decrement the counter and 
			JR	OSKEY 			; loop
$$:			CP	1BH			; If we are not pressing ESC, 
			SCF 				; then flag we've got a character
			RET	NZ		
;
ESCSET: 		PUSH    HL
        		LD      HL,FLAGS
        		BIT     6,(HL)			; ESC DISABLED?
        		JR      NZ,ESCDIS
        		SET     7,(HL)			; SET ESCAPE FLAG
ESCDIS: 		POP     HL
        		RET	
;
ESCTEST:		MOSCALL	mos_getkey
			CP	1BH			; ESC	
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
			DB	'EDI','T'+80h		; EDIT
			DW	STAR_EDIT
			DB	'ERAS','E'+80h		; ERASE
			DW	STAR_ERASE
			DB	'F','X'+80h		; FX
			DW	STAR_FX
			DB	'MKDI','R'+80h		; MKDIR
			DW	STAR_MKDIR		
			DB	'RE','N'+80h		; REN
			DW	STAR_REN
			DB	FFh
			
; *BYE
;
STAR_BYE:		RST.LIS	00h			; Reset MOS
	
; *EDIT linenum
;
STAR_EDIT:		CALL	ASC_TO_NUMBER		; DE: Line number to edit
			EX	DE, HL			; HL: Line number
			CALL	FINDL			; HL: Address in RAM of tokenised line
			LD	A, 41			; F:NZ If the line is not found
			JP	NZ, ERROR_		; Do error 41: No such line in that case
;
; Loop through and detokenise this line
; At this point HL is the first byte of the tokenised line
;
; BBC BASIC for Z80 lines are stored as follows:
;
; - [LEN] [LSB] [MSB] [DATA...] [0x0D]: LSB, MSB = line number
;
STAR_EDIT1:		LD	IX, ACCS		; Destination address
;
			CALL	PROMPT			; Display the prompt
;
			LD	A,(HL)			; Fetch the length from the BASIC line
			SUB	4			; Get the data length
			LD	B, A			; Store in B
			INC 	HL
;
			LD	A, (HL)			; Line number low byte
			INC	HL
			PUSH	BC			; Preserve the data length counter
			PUSH	HL			; Preserve the BASIC pointer 
			LD	H, (HL)			; Line number high byte
			LD	L, A
			CALL	BUF_PBCDL		; Print out the line number
			LD	(IX), ' '		; Followed by a space
			INC	IX
			POP	HL			; Unstack the BASIC pointer
			POP	BC			; And the data length counter
			INC	HL
;
; Now write out the rest of the line, dealing with any tokens along the way
;
$$:			LD	A,(HL)			; Fetch the data
			INC	HL			; Skip to the next byte
			CP	8Dh			; Is it a line number following a GOTO?
			JR	Z, STAR_EDIT2		; Yes, so deal with that
			CALL	BUF_DETOKEN		; Write to screen, detokenise tokens
STAR_EDITL:		DJNZ	$B			; And loop
			XOR	A
			LD	(IX),A			; Write out the final byte
;
; Now invoke the editor
;
			LD	HL, ACCS		; HL: ACCS
			LD	E, L			;  E: 0 - Don't clear the buffer; ACCS is on a page boundary so L is 0
			CALL	OSLINE1			; Invoke the editor
			JP	ONEDIT			; Jump back to the BASIC loop just after the normal line edit
;
STAR_EDIT2:		PUSH    HL			; LD IY,HL
			POP     IY
			PUSH    BC
			CALL    DECODE			; Decode the 3 byte GOTO type line number
			POP     BC
			EXX				; The result is in HL'
			PUSH    BC
			CALL    BUF_PBCDL		; And output it
			POP     BC
			EXX
			PUSH    IY			; LD HL,IY
			POP     HL
			DEC	B			; Knock 3 bytes off the counter
			DEC	B
			DEC	B
			JR	STAR_EDITL		; And jump back to the loop

; *CAT / *.
;
STAR_DOT:
STAR_CAT:		PUSH	IY
			MOSCALL	mos_dir
STAR_MOSERR:		POP	IY			; Handle any MOS errors (also called by other STAR MOS commands)
			OR	A			; If A is 0 there is no error
			RET	Z			
			JP	OSERROR			; Otherwise, jump to OSERROR in OSLOAD

; *CD path
;
STAR_CD:		PUSH 	IY			
			CALL	SKIPSP			; Skip to filename
			LD	DE, ACCS		; Buffer for filename is ACCS (the string accumulator)
			PUSH	DE			; Store buffer address
			CALL	CSTR_FNAME		; Fetch the filename
			POP	HL			; HL: Pointer to filename in ACCS
			MOSCALL	mos_cd			; Call MOS
			JR	STAR_MOSERR		; Handle any errors			
; *ERASE filename
;
STAR_ERASE:		PUSH	IY
			CALL	SKIPSP			; Skip to filename
			LD	DE, ACCS		; Buffer for filename is ACCS (the string accumulator)
			PUSH	DE			; Store buffer address
			CALL	CSTR_FNAME		; Fetch the filename
			POP	HL			; HL: Pointer to filename in ACCS
			MOSCALL	mos_del			; Call MOS
			JR	STAR_MOSERR		; Handle any errors
			
; *REN filename1 filename2
;
STAR_REN:		PUSH	IY
			CALL	SKIPSP			; Skip to first filename in args
			LD	DE, ACCS		; Buffer for filenames is ACCS (the string accumulator)
			CALL	CSTR_FNAME		; Fetch first filename
			CALL	SKIPSP			; Skip to second filename in args
			PUSH	DE			; Store the start address of the second filename in ACCS
			CALL	CSTR_FNAME		; Store in string accumulator
			POP	DE			; DE: Pointer of the second filename
			LD	HL, ACCS		; HL: Pointer to the first filename
			MOSCALL	mos_ren			; Call MOS
			JR	STAR_MOSERR		; Handle any errors
			
; *MKDIR filename
;
STAR_MKDIR:		PUSH	IY
			CALL	SKIPSP			; Skip to filename
			LD	DE, ACCS		; Buffer for filename is ACCS (the string accumulator)
			PUSH	DE			; Stack the buffer
			CALL	CSTR_FNAME		; Convert the filename to a C string (0 terminated)
			POP	HL			; Pop the buffer address
			MOSCALL	mos_mkdir		; Call MOS
			JR	STAR_MOSERR		; Handle any errors

; OSCLI FX n
;
STAR_FX:		CALL	ASC_TO_NUMBER		; C: FX #
			LD	C, E
			CALL	ASC_TO_NUMBER		; B: First parameter
			LD	B, E
			CALL	ASC_TO_NUMBER		; E: Second parameter
			LD	L, B 			; L: First parameter
			LD	H, E 			; H: Second parameter
			LD	A, C 			; A: FX #, and fall through to OSBYTE	
;
; OSBYTE
;  A: FX #
;  L: First parameter
;  H: Second parameter
;
OSBYTE:			CP	13H			; We've only got one *FX command at the moment
			JR	Z, OSBYTE_13		; *FX 13
			JP	HUH			; Anything else trips an error

; OSBYTE 0x13 (FX 19): Wait for vertical blank interrupt
;
OSBYTE_13:		PUSH 	IX
			MOSCALL	mos_sysvars		; Fetch pointer to system variables
			LD.LIL	A, (IX + sysvar_time + 0)
$$:			CP	(IX + sysvar_time + 0)	; Wait for the LSB of clock to tick
			JR	Z, $B
			POP	IX
			LD	L, 0			; Returns 0
			JP	COUNT0

;OSLOAD - Load an area of memory from a file.
;   Inputs: HL addresses filename (CR terminated)
;           DE = address at which to load
;           BC = maximum allowed size (bytes)
;  Outputs: Carry reset indicates no room for file.
; Destroys: A,B,C,D,E,H,L,F
;
OSLOAD:			PUSH	DE			; Stack the load address
			LD	DE, ACCS		; Buffer address for filename
			CALL	CSTR_FNAME		; Fetch filename from MOS into buffer
			POP	DE			; Restore the load address
			LD	HL, ACCS		; HL: Filename
			MOSCALL	mos_load		; Call LOAD in MOS
			RET	NC			; If load returns with carry reset - NO ROOM
			OR	A			; If there is no error (A=0)
			SCF				; Need to set carry indicating there was room
			RET	Z			; Return
;
OSERROR:		PUSH	AF			; Handle the MOS error
			LD	HL, ACCS		; Address of the buffer
			LD	BC, 256			; Length of the buffer
			LD	E, A			; The error code
			MOSCALL	mos_getError		; Copy the error message into the buffer
			POP	AF			
			PUSH	HL			; Stack the address of the error (now in ACCS)		
			ADD	A, 127			; Add 127 to the error code (MOS errors start at 128, and are trappable)
			JP	EXTERR			; Trigger an external error

;OSSAVE - Save an area of memory to a file.
;   Inputs: HL addresses filename (term CR)
;           DE = start address of data to save
;           BC = length of data to save (bytes)
; Destroys: A,B,C,D,E,H,L,F
;
OSSAVE:			PUSH	DE			; Stack the save address
			LD	DE, ACCS		; Buffer address for filename
			CALL	CSTR_FNAME		; Fetch filename from MOS into buffer
			POP	DE			; Restore the save address
			LD	HL, ACCS		; HL: Filename
			MOSCALL	mos_save		; Call SAVE in MOS
			OR	A			; If there is no error (A=0)
			RET	Z			; Just return
			JR	OSERROR			; Trip an error
			
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
$$:			MOSCALL	mos_fopen			
			RET

;OSSHUT - Close disk file(s).
; E = file channel
;  If E=0 all files are closed (except SPOOL)
; Destroys: A,B,C,D,E,H,L,F
;
OSSHUT:			PUSH	BC
			LD	C, E
			MOSCALL	mos_fclose
			POP	BC
			RET
	
; OSBGET - Read a byte from a random disk file.
;  E = file channel
; Returns
;  A = byte read
;  Carry set if LAST BYTE of file
; Destroys: A,B,C,F
;
OSBGET:			PUSH	BC
			LD	C, E
			MOSCALL	mos_fgetc
			POP	BC
			RET
	
; OSBPUT - Write a byte to a random disk file.
;  E = file channel
;  A = byte to write
; Destroys: A,B,C,F
;	
OSBPUT:			PUSH	BC
			LD	C, E
			LD	B, A
			MOSCALL	mos_fputc
			POP	BC
			RET

; OSSTAT - Read file status
;  E = file channel
; Returns
;  F: Z flag set - EOF
;  A: If Z then A = 0
; Destroys: A,D,E,H,L,F
;
OSSTAT:			PUSH	BC
			LD	C, E
			MOSCALL	mos_feof
			POP	BC
			CP	1
			RET
	
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
RESET:			RET
