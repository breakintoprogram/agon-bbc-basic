;
; Title:	BBC Basic for AGON
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	19/05/2023
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
; 11/10/2022:	Fixed bug introduced in previous fix to OSBYTE_13, OSCLI now calls MOS
; 20/10/2022:	ESC in GET now works, tidied up error handling in OSCLI
; 11/01/2023:	Added default .BBC extension to OSLOAD and OSSAVE, STAR_VERSION
; 15/02/2023:	Updated STAR_VERSION TO 1.04
; 26/02/2023:	Fixed STAR_EDIT to use LISTIT instead of duplicated code, added OSBYTE_A0, tweaked OSWRCH, SAVE and LOAD as text files
; 03/03/2023:	Addd .BAS extension for LOAD/SAVE, improvements to OSLOAD_TXT
; 11/03/2023:	Improved keyboard handling (GET, INKEY$)
; 15/03/2023:	Added GETIMS and PUTIMS
; 21/03/2023:	Added FX/OSBYTE functions for keyboard control, STAR_FX fixed to accept a single 16-bit parameter
; 25/03/2023:	Fixed range error in OSBYTE, now calls VBLANK_INIT in OSINIT, improved keyboard handling
; 28/03/2023:	Improved BYE command
; 02/04/2023:	Various keyboard tweaks
; 16/04/2023:	Implemented GETPTR, PUTPTR, GETEXT
; 19/05/2023:	Updated STAR_VERSION TO 1.05, Added OSBYTE_87, Fixed bug in OSLOAD_TXT, implemented GET(x,y)
			
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
			XDEF	GETIMS
			XDEF	RESET
			XDEF	OSLOAD
			XDEF	OSSAVE
			XDEF	EXPR_W2
			XDEF	GETPORT

			XREF	ASC_TO_NUMBER
			XREF	RAM_START
			XREF	RAM_END
			XREF	FLAGS
			XREF	ESCAPE
			XREF	USER
			XREF	RAM_Top
			XREF	EXTERR
			XREF	COUNT0
			XREF	COUNT1
			XREF	EXPRI
			XREF	COMMA
			XREF	XEQ
			XREF	NXT
			XREF	NULLTOCR
			XREF	CRLF
			XREF	CSTR_FNAME
			XREF	CSTR_LINE
			XREF	CSTR_FINDCH
			XREF	CSTR_ENDSWITH
			XREF	CSTR_CAT
			XREF	FINDL
			XREF	OUT_
			XREF	ERROR_
			XREF	ONEDIT
			XREF	TELL
			XREF	OSWRCHPT
			XREF	OSWRCHCH
			XREF	OSWRCHFH
			XREF	LISTON
			XREF	LISTIT
			XREF	PAGE_
			XREF	ONEDIT1
			XREF	CLEAN
			XREF	NEWIT
			XREF	BAD
			XREF	VBLANK_INIT
			XREF	VBLANK_STOP
			XREF	KEYDOWN
			XREF	KEYASCII
			XREF	GETSCHR_1
			XREF	BRAKET
			XREF	TRUE 

; OSLINE: Invoke the line editor
;
OSLINE:			LD 	E, 1			; Default is to clear the buffer

; Entry point to line editor that does not clear the buffer
; 
OSLINE1:		PUSH	IY			
			PUSH	HL			; Buffer address
			LD	BC, 256			; Buffer length
			MOSCALL	mos_editline		; Call the MOS line editor
			POP	HL			; Pop the address
			POP	IY
			PUSH	AF			; Stack the return value (key pressed)
			CALL	NULLTOCR		; Turn the 0 character to a CR
			CALL	CRLF			; Display CRLF
			POP	AF
			CP	1Bh 			; Check if ESC terminated the input
			JP	Z, LTRAP1 		; Yes, so do the ESC thing
			LD	A, (FLAGS)		; Otherwise
			RES	7, A 			; Clear the escape flag
			LD	(FLAGS), A 
			CALL	WAIT_VBLANK 		; Wait a frame 
 			XOR	A			; Return A = 0
			LD	(KEYDOWN), A 
			LD	(KEYASCII), A
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
			VDU	23
			VDU	0
			VDU	vdp_cursor
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
			
; OSWRCH: Write a character out to the ESP32 VDU handler via the MOS
; A: Character to write
;
OSWRCH:			PUSH	HL
			LD	HL, LISTON		; Fetch the LISTON variable
			BIT	3, (HL)			; Check whether we are in *EDIT mode
			JR	NZ, OSWRCH_BUFFER	; Yes, so just output to buffer
;
			LD	HL, (OSWRCHCH)		; L: Channel #
			DEC	L			; If it is 1
			JR	Z, OSWRCH_FILE		; Then we are outputting to a file
;
			POP	HL			; Otherwise
			RST.LIS	10h			; Output the character to MOS
			RET
;	
OSWRCH_BUFFER:		LD	HL, (OSWRCHPT)		; Fetch the pointer buffer
			LD	(HL), A			; Echo the character into the buffer
			INC	HL			; Increment pointer
			LD	(OSWRCHPT), HL		; Write pointer back
			POP	HL			
			RET
;
OSWRCH_FILE:		PUSH	DE
			LD	E, H			; Filehandle to E
			CALL	OSBPUT			; Write the byte out
			POP	DE
			POP	HL
			RET

; OSRDCH: Read a character in from the ESP32 keyboard handler
; This is only called in GETS (eval.asm)
;
OSRDCH:			MOSCALL	mos_getkey		; Read keyboard
			CP	1Bh
			JR	Z, LTRAP1 
			RET

			
;OSKEY - Read key with time-limit, test for ESCape.
;Main function is carried out in user patch.
;   Inputs: HL = time limit (centiseconds)
;  Outputs: Carry reset if time-out
;           If carry set A = character
; Destroys: A,H,L,F
;
OSKEY:			CALL	READKEY			; Read the keyboard 
			JR	Z, $F 			; Skip if we have a key
			LD	A, H 			; Check loop counter
			OR 	L
			RET 	Z 			; Return, we've not got a key at this point
			CALL	WAIT_VBLANK 		; Wait a frame
			DEC 	HL			; Decrement
			JR	OSKEY 			; And loop
;
$$:			LD	HL, KEYDOWN		; We have a key, so 
			LD	(HL), 0			; clear the keydown flag
			CP	1BH			; If we are not pressing ESC, 
			SCF 				; then flag we've got a character
			RET	NZ		
;
; ESCSET
; Set the escape flag (bit 7 of FLAGS = 1) if escape is enabled (bit 6 of FLAGS = 0)
;
ESCSET: 		PUSH    HL
        		LD      HL,FLAGS		; Pointer to FLAGS
        		BIT     6,(HL)			; If bit 6 is set, then
        		JR      NZ,ESCDIS		; escape is disabled, so skip
        		SET     7,(HL)			; Set bit 7, the escape flag
ESCDIS: 		POP     HL
        		RET	
;
; ESCTEST
; Test for ESC key
;
ESCTEST:		CALL	READKEY			; Read the keyboard
			RET	NZ			; Skip if no key is pressed				
			CP	1BH			; If ESC pressed then
			JR	Z,ESCSET		; jump to the escape set routine
			RET

; Read the keyboard
; Returns:
; - A: ASCII of the pressed key
; - F: Z if the key is pressed, otherwise NZ
;
READKEY:		LD	A, (KEYDOWN)		; Get key down
			DEC	A 			; Set Z flag if keydown is 1
			LD	A, (KEYASCII)		; Get key ASCII value
			RET 
;
; TRAP
; This is called whenever BASIC needs to check for ESC
;
TRAP:			CALL	ESCTEST			; Read keyboard, test for ESC, set FLAGS
;
LTRAP:			LD	A,(FLAGS)		; Get FLAGS
			OR	A			; This checks for bit 7; if it is not set then the result will
			RET	P			; be positive (bit 7 is the sign bit in Z80), so return
LTRAP1:			LD	HL,FLAGS 		; Escape is pressed at this point, so
			RES	7,(HL)			; Clear the escape pressed flag and
			JP	ESCAPE			; Jump to the ESCAPE error routine in exec.asm

;OSINIT - Initialise RAM mapping etc.
;If BASIC is entered by BBCBASIC FILENAME then file
;FILENAME.BBC is automatically CHAINed.
;   Outputs: DE = initial value of HIMEM (top of RAM)
;            HL = initial value of PAGE (user program)
;            Z-flag reset indicates AUTO-RUN.
;  Destroys: A,D,E,H,L,F
;
OSINIT:			CALL	VBLANK_INIT
			XOR	A
			LD	(FLAGS), A		; Clear flags and set F = Z
			LD 	HL, USER
			LD	DE, RAM_Top
			LD	E, A			; Page boundary
			RET	

;
;OSCLI - Process a MOS command
;
OSCLI: 			CALL    SKIPSP
			CP      CR
			RET     Z
			CP      '|'
			RET     Z
			EX      DE,HL
			LD      HL,COMDS
OSCLI0:			LD      A,(DE)
			CALL    UPPRC
			CP      (HL)
			JR      Z,OSCLI2
			JR      C,OSCLI6
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
;
OSCLI6:			EX	DE, HL			; HL: Buffer for command
			LD	DE, ACCS		; Buffer for command string is ACCS (the string accumulator)
			PUSH	DE			; Store buffer address
			CALL	CSTR_LINE		; Fetch the line
			POP	HL			; HL: Pointer to command string in ACCS
			PUSH	IY
			MOSCALL	mos_oscli		; Returns OSCLI error in A
			POP	IY
			OR	A			; 0 means MOS returned OK
			RET	Z			; So don't do anything
			JP 	OSERROR			; Otherwise it's a MOS error

HUH:    		LD      A,254			; Bad command error
        		CALL    EXTERR
        		DB    	"Bad command"
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
			DB	'EDI','T'+80h		; EDIT
			DW	STAR_EDIT
			DB	'F','X'+80h		; FX
			DW	STAR_FX
			DB	'VERSIO','N'+80h	; VERSION
			DW	STAR_VERSION
			DB	FFh
						
; *BYE
;
STAR_BYE:		CALL	VBLANK_STOP		; Restore MOS interrupts
			POP.LIL	IX 			; The return address to init
			LD	HL, 0			; The return code
			JP	(IX)
	
; *VERSION
;
STAR_VERSION:		CALL    TELL			; Output the welcome message
			DB    	"BBC BASIC (Agon) Version 1.05\n\r",0
			RET
	
; *EDIT linenum
;
STAR_EDIT:		CALL	ASC_TO_NUMBER		; DE: Line number to edit
			EX	DE, HL			; HL: Line number
			CALL	FINDL			; HL: Address in RAM of tokenised line			
			LD	A, 41			; F:NZ If the line is not found
			JP	NZ, ERROR_		; Do error 41: No such line in that case
;
; Use LISTIT to output the line to the ACCS buffer
;
			INC	HL			; Skip the length byte
			LD	E, (HL)			; Fetch the line number
			INC	HL
			LD	D, (HL)
			INC	HL
			LD	IX, ACCS		; Pointer to where the copy is to be stored
			LD	(OSWRCHPT), IX
			LD	IX, LISTON		; Pointer to LISTON variable in RAM
			LD	A, (IX)			; Store that variable
			PUSH	AF
			LD	(IX), 09h		; Set to echo to buffer
			CALL	LISTIT
			POP	AF
			LD	(IX), A			; Restore the original LISTON variable			
			LD	HL, ACCS		; HL: ACCS
			LD	E, L			;  E: 0 - Don't clear the buffer; ACCS is on a page boundary so L is 0
			CALL	OSLINE1			; Invoke the editor
			JP	ONEDIT			; Jump back to the BASIC loop just after the normal line edit

; OSCLI FX n
;
STAR_FX:		CALL	ASC_TO_NUMBER
			LD	C, E			; C: Save FX #
			CALL	ASC_TO_NUMBER
			LD	A, D  			; Is first parameter > 255?
			OR 	A 			
			JR	Z, STAR_FX1		; Yes, so skip next bit 
			EX	DE, HL 			; Parameter is 16-bit
			JR	STAR_FX2 
;
STAR_FX1:		LD	B, E 			; B: Save First parameter
			CALL	ASC_TO_NUMBER		; Fetch second parameter
			LD	L, B 			; L: First parameter
			LD	H, E 			; H: Second parameter
;
STAR_FX2:		LD	A, C 			; A: FX #, and fall through to OSBYTE	
;
; OSBYTE
;  A: FX #
;  L: First parameter
;  H: Second parameter
;
OSBYTE:			CP	0BH			; *FX 11, n: Keyboard auto-repeat delay
			JR	Z, OSBYTE_0B
			CP	0CH			; *FX 12, n: Keyboard auto-repeat rate
			JR	Z, OSBYTE_0C
			CP	13H			; *FX 19: Wait for vblank
			JR	Z, OSBYTE_13		
			CP	76H			; *FX 118, n: Set keyboard LED
			JR	Z, OSBYTE_76
			CP	87H			
			JP	Z, OSBYTE_87
			CP	A0H			
			JP	Z, OSBYTE_A0		
			JP	HUH			; Anything else trips an error

; OSBYTE 0x0B (FX 11,n): Keyboard auto-repeat delay
; Parameters:
; - HL: Repeat delay
;
OSBYTE_0B:		VDU	23
			VDU	0
			VDU	vdp_keystate
			VDU	L
			VDU	H 
			VDU	0
			VDU 	0
			VDU	255
			RET 

; OSBYTE 0x0C (FX 12,n): Keyboard auto-repeat rate
; Parameters:
; - HL: Repeat rate
;
OSBYTE_0C:		VDU	23
			VDU	0
			VDU	vdp_keystate
			VDU	0
			VDU 	0
			VDU	L
			VDU	H 
			VDU	255
			RET 

; OSBYTE 0x13 (FX 19): Wait for vertical blank interrupt
;
OSBYTE_13:		CALL	WAIT_VBLANK
			LD	L, 0			; Returns 0
			JP	COUNT0
;
WAIT_VBLANK:		PUSH 	IX			; Wait for VBLANK interrupt
			MOSCALL	mos_sysvars		; Fetch pointer to system variables
			LD.LIL	A, (IX + sysvar_time + 0)
$$:			CP.LIL 	A, (IX + sysvar_time + 0)
			JR	Z, $B
			POP	IX
			RET

; OSBYTE 0x76 (FX 118,n): Set Keyboard LED
; Parameters:
; - L: LED (Bit 0: Scroll Lock, Bit 1: Caps Lock, Bit 2: Num Lock)
;
OSBYTE_76:		VDU	23
			VDU	0
			VDU	vdp_keystate
			VDU	0
			VDU 	0
			VDU	0
			VDU	0 
			VDU	L
			RET 

; OSBYTE 0x87: Fetch current mode and character under cursor
;
OSBYTE_87:		PUSH	IX
			CALL	GETCSR			; Get the current screen position
			CALL	GETSCHR_1		; Read character from screen
			LD	L, A 
			MOSCALL	mos_sysvars
			LD.LIL	H, (IX+sysvar_scrMode)	; H: Screen mode
			POP	IX
			JP	COUNT1
			
; OSBYTE 0xA0: Fetch system variable
; Parameters:
; - L: The system variable to fetch
;
OSBYTE_A0:		PUSH	IX
			MOSCALL	mos_sysvars		; Fetch pointer to system variables
			LD.LIL	BC, 0			
			LD	C, L			; BCU = L
			ADD.LIL	IX, BC			; Add to IX
			LD.LIL	L, (IX + 0)		; Fetch the return value
			POP	IX
			JP 	COUNT0

;OSLOAD - Load an area of memory from a file.
;   Inputs: HL addresses filename (CR terminated)
;           DE = address at which to load
;           BC = maximum allowed size (bytes)
;  Outputs: Carry reset indicates no room for file.
; Destroys: A,B,C,D,E,H,L,F
;
OSLOAD:			PUSH	BC			; Stack the size
			PUSH	DE			; Stack the load address
			LD	DE, ACCS		; Buffer address for filename
			CALL	CSTR_FNAME		; Fetch filename from MOS into buffer
			LD	HL, ACCS		; HL: Filename
			CALL	EXT_DEFAULT		; Tack on the extension .BBC if not specified
			CALL	EXT_HANDLER		; Get the default handler
			POP	DE			; Restore the load address
			POP	BC			; Restore the size
			OR	A
			JR 	Z, OSLOAD_BBC
;
; Load the file in as a text file
;
OSLOAD_TXT:		XOR	A			; Set file attributes to read
			CALL	OSOPEN			; Open the file			
			LD 	E, A 			; The filehandle
			OR	A
			LD	A, 4			; File not found error
			JR	Z, OSERROR		; Jump to error handler
			CALL	NEWIT			; Call NEW to clear the program space
;
OSLOAD_TXT1:		LD	HL, ACCS 		; Where the input is going to be stored
;
; First skip any whitespace (indents) at the beginning of the input
;
$$:			CALL	OSBGET			; Read the byte into A
			CP	CR 			; Is it CR?
			JR	Z, OSLOAD_TXT2 		; Yes, so skip to the next line
			CP	21h			; Is it less than or equal to ASCII space?
			JR	C, $B 			; Yes, so keep looping
			LD	(HL), A 		; Store the first character
			INC	L
;
; Now read the rest of the line in
;
$$:			CALL	OSBGET			; Read the byte into A
			LD	(HL), A 		; Store in the input buffer			
			INC	L
			JP	Z, BAD			; If the buffer is full (wrapped to 0) then jump to Bad Program error
			CP	CR			; Check for CR
			JR	NZ, $B 			; If not, then loop to read the rest of the characters in
;
; Finally, handle EOL/EOF
;
OSLOAD_TXT2:		CALL	OSBGET			; Check for LF
			CP	LF			; If it is not LF
			JP	NZ, BAD			; Then jump to Bad Program error
			LD	A, L			; Check for minimum line length
			CP	2			; If it is 2 characters or less (including CR)
			JR	C, $F			; Then don't bother entering it
			PUSH	DE			; Preserve the filehandle
			CALL	ONEDIT1			; Enter the line in memory
			CALL	C,CLEAN			; If a new line has been entered, then call CLEAN to set TOP and write &FFFF end of program marker
			POP	DE
$$:			CALL	OSSTAT			; End of file?
			JR	NZ, OSLOAD_TXT1		; No, so loop
			CALL	OSSHUT			; Close the file
			SCF				; Flag to BASIC that we're good
			RET
;
; Load the file in as a tokenised binary blob
;
OSLOAD_BBC:		MOSCALL	mos_load		; Call LOAD in MOS
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
OSSAVE:			PUSH	BC			; Stack the size
			PUSH	DE			; Stack the save address
			LD	DE, ACCS		; Buffer address for filename
			CALL	CSTR_FNAME		; Fetch filename from MOS into buffer
			LD	HL, ACCS		; HL: Filename
			CALL	EXT_DEFAULT		; Tack on the extension .BBC if not specified
			CALL	EXT_HANDLER		; Get the default handler
			POP	DE			; Restore the save address
			POP	BC			; Restore the size
			OR	A			; Is the extension .BBC
			JR	Z, OSSAVE_BBC		; Yes, so use that
;
; Save the file out as a text file
;
OSSAVE_TXT:		LD 	A, (OSWRCHCH)		; Stack the current channel
			PUSH	AF
			XOR	A
			INC	A			; Make sure C is clear, A is 1, for OPENOUT
			LD	(OSWRCHCH), A
			CALL	OSOPEN			; Open the file
			LD	(OSWRCHFH), A		; Store the file handle for OSWRCH
			LD	IX, LISTON		; Required for LISTIT
			LD	HL, (PAGE_)		; Get start of program area
			EXX
			LD	BC, 0			; Set the initial indent counters
			EXX			
OSSAVE_TXT1:		LD	A, (HL)			; Check for end of program marker
			OR	A		
			JR	Z, OSSAVE_TXT2
			INC	HL			; Skip the length byte
			LD	E, (HL)			; Get the line number
			INC	HL
			LD	D, (HL)
			INC	HL
			CALL	LISTIT			; List the line
			JR	OSSAVE_TXT1
OSSAVE_TXT2:		LD	A, (OSWRCHFH)		; Get the file handle
			LD	E, A
			CALL	OSSHUT			; Close it
			POP	AF			; Restore the channel
			LD	(OSWRCHCH), A		
			RET
;
; Save the file out as a tokenised binary blob
;
OSSAVE_BBC:		MOSCALL	mos_save		; Call SAVE in MOS
			OR	A			; If there is no error (A=0)
			RET	Z			; Just return
			JR	OSERROR			; Trip an error

; Check if an extension is specified in the filename
; Add a default if not specified
; HL: Filename (CSTR format)
;
EXT_DEFAULT:		PUSH	HL			; Stack the filename pointer	
			LD	C, '.'			; Search for dot (marks start of extension)
			CALL	CSTR_FINDCH
			OR	A			; Check for end of string marker
			JR	NZ, $F			; No, so skip as we have an extension at this point			
			LD	DE, EXT_LOOKUP		; Get the first (default extension)
			CALL	CSTR_CAT		; Concat it to string pointed to by HL
$$:			POP	HL			; Restore the filename pointer
			RET
			
; Check if an extension is valid and, if so, provide a pointer to a handler
; HL: Filename (CSTR format)
; Returns:
;  A: Filename extension type (0=BBC tokenised, 1=ASCII untokenised)
;
EXT_HANDLER:		PUSH	HL			; Stack the filename pointer
			LD	C, '.'			; Find the '.'
			CALL	CSTR_FINDCH
			LD	DE, EXT_LOOKUP		; The lookup table
;
EXT_HANDLER_1:		PUSH	HL			; Stack the pointer to the extension
			CALL	CSTR_ENDSWITH		; Check whether the string ends with the entry in the lookup
			POP	HL			; Restore the pointer to the extension
			JR	Z, EXT_HANDLER_2	; We have a match!
;
$$:			LD	A, (DE)			; Skip to the end of the entry in the lookup
			INC	DE
			OR	A
			JR	NZ, $B
			INC	DE			; Skip the file extension # byte
;
			LD	A, (DE)			; Are we at the end of the table?
			OR	A
			JR	NZ, EXT_HANDLER_1	; No, so loop
;			
			LD      A,204			; Throw a "Bad name" error
        		CALL    EXTERR
        		DB    	"Bad name", 0
;
EXT_HANDLER_2:		INC	DE			; Skip to the file extension # byte
			LD	A, (DE)		
			POP	HL			; Restore the filename pointer
			RET
;


; Extension lookup table
; CSTR, TYPE
; 	- 0: BBC (tokenised BBC BASIC for Z80 format)
; 	- 1: Human readable plain text
;
EXT_LOOKUP:		DB	'.BBC', 0, 0		; First entry is the default extension
			DB	'.TXT', 0, 1
			DB	'.ASC', 0, 1
			DB	'.BAS', 0, 1
			DB	0			; End of table
			
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
			DB	F7h
			DW	OSCLI
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
GETPTR:			PUSH		IY
			LD		C, E 
			MOSCALL		mos_getfil 	; HLU: Pointer to FIL structure
			PUSH.LIL	HL
			POP.LIL		IY		; IYU: Pointer to FIL structure
			LD.LIL		L, (IY + FIL.fptr + 0)
			LD.LIL		H, (IY + FIL.fptr + 1)
			LD.LIL		E, (IY + FIL.fptr + 2)
			LD.LIL		D, (IY + FIL.fptr + 3)
			POP		IY
			RET

; PUTPTR - Update file pointer.
;    A = file channel
; DEHL = new pointer (0-&7FFFFF)
; Destroys: A,B,C,D,E,H,L,F
;
PUTPTR:			PUSH		IY 			
			LD		C, A  		; C: Filehandle
			PUSH.LIL	HL 		
			LD.LIL		HL, 2
			ADD.LIL		HL, SP
			LD.LIL		(HL), E 	; 3rd byte of DWORD set to E
			POP.LIL		HL
			LD		E, D  		; 4th byte passed as E
			MOSCALL		mos_flseek
			POP		IY 
			RET
	
; GETEXT - Find file size.
;    E = file channel
; Returns:
; DEHL = file size (0-&800000)
; Destroys: A,B,C,D,E,H,L,F
;
GETEXT:			PUSH		IY 
			LD		C, E 
			MOSCALL		mos_getfil 	; HLU: Pointer to FIL structure
			PUSH.LIL	HL
			POP.LIL		IY		; IYU: Pointer to FIL structure
			LD.LIL		L, (IY + FIL.obj.objsize + 0)
			LD.LIL		H, (IY + FIL.obj.objsize + 1)
			LD.LIL		E, (IY + FIL.obj.objsize + 2)
			LD.LIL		D, (IY + FIL.obj.objsize + 3)			
			POP		IY 
			RET	

; GETIMS - Get time from RTC
;
GETIMS:			PUSH	IY
			LD	HL, ACCS 		; Where to store the time string
			MOSCALL	mos_getrtc
			LD	DE, ACCS		; DE: pointer to start of string accumulator
			LD	E, A 			;  E: now points to the end of the string
			POP	IY
			RET

; GET(port) - Read eZ80 port
; GET(x, y) - Read character from screen position (x, y)
; Called from GET in eval.asm
;
GETPORT:		INC	IY			; Skip '('
			CALL    EXPRI         	  	; Get the first parameter
			EXX
			PUSH	HL
			CALL	NXT
			CP	','			; If there is no comma, then do GET(port)
			JR	NZ, GETPORT1
;
			CALL	COMMA			; Get the second parameter
			CALL	EXPRI
			EXX 
			POP	DE 			; DE: X coordinate 
			CALL 	GETSCHR_1		; Read the character
			LD	L, A			; Character code, or 0 if no match
			JR	C, GETPORT3		; Carry is set, so we have a character
			CALL	BRAKET			; Check for second bracket
			JP	TRUE			; Return -1
;
GETPORT1:		POP	BC 			; Port # in BC
			IN	L, (C)			; Read the port
GETPORT3:		CALL	BRAKET			; Handle the trailing bracket
			JP	COUNT0			; Return the value

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
