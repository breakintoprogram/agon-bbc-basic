;
; Title:	BBC Basic for AGON
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	19/08/2022
;
; Modinfo:
; 24/07/2022:	OSWRCH and OSRDCH now execute code in MOS
; 03/08/2022:	OSLOAD and OSSAVE now execute code in MOS, added * commands, implemented some file I/O commands
; 05/08/2022:	Implemented OSSTAT, assumes MOS will save registers in file I/O commands
; 07/08/2022:	Added CLG, COLOUR, and emulated palette mode for COLOUR and GCOL, fixed GETCSR
; 19/08/2022:	Moved GETSCHR, POINT to agon_graphics.asm, optimised GETCSR, added SOUND
			
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
			XREF	CRTONULL
			XREF	NULLTOCR
			XREF	CRLF

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
PUTCSR:			LD	A, 1Fh				; TAB
			RST.LIS	10h
			LD	A, E				; X
			RST.LIS 10h
			LD	A, L				; Y
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
	
; SOUND channel,volume,pitch,duration
; volume: 0 (off) to -15 (full volume)
; pitch: 0 - 255
; duration: -1 to 254 (duration in 20ths of a second, -1 = play forever)
;
SOUND:			CALL	EXPR_W2			; DE: Channel/Control, HL: Volume
			LD	A, L 			;  A: Volume
			PUSH	AF
			PUSH	DE
			CALL	COMMA
			CALL	EXPR_W2			; DE: Pitch, HL: Duration
			LD	D, E			;  D: Pitch
			LD	E, L 			;  E: Duration
			POP	HL 			; HL: Channel/Control
			POP	AF
			NEG
			CP	16			; Check volume is in bounds
			JP	NC, XEQ			; Out of bounds, do nothing
;
; Store	in VDU vars
; 
			LD	C, A			; Store Volume in C
			LD	A, L
			LD	(VDU_BUFFER+0), A	; Channel
			XOR	A
			LD	(VDU_BUFFER+1), A	; Waveform
; 
; Calculate the volume
; 
			LD	B, 6			; C already contains the volume
			MLT	BC			; Multiply by 6 (0-15 scales to 0-90)
			LD	A, C
			LD	(VDU_BUFFER+2), A
;
; And the frequency
;
			LD	C, E			; Store duration in C
			LD	H, 0			; Lookup the frequency
			LD	L, D
			LD	DE, SOUND_FREQ_LOOKUP
			ADD	HL, HL
			ADD	HL, DE
			LD	A, (HL)
			LD	(VDU_BUFFER+3), A
			INC	HL
			LD	A, (HL)
			LD	(VDU_BUFFER+4), A
;
; And now the duration - multiply it by 50 to convert from 1/20ths of seconds to milliseconds
;
			LD	B, 50			; C contains the duration, so MLT by 50
			MLT	BC
			LD	(VDU_BUFFER+5), BC
;
			PUSH	IX			; Get the system vars in IX
			MOSCALL	mos_sysvars		; Reset the semaphore
SOUND0:			RES.LIL	3, (IX+sysvar_vpd_pflags)
;
			VDU	23			; Send the sound command
			VDU	0
			VDU	5
			VDU	(VDU_BUFFER+0)		; 0: Channel
			VDU	(VDU_BUFFER+1)		; 1: Waveform (0)
			VDU	(VDU_BUFFER+2)		; 2: Volume (0-100)
			VDU	(VDU_BUFFER+3)		; 3: Frequency L
			VDU	(VDU_BUFFER+4)		; 4: Frequency H
			VDU	(VDU_BUFFER+5)		; 5: Duration L
			VDU	(VDU_BUFFER+6)		; 6: Duration H
;
; Wait for acknowledgement
;
$$:			BIT.LIL	3, (IX+sysvar_vpd_pflags)
			JR	Z, $B			; Wait for the result
			CALL	LTRAP			; Check for ESC
			LD.LIL	A, (IX+syscar_audioSuccess)
			AND	A			; Check if VDP has queued the note
			JR	Z, SOUND0		; No, so loop back and send again
;
			POP	IX
			JP	XEQ

; Frequency Lookup Table
; Set up to replicate the BBC Micro audio frequencies
;
; Split over 5 complete octaves, with 53 being middle C
; * C4: 262hz
; + A4: 440hz
;
;	2	3	4	5	6	7	8
;
; B	1	49	97	145	193	241	
; A#	0	45	93	141	189	237	
; A		41	89+	137	185	233	
; G#		37	85	133	181	229	
; G		33	81	129	177	225	
; F#		29	77	125	173	221	
; F		25	73	121	169	217	
; E		21	69	117	165	213	
; D#		17	65	113	161	209	
; D		13	61	109	157	205	253
; C#		9	57	105	153	201	249
; C		5	53*	101	149	197	245
;
SOUND_FREQ_LOOKUP:	DW	 117,  118,  120,  122,  123,  131,  133,  135
			DW	 137,  139,  141,  143,  145,  147,  149,  151
			DW	 153,  156,  158,  160,  162,  165,  167,  170
			DW	 172,  175,  177,  180,  182,  185,  188,  190
			DW	 193,  196,  199,  202,  205,  208,  211,  214
			DW	 217,  220,  223,  226,  230,  233,  236,  240
			DW	 243,  247,  251,  254,  258,  262,  265,  269
			DW	 273,  277,  281,  285,  289,  294,  298,  302
			DW	 307,  311,  316,  320,  325,  330,  334,  339
			DW	 344,  349,  354,  359,  365,  370,  375,  381
			DW	 386,  392,  398,  403,  409,  415,  421,  427
			DW	 434,  440,  446,  453,  459,  466,  473,  480
			DW	 487,  494,  501,  508,  516,  523,  531,  539
			DW	 546,  554,  562,  571,  579,  587,  596,  605
			DW	 613,  622,  631,  641,  650,  659,  669,  679
			DW	 689,  699,  709,  719,  729,  740,  751,  762
			DW	 773,  784,  795,  807,  819,  831,  843,  855
			DW	 867,  880,  893,  906,  919,  932,  946,  960
			DW	 974,  988, 1002, 1017, 1032, 1047, 1062, 1078
			DW	1093, 1109, 1125, 1142, 1158, 1175, 1192, 1210
			DW	1227, 1245, 1263, 1282, 1300, 1319, 1338, 1358
			DW	1378, 1398, 1418, 1439, 1459, 1481, 1502, 1524
			DW	1546, 1569, 1592, 1615, 1638, 1662, 1686, 1711
			DW	1736, 1761, 1786, 1812, 1839, 1866, 1893, 1920
			DW	1948, 1976, 2005, 2034, 2064, 2093, 2123, 2154
			DW	2186, 2217, 2250, 2282, 2316, 2349, 2383, 2418
			DW	2453, 2489, 2525, 2562, 2599, 2637, 2675, 2714
			DW	2754, 2794, 2834, 2876, 2918, 2960, 3003, 3047
			DW	3091, 3136, 3182, 3228, 3275, 3322, 3371, 3420
			DW	3470, 3520, 3571, 3623, 3676, 3729, 3784, 3839
			DW	3894, 3951, 4009, 4067, 4126, 4186, 4247, 4309
			DW	4371, 4435, 4499, 4565, 4631, 4699, 4767, 4836	

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
