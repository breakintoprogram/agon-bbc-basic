;
; Title:	BBC Basic Interpreter - Z80 version
;		Command, Error and Lexical Analysis Module - "MAIN"
; Author:	(C) Copyright  R.T.Russell  1984
; Modified By:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	15/11/2023
;
; Modinfo:
; 07/05/1984:	Version 2.3
; 01/03/1987:	Version 3.0
; 03/05/2022:	Modified by Dean Belfield to assemble with ZDS
; 28/09/2022:	Tidied up KEYWDS and ERRWDS, Added KEYWDS and KEYWDL to XDEFs, entry point ONEDIT for *EDIT
; 12/01/2023:	Added MOS C-style parameter processing routines and autoload functionality
; 26/02/2023:	Text in comments are not detokenised, Tweaks for *EDIT and OSLOAD_TXT
; 24/03/2023:	Removed TEST_FILENAME
; 28/03/2023:	Tweaked for improved BYE command
; 19/05/2023:	Fixed bug in ONEDIT1 for OSLOAD_TXT
; 15/11/2023:	Startup message now includes Agon version

			.ASSUME	ADL = 0				

			INCLUDE	"equs.inc"

			SEGMENT CODE
			
			XDEF	_main
			
			XDEF	COLD
			XDEF	WARM
			XDEF	CLOOP
			XDEF	DELETE
			XDEF	LIST_
			XDEF	RENUM
			XDEF	AUTO
			XDEF	NEW
			XDEF	OLD
			XDEF	LOAD
			XDEF	SAVE
			XDEF	ERROR_
			XDEF	EXTERR
			XDEF	LOAD0
			XDEF	CLEAR
			XDEF	CRLF
			XDEF	OUTCHR
			XDEF	OUT_
			XDEF	FINDL
			XDEF	SETLIN
			XDEF	PBCDL
			XDEF	SAYLN
			XDEF	PUTVAR
			XDEF	GETVAR
			XDEF	GETDEF
			XDEF	CREATE
			XDEF	RANGE
			XDEF	LEXAN2
			XDEF	REPORT
			XDEF	TELL
			XDEF	SPACE_
			XDEF	KEYWDS
			XDEF	KEYWDL
			XDEF	ONEDIT
			XDEF	ONEDIT1
			XDEF	LISTIT
			XDEF	CLEAN
				
			XREF	LISTON
			XREF	ERRTXT
			XREF	OSINIT
			XREF	HIMEM
			XREF	PAGE_
			XREF	CHAIN0
			XREF	PROMPT
			XREF	ERRTRP
			XREF	ERRLIN
			XREF	AUTONO
			XREF	LINENO
			XREF	INCREM
			XREF	OSLINE
			XREF	COUNT
			XREF	NXT
			XREF	BUFFER
			XREF	XEQ
			XREF	TOP
			XREF	EXPRI
			XREF	SEARCH
			XREF	LTRAP
			XREF	LOMEM
			XREF	DECODE
			XREF	EXPRS
			XREF	OSSAVE
			XREF	ERR
			XREF	ERL
			XREF	TRACEN
			XREF	RESET
			XREF	OSSHUT
			XREF	OSLOAD
			XREF	FREE
			XREF	DYNVAR
			XREF	FILL
			XREF	OSWRCH
			XREF	WIDTH
			XREF	COMMA
			XREF	MUL16
			XREF	BRAKET
			XREF	X4OR5
			XREF	LOADN
			XREF	SFIX
			XREF	ITEMI
			XREF	FNPTR
			XREF	PROPTR
			XREF	CHECK
			XREF	TERMQ
			XREF	OSWRCHCH
			XREF	NEWIT
			XREF	BAD
			XREF	STAR_VERSION
;
; A handful of common token IDs
;
TERROR:			EQU     85H
LINE_:			EQU     86H
ELSE_:			EQU     8BH
THEN:			EQU     8CH
LINO:			EQU     8DH
FN:			EQU     A4H
TO:			EQU     B8H
REN:			EQU     CCH
DATA_:			EQU     DCH
DIM:			EQU     DEH
FOR:			EQU     E3H
GOSUB:			EQU     E4H
GOTO:			EQU     E5H
TIF:			EQU     E7H
LOCAL_:			EQU     EAH
NEXT:			EQU     EDH
ON_:			EQU     EEH
PROC:			EQU     F2H
REM:			EQU     F4H
REPEAT:			EQU     F5H
RESTOR:			EQU     F7H
TRACE:			EQU     FCH
UNTIL:			EQU     FDH
;
; This defines the block of tokens that are pseudo-variables.
; There are two versions of each token, a GET and a SET

; Name  : GET : SET
; ------:-----:----
; PTR   : 8Fh : CFh 
; PAGE  : 90h : D0h
; TIME  : 91h : D1h
; LOMEM : 92h : D2h
; HIMEM : 93h : D3h
;
; Examples:
;   LET A% = PAGE : REM This is the GET version
;   PAGE = 40000  : REM This is the SET version
;
TOKLO:			EQU     8FH			; This defines the block of tokens that are pseudo-variables
TOKHI:			EQU     93H			; PTR, PAGE, TIME, LOMEM, HIMEM
OFFSET:			EQU     CFH-TOKLO		; Offset to the parameterised SET versions

; The main routine
; IXU: argv - pointer to array of parameters
;   C: argc - number of parameters
; Returns:
;  HL: Error code, or 0 if OK
;
_main:			LD	HL, ACCS		; Clear the ACCS
			LD	(HL), 0
			LD	A, C			
			CP	2
			JR	Z, AUTOLOAD		; 2 parameters = autoload
			JR	C, COLD			; 1 parameter = normal start
			CALL	STAR_VERSION		; Output the AGON version
			CALL	TELL
			DB	"Usage:\n\r"
			DB	"RUN . <filename>\n\r", 0
			LD	HL, 0			; The error code
			RET
;							
AUTOLOAD:		LD.LIL	HL, (IX+3)		; HLU: Address of filename
			LD	DE, ACCS		;  DE: Destination address
$$:			LD.LIL	A, (HL)			; Fetch the filename byte
			LD	(DE), A			; 
			INC.LIL	HL			; Increase the source pointer
			INC	E			; We only need to increase E as ACCS is on a page boundary
			JR	NZ, $B			; Loop until we hit a 0 byte
			DEC	E
			LD	A, CR
			LD	(DE), A			; Replace the 0 byte with a CR for BBC BASIC
;
COLD:			POP	HL			; Pop the return address to init off SPS
			PUSH.LIL HL 			; Stack it on SPL (*BYE will use this as the return address)
			LD      HL,STAVAR       	; Cold start
			LD      SP,HL
			LD      (HL),10
			INC     L
			LD      (HL),9
			INC     L
			XOR     A
PURGE:			LD      (HL),A          	; Clear scratchpad
			INC     L
			JR      NZ,PURGE
			LD	(OSWRCHCH), A		; Set default output channel to CONSOLE
			LD      A,37H           	; Set LISTO sysvar; the bottom nibble is LISTO (7), top nibble is OPT (3)
			LD      (LISTON),A		
			LD      HL,NOTICE
			LD      (ERRTXT),HL
			CALL    OSINIT			; Call the machine specific OS initialisation routines
			LD      (HIMEM),DE		; This returns HIMEM (ramtop) in DE - store in the HIMEM sysvar
			LD      (PAGE_),HL		; And PAGE in HL (where BASIC program storage starts) - store in PAGE sysvar
			CALL    NEWIT			; From what I can determine, NEWIT always returns with Z flag set
			LD	A,(ACCS)		; Check if there is a filename in ACCS
			OR	A
			JP	NZ,CHAIN0		; Yes, so load and run
			CALL	STAR_VERSION		; Output the AGON version
			CALL    TELL			; Output the welcome message
			DB    	"BBC BASIC (Z80) Version 3.00\n\r"
NOTICE:			DB    	"(C) Copyright R.T.Russell 1987\n\r"
			DB	"\n\r", 0
;			
WARM:			DB 	F6H			; Opcode for OR? Maybe to CCF (the following SCF will be the operand)
;
; This is the main entry point for BASIC
;
CLOOP:			SCF				; See above - not sure why this is here!
			LD      SP,(HIMEM)
			CALL    PROMPT          	; Prompt user
			LD      HL,LISTON		; Pointer to the LISTO/OPT sysvar 
			LD      A,(HL)			; Fetch the value
			AND     0FH             	; Bottom nibble: LISTO
			OR      30H             	; Top nibble: Default to OPT (3)
			LD      (HL),A			; Store back in
			SBC     HL,HL           	; HL: 0
			LD      (ERRTRP),HL		; Clear ERRTRP sysvar 
			LD      (ERRLIN),HL		; Clear ERRLIN sysvar (ON ERROR)
;			
			LD      HL,(AUTONO)		; Get the auto line number
			LD      (LINENO),HL		; Store in line number
			LD      A,H			; If the auto line number is zero then
			OR      L
			JR      Z,NOAUTO		; We're not auto line numbering, so skip the next bit
;
; This section handles auto line numbering
;
			PUSH    HL			; Stack the line number
			CALL    PBCD           	 	; Output the line number
			POP     HL			; Pop the line number back off the stack
			LD      BC,(INCREM)		; Load BC with Increment - but INCREM is just a byte; C is the value
			LD      B,0			; So clear B
			ADD     HL,BC			; Add the increment to the line number
			JP      C,TOOBIG		; And error if we wrap
			LD      (AUTONO),HL		; Store the new auto line number
			LD      A,' '			; Print a space
			CALL    OUTCHR
;
; This section invokes the line editor
;
NOAUTO:			LD      HL,ACCS			; Storage for the line editor (256 bytes)
			CALL    OSLINE          	; Call the line editor in MOS
ONEDIT:			CALL	ONEDIT1			; Enter the line into memory
			CALL    C,CLEAN			; Set TOP, write out &FFFF end of program marker
			JP      CLOOP			; Jump back to immediate mode
;
; This bit enters the line into memory
; Also called from OSLOAD_TXT
; Returns:
; F: C if a new line has been entered (CLEAN will need to be called)
;
ONEDIT1:		XOR     A			; Entry point after *EDIT
			LD      (COUNT),A
			LD      IY,ACCS
			CALL    LINNUM			; HL: The line number from the input buffer
			CALL    NXT			; Skip spaces
			LD      A,H			; HL: The line number will be 0 for immediate mode or when auto line numbering is used
			OR      L
			JR      Z,LNZERO        	; Skip if there is no line number in the input buffer
			LD      (LINENO),HL		; Otherwise store it
;
; This bit does the lexical analysis and tokenisation
;
LNZERO:			LD      DE,BUFFER		; Buffer for tokenised BASIC
			LD      C,1             	; Left mode
			CALL    LEXAN2          	; Lexical analysis on the user input
			LD      (DE),A          	; Terminator
			XOR     A
			LD      B,A
			LD      C,E             	; BC: Line length
			INC     DE
			LD      (DE),A          	; Zero next
			LD      HL,(LINENO)		; Get the line number
			LD      A,H			; Is it zero, i.e. a command with no line number?
			OR      L
			LD      IY,BUFFER       	; Yes, so we're in immediate mode
			JP      Z,XEQ           	; Execute it
;
; This section stores the BASIC line in memory
;
			PUSH    BC
			PUSH    HL
			CALL    SETTOP          	; Set TOP sysvar
			POP     HL
			CALL    FINDL			; Find the address of the line
			CALL    Z,DEL			; Delete the existing line if found
			POP     BC
			LD      A,C			; Check for the line length being zero, i.e.
			OR      A			; the user has just entered a line number in the command line
			RET	Z			; If so, then don't do anything else
			ADD     A,4
			LD      C,A             	; Length inclusive
			PUSH    DE              	; DE: Line number (fetched from the call to FINDL)
			PUSH    BC              	; BC: Line length
			EX      DE,HL			; DE: Address of the line in memory
			LD      HL,(TOP)		; HL: TOP (the first free location after the end of the BASIC program)
			PUSH    HL			; Stack TOP (current TOP value)
			ADD     HL,BC			; Add the line length to HL, the new TOP value
			PUSH    HL			; Stack HL (new TOP value)
			INC     H			; Add 256 to HL
			XOR     A
			SBC     HL,SP			; Check whether HL is in the same page as the current stack pointer
			POP     HL			; Pop HL (new TOP value)
			JP      NC,ERROR_        	; If HL is in the stack page, then error: "No room"
			LD      (TOP),HL		; Store new value of TOP
			EX      (SP),HL			; HL: TOP (current TOP value), top of stack now contains new TOP value
			PUSH    HL			; PUSH current TOP value
			INC     HL			
			OR      A
			SBC     HL,DE			; DE: Address of the line in memory 
			LD      B,H             	; BC: Amount to move
			LD      C,L
			POP     HL			; HL: Destination (current TOP value)
			POP     DE			; DE: Source (new TOP value)
			JR      Z,ATEND			; If current TOP and new TOP are the same, i.e. adding a line at the end, then skip...		
			LDDR                    	; Otherwise, make space for the new line in the program
ATEND:			POP     BC              	; BC: Line length
			POP     DE              	; DE: Line number
			INC     HL			; HL: Destination address
			LD      (HL),C          	; Store length
			INC     HL
			LD      (HL),E          	; Store line number
			INC     HL
			LD      (HL),D
			INC     HL
			LD      DE,BUFFER		; DE: Location of the new, tokenised line
			EX      DE,HL			; HL: Location of the new, tokensied line, DE: Destination address in BASIC program
			DEC     C			; Subtract 3 from the number of bytes to copy to
			DEC     C			; compensate for the 3 bytes stored above (length and line number)
			DEC     C	
			LDIR                    	; Add the line to the BASIC program
			SCF				; To flag we need to call CLEAN
			RET
;
; List of tokens and keywords. If a keyword is followed by 0 then
; it will only match with the keyword followed immediately by
; a delimiter
;
KEYWDS:			DB    80H, 'AND'
			DB    94H, 'ABS'
			DB    95H, 'ACS'
			DB    96H, 'ADVAL'
			DB    97H, 'ASC'
			DB    98H, 'ASN'
			DB    99H, 'ATN'
			DB    C6H, 'AUTO'
			DB    9AH, 'BGET', 0
			DB    D5H, 'BPUT', 0
			DB    FBH, 'COLOUR'
			DB    FBH, 'COLOR'
			DB    D6H, 'CALL'
			DB    D7H, 'CHAIN'
			DB    BDH, 'CHR$'
			DB    D8H, 'CLEAR', 0
			DB    D9H, 'CLOSE', 0
			DB    DAH, 'CLG', 0
			DB    DBH, 'CLS', 0
			DB    9BH, 'COS'
			DB    9CH, 'COUNT', 0
			DB    DCH, 'DATA'
			DB    9DH, 'DEG'
			DB    DDH, 'DEF'
			DB    C7H, 'DELETE'
			DB    81H, 'DIV'
			DB    DEH, 'DIM'
			DB    DFH, 'DRAW'
			DB    E1H, 'ENDPROC', 0
			DB    E0H, 'END', 0
			DB    E2H, 'ENVELOPE'
			DB    8BH, 'ELSE'
			DB    A0H, 'EVAL'
			DB    9EH, 'ERL', 0
			DB    85H, 'ERROR'
			DB    C5H, 'EOF', 0
			DB    82H, 'EOR'
			DB    9FH, 'ERR', 0
			DB    A1H, 'EXP'
			DB    A2H, 'EXT', 0
			DB    E3H, 'FOR'
			DB    A3H, 'FALSE', 0
			DB    A4H, 'FN'
			DB    E5H, 'GOTO'
			DB    BEH, 'GET$'
			DB    A5H, 'GET'
			DB    E4H, 'GOSUB'
			DB    E6H, 'GCOL'
			DB    93H, 'HIMEM', 0
			DB    E8H, 'INPUT'
			DB    E7H, 'IF'
			DB    BFH, 'INKEY$'
			DB    A6H, 'INKEY'
			DB    A8H, 'INT'
			DB    A7H, 'INSTR('
			DB    C9H, 'LIST'
			DB    86H, 'LINE'
			DB    C8H, 'LOAD'
			DB    92H, 'LOMEM', 0
			DB    EAH, 'LOCAL'
			DB    C0H, 'LEFT$('
			DB    A9H, 'LEN'
			DB    E9H, 'LET'
			DB    ABH, 'LOG'
			DB    AAH, 'LN'
			DB    C1H, 'MID$('
			DB    EBH, 'MODE'
			DB    83H, 'MOD'
			DB    ECH, 'MOVE'
			DB    EDH, 'NEXT'
			DB    CAH, 'NEW', 0
			DB    ACH, 'NOT'
			DB    CBH, 'OLD', 0
			DB    EEH, 'ON'
			DB    87H, 'OFF'
			DB    84H, 'OR'
			DB    8EH, 'OPENIN'
			DB    AEH, 'OPENOUT'
			DB    ADH, 'OPENUP'
			DB    FFH, 'OSCLI'
			DB    F1H, 'PRINT'
			DB    90H, 'PAGE', 0
			DB    8FH, 'PTR', 0
			DB    AFH, 'PI', 0
			DB    F0H, 'PLOT'
			DB    B0H, 'POINT('
			DB    F2H, 'PROC'
			DB    B1H, 'POS', 0
			DB    CEH, 'PUT'
			DB    F8H, 'RETURN', 0
			DB    F5H, 'REPEAT'
			DB    F6H, 'REPORT', 0
			DB    F3H, 'READ'
			DB    F4H, 'REM'
			DB    F9H, 'RUN', 0
			DB    B2H, 'RAD'
			DB    F7H, 'RESTORE'
			DB    C2H, 'RIGHT$('
			DB    B3H, 'RND', 0
			DB    CCH, 'RENUMBER'
			DB    88H, 'STEP'
			DB    CDH, 'SAVE'
			DB    B4H, 'SGN'
			DB    B5H, 'SIN'
			DB    B6H, 'SQR'
			DB    89H, 'SPC'
			DB    C3H, 'STR$'
			DB    C4H, 'STRING$('
			DB    D4H, 'SOUND'
			DB    FAH, 'STOP', 0
			DB    B7H, 'TAN'
			DB    8CH, 'THEN'
			DB    B8H, 'TO'
			DB    8AH, 'TAB('
			DB    FCH, 'TRACE'
			DB    91H, 'TIME', 0
			DB    B9H, 'TRUE', 0
			DB    FDH, 'UNTIL'
			DB    BAH, 'USR'
			DB    EFH, 'VDU'
			DB    BBH, 'VAL'
			DB    BCH, 'VPOS', 0
			DB    FEH, 'WIDTH'
			DB    D3H, 'HIMEM'
			DB    D2H, 'LOMEM'
			DB    D0H, 'PAGE'
			DB    CFH, 'PTR'
			DB    D1H, 'TIME'
;
; These are indexed from the ERRWDS table
;
			DB    01H, 'Missing '
			DB    02H, 'No such '
			DB    03H, 'Bad '
			DB    04H, ' range'
			DB    05H, 'variable'
			DB    06H, 'Out of'
			DB    07H, 'No '
			DB    08H, ' space'

KEYWDL:			EQU     $-KEYWDS
			DW    -1
;
; Error messages
;
ERRWDS:			DB    7, 'room', 0		;  0: No room
			DB    6, 4, 0			;  1: Out of range
			DB    0				;  2: *
			DB    0				;  3: *
			DB    'Mistake', 0		;  4: Mistake
			DB    1, ',', 0			;  5: Missing ,
			DB    'Type mismatch', 0	;  6: Type mismatch
			DB    7, FN, 0			;  7: No FN
			DB    0				;  8: *
			DB    1, 34, 0			;  9: Missing "
			DB    3, DIM, 0			; 10: Bad DIM
			DB    DIM, 8, 0			; 11: DIM space
			DB    'Not ', LOCAL_, 0		; 12: Not LOCAL
			DB    7, PROC, 0		; 13: No PROC
			DB    'Array', 0		; 14: Array
			DB    'Subscript', 0		; 15: Subscript
			DB    'Syntax error', 0		; 16: Syntax error
			DB    'Escape', 0		; 17: Escape
			DB    'Division by zero', 0	; 18: Division by zero
			DB    'String too long', 0	; 19: String too long
			DB    'Too big', 0		; 20: Too big
			DB    '-ve root', 0		; 21: -ve root
			DB    'Log', 4, 0		; 22: Log range
			DB    'Accuracy lost', 0	; 23: Accuracy lost
			DB    'Exp', 4, 0		; 24: Exp range
			DB    0				; 25: *
			DB    2, 5, 0			; 26: No such variable
			DB    1, ')', 0			; 27: Missing )
			DB    3, 'HEX', 0		; 28: Bad HEX
			DB    2, FN, '/', PROC, 0	; 29: No such FN/PROC
			DB    3, 'call', 0		; 30: Bad call
			DB    'Arguments', 0		; 31: Arguments
			DB    7, FOR, 0			; 32: No FOR
			DB    "Can't match ", FOR, 0	; 33: Can't match FOR
			DB    FOR, ' ', 5, 0		; 34: FOR variable
			DB    0				; 35: *
			DB    7, TO, 0			; 36: No TO
			DB    0				; 37: *
			DB    7, GOSUB, 0		; 38: No GOSUB
			DB    ON_, ' syntax', 0		; 39: ON syntax
			DB    ON_, 4, 0			; 40: ON range
			DB    2, 'line', 0		; 41: No such line
			DB    6, ' ', DATA_, 0		; 42: Out of DATA
			DB    7, REPEAT, 0		; 43: No REPEAT
			DB    0				; 44: *
			DB    1, '#', 0			; 45: Missing #
;
; COMMANDS:
;
; DELETE line,line
;
DELETE:			CALL    SETTOP          	; Set TOP sysvar (first free byte at end of BASIC program)
			CALL    DLPAIR			; Get the line number pair - HL: BASIC program address, BC: second number (or 0 if missing)
DELET1:			LD      A,(HL)			; Check whether it's the last line
			OR      A			
			JR      Z,WARMNC		; Yes, so do nothing
			INC     HL			; Skip the line length byte
			LD      E,(HL)			; Fetch the line number in DE
			INC     HL
			LD      D,(HL)
			LD      A,D			; If the line number is zero then
			OR      E
			JR      Z,CLOOP1        	; Do nothing
			DEC     HL			; Decrement BASIC program pointer back to length
			DEC     HL
			EX      DE,HL			; Check if we've gone past the terminating line
			SCF
			SBC     HL,BC
			EX      DE,HL
			JR      NC,WARMNC		; Yes, so exit back to BASIC prompt
			PUSH    BC			
			CALL    DEL			; Delete the line pointed to by HL
			POP     BC
			JR      DELET1			; And loop round to the next line
;
; LISTO expr
;
LISTO:			INC     IY              	; Skip "O" byte
			CALL    EXPRI			; Get expr
			EXX
			LD      A,L
			LD      (LISTON),A		; Store in LISTON sysvar
CLOOP1:			JP      CLOOP
;
; LIST
; LIST line
; LIST line,line [IF string]
; LIST ,line
; LIST line,
;
LIST_:			CP      'O'			; Check for O (LISTO)
			JR      Z,LISTO			; and jump to LISTO if zero
			CALL    DLPAIR			; Get the line number pair - HL: BASIC program address, BC: second number (or 0 if missing)
			CALL    NXT			; Skip space
			CP      TIF             	; Check for IF clause (token IF)
			LD      A,0             	; Initialise the IF clause string length
			JR      NZ,LISTB		; If there is no IF clause, skip the next bit
;
			INC     IY              	; Skip the IF token
			CALL    NXT             	; And skip any spaces
			EX      DE,HL			; DE: Address in memory
			PUSH    IY			; LD IY, HL
			POP     HL              	; HL is now the address of the tokenised line
			LD      A,CR			
			PUSH    BC			; Stack the second line number arg
			LD      BC,256
			CPIR                    	; Locate CR byte
			LD      A,C
			CPL                    	 	; A: Substring length (of IF clause)
			POP     BC			; Restore the second line number arg
			EX      DE,HL			; HL: Address in memory
;
LISTB:			LD      E,A             	; E: IF clause string length
			LD      A,B			; Check whether a second line number was passed (BC!=0)
			OR      C
			JR      NZ,LISTA		; If there isn't a second line number
			DEC     BC			; then we set it to the maximum of 65535
;
LISTA:			EXX
			LD      IX,LISTON		; IX : Pointer to the LISTON (LISTO and OPT) sysvar
			LD      BC,0            	; BC': Indentation counter (C: FOR/NEXT, B: REPEAT/UNTIL)
			EXX
			LD      A,20			; Number of lines to list
;
LISTC:			PUSH    BC              	; Save second line number
			PUSH    DE              	; Save IF clause length
			PUSH    HL              	; Save BASIC program counter
			EX      AF,AF'
;
; BBC BASIC for Z80 lines are stored as follows:
;
; - [LEN] [LSB] [MSB] [DATA...] [0x0D]: LSB, MSB = line number
; - [&00] [&FF] [&FF]: End of program marker
;
; This is the Russell format and different to the Wilson/Acorn format: https://www.beebwiki.mdfs.net/Program_format
;
			LD      A,(HL)			; Check for end of program marker
			OR      A			; If found
			JR      Z,WARMNC		; Jump to WARMNC (F=NC, so will jump to WARM)
;
; Check if past terminating line number
;
			LD      A,E             	; A: IF clause length
			INC     HL			; Skip the length byte	
			LD      E,(HL)			; Fetch the line number in DE
			INC     HL
			LD      D,(HL)          
			DEC     HL			; Step HL back to the length byte	
			DEC     HL
			PUSH    DE             	 	; Push the line number on the stack
			EX      DE,HL			; HL: line number
			SCF				; Do a 16-bit compare of HL and DE
			SBC     HL,BC
			EX      DE,HL
			POP     DE              	; Restore the line number
WARMNC:			JP      NC,WARM			; If exceeded the terminating line number then jump to WARM
			LD      C,(HL)          	; C: Line length + 4
			LD      B,A             	; B: IF clause length
;
; Check if "UNLISTABLE":
;
			LD      A,D			; TODO: What is "UNLISTABLE?"
			OR      E
			JP      Z,CLOOP
;
; Check for IF clause:
;
			INC     HL			; Skip the length
			INC     HL			; Skip the line number
			INC     HL              	; HL: Address of the tokenised BASIC line
			DEC     C			;  C: Line length
			DEC     C
			DEC     C
			DEC     C              	
			PUSH    DE              	; Save the line number
			PUSH    HL              	; Save the BASIC program address
			XOR     A               	;
			CP      B              	 	; Check for an IF clause (B!=0)
			PUSH    IY			; LD IY, DE
			POP     DE              	; DE: Address of the IF clause string in the input buffer
			CALL    NZ,SEARCH      		; If there is an IF clause (B!=0) then search for it
			POP     HL              	; Restore BASIC program address
			POP     DE              	; Restore line number
			PUSH    IY			
			CALL    Z,LISTIT        	; List if no IF clause OR there is an IF clause match
			POP     IY
;
			EX      AF,AF'
			DEC     A			; Decrement line list counter
			CALL    LTRAP			; TODO: This destroys A - is this a bug I've introduced in LTRAP?
			POP     HL             	 	; Restore BASIC program address to beginning of line
			LD      E,(HL)			; Fetch the length of line in DE
			LD      D,0
			ADD     HL,DE           	; Go to the next line
			POP     DE              	; Restore IF clause length
			POP     BC              	; Restore second line number
			JR      LISTC			; Loop back to do next line
;
; RENUMBER
; RENUMBER start
; RENUMBER start,increment
; RENUMBER ,increment
;
RENUM:			CALL    CLEAR           	; Uses the heap so clear all dynamic variables and function/procedure pointers
			CALL    PAIR            	; Fetch the parameters - HL: start (NEW line number), BC: increment
			EXX
			LD      HL,(PAGE_)		; HL: Top of program
			LD      DE,(LOMEM)		; DE: Start address of the heap
;
; Build the table
;
RENUM1:			LD      A,(HL)          	; Fetch the line length byte
			OR      A			; Is it zero, i.e. the end of program marker?
			JR      Z,RENUM2		; Yes, so skip to the next part
			INC     HL			
			LD      C,(HL)          	; BC: The OLD line number
			INC     HL
			LD      B,(HL)
			LD      A,B
			OR      C
			JP      Z,CLOOP        		; If the line number is zero, then exit back to the command line
			EX      DE,HL			; DE: Pointer to BASIC program, HL: Pointer to heap
			LD      (HL),C			; Store the OLD line number in the heap
			INC     HL
			LD      (HL),B
			INC     HL
			EXX				; HL: line number, BC: increment			
			PUSH    HL			; HL: Stack the NEW line number value
			ADD     HL,BC           	; Add the increment
			JP      C,TOOBIG        	; If > 65535, then error: "Too big"
			EXX				; DE: Pointer to BASIC program, HL: Pointer to heap
			POP     BC			; BC: Pop the NEW line number value off the stack
			LD      (HL),C			; Store the NEW line number in the heap
			INC     HL
			LD      (HL),B
			INC     HL
			EX      DE,HL			; HL: Pointer to BASIC program, DE: Pointer to heap
			DEC     HL			; Back up to the line length byte
			DEC     HL
			XOR     A			; Not sure why this is done here instead of LD B,0
			LD      B,A			; BC: Line length
			LD      C,(HL)
			ADD     HL,BC           	; Advance HL to next line
			EX      DE,HL			; DE: Pointer to BASIC program, HL: Pointer to heap
			PUSH    HL
			INC     H			; Increment to next page
			SBC     HL,SP			; Subtract from SP
			POP     HL			
			EX      DE, HL			; HL: Pointer to BASIC program, DE: Pointer to heap
			JR      C,RENUM1        	; Loop, as the heap pointer has not strayed into the stack page
			CALL    EXTERR          	; Otherwise throw error: "RENUMBER space'
			DB    	REN
			DB    	8
			DB    	0
;
; At this point a list of BASIC line numbers have been written to the heap
; as word pairs:
; - DW: The OLD line number
; - DW: The NEW line number
;
RENUM2:			EX      DE,HL			; HL: Pointer to the end of the heap
			LD      (HL),-1			; Mark the end with FFFFh
			INC     HL
			LD      (HL),-1
			LD      DE,(LOMEM)		; DE: Pointer to the start of the heap
			EXX				
			LD      HL,(PAGE_)		; HL: Start of the BASIC program area
RENUM3:			LD      C,(HL)			; Fetch the first line length byte
			LD      A,C			; If it is zero, then no program, so...
			OR      A
			JP      Z,WARM			; Jump to warm start
			EXX				; HL: Pointer to end of heap, DE: Pointer to start of heap
			EX      DE,HL			; DE: Pointer to end of heap, HL: Pointer to start of heap
			INC     HL			; Skip to the NEW line number	
			INC     HL
			LD      E,(HL)			; DE: The NEW line number
			INC     HL
			LD      D,(HL)
			INC     HL
			PUSH    DE			; Stack the NEW line number
			EX      DE,HL			; HL: The NEW line number, DE: Pointer to the end of heap
			LD      (LINENO),HL		; Store the line number in LINENO
			EXX				; HL: Pointer to the BASIC program area
			POP     DE			; DE: The NEW line number
			INC     HL
			LD      (HL),E          	; Write out the NEW line number to the BASIC program
			INC     HL
			LD      (HL),D
			INC     HL
			DEC     C			; Subtract 3 from the line length to compensate for increasing HL by 3 above
			DEC     C
			DEC     C
			LD      B,0			; BC: Line length
;
RENUM7:			LD      A,LINO			; A: The token code that precedes any line number encoded in BASIC (i.e. GOTO/GOSUB)
			CPIR                    	; Search for the token
			JR      NZ,RENUM3		; If not found, then loop to process the next line
;
; Having established this line contains at least one encoded line number, we need to update it to point to the new line number
;
			PUSH    BC			; Stack everything
			PUSH    HL
			PUSH    HL			; HL: Pointer to encoded line number
			POP     IY			; IY: Pointer to encoded line number
			EXX				 
			CALL    DECODE			; Decode the encoded line number (in HL')
			EXX				; HL: Decoded line number
			LD      B,H			; BC: Decoded line number
			LD      C,L
			LD      HL,(LOMEM)		; HL: Pointer to heap
;
; This section of code cross-references the decoded (OLD) line number with the list
; created previously in the global heap
;
RENUM4:			LD      E,(HL)          	; DE: The OLD line number
			INC     HL
			LD      D,(HL)
			INC     HL
			EX      DE,HL			; HL: The OLD line number, DE: Pointer in the global heap
			OR      A               	; Clear the carry and...
			SBC     HL,BC			; Compare by means of subtraction the OLD line number against the one in the heap
			EX      DE,HL			; HL: Pointer in the global heap
			LD      E,(HL)          	; DE: The NEW line number
			INC     HL
			LD      D,(HL)
			INC     HL
			JR      C,RENUM4		; Loop until there is a match (Z) or not (NC)
			EX      DE,HL			; DE: Pointer in the global heap
			JR      Z,RENUM5        	; If Z flag is set, there is an exact match to the decoded line number on the heap
;
			CALL    TELL			; Display this error if the line number is not found
			DB    	'Failed at '
			DB    	0
			LD      HL,(LINENO)
			CALL    PBCDL
			CALL    CRLF
			JR      RENUM6			; And carry on renumbering
;
; This snippet re-encodes the line number in the BASIC program
;
RENUM5:			POP     DE			; DE: Pointer to the encoded line number in the listing
			PUSH    DE
			DEC     DE			; Back up a byte to the LINO token
			CALL    ENCODE          	; Re-write the new line number out
RENUM6:			POP     HL			; HL: Pointer to the encoded line number in the listing
			POP     BC			; BC: The remaining line length
			JR      RENUM7			; Carry on checking for any more encoded line numbers in this line
;
; AUTO
; AUTO start,increment
; AUTO start
; AUTO ,increment
;
AUTO:			CALL    PAIR			; Get the parameter pair (HL: first parameter, BC: second parameter)
			LD      (AUTONO),HL		; Store the start in AUTONO
			LD      A,C			; Increment is 8 bit (0-255)
			LD      (INCREM),A		; Store that in INCREM
			JR      CLOOP0			; Jump back indirectly to the command loop via CLOOP0 (optimisation for size)
;
; BAD
; NEW
;
BAD:			CALL    TELL            	; Output "Bad program" error
			DB    3				; Token for "BAD"
			DB    'program'
			DB    CR
			DB    LF
			DB    0				; Falls through to NEW	
;
NEW:			CALL    NEWIT			; Call NEWIT (clears program area and variables)
			JR      CLOOP0			; Jump back indirectly to the command loop via CLOOP0 (optimisation for size)	
;
; OLD
;
OLD:			LD      HL,(PAGE_)		; HL: The start of the BASIC program area
			PUSH    HL			; Stack it
			INC     HL			; Skip the potential length byte of first line of code
			INC     HL			; And the line number word
			INC     HL
			LD      BC,252			; Look for a CR in the first 252 bytes of code; maximum line length
			LD      A,CR
			CPIR
			JR      NZ,BAD			; If not found, then the first line of code is not a valid BBC BASIC code
			LD      A,L			; It could still be garbage though! Store the position in A; this requires
			POP     HL			; PAGE to be on a 256 page boundary, and is now the length of the first line
			LD      (HL),A			; Restore the length byte (this will have been set to 0 by NEW)
			CALL    CLEAN			; Further checks for bad program, set TOP, write out &FFFF end of program marker
CLOOP0:			JP      CLOOP			; Jump back to the command loop
;
; LOAD filename
;
LOAD:			CALL    EXPRS           	; Get the filename
			LD      A,CR			; DE points to the last byte of filename in ACCS
			LD      (DE),A			; Terminate filename with a CR
			CALL    LOAD0			; Load the file in, then CLEAN
			CALL    CLEAR			; Further checks for bad program, set TOP, write out &FFFF end of program marker
			JR      WARM0			; Jump back to the command loop
;
; SAVE filename
;
SAVE:			CALL    SETTOP          	; Set TOP sysvar
			CALL    EXPRS           	; Get the filename
			LD      A,CR			; Terminate the filename with a CR
			LD      (DE),A
			LD      DE,(PAGE_)		; DE: Start of program memory
			LD      HL,(TOP)		; HL: Top of program memory
			OR      A			; Calculate program size (TOP-PAGE)
			SBC     HL,DE
			LD      B,H             	; BC: Length of program in bytes
			LD      C,L
			LD      HL,ACCS			; HL: Address of the filename
			CALL    OSSAVE			; Call the SAVE routine in patch.asm
WARM0:			JP      WARM			; Jump back to the command loop

;
; ERROR
; Called whenever BASIC needs to halt with an error
; Error messages are indexed from 0
; Inputs:
;  A: Error number
;
ERROR_:			LD      SP,(HIMEM)		; Set SP to HIMEM
			LD      HL,ERRWDS		; Index into the error string table
			OR      A			; We don't need to search for the first error
			JR      Z,ERROR1		; So skip the search routine
;
; Search the error table for error #A
; HL will end up being the pointer into the correct error
; There is no bounds checking on this, so invalid error numbers will probably output garbage
;
			LD      B,A             	; Store error number in B
			EX      AF,AF'			; Store error number in AF'
			XOR     A
ERROR0:			CP      (HL)			; Compare the character with 0 (the terminator byte)
			INC     HL			; Increment the string pointer
			JR      NZ,ERROR0		; Loop until with hit a 0
			DJNZ    ERROR0			; Decrements the error number and loop until 0
			EX      AF,AF'			; Restore the error number from AF'
;
; At this point HL points to the tokenised error string
;
ERROR1:			PUSH    HL			; Stack the error string pointer and fall through to EXTERR

; 
; EXTERR
; Inputs:
;  A: Error number
;
; This is the entry point for external errors, i.e. ones not in the ERRWDS table
; The error text immediately follows the CALL to EXTERR, for example:
; > CALL  EXTERR
; > DB    "Silly", 0
; So we can get the address of the string by popping the return address off the stack
;			
EXTERR:			POP     HL			; Pop the error string pointer
			LD      (ERRTXT),HL		; Store in ERRTXT sysvar
			LD      SP,(HIMEM)		; Set SP to HIMEM
			LD      (ERR),A			; Store error number in ERR sysvar
			CALL    SETLIN			; Get line number
			LD      (ERL),HL		; Store in ERL sysvar
			OR      A			; Is error number 0?
			JR      Z,ERROR2		; Yes, so skip the next bit as error number 0 is untrappable
;
			LD      HL,(ERRTRP)		; Check whether the error is trapped
			LD      A,H
			OR      L
			PUSH    HL			; HL: Error line
			POP     IY			; IY: HL
			JP      NZ,XEQ         	 	; If error trapped, jump to XEQ
;
ERROR2:			LD      HL,0
			LD      (AUTONO),HL		; Cancel AUTO
			LD      (TRACEN),HL     	; Cancel TRACE
			CALL    RESET           	; Reset OPSYS
			CALL    CRLF			; Output newline
			CALL    REPORT          	; Output the error message
			CALL    SAYLN			; Output " at line nnnn" message.
			LD      E,0			; Close all files
			CALL    C,OSSHUT        	
			CALL    CRLF			; Output newline
			JP      CLOOP			; Back to CLOOP
;
; SUBROUTINES:
;
; LEX - SEARCH FOR KEYWORDS
;   Inputs: HL = start of keyword table
;           IY = start of match text
;  Outputs: If found, Z-flag set, A=token.
;           If not found, Z-flag reset, A=(IY).
;           IY updated (if NZ, IY unchanged).
; Destroys: A,B,H,L,IY,F
;
LEX:			LD      HL,KEYWDS		; Address of the keywords table
;
LEX0:			LD      A,(IY)			; Fetch the character to match
			LD      B,(HL)			; B: The token from the keywords table
			INC     HL			; Increment the pointer in the keywords table
			CP      (HL)			; Compare the first characters
			JR      Z,LEX2			; If there is a match, then skip to LEX2
			RET     C               	; No match, so fail
;
; This snippet of code skips to the next token in the KEYWDS table
;
LEX1:			INC     HL			; Increment the pointer
			BIT     7,(HL)			; Check if bit 7 set (all token IDs have bit 7 set)
			JR      Z,LEX1			; No, so loop
			JR      LEX0			; At this point HL is pointing to the start of the next keyword
;
LEX2:			PUSH    IY              	; Save the input pointer
LEX3:			INC     HL			; Increment the keyword pointer
			BIT     7,(HL)			; If we've reached the end (marked by the start of the next token) then
			JR      NZ,LEX6         	; Jump to here as we've found a token
			INC     IY			; Increment the text pointer
			LD      A,(IY)			; Fetch the character
			CP      '.'			; Is it an abbreviated keyword?
			JR      Z,LEX6          	; Yes, so we'll return with the token we've found
			CP      (HL)			; Compare with the keywords list
			JR      Z,LEX3			; It's a match, so continue checking this keyword
			CALL    RANGE1			; Is it alphanumeric, '@', '_' or '`'
			JR      C,LEX5			; No, so check whether keyword needs to be immediately delimited
;	
LEX4:			POP     IY              	; Restore the input pointer ready for the next search
			JR      LEX1			; And loop back to start again
;
; This section handles the 0 byte at the end of keywords that indicate the keyword needs to be
; immediately delimited
;
LEX5:			LD      A,(HL)			; Fetch the byte from the keywords table	
			OR      A			; If it is not zero, then...
			JR      NZ,LEX4			; Keep searching
			DEC     IY			; If it is zero, then skip the input pointer back one byte
;
; We've found a token at this point
;
LEX6:			POP     AF			; Discard IY input pointer pushed on the stack
			XOR     A			; Set the Z flag
			LD      A,B			; A: The token
			RET
;
; DEL - DELETE A PROGRAM LINE.
;   Inputs: HL addresses program line.
; Destroys: B,C,F
;
; This simply erases the line by moving all of the code after the line to be deleted back over
; it using an LDIR
;
DEL:			PUSH    DE
			PUSH    HL
			PUSH    HL			; HL: Address of the program line
			LD      B,0			; BC: Length of the line
			LD      C,(HL)
			ADD     HL,BC			; HL: Advanced to the start of the next line
			PUSH    HL
			EX      DE,HL			; DE: Pointer to the next line
			LD      HL,(TOP)		; HL: Pointer to the end of the program
			SBC     HL,DE			
			LD      B,H			; BC: Size of block to move
			LD      C,L
			POP     HL			; HL: Pointer to next line
			POP     DE			; DE: Pointer to this line
			LDIR                    	; Delete the line
			LD      (TOP),DE		; Adjust TOP
			POP     HL
			POP     DE
			RET
;
;LOAD0 - LOAD A DISK FILE THEN CLEAN.
;   Inputs: Filename in ACCS (term CR)
; Destroys: A,B,C,D,E,H,L,F
;
;CLEAN - CHECK FOR BAD PROGRAM, FIND END OF TEXT
; AND WRITE FF FF, THEN LOAD (TOP).
; Destroys: A,B,C,H,L,F
;
LOAD0: 			LD      DE,(PAGE_)		; DE: Beginning of BASIC program area
			LD      HL,-256			
			ADD     HL,SP			
			SBC     HL,DE           	; Find available space
			LD      B,H
			LD      C,L
			LD      HL,ACCS
			CALL    OSLOAD          	; Call the OSLOAD function in patch
			CALL    NC,NEWIT		; If NC then NEW
			LD      A,0
			JP      NC,ERROR_        	; And trigger a "No room" error, otherwise...
;							
CLEAN:			CALL    SETTOP			; Set TOP sysvar
			DEC     HL			; Write out the end of program markers
			LD      (HL),-1         	
			DEC     HL
			LD      (HL),-1
			JR      CLEAR			; Clear all dynamic variables and function/procedure pointers
;
; Set the TOP sysvar; the first free location after the end of the current program
; Returns:
; - HL: TOP
;
SETTOP:			LD      HL,(PAGE_)		; Start at beginning of BASIC program area
			LD      B,0			;  B: 0
			LD      A,CR			; End of line marker
SETOP1:			LD      C,(HL)			; BC: Get first byte of program line (line length)
			INC     C			; Check for zero
			DEC     C
			JR      Z,SETOP2		; If it is zero, we've reached the end
			ADD     HL,BC			; Skip to next line 
			DEC     HL			; Check end of previous line
			CP      (HL)
			INC     HL
			JR      Z,SETOP1		; If CR then loop
			JP      BAD			; If anything else, then something has gone wrong - trip a Bad Program error
;
SETOP2:			INC     HL             		; Skip the 3 byte end of program marker (&00, &FF, &FF)
			INC     HL			; NB: Called from NEWIT
			INC     HL
			LD      (TOP),HL		; Store in TOP sysvar
			RET
;
; NEWIT - NEW PROGRAM THEN CLEAR
;   Destroys: H,L
;
; CLEAR - CLEAR ALL DYNAMIC VARIABLES INCLUDING
; FUNCTION AND PROCEDURE POINTERS.
;   Destroys: Nothing
;
NEWIT:			LD      HL,(PAGE_)		; HL: First byte of BASIC program area
			LD      (HL),0			; Stick a 0 in there
			CALL    SETOP2			; Skip three bytes to get to end of empty BASIC program area and set TOP sysvar
;
CLEAR:			PUSH    HL			; Stack the BASIC program pointer
			LD      HL,(TOP)		; Get the TOP sysvar - first available byte after BASIC
			LD      (LOMEM),HL		; Set the LOMEM sysvar
			LD      (FREE),HL		; And the FREE sysvar with that value
			LD      HL,DYNVAR		; Get the pointer to the dynamic variable pointers buffer in RAM
			PUSH    BC			
			LD      B,2*(54+2)		; Loop counter
CLEAR1:			LD      (HL),0			; Clear the dynamic variable pointers
			INC     HL
			DJNZ    CLEAR1
			POP     BC
			POP     HL			; Restore the BASIC program pointer
			RET
;
;LISTIT - LIST A PROGRAM LINE.
;    Inputs: HL addresses line
;            DE = line number (binary)
;            IX = Pointer to LISTON
;             B = FOR/NEXT indent level
;             C = REPEAT/UNTIL indent level 
;  Destroys: A,D,E,B',C',D',E',H',L',IY,F
;
LISTIT:			PUSH    HL			; Stack the address of the line
			EX      DE,HL			; HL: Line number
			PUSH    BC
			CALL    PBCD			; Print the line number
			POP     BC
			POP     HL			; HL: Address of the first token/character
			LD      A,(HL)			; Fetch the token
			CP      NEXT			; Is it NEXT...
			CALL    Z,INDENT		; Yes, so indent in
			CP      UNTIL			; Or is it UNTIL...
			CALL    Z,INDENT		; Yes, so indent in
			EXX
			LD      A,' '
			BIT     0,(IX)			; If BIT 0 of LISTON is set
			CALL    NZ,OUTCHR		; Then print a space after the line number
			LD      A,B			; Fetch the FOR/NEXT indent level
			ADD     A,A			; Multiply by 2
			BIT     1,(IX)			; If BIT 1 of LISTON is set
			CALL    NZ,FILL			; Then print the FOR/NEXT indent
			LD      A,C			; Fetch the REPEAT/UNTIL indent level
			ADD     A,A			; Multiply by 2
			BIT     2,(IX)			; If BIT 2 of LISTON is set
			CALL    NZ,FILL			; Then print the REPEAT/UNTIL indent
			EXX
			LD      A,(HL)			; Fetch the token
			CP      FOR			; Is it FOR?
			CALL    Z,INDENT		; Yes, so indent
			CP      REPEAT			; Is it REPEAT?
			CALL    Z,INDENT		; Yes, so indent
			LD      E,0			; E: The quote counter - reset to 0
LIST8:			LD      A,(HL)			; Fetch a character / token byte
			INC     HL			
			CP      CR			; Is it end of line?
			JR      Z,LISTE			; Yes, so finish (DB: Used to jump to CRLF, modified for *EDIT)
			CP      34			; Is it a quote character?
			JR      NZ,LIST7		; No, so skip to next bit
			INC     E			; Otherwise increment quote counter
LIST7:			CALL    LOUT			; Output the character / token
			JR      LIST8			; And repeat
;
; DB: Modification for *EDIT
; Terminate the line with either a CRLF or a NUL character
; 
LISTE:			BIT 	3,(IX)			; Are we printing to buffer?
			JR	Z, CRLF			; Yes, so print a CRLF
			XOR	A			; Otherwise print a NUL (0)
			JP	OSWRCH
;
; Decode the 3 byte GOTO type line number
;
PRLINO:			PUSH    HL			; Swap HL and IY
			POP     IY			; IY: Pointer to the line number
			PUSH    BC		
			CALL    DECODE			; Decode
			POP     BC
			EXX
			PUSH    BC
			CALL    PBCDL			; Output the line number
			POP     BC
			EXX
			PUSH    IY			; Swap HL and IY
			POP     HL			; HL: Pointer to the next character in the line
			RET
;
; DB: Modification for internationalisation
;
PRREM:			CALL	OUT_			; Output the REM token
$$:			LD	A, (HL)			; Fetch the character
			CP	CR			; If it is end of line, then
			RET	Z			; we have finished
			CALL	OUTCHR			; Ouput the character
			INC	HL			
			JR	$B			; And loop		
;
; DB: End of modification
;
LOUT:			BIT     0,E			; If the quote counter is odd (bit 1 set) then
			JR      NZ,OUTCHR		; don't tokenise, just output the character
			CP	REM			; DB: Is it REM
			JR	Z, PRREM		; DB: Yes so jump to the special case for REM
			CP      LINO			; Is it a line number (following GOTO/GOSUB etc)?
			JR      Z,PRLINO		; Yes, so decode and print the line number
			CALL    OUT_			; Output a character / keyword
			LD      A,(HL)			; Fetch the next character	
;
; This block of code handles the indentation
; B: Counter for FOR/NEXT indent
; C: Counter for REPEAT/UNTIL indent
;
INDENT:			EXX
			CP      FOR			; If the token is FOR
			JR      Z,IND1			; Then INC B
			CP      NEXT			; If it is NEXT
			JR      NZ,IND2_		; Then...
			DEC     B			; DEC B
			JP      P,IND2_			; If we have gone below 0 then
IND1:			INC     B			; Increment back to 0
;
IND2_:			CP      REPEAT			; If the token is REPEAT
			JR      Z,IND3			; Then INC C
			CP      UNTIL			; If it is UNTIL
			JR      NZ,IND4			; Then...
			DEC     C			; DEC C
			JP      P,IND4			; If we have gone below 0 then
IND3:			INC     C			; Incremet back to 0
IND4:			EXX		
			RET
;
;CRLF - SEND CARRIAGE RETURN, LINE FEED.
;  Destroys: A,F
;OUTCHR - OUTPUT A CHARACTER TO CONSOLE.
;    Inputs: A = character
;  Destroys: A,F
;
CRLF:			LD      A,CR			; Output CR
			CALL    OUTCHR
			LD      A,LF			; Output LF
;
OUTCHR:			CALL    OSWRCH			; Output the character in A
			SUB     CR			; Check for CR
			JR      Z,CARRET		; If it is CR then A will be 0, this will clear the count
			RET     C              		; If it is less than CR, it is non-printing, so don't increment the count
			LD      A,(COUNT)		; Increment the count
			INC     A
;
CARRET:			LD      (COUNT),A		; Store the new count value	
			RET     Z			; Return if the count has wrapped to 0
			PUSH    HL			; Now check if count = print width		
			LD      HL,(WIDTH)		; Get the print width; it's a byte value, so
			CP      L			; L is the width. Compare it with count.
			POP     HL
			RET     NZ			; If we've not hit print width, then just return
			JR      CRLF			; Otherwise output CRLF
;
; OUT - SEND CHARACTER OR KEYWORD
;   Inputs: A = character (>=10, <128)
;           A = Token (<10, >=128)
;  Destroys: A,F
;
OUT_:			CP      138			; Neat trick to do condition: If A >= 10 or < 128 then PE flag is set
			JP      PE,OUTCHR		; If so, then it's a character, so just output it
;
; This bit looks up the character in the KEYWDS token table and expands it
; Note the CP 138; this sets the overflow flag as follows:
;
; NB:
;  1. Any 8-bit number between 128 and 255 is negative (two's complement) so 138 is -118, 128 = -128
;  2. CP is effectively a SUB; sets the flags without affecting A
;  3. The operation n - -118 ~ n + 118
;
; So:
;  *   9 CP 138 ~    9 + 118 = 127 = no overflow : token
;  *  10 CP 138 ~   10 + 118 = 128 =    overflow : character
;  * 127 CP 138 ~  127 + 118 = 245 =    overflow : character
;  * 128 CP 138 ~ -128 + 118 = -10 = no overflow : token
;
			PUSH    BC			; Preserve BC and HL
			PUSH    HL
			LD      HL,KEYWDS		; The list of tokens and keywords
			LD      BC,KEYWDL		; The length of the keyword list
			CPIR				; We can just do a straight CPIR as the token characters are unique in the list
;							; At this point HL points to the next byte, the first character of the token
TOKEN1:			LD      A,(HL)			; Fetch the character
			INC     HL			; Increment to the next byte in the token table
			CP      138			; If A >= 10 or < 128, i.e. we've not hit the token code for the next token
			PUSH    AF			; Then...
			CALL    PE,OUTCHR		; Output the character...
			POP     AF			; 
			JP      PE,TOKEN1		; And loop to the next character 
			POP     HL			; Done, so tidy up the stack and exit
			POP     BC
			RET
;
; FINDL - FIND PROGRAM LINE
;   Inputs: HL = line number (binary)
;  Outputs: HL addresses line (if found)
;           DE = line number
;           Z-flag set if found.
; Destroys: A,B,C,D,E,H,L,F
;
FINDL:			EX      DE,HL			; DE: Line number (binary)
			LD      HL,(PAGE_)		; HL: Top of BASIC program area
			XOR     A               	;  A: 0
			CP      (HL)			; Check for end of program marker
			INC     A			;  A: 1
			RET     NC			; Return with 1 if 0 
			XOR     A               	; Clear the carry flag
			LD      B,A			;  B: 0
;
FINDL1:			LD      C,(HL)			;  C: The line length
			PUSH    HL			; Stack the current program counter
			INC     HL			; Skip to the line number bytes
			LD      A,(HL)			; Fetch the line number (in binary) from the BASIC line in HL
			INC     HL
			LD      H,(HL)
			LD      L,A
			SBC     HL,DE			; Compare with the line number we're searching for
			POP     HL			; Get the current program counter
			RET     NC              	; Then return if found or past (Z flag will be set if line number matches)
			ADD     HL,BC			; Skip to the next line (B was set to 0 before the loop was entered)
			JP      FINDL1			; And loop
;
; SETLIN - Search program for line containing address
;          Update (LINENO)
;   Inputs: Address in (ERRLIN)
;  Outputs: Line number in HL and (LINENO)
; Destroys: B,C,D,E,H,L,F
;
SETLIN:			LD      B, 0			; Zero B for later
			LD      DE, (ERRLIN)		; DE: Address of line
			LD      HL, (PAGE_)		; HL: Start of user program area
			OR      A			; Do a 16 bit compare without destroying HL
			SBC     HL, DE			;  Z: DE = HL, NC: DE <= HL
			ADD     HL, DE			;  C: DE > HL
			JR      NC, SET3		; So skip, as the address is less than or equal to the top of program area
;
SET1:			LD      C, (HL)			; Get the length of the line; zero indicates the end of the BASIC program
			INC     C			; This is a way to check for zero without using the accumulator
			DEC     C			; If it is zero, then...
			JR      Z, SET3			; We've reached the end of the current BASIC program, not found the line
			ADD     HL, BC			; Skip to the next line (we set B to 0 at the top of this subroutine)
			SBC     HL, DE			; Do a 16-bit compare; the previous ADD will have cleared the carry flag
			ADD     HL, DE			
			JR      C, SET1			; Loop whilst DE (the address to search for) is > HL (the current line)
			SBC     HL, BC			; We've found it, so back up to the beginning of the line
			INC     HL			; Skip the length counter
			LD      E, (HL)          	; Fetch the line number
			INC     HL
			LD      D, (HL)
			EX      DE, HL			; HL: The line number
SET2:			LD      (LINENO), HL		; Store in the variable LINENO
			RET
;
SET3:			LD      HL, 0			; We've not found the line at this point so
			JR      SET2			; Set LINENO to 0
;
;SAYLN - PRINT " at line nnnn" MESSAGE.
;  Outputs: Carry=0 if line number is zero.
;           Carry=1 if line number is non-zero.
; Destroys: A,B,C,D,E,H,L,F
;
SAYLN:			LD      HL,(LINENO)		; Get the LINENO sysvar
			LD      A,H			; If it is zero then
			OR      L			
			RET     Z			; Don't need to do anything; return with F:C set to 0
			CALL    TELL			; Output the error message
			DB    	' at line ', 0		
PBCDL:			LD      C,0			; C: Leading character (NUL)
			JR      PBCD0			; Output the line number; return with F:C set to 1
;
; PBCD - PRINT NUMBER AS DECIMAL INTEGER.
;   Inputs: HL = number (binary).
;  Outputs: Carry = 1
; Destroys: A,B,C,D,E,H,L,F
;
PBCD:			LD      C,' '			; C: Leading character (" ")
PBCD0:			LD      B,5			; Number of digits in result
			LD      DE,10000		; Start off with the 10,000 column
PBCD1:			XOR     A			; Counter
PBCD2:			SBC     HL,DE			; Loop and count how many 10,000s we have
			INC     A
			JR      NC,PBCD2
			ADD     HL,DE			; The loop overruns by one, so adjust here
			DEC     A			; A: Number of 10,000s
			JR      Z,PBCD3			; If it is 0, then skip the next bit
			SET     4,C			; C: Set to '0' ASCII (30h)
			SET     5,C
PBCD3:			OR      C			; A is then an ASCII character, or 00h if we've not processed any non-zero digits yet
			CALL    NZ,OUTCHR		; If it is not a leading NUL character then output it
			LD      A,B			; If on first transition, skip this
			CP      5			; TODO: Need to find out why 
			JR      Z,PBCD4			 
			ADD     HL,HL			; HL x  2 : We shift the number being tested left,
			LD      D,H			;         : rather than shifting DE right
			LD      E,L			;         : This makes a lot of sense
			ADD     HL,HL			; HL x  4
			ADD     HL,HL			; HL x  8
			ADD     HL,DE			; HL x 10
PBCD4:			LD      DE,1000			; Set the column heading to 1,000s for subsequent runs
			DJNZ    PBCD1			; Loop until done
			SCF				; SCF set for SAYLN in this module
			RET
;
; PUTVAR - CREATE VARIABLE AND INITIALISE TO ZERO.
;   Inputs: HL, IY as returned from GETVAR (NZ).
;  Outputs: As GETVAR.
; Destroys: everything
;
PUTVAR:			CALL    CREATE			; Create the variable
			LD      A,(IY)			; Fetch the next character
			CP      '('			; Check for bad use of array
			JR      NZ,GETVZ        	; It's fine, so set the exit conditions
ARRAY:			LD      A,14            	; Otherwise Error: 'Array'
ERROR3:			JP      ERROR_
;
;GETVAR - GET LOCATION OF VARIABLE, RETURN IN HL & IX
;   Inputs: IY addresses first character.
;  Outputs: Carry set and NZ if illegal character.
;           Z-flag set if variable found, then:
;            A = variable type (0,4,5,128 or 129)
;            HL = IX = variable pointer.
;            IY updated
;           If Z-flag & carry reset, then:
;            HL, IY set for subsequent PUTVAR call.
; Destroys: everything
;
GETVAR:			LD      A,(IY)			; Get the first character
			CP      '$'			; Is it a string?
			JR      Z,GETV4			; Yes, so branch here
			CP      '!'			; Is it indirection (32-bit)?
			JR      Z,GETV5			; Yes, so branch here
			CP      '?'			; Is it indirection (8-bit)?
			JR      Z,GETV6			; Yes, so branch here
;
			CALL    LOCATE			; Locate the variable
			RET     NZ			; And exit here if not found
;
; At this point:
;  HL: Address of variable in memory
;   D: Variable type (4 = Integer, 5 = Floating point, 129 = String)
;
			LD      A,(IY)			; Further checks
			CP      '('             	; Is it an array?
			JR      NZ,GETVX        	; No, so exit
;
; We are processing an array at this point
;
			PUSH    DE              	; Save the variable type (in D)
			LD      A,(HL)          	; Fetch the number of dimensions
			OR      A
			JR      Z,ARRAY			; If there are none, then Error: 'Array'
			INC     HL			; 
			LD      DE,0            	; Accumulator
			PUSH    AF
			INC     IY              	; Skip "("
			JR      GETV3
;
GETV2:			PUSH    AF
			CALL    COMMA
GETV3:			PUSH    HL
			PUSH    DE
			CALL    EXPRI           ;SUBSCRIPT
			EXX
			POP     DE
			EX      (SP),HL
			LD      C,(HL)
			INC     HL
			LD      B,(HL)
			INC     HL
			EX      (SP),HL
			EX      DE,HL
			PUSH    DE
			CALL    MUL16           ;HL=HL*BC
			POP     DE
			ADD     HL,DE
			EX      DE,HL
			OR      A
			SBC     HL,BC
			LD      A,15
			JR      NC,ERROR3       ;"Subscript"
			POP     HL
			POP     AF
			DEC     A               ;DIMENSION COUNTER
			JR      NZ,GETV2
			CALL    BRAKET          ;CLOSING BRACKET
			POP     AF              ;RESTORE TYPE
			PUSH    HL
			CALL    X4OR5           ;DE=DE*n
			POP     HL
			ADD     HL,DE
			LD      D,A             ;TYPE
			LD      A,(IY)
GETVX:			CP      '?'
			JR      Z,GETV9
			CP      '!'
			JR      Z,GETV8
GETVZ:			PUSH    HL              ;SET EXIT CONDITIONS
			POP     IX
			LD      A,D
			CP      A
			RET
;
; Process strings, unary & binary indirection:
;
GETV4:			LD      A,128           	; Static strings
			JR      GETV7
;
GETV5:			LD      A,4             	; Unary 32-bit indirection
			JR      GETV7
;
GETV6:			XOR     A               	; Unary 8-bit indirection
;
GETV7:			LD      HL,0
			PUSH    AF
			JR      GETV0
;
GETV8:			LD      B,4             	; Binary 32-bt indirection
			JR      GETVA
;
GETV9:			LD      B,0             	; Binary 8-bit indirection
;
GETVA:			PUSH    HL
			POP     IX
			LD      A,D            		; Fetch the variable type
			CP      129			; Is it a string?
			RET     Z               	; Yes, so exit here
			PUSH    BC			
			CALL    LOADN           	; LEFT OPERAND
			CALL    SFIX
			EXX
;
GETV0:			PUSH    HL
			INC     IY
			CALL    ITEMI
			EXX
			POP     DE
			POP     AF
			ADD     HL,DE
			PUSH    HL
			POP     IX
			CP      A
			RET
;
;GETDEF - Find entry for FN or PROC in dynamic area.
;   Inputs: IY addresses byte following "DEF" token.
;  Outputs: Z flag set if found
;           Carry set if neither FN or PROC first.
;           If Z: HL points to entry
;                 IY addresses delimiter
; Destroys: A,D,E,H,L,IY,F
;
GETDEF:			LD      A,(IY+1)
			CALL    RANGE1
			RET     C
			LD      A,(IY)
			LD      HL,FNPTR
			CP      FN
			JR      Z,LOC2
			LD      HL,PROPTR
			CP      PROC
			JR      Z,LOC2
			SCF
			RET
;
; LOCATE - Try to locate variable name in static or
; dynamic variables.  If illegal first character return
; carry, non-zero.  If found, return no-carry, zero.
; If not found, return no-carry, non-zero.
;   Inputs: IY addresses first character of name.
;           A=(IY)
;  Outputs: Z-flag set if found, then:
;            IY addresses terminator
;            HL addresses location of variable
;            D=type of variable:  4 = integer
;                                 5 = floating point
;                               129 = string
; Destroys: A,D,E,H,L,IY,F
;
; Variable names can start with any letter of the alphabet (upper or lower case), underscore (_), or the grave accent (`)
; They can contain any alphanumeric character and underscore (_)
; String variables are postfixed with the dollar ($) character
; Integer variables are postfixed with the percent (%) character
; Static integer variables are named @%, A% to Z%
; All other variables are dynamic
;
LOCATE:			SUB     '@'			; Check for valid range
			RET     C			; First character not "@", "A" to "Z" or "a" to "z", so not a variable
			LD      H,0			; Zero top byte of H
			CP      'Z'-'@'+1		; Check for static ("@", "A" to "Z"); if it is not static...
			JR      NC,LOC0         	; Then branch here
			ADD     A,A			
			LD      L,A			; HL: Static variable index * 2
			LD      A,(IY+1)        	; Check the 2nd character
			CP      '%'			; If not "%" then it is not static...
			JR      NZ,LOC1         	; Branch here
			LD      A,(IY+2)		; Check the 3rd character
			CP      '('			; If it is "(" (array) then it is not static...
			JR      Z,LOC1          	; Branch here
;
; At this point we're dealing with a static variable
;
			ADD     HL,HL			; HL: Static variable index * 4
			LD      DE,STAVAR       	; The static variable area in memory
			ADD     HL,DE			; HL: The address of the static variable
			INC     IY			; Skip the program pointer past the static variable name
			INC     IY
			LD      D,4             	; Set the type to be integer
			XOR     A			; Set the Z flag
			RET
;
; At this point it's potentially a dynamic variable, just need to do a few more checks
;
LOC0:			CP      '_'-'@'			; Check the first character is in
			RET     C			; the range "_" to 
			CP      'z'-'@'+1		; "z" (lowercase characters only)
			CCF				; If it is not in range then
			DEC     A               	; Set NZ flag and
			RET     C			; Exit here
			SUB     3			; This brings it in the range of 27 upwards (need to confirm)
			ADD     A,A			; Multiply by 2
			LD      L,A
;
LOC1:			LD      DE,DYNVAR       	; The dynamic variable storage
			DEC     L
			DEC     L
			SCF
			RET     M
			ADD     HL,DE			; HL: Address of first entry
;
LOC2:			LD      E,(HL)
			INC     HL
			LD      D,(HL)
			LD      A,D
			OR      E
			JR      Z,LOC6          ;UNDEFINED VARIABLE
			LD      H,D
			LD      L,E
			INC     HL              ;SKIP LINK
			INC     HL
			PUSH    IY
LOC3:			LD      A,(HL)          ;COMPARE
			INC     HL
			INC     IY
			CP      (IY)
			JR      Z,LOC3
			OR      A               ;0=TERMINATOR
			JR      Z,LOC5          ;FOUND (MAYBE)
LOC4:			POP     IY
			EX      DE,HL
			JP      LOC2            ;TRY NEXT ENTRY
;
LOC5:			DEC     IY
			LD      A,(IY)
			CP      '('
			JR      Z,LOC5A         ;FOUND
			INC     IY
			CALL    RANGE
			JR      C,LOC5A         ;FOUND
			CP      '('
			JR      Z,LOC4          ;KEEP LOOKING
			LD      A,(IY-1)
			CALL    RANGE1
			JR      NC,LOC4         ;KEEP LOOKING
LOC5A:			POP     DE
TYPE_:			LD      A,(IY-1)
			CP      '$'
			LD      D,129
			RET     Z               ;STRING
			CP      '%'
			LD      D,4
			RET     Z               ;INTEGER
			INC     D
			CP      A
			RET
;
LOC6:			INC     A               ;SET NZ
			RET
;
; CREATE - CREATE NEW ENTRY, INITIALISE TO ZERO.
;   Inputs: HL, IY as returned from LOCATE (NZ).
;  Outputs: As LOCATE, GETDEF.
; Destroys: As LOCATE, GETDEF.
;
CREATE:			XOR     A
			LD      DE,(FREE)
			LD      (HL),D
			DEC     HL
			LD      (HL),E
			EX      DE,HL
			LD      (HL),A
			INC     HL
			LD      (HL),A
			INC     HL
LOC7:			INC     IY
			CALL    RANGE           ;END OF VARIABLE?
			JR      C,LOC8
			LD      (HL),A
			INC     HL
			CALL    RANGE1
			JR      NC,LOC7
			CP      '('
			JR      Z,LOC8
			LD      A,(IY+1)
			CP      '('
			JR      Z,LOC7
			INC     IY
LOC8:			LD      (HL),0          ;TERMINATOR
			INC     HL
			PUSH    HL
			CALL    TYPE_
			LD      A,5
			CP      D
			JR      Z,LOC9
			DEC     A
LOC9:			LD      (HL),0          ;INITIALISE TO ZERO
			INC     HL
			DEC     A
			JR      NZ,LOC9
			LD      (FREE),HL
			CALL    CHECK
			POP     HL
			XOR     A
			RET
;
; LINNUM - GET LINE NUMBER FROM TEXT STRING
;   Inputs: IY = Text Pointer
;  Outputs: HL = Line number (zero if none)
;           IY updated
; Destroys: A,D,E,H,L,IY,F
;
; This bit of code performs a BASE 10 shift to build up the number
; So if the string passed is "345", the algorithm does this:
;
;    HL : Digit	: Operation
; ----- : ----- : ---------
; 00000 :	:
; 00003 :     3	: Multiply HL  (0) by 10   (0) and add 3   (3)
; 00034 :     4 : Multiply HL  (3) by 10  (30) and add 4  (34)
; 00345 :     5	: Multiply HL (34) by 10 (340) and add 5 (345)
;
; The multiply by 10 is done by an unrolled shift and add loop
;
LINNUM:			CALL    NXT			; Skip whitespace to the first character
			LD      HL,0			; The running total
LINNM1:			LD      A,(IY)			; A: Fetch the digit to add in
			SUB     '0'			; Sub ASCII '0' to make a binary number (0-9)
			RET     C			; And return if less than 0
			CP      10			; Or greater than or equal to 10
			RET     NC			; As we've hit a non-numeric character (end of number) at this point
			INC     IY			; Increment the string pointer
			LD      D,H			; This next block multiplys HL by 10, shifting the result left in BASE 10
			LD      E,L			; Store the original number in DE
			ADD     HL,HL           	; *2
			JR      C,TOOBIG		; At each point, error if > 65535 (carry flag set)
			ADD     HL,HL           	; *4
			JR      C,TOOBIG
			ADD     HL,DE           	; *5
			JR      C,TOOBIG	
			ADD     HL,HL           	; *10
			JR      C,TOOBIG
			LD      E,A			; A->DE: the digit to add in
			LD      D,0
			ADD     HL,DE           	; Add in the digit to the running total
			JR      NC,LINNM1       	; And if it is still <= 65535, loop
;
TOOBIG:			LD      A,20
			JP      ERROR_           	; Error: "Too big"
;
; PAIR - GET PAIR OF LINE NUMBERS FOR RENUMBER/AUTO.
;   Inputs: IY = text pointer
;  Outputs: HL = first number (10 by default)
;           BC = second number (10 by default)
; Destroys: A,B,C,D,E,H,L,B',C',D',E',H',L',IY,F
;
PAIR:			CALL    LINNUM          	; Parse the first line number
			LD      A,H			; If it is not zero, then...
			OR      L
			JR      NZ,PAIR1		; Skip...
			LD      L,10			; HL: the default value (10)
;
PAIR1:			CALL    TERMQ			; Check for ELSE, : or CR
			INC     IY			; Skip to next character
			PUSH    HL			; Stack the first line number
			LD      HL,10			; HL: the second default (10)
			CALL    NZ,LINNUM       	; Parse the second line number
			EX      (SP),HL			; HL: The first line number (off the stack)
			POP     BC			; BC: Second line number
			LD      A,B			; If the second line number is not zero then...
			OR      C			; We're good...
			RET     NZ			; Exit, otherwise...
			CALL    EXTERR			; Throw error: "Silly"
			DB    	'Silly', 0
;
; DLPAIR - GET PAIR OF LINE NUMBERS FOR DELETE/LIST.
;   Inputs: IY = text pointer
;  Outputs: HL = points to program text
;           BC = second number (0 by default)
; Destroys: A,B,C,D,E,H,L,IY,F
;
DLPAIR:			CALL    LINNUM			; Parse the first line number
			PUSH    HL			; Stack it
			CALL    TERMQ			; Check for ELSE, : or CR
			JR      Z,DLP1			; And exit if so 
			CP      TIF			; Is the token IF?
			JR      Z,DLP1			; Yes, so skip the next bit...
			INC     IY			; Otherwise...
			CALL    LINNUM			; Fetch the second line number
DLP1:			EX      (SP),HL			; HL: The first line number (off the stack)
			CALL    FINDL			; HL: Find the address of the line
			POP     BC			; BC: The second number
			RET
;
; TEST FOR VALID CHARACTER IN VARIABLE NAME:
;   Inputs: IY addresses character
;  Outputs: Carry set if out-of-range.
; Destroys: A,F
;
; It is called here to check the following
; In range: "$", "%" and "("
;   Plus all characters in RANGE1 and RANGE2
;
RANGE:			LD      A,(IY)			; Fetch the character
			CP      '$'			; Postfix for string variable is valid
			RET     Z
			CP      '%'			; Postfix for integer variable is valid
			RET     Z
			CP      '('			; Postfix for array is valid
			RET     Z
;
; It is called here to check the following
; In range: "0" to "9" and "@""
;   Plus all characters in RANGE2
;
RANGE1:			CP      '0'			; If it is between '0'...
			RET     C			 
			CP      '9'+1			; And '9'...
			CCF
			RET     NC			; Then it is valid
			CP      '@'             	; The prefix @ is valid (@% controls numeric print formatting - v2.4)
			RET     Z
;
; It is called here to check the following
; In range: "A" to "Z", "a' to "z", "_" and "`"
;	
RANGE2:			CP      'A'			; If it is between 'A'...
			RET     C
			CP      'Z'+1			; And 'Z'...
			CCF
			RET     NC			; Then it is valid
			CP      '_'			; If it is underscore, grave, or between 'a'
			RET     C
			CP      'z'+1			; And 'z'
			CCF				; Then it is valid
			RET
;
; Throw a 'LINE space' error (line too long)
; This is called from LEXAN
;
SPACE_: 		XOR     A
			CALL    EXTERR          	; "LINE space"
			DB    	LINE_, 8, 0
;
; LEXAN - LEXICAL ANALYSIS.
;  Bit 0,C: 1=left, 0=right
;  Bit 3,C: 1=in HEX
;  Bit 4,C: 1=accept line number
;  Bit 5,C: 1=in variable, FN, PROC
;  Bit 6,C: 1=in REM, DATA, *
;  Bit 7,C: 1=in quotes
;   Inputs: IY addresses source string
;           DE addresses destination string (must be page boundary)
;            C sets initial mode
;  Outputs: DE, IY updated
;            A holds carriage return
;
LEXAN1:			LD      (DE),A          	; Transfer to buffer
			INC     DE              	; Increment the pointers
			INC     IY			; And fall through to the main function
;
; This is the main entry point
;
LEXAN2:			LD      A,E             	; Destination buffer on page boundary, so E can be used as length
			CP      252             	; If it is >= 252 bytes, then...
			JR      NC,SPACE_        	; Throw a 'LINE space' error (line too long)
			LD      A,(IY)			; Fetch character from source string
			CP      CR			; If it is a CR
			RET     Z               	; Then it is end of line; we're done parsing
			CALL    RANGE1			; Is it alphanumeric, '@', '_' or '`'
			JR      NC,LEXAN3		; Yes, so skip
			RES     5,C             	; FLAG: NOT IN VARIABLE
			RES     3,C             	; FLAG: NOT IN HEX
;
LEXAN3:			CP      ' '			; Ignore spaces
			JR      Z,LEXAN1        	
			CP      ','			; Ignore commas
			JR      Z,LEXAN1 
			CP      'G'			; If less then 'G'
			JR      C,LEXAN4		; Yes, so skip
			RES     3,C             	; FLAG: NOT IN HEX
;
LEXAN4:			CP      34			; Is it a quote character?
			JR      NZ,LEXAN5		; No, so skip
			RL      C			; Toggle bit 7 of C by shifting it into carry flag
			CCF                     	; Toggle the carry
			RR      C			; And then shifting it back into bit 7 of C
;
LEXAN5:			BIT     4,C			; Accept line number?
			JR      Z,LEXAN6		; No, so skip
			RES     4,C			; FLAG: DON'T ACCEPT LINE NUMBER
			PUSH    BC			
			PUSH    DE
			CALL    LINNUM         		; Parse the line number to HL
			POP     DE
			POP     BC
			LD      A,H			; If it is not zero
			OR      L
			CALL    NZ,ENCODE       	; Then encode the line number HL to the destination (DE)
			JR      LEXAN2          	; And loop
;
LEXAN6:			DEC     C			; Check for C=1 (LEFT)
			JR      Z,LEXAN7        	; If so, skip
			INC     C			; Otherwise restore C
			JR      NZ,LEXAN1		; If C was 0 (RIGHT) then...
			OR      A			; Set the flags based on the character
			CALL    P,LEX           	; Tokenise if A < 128
			JR      LEXAN8			; And skip
;
; Processing the LEFT hand side here
; 
LEXAN7:			CP      '*'			; Is it a '*' (for star commands)
			JR      Z,LEXAN9		; Yes, so skip to quit tokenising
			OR      A			; Set the flags based on the character
			CALL    P,LEX           	; Tokenise if A < 128
;
; This bit of code checks if the tokens are one of the pseudo-variables PTR, PAGE, TIME, LOMEM, HIMEM
; These tokens are duplicate in the table with a GET version and a SET version offset by the define OFFSET (40h)
; Examples:
;   LET A% = PAGE : REM This is the GET version
;   PAGE = 40000  : REM This is the SET version
;
			CP      TOKLO			; TOKLO is 8Fh
			JR      C,LEXAN8		; If A is < 8Fh then skip to LEX8
			CP      TOKHI+1			; TOKHI is 93h
			JR      NC,LEXAN8		; If A is >= 94h then skip to LEX8
			ADD     A,OFFSET       		; Add OFFSET (40h) to make the token the SET version
;
LEXAN8:			CP      REM			; If the token is REM
			JR      Z,LEXAN9		; Then stop tokenising
			CP      DATA_			; If it is not DATA then
			JR      NZ,LEXANA		; Skip
LEXAN9:			SET     6,C             	; FLAG: STOP TOKENISING
;
LEXANA:			CP      FN			; If the token is FN
			JR      Z,LEXANB		
			CP      PROC			; Or the token is PROC
			JR      Z,LEXANB		; Then jump to here
			CALL    RANGE2			; Otherwise check the input is alphanumeric, "_" or "`"
			JR      C,LEXANC		; Jump here if out of range
;
LEXANB:			SET     5,C             	; FLAG: IN VARIABLE/FN/PROC
LEXANC:			CP      '&'			; Check for hex prefix
			JR      NZ,LEXAND		; If not, skip
			SET     3,C             	; FLAG: IN HEX
;
LEXAND:			LD      HL,LIST1		; List of tokens that must be followed by a line number	
			PUSH    BC			
			LD      BC,LIST1L		; The list length
			CPIR				; Check if the token is in this list
			POP     BC
			JR      NZ,LEXANE		; If not, then skip
			SET     4,C             	; FLAG: ACCEPT LINE NUMBER
;
LEXANE:			LD      HL,LIST2		; List of tokens that switch the lexical analysis back to LEFT mode
			PUSH    BC
			LD      BC,LIST2L		; The list length
			CPIR				; Check if the token is in this list
			POP     BC		
			JR      NZ,LEXANF		; If not, then skip
			SET     0,C             	; FLAG: ENTER LEFT MODE
LEXANF:			JP      LEXAN1			; And loop

;
; LIST1: List of tokens that must be followed by line numbers
; LIST2: List of tokens that switch the lexical analysis back to LEFT mode
;
LIST1:			DB	GOTO
			DB	GOSUB
			DB	RESTOR
			DB	TRACE
LIST2:			DB	THEN
			DB	ELSE_
LIST1L:			EQU     $-LIST1
			DB	REPEAT
			DB	TERROR
			DB    	':'
LIST2L:			EQU     $-LIST2
;
; ENCODE - ENCODE LINE NUMBER INTO PSEUDO-BINARY FORM.
;   Inputs: HL=line number, DE=string pointer
;  Outputs: DE updated, BIT 4,C set.
; Destroys: A,B,C,D,E,F
;
; Thanks to Matt Godblot for this explanation (https://xania.org/200711/bbc-basic-line-number-format)
;
; The line number is spread over three bytes and kept in the range of normal ASCII values so the interpreter
; can make this short cut in skipping to the non-ASCII token ELSE. The algorithm used splits the top two bits off
; each of the two bytes of the 16-bit line number. These bits are combined (in binary as 00LlHh00),
; exclusive-ORred with 0x54, and stored as the first byte of the 3-byte sequence. The remaining six bits of
; each byte are then stored, in LO/HI order, ORred with 0x40.
;
ENCODE:			SET     4,C			; Set bit 4 of C (for lexical analysis - accept line number)
			EX      DE, HL			; HL: string pointer, DE: line number
			LD      (HL), LINO		; Store 8Dh first to flag next bytes as an encoded line number
			INC     HL
			LD      A,D			; Get the high byte
			AND     0C0H			; Get the top two bits	DD000000
			RRCA				; Shift right		00DD0000
			RRCA
			LD      B,A			; Store in B
			LD      A,E			; Get the low byte
			AND     0C0H			; Get the top two bits	EE000000
			OR      B			; Combine with D	EEDD0000
			RRCA				; Shift right		00EEDD00
			RRCA
			XOR     01010100B		; XOR with 54h
			LD      (HL),A			; Store this as the second byte
			INC     HL
			LD      A,E			; Get the low byte
			AND     3FH			; Strip the top two bits off
			OR      '@'			; OR with 40h
			LD      (HL),A			; Store
			INC     HL		
			LD      A,D			; Get the high byte
			AND     3FH			; Strip the top two bits off
			OR      '@'			; OR with 40h
			LD      (HL),A			; Store
			INC     HL
			EX      DE,HL			; DE: string pointer, HL: line number	
			RET
;
; TEXT - OUTPUT MESSAGE.
;   Inputs: HL addresses text (terminated by nul)
;  Outputs: HL addresses character following nul.
; Destroys: A,H,L,F
;
REPORT:			LD      HL, (ERRTXT)		; Output an error message pointed to by ERRTXT
;
TEXT_:			LD      A, (HL)			; Fetch the character
			INC     HL			; Increment pointer to next character
			OR      A			; Check for the nul (0) string terminator
			RET     Z			; And return if so
			CALL    OUT_			; Output the character; note that OUT_ will detokenise tokens
			JR      TEXT_			; And loop
;
; TELL - OUTPUT MESSAGE.
;   Inputs: Text follows subroutine call (term=nul)
; Destroys: A,F
;
; Example usage:
;
;	CALL	TELL			Call the function
;	DB	"Hello World", 0	Followed by a zero terminated string
;	LD	A, (1234H)		Program execution will carry on here after the message is output
;
TELL:			EX      (SP), HL		; Get the return address off the stack into HL, this is the
			CALL    TEXT_			; first byte of the string that follows it. Print it, then
			EX      (SP), HL		; HL will point to the next instruction, swap this back onto the stack	
			RET				; at this point we'll return to the first instruction after the message