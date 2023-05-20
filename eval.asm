;
; Title:	BBC Basic Interpreter - Z80 version
;		Expression Evaluation & Arithmetic Module - "EVAL"
; Author:	(C) Copyright  R.T.Russell  1984
; Modified By:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	19/05/2022
;
; Modinfo:
; 07/05/1984:	Version 2.3
; 01/03/1987:	Modified to use external FPP
; 08/03/1987:	Version 3.0
; 30/09/1992:	INSTR bug fixed
; 03/05/2022:	Modified by Dean Belfield to assemble with ZDS
; 26/07/2022:	Fixed bug with INT caused when converting source to run on ZDS
; 19/08/2022:	INKEY1 is now XREFd
; 19/05/2023:	Added COUNT1 to XDEF and call to GETPORT for GET(x,y)

			.ASSUME	ADL = 0

			INCLUDE	"equs.inc"

			SEGMENT CODE
				
			XDEF	EXPR
			XDEF	EXPRN
			XDEF	EXPRI
			XDEF	EXPRS
			XDEF	ITEMI
			XDEF	LOADN
			XDEF	LOAD4
			XDEF	CONS
			XDEF	DLOAD5
			XDEF	LOADS
			XDEF	SFIX
			XDEF	VAL0
			XDEF	SEARCH
			XDEF	SWAP
			XDEF	TEST
			XDEF	DECODE
			XDEF	HEXSTR
			XDEF	STR
			XDEF	ZERO
			XDEF	PUSHS
			XDEF	POPS
			XDEF	COMMA
			XDEF	BRAKET
			XDEF	NXT
			XDEF	COUNT0
			XDEF	COUNT1
			XDEF	TRUE
				
			XREF	ADVAL
			XREF	FN
			XREF	POINT
			XREF	USR
			XREF	SYNTAX
			XREF	ERROR_
			XREF	CHECK
			XREF	GETVAR
			XREF	LISTON
			XREF	RANGE
			XREF	FPP
			XREF	GETCSR
			XREF	CHANEL
			XREF	OSSTAT
			XREF	OSBGET
			XREF	LOMEM
			XREF	HIMEM
			XREF	PAGE_
			XREF	TOP
			XREF	ERL
			XREF	ERR
			XREF	COUNT
			XREF	OSOPEN
			XREF	GETEXT
			XREF	GETPTR
			XREF	GETIME
			XREF	GETIMS
			XREF	LEXAN2
			XREF	RANDOM
			XREF	STORE5
			XREF	GETSCHR
			XREF	OSRDCH
			XREF	OSKEY
			XREF	INKEY1
			XREF	GETPORT
;
; BINARY FLOATING POINT REPRESENTATION:
;    32 BIT SIGN-MAGNITUDE NORMALIZED MANTISSA
;     8 BIT EXCESS-128 SIGNED EXPONENT
;    SIGN BIT REPLACES MANTISSA MSB (IMPLIED "1")
;    MANTISSA=0 & EXPONENT=0 IMPLIES VALUE IS ZERO.
;
; BINARY INTEGER REPRESENTATION:
;    32 BIT 2'S-COMPLEMENT SIGNED INTEGER
;     "EXPONENT" BYTE = 0 (WHEN PRESENT)
;
; NORMAL REGISTER ALLOCATION: MANTISSA - HLH'L'
;                             EXPONENT - C
;

;
; Table of addresses for functions
;
FUNTOK:			EQU	8DH			; First token number
;
FUNTBL:			DW	DECODE			; Line number
			DW	OPENIN			; OPENIN
			DW	PTR			; PTR
			DW	PAGEV			; PAGE
			DW	TIMEV			; TIME
			DW	LOMEMV			; LOMEM
			DW	HIMEMV			; HIMEM
			DW	ABSV			; ABS
			DW	ACS			; ACS
			DW	ADVAL			; ADVAL
			DW	ASC			; ASC
			DW	ASN			; ASN
			DW	ATN			; ATN
			DW	BGET			; BGET
			DW	COS			; COS
			DW	COUNTV			; COUNT
			DW	DEG			; DEG
			DW	ERLV			; ERL
			DW	ERRV			; ERR
			DW	EVAL_			; EVAL
			DW	EXP			; EXP
			DW	EXT			; EXT
			DW	ZERO			; FALSE
			DW	FN			; FN
			DW	GET			; GET
			DW	INKEY			; INKEY
			DW	INSTR			; INSTR(
			DW	INT_			; INT
			DW	LEN			; LEN
			DW	LN			; LN
			DW	LOG			; LOG
			DW	NOTK			; NOT
			DW	OPENUP			; OPENUP
			DW	OPENOT			; OPENOUT
			DW	PI			; PI
			DW	POINT			; POINT(
			DW	POS			; POS
			DW	RAD			; RAD
			DW	RND			; RND
			DW	SGN			; SGN
			DW	SIN			; SIN
			DW	SQR			; SQR
			DW	TAN			; TAN
			DW	TOPV			; TO(P)
			DW	TRUE			; TRUE
			DW	USR			; USR
			DW	VAL			; VAL
			DW	VPOS			; VPOS
			DW	CHRS			; CHRS
			DW	GETS			; GETS
			DW	INKEYS			; INKEYS
			DW	LEFTS			; LEFTS(
			DW	MIDS			; MIDS(
			DW	RIGHTS			; RIGHTS(
			DW	STRS			; STR$
			DW	STRING_			; STRINGS(
			DW	EOF			; EOF
;
FUNTBL_END:		EQU	$
TCMD:			EQU     FUNTOK+(FUNTBL_END-FUNTBL)/2
;
ANDK:			EQU     80H
DIVK:			EQU     81H
EORK:			EQU     82H
MODK:			EQU     83H
ORK:			EQU     84H
;
SOPTBL:			DW	SLE			; <= (STRING)
			DW	SNE			; <>
			DW	SGE			; >=
			DW	SLT			; <
			DW	SEQ			; =
			DW	SGT			; >
;
; EXPR - VARIABLE-TYPE EXPRESSION EVALUATION
;     Expression type is returned in A'F':
;        Numeric - A' bit 7=0, F' sign bit cleared.
;         String - A' bit 7=1, F' sign bit set.
; Floating-point or integer result returned in HLH'L'C
; Integer result denoted by C=0 and HLH'L' non-zero.
; String result returned in string accumulator, DE set.
;
; Hierarchy is: (1) Variables, functions, constants, bracketed expressions.
;               (2) ^
;               (3) * / MOD DIV
;               (4) + -
;               (5) = <> <= >= > <
;               (6) AND
;               (7) EOR OR

;
; Level 7: EOR and OR
;
EXPR:			CALL    EXPR1			; Get first operator by calling Level 6
EXPR0A:			CP      EORK            	; Is operator EOR?
			JR      Z,EXPR0B		; Yes, so skip to next bit
			CP      ORK			; Is operator OR
			RET     NZ			; No, so return
;
EXPR0B:			CALL    SAVE            	; Save first operand
			CALL    EXPR1           	; Get second operand
			CALL    DOIT            	; Do the operation
			JR      EXPR0A          	; And continue
;
; Level 6: AND
;
EXPR1:			CALL    EXPR2			; Get first operator by calling Level 5
EXPR1A:			CP      ANDK			; Is operator AND?
			RET     NZ			; No, so return
			CALL    SAVE			; Save first operand
			CALL    EXPR2			; Get second operand
			CALL    DOIT			; Do the operation
			JR      EXPR1A			; And continue
;
; Level 5: Comparisons
;
EXPR2:			CALL    EXPR3			; Get first operator by calling Level 4
			CALL    RELOP?			; Is it ">", "=" or "<"?
			RET     NZ			; No, so return
			LD      B,A			; Store the first operator in B
			INC     IY              	; Bump over operator
			CALL    NXT			; 
			CALL    RELOP?          	; Is it a compound operator?
			JR      NZ,EXPR2B		; No, so skip next bit
			INC     IY			; Bump over operator
			CP      B			; Compare with first
			JP      Z,SYNTAX        	; Trap illegal combinations ">>", "==", "<<" (but not "><", "=>", "=<")
			ADD     A,B			
			LD      B,A			; B: Unique code for the compound operator
EXPR2B:			LD      A,B			; A: Code for the operator/compound operator
			EX      AF,AF'
			JP      M,EXPR2S		; If it is a string, then branch here to handle it
			EX      AF,AF'
			SUB     4
			CP      '>'-4
			JR      NZ,EXPR2C
			ADD     A,2
EXPR2C:			CALL    SAVE1
			CALL    EXPR3
			CALL    DOIT            	; NB: Must NOT be "JP DOIT"
			RET
;
EXPR2S:			EX      AF,AF'			; Handle string comparisons
			DEC     A
			AND     7
			CALL    PUSHS           	; Save string on the stack
			PUSH    AF              	; Save the operator
			CALL    EXPR3           	; Get the second string
			EX      AF,AF'
			JP      P,TYPE_
			POP     AF
			LD      C,E             	; Length of string #2
			POP     DE
			LD      HL,0
			ADD     HL,SP
			LD      B,E             	; Length of string #1
			PUSH    DE
			LD      DE,ACCS
			EX      DE,HL
			CALL    DISPT2
			POP     DE
			EX      DE,HL
			LD      H,0
			ADD     HL,SP
			LD      SP,HL
			EX      DE,HL
			XOR     A               	; Numeric marker
			LD      C,A             	; Integer marker
			EX      AF,AF'
			LD      A,(IY)
			RET
;
; Level 4: + and -
;
EXPR3:			CALL    EXPR4			; Get first operator by calling Level 3
EXPR3A:			CP      '-'			; Is it "-"?
			JR      Z,EXPR3B		; Yes, so skip the next bit
			CP      '+'			; Is it "+"?
			RET     NZ			; No, so return
			EX      AF,AF'			; Get the type
			JP      M,EXPR3S		; Branch here if string
			EX      AF,AF'
EXPR3B:			CALL    SAVE			; Save the first operator
			CALL    EXPR4			; Fetch the second operator
			CALL    DOIT			; Do the operation
			JR      EXPR3A			; And continue
;
EXPR3S:			EX      AF,AF'			; Handle string concatenation
			INC     IY              	; Bump past the "+"
			CALL    PUSHS           	; Save the string on the stack
			CALL    EXPR4           	; Fetch the second operator
			EX      AF,AF'
			JP      P,TYPE_			; If it is not a string, then Error: "Type mismatch"
			LD      C,E             	; C: String length
			POP     DE
			PUSH    DE
			LD      HL,ACCS
			LD      D,H
			LD      A,C
			OR      A
			JR      Z,EXP3S3
			LD      B,L
			LD      L,A             	; Source
			ADD     A,E
			LD      E,A             	; Destination
			LD      A,19
			JP      C,ERROR_         	; A carry indicates string > 255 bytes, so Error: "String too long"
			PUSH    DE
			DEC     E
			DEC     L
			LDDR                    	; Copy
			POP     DE
EXP3S3:			EXX
			POP     BC
			CALL    POPS            	; Restore from stack
			EXX
			OR      80H             	; Flag as a string
			EX      AF,AF'
			LD      A,(IY)			; Fetch the next character
			JR      EXPR3A			; And continue
;
; Level 3: * / MOD DIV
;
EXPR4:			CALL    EXPR5			; Get first operator by calling Level 2
EXPR4A:			CP      '*'			; "*" is valid
			JR      Z,EXPR4B
			CP      '/'			; "/" is valid
			JR      Z,EXPR4B
			CP      MODK			; MOD token is valid
			JR      Z,EXPR4B
			CP      DIVK			; DIV token is valid
			RET     NZ			; And return if it is anything else
EXPR4B:			CALL    SAVE
			CALL    EXPR5
			CALL    DOIT
			JR      EXPR4A
;
; Level 2: ^
;
EXPR5:			CALL    ITEM			; Get variable
			OR      A               	; Test type
			EX      AF,AF'          	; Save type 
EXPR5A:			CALL    NXT			; Skip spaces
			CP      '^'			; Is the operator "^"?
			RET     NZ			; No, so return
			CALL    SAVE			; Save first operand
			CALL    ITEM			; Get second operand
			OR      A			; Test type
			EX      AF,AF'			; Save type
			CALL    DOIT			; Do the operation
			JR      EXPR5A			; And continue
;
; Evaluate a numeric expression
;
EXPRN:			CALL    EXPR			; Evaluate expression
			EX      AF,AF'			; Get the type
			RET     P			; And return if it is a number
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Evaluate a fixed-point expression 
;
EXPRI:			CALL    EXPR			; Evaluate the expression
			EX      AF,AF'			; Get the type
			JP      P,SFIX			; If it is numeric, then convert to fixed-point notation
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;	
; Evaluate a string expression
;	
EXPRS:			CALL    EXPR			; Evaluate the expression
			EX      AF,AF'			; Get the type
			RET     M			; And return if it is a string
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Get a numeric variable
;
ITEMN:			CALL    ITEM			; Get the variable
			OR      A			; Test the type
			RET     P			; And return if it is a number
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Get a fixed-point variable 
;
ITEMI:			CALL    ITEM			; Get the variable
			OR      A			; Test the type
			JP      P,SFIX			; If it is numeric, then convert to fixed-point notation
			JR      TYPE_			; Otherwise Error: "Type mismatch"
;
; Get a string variable 
;
ITEMS:			CALL    ITEM			; Get the variable
			OR      A			; Test the type
			RET     M			; If it is a string, then return
;							; Otherwise
TYPE_:			LD      A,6			; Error: "Type mismatch"
			JP      ERROR_           	
;
; Evaluate a bracketed expression
;
ITEM1:			CALL    EXPR            	; Evaluate the expression
			CALL    BRAKET			; Check for closing bracket
			EX      AF,AF'
			RET
;
; HEX - Get hexadecimal constant.
;   Inputs: ASCII string at (IY)
;  Outputs: Integer result in H'L'HL, C=0, A7=0.
;           IY updated (points to delimiter)
;
HEX:			CALL    ZERO			; Set result to 0
			CALL    HEXDIG			; Fetch the character from IY
			JR      C,BADHEX		; If invalid HEX character, then Error: "Bad HEX"
HEX1:			INC     IY			; Move pointer to next character
			AND     0FH			; Clear the top nibble
			LD      B,4			; Loop counter
;
HEX2:			EXX				; Shift the result left B (4) times. This makes
			ADD     HL,HL			; space for the incoming nibble in the least significant 4 bits
			EXX				; .
			ADC     HL,HL			; .
			DJNZ    HEX2			; And loop
			EXX
			OR      L			; OR in the digit
			LD      L,A
			EXX
;
			CALL    HEXDIG			; Fetch the next character
			JR      NC,HEX1			; If it is a HEX digit then loop
			XOR     A			; Clear A
			RET
;
BADHEX:			LD      A,28
			JP      ERROR_          	; Error: "Bad HEX"
;
; MINUS - Unary minus.
;   Inputs: IY = text pointer
;  Outputs: Numeric result, same type as argument.
;           Result in H'L'HLC
;
MINUS:			CALL    ITEMN			; Get the numeric argument
MINUS0:			DEC     C			; Check exponent (C)
			INC     C			; If it is zero, then it's either a FP zero or an integer
			JR      Z,NEGATE        	; So do an integer negation
;
			LD      A,H			; Do a FP negation by 
			XOR     80H             	; Toggling the sign bit (H)
			LD      H,A
			XOR     A               	; Numeric marker
			RET
;
NEGATE:			EXX				; This section does a two's complement negation on H'L'HLC
			LD      A,H			; First do a one's complement by negating all the bytes
			CPL
			LD      H,A
			LD      A,L
			CPL
			LD      L,A
			EXX
			LD      A,H
			CPL
			LD      H,A
			LD      A,L
			CPL
			LD      L,A
ADD1:			EXX				; Then add 1
			INC     HL			
			LD      A,H
			OR      L
			EXX
			LD      A,0             	; Numeric marker
			RET     NZ
			INC     HL
			RET
;
; ITEM - VARIABLE TYPE NUMERIC OR STRING ITEM.
; Item type is returned in A:  Bit 7=0 numeric.
;                              Bit 7=1 string.
; Numeric item returned in HLH'L'C.
; String item returned in string accumulator,
;   DE addresses byte after last (E=length).
;
ITEM:			CALL    CHECK			; Check there's at least a page of free memory left and Error: "No room" if not
			CALL    NXT			; Skip spaces
			INC     IY			; Move to the prefix character
			CP      '&'			; If `&`
			JR      Z,HEX           	; Then get a HEX constant
			CP      '-'			; If `-`
			JR      Z,MINUS         	; Then get a negative number
			CP      '+'			; If `+`
			JR      Z,ITEMN         	; Then just fetch the number (unary plus)
			CP      '('			; If `(`
			JR      Z,ITEM1         	; Start of a bracketed expression
			CP      34			; If `"`
			JR      Z,CONS          	; Start of a string constant
			CP      TCMD			; Is it out of range of the function table?
			JP      NC,SYNTAX       	; Error: "Syntax Error"
			CP      FUNTOK			; If it is in range, then 
			JP      NC,DISPAT       	; It's a function
			DEC     IY			
			CP      ':'
			JR      NC,ITEM2        ;VARIABLE?
			CP      '0'
			JR      NC,CON          ;NUMERIC CONSTANT
			CP      '.'
			JR      Z,CON           ;NUMERIC CONSTANT
ITEM2:			CALL    GETVAR          ;VARIABLE
			JR      NZ,NOSUCH
			OR      A
			JP      M,LOADS         ;STRING VARIABLE
LOADN:			OR      A
			JR      Z,LOAD1         ;BYTE VARIABLE
			LD      C,0
			BIT     0,A
			JR      Z,LOAD4         ;INTEGER VARIABLE
LOAD5:			LD      C,(IX+4)
LOAD4:			EXX
			LD      L,(IX+0)
			LD      H,(IX+1)
			EXX
			LD      L,(IX+2)
			LD      H,(IX+3)
			RET
;
LOAD1:			LD      HL,0
			EXX
			LD      H,0
			LD      L,(IX+0)
			EXX
			LD      C,H
			RET
;
NOSUCH:			JP      C,SYNTAX
			LD      A,(LISTON)
			BIT     5,A
			LD      A,26
			JR      NZ,ERROR0       ;"No such variable"
NOS1:			INC     IY
			CALL    RANGE
			JR      NC,NOS1
			LD      IX,PC
			XOR     A
			LD      C,A
			JR      LOAD4
;
;CONS - Get string constant from ASCII string.
;   Inputs: ASCII string at (IY)
;  Outputs: Result in string accumulator.
;           D = MS byte of ACCS, E = string length
;           A7 = 1 (string marker)
;           IY updated
;
CONS:			LD      DE,ACCS
CONS3:			LD      A,(IY)
			INC     IY
			CP      34		;ASCII ""
			JR      Z,CONS2
CONS1:			LD      (DE),A
			INC     E
			CP      CR
			JR      NZ,CONS3
			LD      A,9
ERROR0:			JP      ERROR_           ;"Missing """
;
CONS2:			LD      A,(IY)
			CP      34H		;ASCII "
			INC     IY
			JR      Z,CONS1
			DEC     IY
			LD      A,80H           ;STRING MARKER
			RET
;
;CON - Get unsigned numeric constant from ASCII string.
;   Inputs: ASCII string at (IY).
;  Outputs: Variable-type result in HLH'L'C
;           IY updated (points to delimiter)
;           A7 = 0 (numeric marker)
;
CON:			PUSH    IY
			POP     IX
			LD      A,36
			CALL    FPP
			JR      C,ERROR0
			PUSH    IX
			POP     IY
			XOR     A
			RET
;
DLOAD5:		LD      B,(IX+4)
			EXX
			LD      E,(IX+0)
			LD      D,(IX+1)
			EXX
			LD      E,(IX+2)
			LD      D,(IX+3)
			RET
;
LOADS:			LD      DE,ACCS
			RRA
			JR      NC,LOADS2       ;FIXED STRING
			CALL    LOAD4
			EXX
			LD      A,L
			EXX
			OR      A
			LD      C,A
			LD      A,80H           ;STRING MARKER
			RET     Z
			LD      B,0
			LDIR
			RET
LOADS2:			LD      A,(HL)
			LD      (DE),A
			INC     HL
			CP      CR
			LD      A,80H           ;STRING MARKER
			RET     Z
			INC     E
			JR      NZ,LOADS2
			RET                     ;RETURN NULL STRING
;
;VARIABLE-TYPE FUNCTIONS:
;
;Result returned in HLH'L'C (floating point)
;Result returned in HLH'L' (C=0) (integer)
;Result returned in string accumulator & DE (string)
;All registers destroyed.
;IY (text pointer) updated.
;Bit 7 of A indicates type: 0 = numeric, 1 = string.
;
;
;POS - horizontal cursor position.
;VPOS - vertical cursor position.
;EOF - return status of file.
;BGET - read byte from file.
;INKEY - as GET but wait only n centiseconds.
;GET - wait for keypress and return ASCII value.
;GET(n) - input from Z80 port n.
;ASC - ASCII value of string.
;LEN - length of string.
;LOMEM - location of dynamic variables.
;HIMEM - top of available RAM.
;PAGE - start of current text page.
;TOP - address of first free byte after program.
;ERL - line number where last error occurred.
;ERR - number of last error.
;COUNT - number of printing characters since CR.
;Results are integer numeric.
;
POS:			CALL    GETCSR
			EX      DE,HL
			JR      COUNT1
VPOS:			CALL    GETCSR
			JR      COUNT1
EOF:			CALL    CHANEL
			CALL    OSSTAT
			JP      Z,TRUE
			JP      ZERO
BGET:			CALL    CHANEL          ;CHANNEL NUMBER
			CALL    OSBGET
			LD      L,A
			JR      COUNT0
INKEY:			CALL    INKEYS
			JR      ASC0
GET:			CALL    NXT
			CP      '('
;			JR      NZ,GET0
;			CALL    ITEMI           ;PORT ADDRESS
;			EXX
;			LD      B,H
;			LD      C,L
;			IN      L,(C)           ;INPUT FROM PORT BC
;			JR      COUNT0
			JP	Z, GETPORT	;NEW CODE IN PATCH.Z80
GET0:			CALL    GETS
			JR      ASC1
ASC:			CALL    ITEMS
ASC0:			XOR     A
			CP      E
			JP      Z,TRUE          ;NULL STRING
ASC1:			LD      HL,(ACCS)
			JR      COUNT0
LEN:			CALL    ITEMS
			EX      DE,HL
			JR      COUNT0
LOMEMV:			LD      HL,(LOMEM)
			JR      COUNT1
HIMEMV:			LD      HL,(HIMEM)
			JR      COUNT1
PAGEV:			LD      HL,(PAGE_)
			JR      COUNT1
TOPV:			LD      A,(IY)
			INC     IY              ;SKIP "P"
			CP      'P'
			JP      NZ,SYNTAX       ;"Syntax Error"
			LD      HL,(TOP)
			JR      COUNT1
ERLV:			LD      HL,(ERL)
			JR      COUNT1
ERRV:			LD      HL,(ERR)
			JR      COUNT0
COUNTV:			LD      HL,(COUNT)
COUNT0:			LD      H,0
COUNT1:			EXX
			XOR     A
			LD      C,A             ;INTEGER MARKER
			LD      H,A
			LD      L,A
			RET
;
;OPENIN - Open a file for reading.
;OPENOUT - Open a file for writing.
;OPENUP - Open a file for reading or writing.
;Result is integer channel number (0 if error)
;
OPENOT:			XOR     A
			DB    21H             ;SKIP NEXT 2 BYTES
OPENUP:			LD      A,2
			DB    21H             ;SKIP NEXT 2 BYTES
OPENIN:			LD      A,1
			PUSH    AF              ;SAVE OPEN TYPE
			CALL    ITEMS           ;FILENAME
			LD      A,CR
			LD      (DE),A
			POP     AF              ;RESTORE OPEN TYPE
			ADD     A,-1            ;AFFECT FLAGS
			LD      HL,ACCS
			CALL    OSOPEN
			LD      L,A
			JR      COUNT0
;
;EXT - Return length of file.
;PTR - Return current file pointer.
;Results are integer numeric.
;
EXT:			CALL    CHANEL
			CALL    GETEXT
			JR      TIME0
;
PTR:			CALL    CHANEL
			CALL    GETPTR
			JR      TIME0
;
;TIME - Return current value of elapsed time.
;Result is integer numeric.
;
TIMEV:			LD      A,(IY)
			CP      '$'
			JR      Z,TIMEVS
			CALL    GETIME
TIME0:			PUSH    DE
			EXX
			POP     HL
			XOR     A
			LD      C,A
			RET
;
;TIME$ - Return date/time string.
;Result is string
;
TIMEVS:			INC     IY              ;SKIP $
			CALL    GETIMS
			LD      A,80H           ;MARK STRING
			RET
;
;String comparison:
;
SLT:			CALL    SCP
			RET     NC
			JR      TRUE
;
SGT:			CALL    SCP
			RET     Z
			RET     C
			JR      TRUE
;
SGE:			CALL    SCP
			RET     C
			JR      TRUE
;
SLE:			CALL    SCP
			JR      Z,TRUE
			RET     NC
			JR      TRUE
;
SNE:			CALL    SCP
			RET     Z
			JR      TRUE
;
SEQ:			CALL    SCP
			RET     NZ
TRUE:			LD      A,-1
			EXX
			LD      H,A
			LD      L,A
			EXX
			LD      H,A
			LD      L,A
			INC     A
			LD      C,A
			RET
;
;PI - Return PI (3.141592654)
;Result is floating-point numeric.
;
PI:			LD      A,35
			JR      FPP1
;
;ABS - Absolute value
;Result is numeric, variable type.
;
ABSV:			LD      A,16
			JR      FPPN
;
;NOT - Complement integer.
;Result is integer numeric.
;
NOTK:			LD      A,26
			JR      FPPN
;
;DEG - Convert radians to degrees
;Result is floating-point numeric.
;
DEG:			LD      A,21
			JR      FPPN
;
;RAD - Convert degrees to radians
;Result is floating-point numeric.
;
RAD:			LD      A,27
			JR      FPPN
;
;SGN - Return -1, 0 or +1
;Result is integer numeric.
;
SGN:			LD      A,28
			JR      FPPN
;
;INT - Floor function
;Result is integer numeric.
;
INT_:			LD      A,23
			JR      FPPN
;
;SQR - square root
;Result is floating-point numeric.
;
SQR:			LD      A,30
			JR      FPPN
;
;TAN - Tangent function
;Result is floating-point numeric.
;
TAN:			LD      A,31
			JR      FPPN
;
;COS - Cosine function
;Result is floating-point numeric.
;
COS:			LD      A,20
			JR      FPPN
;
;SIN - Sine function
;Result is floating-point numeric.
;
SIN:			LD      A,29
			JR      FPPN
;
;EXP - Exponential function
;Result is floating-point numeric.
;
EXP:			LD      A,22
			JR      FPPN
;
;LN - Natural log.
;Result is floating-point numeric.
;
LN:			LD      A,24
			JR      FPPN
;
;LOG - base-10 logarithm.
;Result is floating-point numeric.
;
LOG:			LD      A,25
			JR      FPPN
;
;ASN - Arc-sine
;Result is floating-point numeric.
;
ASN:			LD      A,18
			JR      FPPN
;
;ATN - arc-tangent
;Result is floating-point numeric.
;
ATN:			LD      A,19
			JR      FPPN
;
;ACS - arc-cosine
;Result is floating point numeric.
;
ACS:			LD      A,17
FPPN:			PUSH    AF
			CALL    ITEMN
			POP     AF
FPP1:			CALL    FPP
			JP      C,ERROR_
			XOR     A
			RET
;
;SFIX - Convert to fixed-point notation
;
SFIX:			LD      A,38
			JR      FPP1
;
;SFLOAT - Convert to floating-point notation
;
SFLOAT:			LD      A,39
			JR      FPP1
;
;VAL - Return numeric value of string.
;Result is variable type numeric.
;
VAL:			CALL    ITEMS
VAL0:			XOR     A
			LD      (DE),A
			LD      IX,ACCS
			LD      A,36
			JR      FPP1
;
;EVAL - Pass string to expression evaluator.
;Result is variable type (numeric or string).
;
EVAL_:			CALL    ITEMS
			LD      A,CR
			LD      (DE),A
			PUSH    IY
			LD      DE,ACCS
			LD      IY,ACCS
			LD      C,0
			CALL    LEXAN2          ;TOKENISE
			LD      (DE),A
			INC     DE
			XOR     A
			CALL    PUSHS           ;PUT ON STACK
			LD      IY,2
			ADD     IY,SP
			CALL    EXPR
			POP     IY
			ADD     IY,SP
			LD      SP,IY           ;ADJUST STACK POINTER
			POP     IY
			EX      AF,AF'
			RET
;
;RND - Random number function.
; RND gives random integer 0-&FFFFFFFF
; RND(-n) seeds random number & returns -n.
; RND(0) returns last value in RND(1) form.
; RND(1) returns floating-point 0-0.99999999.
; RND(n) returns random integer 1-n.
;
RND:			LD      IX,RANDOM
			CALL    NXT
			CP      '('
			JR      Z,RND5          ;ARGUMENT FOLLOWS
			CALL    LOAD5
RND1:			RR      C
			LD      B,32
RND2:			EXX                     ;CALCULATE NEXT
			ADC     HL,HL
			EXX
			ADC     HL,HL
			BIT     3,L
			JR      Z,RND3
			CCF
RND3:			DJNZ    RND2
RND4:			RL      C               ;SAVE CARRY
			CALL    STORE5          ;STORE NEW NUMBER
			XOR     A
			LD      C,A
			RET
RND5:			CALL    ITEMI
			LD      IX,RANDOM
			BIT     7,H             ;NEGATIVE?
			SCF
			JR      NZ,RND4         ;SEED
			CALL    TEST
			PUSH    AF
			CALL    SWAP
			EXX
			CALL    LOAD5
			CALL    NZ,RND1         ;NEXT IF NON-ZERO
			EXX                     ;SCRAMBLE (CARE!)
			LD      C,7FH
RND6:			BIT     7,H             ;FLOAT
			JR      NZ,RND7
			EXX
			ADD     HL,HL
			EXX
			ADC     HL,HL
			DEC     C
			JR      NZ,RND6
RND7:			RES     7,H             ;POSITIVE 0-0.999999
			POP     AF
			RET     Z               ;ZERO ARGUMENT
			EXX
			LD      A,E
			DEC     A
			OR      D
			EXX
			OR      E
			OR      D
			RET     Z               ;ARGUMENT=1
			LD      B,0             ;INTEGER MARKER
			LD      A,10
			CALL    FPP             ;MULTIPLY
			JP      C,ERROR_
			CALL    SFIX
			JP      ADD1
;
;INSTR - String search.
;Result is integer numeric.
;
INSTR:			CALL    EXPRSC          ;STRING TO SEARCH
			CALL    PUSHS           ;SAVE STRING ON STACK
			CALL    EXPRS           ;SUB-STRING
			POP     BC
			LD      HL,0
			ADD     HL,SP           ;HL ADDRESSES MAIN
			PUSH    BC              ;C = MAIN STRING LENGTH
			LD      B,E             ;B = SUB-STRING LENGTH
			CALL    NXT
			CP      ','
			LD      A,0
			JR      NZ,INSTR1
			INC     IY              ;SKIP COMMA
			PUSH    BC              ;SAVE LENGTHS
			PUSH    HL              ;SAVE MAIN ADDRESS
			CALL    PUSHS
			CALL    EXPRI
			POP     BC
			CALL    POPS
			POP     HL              ;RESTORE MAIN ADDRESS
			POP     BC              ;RESTORE LENGTHS
			EXX
			LD      A,L
			EXX
			OR      A
			JR      Z,INSTR1
			DEC     A
INSTR1:			LD      DE,ACCS         ;DE ADDRESSES SUB
			CALL    SEARCH
			POP     DE
			JR      Z,INSTR2        ;N.B. CARRY CLEARED
			SBC     HL,HL
			ADD     HL,SP
INSTR2:			SBC     HL,SP
			EX      DE,HL
			LD      H,0
			ADD     HL,SP
			LD      SP,HL
			EX      DE,HL
			CALL    BRAKET
			JP      COUNT1
;
;SEARCH - Search string for sub-string
;   Inputs: Main string at HL length C
;           Sub-string  at DE length B
;           Starting offset A
;  Outputs: NZ - not found
;           Z - found at location HL-1
;           Carry always cleared
;
SEARCH:		PUSH    BC
			LD      B,0
			LD      C,A
			ADD     HL,BC           ;NEW START ADDRESS
			POP     BC
			SUB     C
			JR      NC,SRCH4
			NEG
			LD      C,A             ;REMAINING LENGTH
SRCH1:			LD      A,(DE)
			PUSH    BC
			LD      B,0
			CPIR                    ;FIND FIRST CHARACTER
			LD      A,C
			POP     BC
			JR      NZ,SRCH4
			LD      C,A
			DEC     B               ;Bug fix
			CP      B               ;Bug fix
			INC     B               ;Bug fix
			JR      C,SRCH4         ;Bug fix
			PUSH    BC
			PUSH    DE
			PUSH    HL
			DEC     B
			JR      Z,SRCH3         ;FOUND !
SRCH2:			INC     DE
			LD      A,(DE)
			CP      (HL)
			JR      NZ,SRCH3
			INC     HL
			DJNZ    SRCH2
SRCH3:			POP     HL
			POP     DE
			POP     BC
			JR      NZ,SRCH1
			XOR     A               ;Z, NC
			RET                     ;FOUND
;
SRCH4:			OR      0FFH            ;NZ, NC
			RET                     ;NOT FOUND
;
;CHRS - Return character with given ASCII value.
;Result is string.
;
CHRS:			CALL    ITEMI
			EXX
			LD      A,L
			JR      GET1
;
;GETS - Return key pressed as stringor character at position (X,Y).
;Result is string.
;
GETS:			CALL	NXT		;NEW CODE FOR GET$(X,Y)
			CP	'('
			JP	Z, GETSCHR	;CALL FUNCTION IN PATCH.Z80
			CALL    OSRDCH
GET1:			SCF
			JR      INKEY1
;
;INKEYS - Wait up to n centiseconds for keypress.
;         Return key pressed as string or null
;         string if time elapsed.
;Result is string.
;
INKEYS:			CALL    ITEMI
			EXX
			CALL    OSKEY
INKEY1:			LD      DE,ACCS
			LD      (DE),A
			LD      A,80H
			RET     NC
			INC     E
			RET
;
;MID$ - Return sub-string.
;Result is string.
;
MIDS:			CALL    EXPRSC
			CALL    PUSHS           ;SAVE STRING ON STACK
			CALL    EXPRI
			POP     BC
			CALL    POPS
			EXX
			LD      A,L
			EXX
			OR      A
			JR      Z,MIDS1
			DEC     A
			LD      L,A
			SUB     E
			LD      E,0
			JR      NC,MIDS1
			NEG
			LD      C,A
			CALL    RIGHT1
MIDS1:			CALL    NXT
			CP      ','
			INC     IY
			JR      Z,LEFT1
			DEC     IY
			CALL    BRAKET
			LD      A,80H
			RET
;
;LEFT$ - Return left part of string.
;Carry cleared if entire string returned.
;Result is string.
;
LEFTS:			CALL    EXPRSC
LEFT1:			CALL    PUSHS           ;SAVE STRING ON STACK
			CALL    EXPRI
			POP     BC
			CALL    POPS
			CALL    BRAKET
			EXX
			LD      A,L
			EXX
			CP      E
			JR      NC,LEFT3
			LD      L,E             ;FOR RIGHTS
LEFT2:			LD      E,A
LEFT3:			LD      A,80H           ;STRING MARKER
			RET
;
;RIGHT$ - Return right part of string.
;Result is string.
;
RIGHTS:			CALL    LEFTS
			RET     NC
			INC     E
			DEC     E
			RET     Z
			LD      C,E
			LD      A,L
			SUB     E
			LD      L,A
RIGHT1:			LD      B,0
			LD      H,D
			LD      E,B
			LDIR                    ;MOVE
			LD      A,80H
			RET
;
;STRINGS - Return n concatenations of a string.
;Result is string.
;
STRING_:			CALL    EXPRI
			CALL    COMMA
			EXX
			LD      A,L
			EXX
			PUSH    AF
			CALL    EXPRS
			CALL    BRAKET
			POP     AF
			OR      A
			JR      Z,LEFT2         ;N=0
			DEC     A
			LD      C,A
			LD      A,80H           ;STRING MARKER
			RET     Z
			INC     E
			DEC     E
			RET     Z               ;NULL STRING
			LD      B,E
			LD      H,D
			LD      L,0
STRIN1:			PUSH    BC
STRIN2:			LD      A,(HL)
			INC     HL
			LD      (DE),A
			INC     E
			LD      A,19
			JP      Z,ERROR_         ;"String too long"
			DJNZ    STRIN2
			POP     BC
			DEC     C
			JR      NZ,STRIN1
			LD      A,80H
			RET
;
;SUBROUTINES
;
;SWAP - Swap arguments
;Exchanges DE,HL D'E',H'L' and B,C
;Destroys: A,B,C,D,E,H,L,D',E',H',L'
;
SWAP:			LD      A,C
			LD      C,B
			LD      B,A
			EX      DE,HL
			EXX
			EX      DE,HL
			EXX
			RET
;
;TEST - Test HLH'L' for zero
;Outputs: Z-flag set & A=0 if zero
;Destroys: A,F
;
TEST:			LD      A,H
			OR      L
			EXX
			OR      H
			OR      L
			EXX
			RET
;
;DECODE - Decode line number in pseudo-binary.
;   Inputs: IY = Text pointer.
;   Outputs: HL=0, H'L'=line number, C=0.
;   Destroys: A,C,H,L,H',L',IY,F
;
DECODE:			EXX
			LD      A,(IY)
			INC     IY
			RLA
			RLA
			LD      H,A
			AND     0C0H
			XOR     (IY)
			INC     IY
			LD      L,A
			LD      A,H
			RLA
			RLA
			AND     0C0H
			XOR     (IY)
			INC     IY
			LD      H,A
			EXX
			XOR     A
			LD      C,A
			LD      H,A
			LD      L,A
			RET
;
;HEXSTR - convert numeric value to HEX string.
;   Inputs: HLH'L'C = integer or floating-point number
;  Outputs: String in string accumulator.
;           E = string length.  D = ACCS/256
;
HEXSTS:			INC     IY              ;SKIP TILDE
			CALL    ITEMN
			CALL    HEXSTR
			LD      A,80H
			RET
;
HEXSTR:			CALL    SFIX
			LD      BC,8
			LD      DE,ACCS
HEXST1:			PUSH    BC
			LD      B,4
			XOR     A
HEXST2:			EXX
			ADD     HL,HL
			EXX
			ADC     HL,HL
			RLA
			DJNZ    HEXST2
			POP     BC
			DEC     C
			RET     M
			JR      Z,HEXST3
			OR      A
			JR      NZ,HEXST3
			CP      B
			JR      Z,HEXST1
HEXST3:			ADD     A,90H
			DAA
			ADC     A,40H
			DAA
			LD      (DE),A
			INC     DE
			LD      B,A
			JR      HEXST1
;
;Function STR - convert numeric value to ASCII string.
;   Inputs: HLH'L'C = integer or floating-point number.
;  Outputs: String in string accumulator.
;           E = length, D = ACCS/256
;           A = 80H (type=string)
;
;First normalise for decimal output:
;
STRS:			CALL    NXT
			CP      '~'
			JR      Z,HEXSTS
			CALL    ITEMN
			LD      IX,STAVAR
			LD      A,(IX+3)
			OR      A
			LD      IX,G9-1         ;G9 FORMAT
			JR      Z,STR0
STR:			LD      IX,STAVAR
STR0:			LD      DE,ACCS
			LD      A,37
			CALL    FPP
			JP      C,ERROR_
			BIT     0,(IX+2)
STR1:			LD      A,80H           ;STRING MARKER
			RET     Z
			LD      A,C
			ADD     A,4
STR2:			CP      E
			JR      Z,STR1
			EX      DE,HL	
			LD      (HL),' '        ;TRAILING SPACE
			INC     HL
			EX      DE,HL
			JR      STR2
;
G9:			DW    9
;
;STRING COMPARE
;Compare string (DE) length B with string (HL) length C.
;Result preset to false.
;
SCP:			CALL	SCP0
;
ZERO:			LD      A,0
			EXX
			LD      H,A
			LD      L,A
			EXX
			LD      H,A
			LD      L,A
			LD      C,A
			RET
;
SCP0:			INC     B
			INC     C
SCP1:			DEC     B
			JR      Z,SCP2
			DEC     C
			JR      Z,SCP3
			LD      A,(DE)
			CP      (HL)
			RET     NZ
			INC     DE
			INC     HL
			JR      SCP1
SCP2:			OR      A
			DEC     C
			RET     Z
			SCF
			RET
SCP3:			OR      A
			INC     C
			RET
;
;PUSHS - SAVE STRING ON STACK.
;    Inputs: String in string accumulator.
;            E = string length.
;            A - saved on stack.
;  Destroys: B,C,D,E,H,L,IX,SP,F
;
PUSHS:			CALL    CHECK
			POP     IX              ;RETURN ADDRESS
			OR      A               ;CLEAR CARRY
			LD      HL,ACCS
			LD      D,H
			LD      B,L             ;B=0
			SBC     HL,DE
			ADD     HL,SP
			LD      SP,HL
			LD      D,A
			PUSH    DE
			JR      Z,PUSHS1        ;ZERO LENGTH
			LD      C,E
			LD      DE,ACCS
			EX      DE,HL
			LDIR                    ;COPY TO STACK
			CALL    CHECK
PUSHS1:			JP      (IX)            ;"RETURN"
;
;POPS - RESTORE STRING FROM STACK.
;    Inputs: C = string length.
;   Outputs: String in string accumulator.
;            E = string length.
;  Destroys: B,C,D,E,H,L,IX,SP,F
;
POPS:			POP     IX              ;RETURN ADDRESS
			LD      HL,0
			LD      B,H             ;B=0
			ADD     HL,SP
			LD      DE,ACCS
			INC     C
			DEC     C
			JR      Z,POPS1         ;ZERO LENGTH
			LDIR                    ;COPY FROM STACK
POPS1:			LD      SP,HL
			JP      (IX)            ;"RETURN"
;
HEXDIG:			LD      A,(IY)
			CP      '0'
			RET     C
			CP      '9'+1
			CCF
			RET     NC
			CP      'A'
			RET     C
			SUB     'A'-10
			CP      16
			CCF
			RET
;
RELOP?:			CP      '>'
			RET     NC
			CP      '='
			RET     NC
			CP      '<'
			RET
;
EXPRSC:			CALL    EXPRS
COMMA:			CALL    NXT
			INC     IY
			CP      ','
			RET     Z
			LD      A,5
			JR      ERROR1          ;"Missing ,"
;
BRAKET:			CALL    NXT
			INC     IY
			CP      ')'
			RET     Z
			LD      A,27
ERROR1:			JP      ERROR_           ;"Missing )"
;
SAVE:			INC     IY
SAVE1:			EX      AF,AF'
			JP      M,TYPE_
			EX      AF,AF'
			EX      (SP),HL
			EXX
			PUSH    HL
			EXX
			PUSH    AF
			PUSH    BC
			JP      (HL)
;
DOIT:			EX      AF,AF'
			JP      M,TYPE_
			EXX
			POP     BC              ;RETURN ADDRESS
			EXX
			LD      A,C
			POP     BC
			LD      B,A
			POP     AF              ;OPERATOR
			EXX
			EX      DE,HL
			POP     HL
			EXX
			EX      DE,HL
			POP     HL
			EXX
			PUSH    BC
			EXX
			AND     0FH
			CALL    FPP
			JR      C,ERROR1
			XOR     A
			EX      AF,AF'          ;TYPE
			LD      A,(IY)
			RET
;
; Skip spaces
; - IY: String pointer
; Returns:
;  - A: The non-space character found
; - IY: Points to the character before that
; 
NXT:			LD      A,(IY)			; Fetch the character	
			CP      ' '			; If it is space, then return
			RET     NZ
			INC     IY			; Increment the pointer and
			JP      NXT			; Loop
;
DISPT2:			PUSH    HL
			LD      HL,SOPTBL
			JR      DISPT0
;
DISPAT:			PUSH    HL
			SUB     FUNTOK
			LD      HL,FUNTBL
DISPT0:			PUSH    BC
			ADD     A,A
			LD      C,A
			LD      B,0
			ADD     HL,BC
			LD      A,(HL)
			INC     HL
			LD      H,(HL)
			LD      L,A
			POP     BC
			EX      (SP),HL
			RET                     ;OFF TO ROUTINE

