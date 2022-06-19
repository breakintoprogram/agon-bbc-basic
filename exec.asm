;
; Title:	BBC Basic Interpreter - Z80 version
;		Statement Execution & Assembler Module - "EXEC"
; Author:	(C) Copyright  R.T.Russell  1984
; Modified By:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:
; 27/01/1984:	Version 2.1
; 02/03/1987:	Version 3.0
; 11/06/1987:	Version 3.1
; 03/05/2022:	Modified by Dean Belfield to assemble with ZDS

			.ASSUME	ADL = 0

			INCLUDE	"equs.inc"

			SEGMENT CODE
				
			XDEF	XEQ
			XDEF	CHAIN0
			XDEF	RUN
			XDEF	SYNTAX
			XDEF	ESCAPE
			XDEF	FN
			XDEF	USR
			XDEF	STORE5
			XDEF	STORE4
			XDEF	CHECK
			XDEF	TERMQ
			XDEF	FILL
			XDEF	X4OR5
			XDEF	MUL16
			XDEF	CHANEL
				
			XREF	AUTO
			XREF	DELETE
			XREF	LOAD
			XREF	LIST_
			XREF	NEW
			XREF	OLD
			XREF	RENUM
			XREF	SAVE
			XREF	SOUND
			XREF	CLG
			XREF	DRAW
			XREF	ENVEL
			XREF	GCOL
			XREF	MODE
			XREF	MOVE
			XREF	PLOT
			XREF	COLOUR
			XREF	EXPRS
			XREF	HIMEM
			XREF	LOAD0
			XREF	RANDOM
			XREF	CLEAR
			XREF	ERRTRP
			XREF	PAGE_
			XREF	DATAPTR
			XREF	ERRLIN
			XREF	TRAP
			XREF	NXT
			XREF	SETLIN
			XREF	CLOOP
			XREF	OSSHUT
			XREF	WARM
			XREF	TRACEN
			XREF	OUTCHR
			XREF	PBCDL
			XREF	OSCLI
			XREF	LISTON
			XREF	GETVAR
			XREF	PUTVAR
			XREF	DATPTR
			XREF	ERROR_
			XREF	EXPR
			XREF	CREATE
			XREF	EXPRI
			XREF	BRAKET
			XREF	FREE
			XREF	OSBPUT
			XREF	COUNT
			XREF	STR
			XREF	HEXSTR
			XREF	CRLF
			XREF	ITEMI
			XREF	FINDL
			XREF	TEST
			XREF	EXPRN
			XREF	DLOAD5
			XREF	LOADN
			XREF	FPP
			XREF	SWAP
			XREF	GETDEF
			XREF	ZERO
			XREF	OSBGET
			XREF	BUFFER
			XREF	CONS
			XREF	VAL0
			XREF	OSLINE
			XREF	CLRSCN
			XREF	TELL
			XREF	SAYLN
			XREF	REPORT
			XREF	PUTPTR
			XREF	PUTIME
			XREF	PUTIMS
			XREF	LOMEM
			XREF	WIDTH
			XREF	OSWRCH
			XREF	COMMA
			XREF	OSCALL
			XREF	SFIX
			XREF	LOAD4
			XREF	PUSHS
			XREF	POPS
			XREF	LOADS
			XREF	PUTCSR
			XREF	OUT_
				

TAND:			EQU     80H
TOR:			EQU     84H
TERROR:			EQU     85H
LINE_:			EQU     86H
OFF_:			EQU     87H
STEP:			EQU     88H
SPC:			EQU     89H
TAB:			EQU     8AH
ELSE_:			EQU     8BH
THEN:			EQU     8CH
LINO:			EQU     8DH
TO:				EQU     0B8H
TCMD:			EQU     0C6H
TCALL:			EQU     0D6H
DATA_:			EQU     0DCH
DEF_:			EQU     0DDH
TGOSUB:			EQU     0E4H
TGOTO:			EQU     0E5H
TON:			EQU     0EEH
TPROC:			EQU     0F2H
TSTOP:			EQU     0FAH
;
CMDTAB:			DW    AUTO
			DW    DELETE
			DW    LOAD
			DW    LIST_
			DW    NEW
			DW    OLD
			DW    RENUM
			DW    SAVE
			DW    PUT
			DW    PTR
			DW    PAGEV
			DW    TIMEV
			DW    LOMEMV
			DW    HIMEMV
			DW    SOUND
			DW    BPUT
			DW    CALL_
			DW    CHAIN
			DW    CLR
			DW    CLOSE
			DW    CLG
			DW    CLS
			DW    REM             ;DATA
			DW    REM             ;DEF
			DW    DIM
			DW    DRAW
			DW    END_
			DW    ENDPRO
			DW    ENVEL
			DW    FOR
			DW    GOSUB
			DW    GOTO
			DW    GCOL
			DW    IF_
			DW    INPUT
			DW    LET
			DW    LOCAL_
			DW    MODE
			DW    MOVE
			DW    NEXT
			DW    ON_
			DW    VDU
			DW    PLOT
			DW    PRINT_
			DW    PROC
			DW    READ
			DW    REM
			DW    REPEAT
			DW    REPOR
			DW    RESTOR
			DW    RETURN
			DW    RUN
			DW    STOP
			DW    COLOUR
			DW    TRACE
			DW    UNTIL
			DW    WIDTHV
			DW    CLI             ;OSCLI
;
RUN:			CALL    TERMQ
			JR      Z,RUN0
CHAIN:			CALL    EXPRS
			LD      A,CR
			LD      (DE),A
CHAIN0:			LD      SP,(HIMEM)
			CALL    LOAD0
RUN0:			LD      SP,(HIMEM)      ;PREPARE FOR RUN
			LD      IX,RANDOM
;
; dtrg: bugfix here; on emulators, R is always zero, and the original
; code always waiting until it was non-zero, resulting in a hang.
; Instead we crudely hack around it.
;
			LD      A, R
			JR      NZ, $F
			INC	A
;
$$:			RLCA
			RLCA
			LD      (IX+3),A
			SBC     A,A
			LD      (IX+4),A
			CALL    CLEAR
			LD      HL,0
			LD      (ERRTRP),HL
			LD      HL,(PAGE_)
			LD      A,DATA_
			CALL    SEARCH          ;LOOK FOR "DATA"
			LD      (DATPTR),HL     ;SET DATA POINTER
			LD      IY,(PAGE_)
XEQ0:			CALL    NEWLIN
XEQ:			LD      (ERRLIN),IY     ;ERROR POINTER
			CALL    TRAP            ;CHECK KEYBOARD
XEQ1:			CALL    NXT
			INC     IY
			CP      ':'             ;SEPARATOR
			JR      Z,XEQ1
			CP      CR
			JR      Z,XEQ0          ;NEW PROGRAM LINE
			SUB     TCMD
			JR      C,LET0          ;IMPLIED "LET"
			ADD     A,A
			LD      C,A
			LD      B,0
			LD      HL,CMDTAB
			ADD     HL,BC
			LD      A,(HL)          ;TABLE ENTRY
			INC     HL
			LD      H,(HL)
			LD      L,A
			CALL    NXT
			JP      (HL)            ;EXECUTE STATEMENT
;
;END
;
END_:			CALL    SETLIN          ;FIND CURRENT LINE
			LD      A,H
			OR      L               ;DIRECT?
			JP      Z,CLOOP
			LD      E,0
			CALL    OSSHUT          ;CLOSE ALL FILES
			JP      WARM            ;"Ready"
;
NEWLIN:			LD      A,(IY+0)        ;A=LINE LENGTH
			LD      BC,3
			ADD     IY,BC
			OR      A
			JR      Z,END_           ;LENGTH=0, EXIT
			LD      HL,(TRACEN)
			LD      A,H
			OR      L
			RET     Z
			LD      D,(IY-1)        ;DE = LINE NUMBER
			LD      E,(IY-2)
			SBC     HL,DE
			RET     C
			EX      DE,HL
			LD      A,'['           ;TRACE
			CALL    OUTCHR
			CALL    PBCDL
			LD      A,']'
			CALL    OUTCHR
			LD      A,' '
			JP      OUTCHR
;
;ROUTINES FOR EACH STATEMENT:
;
;OSCLI
;
CLI:			CALL    EXPRS
			LD      A,CR
			LD      (DE),A
			LD      HL,ACCS
			CALL    OSCLI
			JR      XEQ
;
;REM, *
;
EXT:			PUSH    IY
			POP     HL
			CALL    OSCLI
REM:			PUSH    IY
			POP     HL
			LD      A,CR
			LD      B,A
			CPIR                    ;FIND LINE END
			PUSH    HL
			POP     IY
			JP      XEQ0
;
;[LET] var = expr
;
LET0:			CP      ELSE_-TCMD
			JR      Z,REM
			CP      ('*'-TCMD) & 0FFH
			JR      Z,EXT
			CP      ('='-TCMD) & 0FFH
			JR      Z,FNEND
			CP      ('['-TCMD) & 0FFH
			JR      Z,ASM
			DEC     IY
LET:			CALL    ASSIGN
			JP      Z,XEQ
			JR      C,SYNTAX        ;"Syntax error"
			PUSH    AF              ;SAVE STRING TYPE
			CALL    EQUALS
			PUSH    HL
			CALL    EXPRS
			POP     IX
			POP     AF
			CALL    STACCS
XEQR:			JP      XEQ
;
ASM0:			CALL    NEWLIN
ASM:			LD      (ERRLIN),IY
			CALL    TRAP
			CALL    ASSEM
			JR      C,SYNTAX
			CP      CR
			JR      Z,ASM0
			LD      HL,LISTON
			LD      A,(HL)
			AND     0FH
			OR      30H
			LD      (HL),A
			JR      XEQR
;
VAR_:			CALL    GETVAR
			RET     Z
			JP      NC,PUTVAR
SYNTAX:			LD      A,16            ;"Syntax error"
			DB    21H
ESCAPE:			LD      A,17            ;"Escape"
ERROR0:			JP      ERROR_
;
;=
;
FNEND:			CALL    EXPR            ;FUNCTION RESULT
			LD      B,E
			EX      DE,HL
			EXX                     ;SAVE RESULT
			EX      DE,HL           ; IN DEB'C'D'E'
FNEND5:			POP     BC
			LD      HL,LOCCHK
			OR      A
			SBC     HL,BC
			JR      Z,FNEND0        ;LOCAL VARIABLE
			LD      HL,FNCHK
			OR      A
			SBC     HL,BC
			LD      A,7
			JR      NZ,ERROR0       ;"No FN"
			POP     IY
			LD      (ERRLIN),IY     ;IN CASE OF ERROR
			EX      DE,HL
			EXX
			EX      DE,HL
			LD      DE,ACCS
			LD      E,B
			EX      AF,AF'
			RET
;
FNEND0:			POP     IX
			POP     BC
			LD      A,B
			OR      A
			JP      M,FNEND1        ;STRING
			POP     HL
			EXX
			POP     HL
			EXX
			CALL    STORE
			JR      FNEND5
FNEND1:			LD      HL,0
			ADD     HL,SP
			PUSH    DE
			LD      E,C
			CALL    STORES
			POP     DE
			LD      SP,HL
			JR      FNEND5
;
;DIM var(dim1[,dim2[,...]])[,var(...]
;DIM var expr[,var expr...]
;
DIM:			CALL    GETVAR          ;VARIABLE
			JR      C,BADDIM
			JP      Z,DIM4
			CALL    CREATE
			PUSH    HL
			POP     IX
			LD      A,(IY)
			CP      '('
			LD      A,D
			JR      NZ,DIM4
			PUSH    HL
			PUSH    AF              ;SAVE TYPE
			LD      DE,1
			LD      B,D             ;DIMENSION COUNTER
DIM1:			INC     IY
			PUSH    BC
			PUSH    DE
			PUSH    IX
			CALL    EXPRI           ;DIMENSION SIZE
			BIT     7,H
			JR      NZ,BADDIM
			EXX
			INC     HL
			POP     IX
			INC     IX
			LD      (IX),L          ;SAVE SIZE
			INC     IX
			LD      (IX),H
			POP     BC
			CALL    MUL16           ;HL=HL*BC
			JR      C,NOROOM        ;TOO LARGE
			EX      DE,HL           ;DE=PRODUCT
			POP     BC
			INC     B               ;DIMENSION COUNTER
			LD      A,(IY)
			CP      ','             ;ANOTHER
			JR      Z,DIM1
			CALL    BRAKET          ;CLOSING BRACKET
			POP     AF              ;RESTORE TYPE
			INC     IX
			EX      (SP),IX
			LD      (IX),B          ;NO. OF DIMENSIONS
			CALL    X4OR5           ;DE=DE*n
			POP     HL
			JR      C,NOROOM
DIM3:			ADD     HL,DE
			JR      C,NOROOM
			PUSH    HL
			INC     H
			JR      Z,NOROOM
			SBC     HL,SP
			JR      NC,NOROOM       ;OUT OF SPACE
			POP     HL
			LD      (FREE),HL
DIM2:			LD      A,D
			OR      E
			JR      Z,DIM5
			DEC     HL
			LD      (HL),0          ;INITIALISE ARRAY
			DEC     DE
			JR      DIM2
DIM5:			CALL    NXT
			CP      ','             ;ANOTHER VARIABLE?
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			JR      DIM
;
BADDIM:			LD      A,10            ;"Bad DIM"
			DB    21H
NOROOM:			LD      A,11            ;"DIM space"
ERROR1:			JP      ERROR_
;
DIM4:			OR      A
			JR      Z,BADDIM
			JP      M,BADDIM        
			LD      B,A
			LD      A,(IY-1)
			CP      ')'
			LD      A,B
			JR      Z,BADDIM
			LD      HL,(FREE)
			EXX
			LD      HL,0
			LD      C,H
			CALL    STORE           ;RESERVED AREA
			CALL    EXPRI
			EXX
			INC     HL
			EX      DE,HL
			LD      HL,(FREE)
			JR      DIM3
;
;PRINT list...
;PRINT #channel,list...
;
PRINT_:			CP      '#'
			JR      NZ,PRINT0
			CALL    CHNL            ;CHANNEL NO. = E
PRNTN1:			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			PUSH    DE
			CALL    EXPR            ;ITEM TO PRINT
			EX      AF,AF'
			JP      M,PRNTN2        ;STRING
			POP     DE
			PUSH    BC
			EXX
			LD      A,L
			EXX
			CALL    OSBPUT
			EXX
			LD      A,H
			EXX
			CALL    OSBPUT
			LD      A,L
			CALL    OSBPUT
			LD      A,H
			CALL    OSBPUT
			POP     BC
			LD      A,C
			CALL    OSBPUT
			JR      PRNTN1
PRNTN2:			LD      C,E
			POP     DE
			LD      HL,ACCS
			INC     C
PRNTN3:			DEC     C
			JR      Z,PRNTN4
			LD      A,(HL)
			INC     HL
			PUSH    BC
			CALL    OSBPUT
			POP     BC
			JR      PRNTN3
PRNTN4:			LD      A,CR
			CALL    OSBPUT
			JR      PRNTN1
;
PRINT6:			LD      B,2
			JR      PRINTC
PRINT8:			LD      BC,100H
			JR      PRINTC
PRINT9:			LD      HL,STAVAR
			XOR     A
			CP      (HL)
			JR      Z,PRINT0
			LD      A,(COUNT)
			OR      A
			JR      Z,PRINT0
PRINTA:			SUB     (HL)
			JR      Z,PRINT0
			JR      NC,PRINTA
			NEG
			CALL    FILL
PRINT0:			LD      A,(STAVAR)
			LD      C,A             ;PRINTS
			LD      B,0             ;PRINTF
PRINTC:			CALL    TERMQ
			JR      Z,PRINT4
			RES     0,B
			INC     IY
			CP      '~'
			JR      Z,PRINT6
			CP      ';'
			JR      Z,PRINT8
			CP      ','
			JR      Z,PRINT9
			CALL    FORMAT          ;SPC, TAB, '
			JR      Z,PRINTC
			DEC     IY
			PUSH    BC
			CALL    EXPR            ;VARIABLE TYPE
			EX      AF,AF'
			JP      M,PRINT3        ;STRING
			POP     DE
			PUSH    DE
			BIT     1,D
			PUSH    AF
			CALL    Z,STR           ;DECIMAL
			POP     AF
			CALL    NZ,HEXSTR       ;HEX
			POP     BC
			PUSH    BC
			LD      A,C
			SUB     E
			CALL    NC,FILL         ;RIGHT JUSTIFY
PRINT3:			POP     BC
			CALL    PTEXT           ;PRINT
			JR      PRINTC
PRINT4:			BIT     0,B
			CALL    Z,CRLF
			JP      XEQ
;
;
ONERR:			INC     IY              ;SKIP "ERROR"
			LD      HL,0
			LD      (ERRTRP),HL
			CALL    NXT
			CP      OFF_
			INC     IY
			JP      Z,XEQ
			DEC     IY
			LD      (ERRTRP),IY
			JP      REM
;
;ON expr GOTO line[,line...] [ELSE statement]
;ON expr GOTO line[,line...] [ELSE line]
;ON expr GOSUB line[,line...] [ELSE statement]
;ON expr GOSUB line[,line...] [ELSE line]
;ON expr PROCone [,PROCtwo..] [ELSE PROCotherwise]
;ON ERROR statement [:statement...]
;ON ERROR OFF
;
ON_:			CP      TERROR
			JR      Z,ONERR         ;"ON ERROR"
			CALL    EXPRI
			LD      A,(IY)
			INC     IY
			LD      E,','           ;SEPARATOR
			CP      TGOTO
			JR      Z,ON1
			CP      TGOSUB
			JR      Z,ON1
			LD      E,TPROC
			CP      E
			LD      A,39
			JR      NZ,ERROR2       ;"ON syntax"
ON1:			LD      D,A
			EXX
			PUSH    HL
			EXX
			POP     BC              ;ON INDEX
			LD      A,B
			OR      H
			OR      L
			JR      NZ,ON4          ;OUT OF RANGE
			OR      C
			JR      Z,ON4
			DEC     C
			JR      Z,ON3           ;INDEX=1
ON2:			CALL    TERMQ
			JR      Z,ON4           ;OUT OF RANGE
			INC     IY              ;SKIP DELIMITER
			CP      E
			JR      NZ,ON2
			DEC     C
			JR      NZ,ON2
ON3:			LD      A,E
			CP      TPROC
			JR      Z,ONPROC
			PUSH    DE
			CALL    ITEMI           ;LINE NUMBER
			POP     DE
			LD      A,D
			CP      TGOTO
			JR      Z,GOTO2
			CALL    SPAN            ;SKIP REST OF LIST
			JR      GOSUB1
;
ON4:			LD      A,(IY)
			INC     IY
			CP      ELSE_
			JP      Z,IF1           ;ELSE CLAUSE
			CP      CR
			JR      NZ,ON4
			LD      A,40
ERROR2:			JP      ERROR_           ;"ON range"
;
ONPROC:			LD      A,TON
			JP      PROC
;
;GOTO line
;
GOTO:			CALL    ITEMI           ;LINE NUMBER
GOTO1:			CALL    TERMQ
			JP      NZ,SYNTAX
GOTO2:			EXX
			CALL    FINDL
			PUSH    HL
			POP     IY
			JP      Z,XEQ0
			LD      A,41
			JR      ERROR2          ;"No such line"
;
;GOSUB line
;
GOSUB:			CALL    ITEMI           ;LINE NUMBER
GOSUB1:			PUSH    IY              ;TEXT POINTER
			CALL    CHECK           ;CHECK ROOM
			CALL    GOTO1           ;SAVE MARKER
GOSCHK:			EQU     $
;
;RETURN
;
RETURN:			POP     DE              ;MARKER
			LD      HL,GOSCHK
			OR      A
			SBC     HL,DE
			POP     IY
			JP      Z,XEQ
			LD      A,38
			JR      ERROR2          ;"No GOSUB"
;
;REPEAT
;
REPEAT:			PUSH    IY
			CALL    CHECK
			CALL    XEQ
REPCHK:			EQU     $
;
;UNTIL expr
;
UNTIL:			POP     BC
			PUSH    BC
			LD      HL,REPCHK
			OR      A
			SBC     HL,BC
			LD      A,43
			JR      NZ,ERROR2       ;"No REPEAT"
			CALL    EXPRI
			CALL    TEST
			POP     BC
			POP     DE
			JR      NZ,XEQ2         ;TRUE
			PUSH    DE
			PUSH    BC
			PUSH    DE
			POP     IY
XEQ2:			JP      XEQ
;
;FOR var = expr TO expr [STEP expr]
;
FORVAR:			LD      A,34
			JR      ERROR2          ;"FOR variable"
;
FOR:			CALL    ASSIGN
			JR      NZ,FORVAR       ;"FOR variable"
			PUSH    AF              ;SAVE TYPE
			LD      A,(IY)
			CP      TO
			LD      A,36
			JR      NZ,ERROR2       ;"No TO"
			INC     IY
			PUSH    IX
			CALL    EXPRN           ;LIMIT
			POP     IX
			POP     AF
			LD      B,A             ;TYPE
			PUSH    BC              ;SAVE ON STACK
			PUSH    HL
			LD      HL,0
			LD      C,H
			EXX
			PUSH    HL
			LD      HL,1            ;PRESET STEP
			EXX
			LD      A,(IY)
			CP      STEP
			JR      NZ,FOR1
			INC     IY
			PUSH    IX
			CALL    EXPRN           ;STEP
			POP     IX
FOR1:			PUSH    BC
			PUSH    HL
			EXX
			PUSH    HL
			EXX
			PUSH    IY              ;SAVE TEXT POINTER
			PUSH    IX              ;LOOP VARIABLE
			CALL    CHECK
			CALL    XEQ
FORCHK:			EQU     $
;
;NEXT [var[,var...]]
;
NEXT:			POP     BC              ;MARKER
			LD      HL,FORCHK
			OR      A
			SBC     HL,BC
			LD      A,32
			JR      NZ,ERROR3       ;"No FOR"
			CALL    TERMQ
			POP     HL
			PUSH    HL
			PUSH    BC
			PUSH    HL
			CALL    NZ,GETVAR       ;VARIABLE
			POP     DE
			EX      DE,HL
			OR      A
NEXT0:			SBC     HL,DE
			JR      NZ,NEXT1
			PUSH    DE
			LD      IX,6+2
			ADD     IX,SP
			CALL    DLOAD5          ;STEP
			LD      A,(IX+11)       ;TYPE
			POP     IX
			CALL    LOADN           ;LOOP VARIABLE
			BIT     7,D             ;SIGN?
			PUSH    AF
			LD      A,'+' & 0FH
			CALL    FPP             ;ADD STEP
			JR      C,ERROR3
			POP     AF              ;RESTORE TYPE
			PUSH    AF
			CALL    STORE           ;UPDATE VARIABLE
			LD      IX,12+2
			ADD     IX,SP
			CALL    DLOAD5          ;LIMIT
			POP     AF
			CALL    Z,SWAP
			LD      A,0+('<'-4) & 0FH
			CALL    FPP             ;TEST AGAINST LIMIT
			JR      C,ERROR3
			INC     H
			JR      NZ,LOOP_         ;KEEP LOOPING
			LD      HL,18
			ADD     HL,SP
			LD      SP,HL
			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			JR      NEXT
;
LOOP_:			POP     BC
			POP     DE
			POP     IY
			PUSH    IY
			PUSH    DE
			PUSH    BC
			JP      XEQ
;
NEXT1:			LD      HL,18
			ADD     HL,SP
			LD      SP,HL           ;"POP" THE STACK
			POP     BC
			LD      HL,FORCHK
			SBC     HL,BC
			POP     HL              ;VARIABLE POINTER
			PUSH    HL
			PUSH    BC
			JR      Z,NEXT0
			LD      A,33
ERROR3:			JP      ERROR_           ;"Can't match FOR"
;
;FNname
;N.B. ENTERED WITH A <> TON
;
FN:			PUSH    AF              ;MAKE SPACE ON STACK
			CALL    PROC1
FNCHK:			EQU     $
;
;PROCname
;N.B. ENTERED WITH A = ON PROC FLAG
;
PROC:			PUSH    AF              ;MAKE SPACE ON STACK
			CALL    PROC1
PROCHK:			EQU     $
PROC1:			CALL    CHECK
			DEC     IY
			PUSH    IY
			CALL    GETDEF
			POP     BC
			JR      Z,PROC4
			LD      A,30
			JR      C,ERROR3        ;"Bad call"
			PUSH    BC
			LD      HL,(PAGE_)
PROC2:			LD      A,DEF_
			CALL    SEARCH          ;LOOK FOR "DEF"
			JR      C,PROC3
			PUSH    HL
			POP     IY
			INC     IY              ;SKIP DEF
			CALL    NXT
			CALL    GETDEF
			PUSH    IY
			POP     DE
			JR      C,PROC6
			CALL    NZ,CREATE
			PUSH    IY
			POP     DE
			LD      (HL),E
			INC     HL
			LD      (HL),D          ;SAVE ADDRESS
PROC6:			EX      DE,HL
			LD      A,CR
			LD      B,A
			CPIR                    ;SKIP TO END OF LINE
			JR      PROC2
PROC3:			POP     IY              ;RESTORE TEXT POINTER
			CALL    GETDEF
			LD      A,29
			JR      NZ,ERROR3       ;"No such FN/PROC"
PROC4:			LD      E,(HL)
			INC     HL
			LD      D,(HL)          ;GET ADDRESS
			LD      HL,2
			ADD     HL,SP
			CALL    NXT             ;ALLOW SPACE BEFORE (
			PUSH    DE              ;EXCHANGE DE,IY
			EX      (SP),IY
			POP     DE
			CP      '('             ;ARGUMENTS?
			JR      NZ,PROC5
			CALL    NXT             ;ALLOW SPACE BEFORE (
			CP      '('
			JP      NZ,SYNTAX       ;"Syntax error"
			PUSH    IY
			POP     BC              ;SAVE IY IN BC
			EXX
			CALL    SAVLOC          ;SAVE DUMMY VARIABLES
			CALL    BRAKET          ;CLOSING BRACKET
			EXX
			PUSH    BC
			POP     IY              ;RESTORE IY
			PUSH    HL
			CALL    ARGUE           ;TRANSFER ARGUMENTS
			POP     HL
PROC5:			LD      (HL),E          ;SAVE "RETURN ADDRESS"
			INC     HL
			LD      A,(HL)
			LD      (HL),D
			CP      TON             ;WAS IT "ON PROC" ?
			JP      NZ,XEQ
			PUSH    DE
			EX      (SP),IY
			CALL    SPAN            ;SKIP REST OF ON LIST
			EX      (SP),IY
			POP     DE
			LD      (HL),D
			DEC     HL
			LD      (HL),E
			JP      XEQ
;
;LOCAL var[,var...]
;
LOCAL_:			POP     BC
			PUSH    BC
			LD      HL,FNCHK
			OR      A
			SBC     HL,BC
			JR      Z,LOCAL1
			LD      HL,PROCHK
			OR      A
			SBC     HL,BC
			JR      Z,LOCAL1
			LD      HL,LOCCHK
			OR      A
			SBC     HL,BC
			LD      A,12
			JP      NZ,ERROR_        ;"Not LOCAL"
LOCAL1:			PUSH    IY
			POP     BC
			EXX
			DEC     IY
			CALL    SAVLOC
			EXX
			PUSH    BC
			POP     IY
LOCAL2:			CALL    GETVAR
			JP      NZ,SYNTAX
			OR      A               ;TYPE
			EX      AF,AF'
			CALL    ZERO
			EX      AF,AF'
			PUSH    AF
			CALL    P,STORE         ;ZERO
			POP     AF
			LD      E,C
			CALL    M,STORES
			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			JR      LOCAL2
;
;ENDPROC
;
ENDPRO:			POP     BC
			LD      HL,LOCCHK
			OR      A
			SBC     HL,BC
			JR      Z,UNSTK         ;LOCAL VARIABLE
			LD      HL,PROCHK       ;PROC MARKER
			OR      A
			SBC     HL,BC
			POP     IY
			JP      Z,XEQ
			LD      A,13
			JP      ERROR_           ;"No PROC"
;
UNSTK:			POP     IX
			POP     BC
			LD      A,B
			OR      A
			JP      M,UNSTK1        ;STRING
			POP     HL
			EXX
			POP     HL
			EXX
			CALL    STORE
			JR      ENDPRO
UNSTK1:			LD      HL,0
			ADD     HL,SP
			LD      E,C
			CALL    STORES
			LD      SP,HL
			JR      ENDPRO
;
;INPUT #channel,var,var...
;
INPUTN:			CALL    CHNL            ;E = CHANNEL NUMBER
INPN1:			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			PUSH    DE
			CALL    VAR_
			POP     DE
			PUSH    AF              ;SAVE TYPE
			PUSH    HL              ;VARPTR
			OR      A
			JP      M,INPN2         ;STRING
			CALL    OSBGET
			EXX
			LD      L,A
			EXX
			CALL    OSBGET
			EXX
			LD      H,A
			EXX
			CALL    OSBGET
			LD      L,A
			CALL    OSBGET
			LD      H,A
			CALL    OSBGET
			LD      C,A
			POP     IX
			POP     AF              ;RESTORE TYPE
			PUSH    DE              ;SAVE CHANNEL
			CALL    STORE
			POP     DE
			JR      INPN1
INPN2:			LD      HL,ACCS
INPN3:			CALL    OSBGET
			CP      CR
			JR      Z,INPN4
			LD      (HL),A
			INC     L
			JR      NZ,INPN3
INPN4:			POP     IX
			POP     AF
			PUSH    DE
			EX      DE,HL
			CALL    STACCS
			POP     DE
			JR      INPN1
;
;INPUT ['][SPC(x)][TAB(x[,y])]["prompt",]var[,var...]
;INPUT LINE [SPC(x)][TAB(x[,y])]["prompt",]var[,var...]
;
INPUT:			CP      '#'
			JR      Z,INPUTN
			LD      C,0             ;FLAG PROMPT
			CP      LINE_
			JR      NZ,INPUT0
			INC     IY              ;SKIP "LINE"
			LD      C,80H
INPUT0:			LD      HL,BUFFER
			LD      (HL),CR         ;INITIALISE EMPTY
INPUT1:			CALL    TERMQ
			JP      Z,XEQ           ;DONE
			INC     IY
			CP      ','
			JR      Z,INPUT3        ;SKIP COMMA
			CP      ';'
			JR      Z,INPUT3
			PUSH    HL              ;SAVE BUFFER POINTER
			CP      34		;ASCII ""
			JR      NZ,INPUT6
			PUSH    BC
			CALL    CONS
			POP     BC
			CALL    PTEXT           ;PRINT PROMPT
			JR      INPUT9
INPUT6:			CALL    FORMAT          ;SPC, TAB, '
			JR      NZ,INPUT2
INPUT9:			POP     HL
			SET     0,C             ;FLAG NO PROMPT
			JR      INPUT0
INPUT2:			DEC     IY
			PUSH    BC
			CALL    VAR_
			POP     BC
			POP     HL
			PUSH    AF              ;SAVE TYPE
			LD      A,(HL)
			INC     HL
			CP      CR              ;BUFFER EMPTY?
			CALL    Z,REFILL
			BIT     7,C
			PUSH    AF
			CALL    NZ,LINES
			POP     AF
			CALL    Z,FETCHS
			POP     AF              ;RESTORE TYPE
			PUSH    BC
			PUSH    HL
			OR      A
			JP      M,INPUT4        ;STRING
			PUSH    AF
			PUSH    IX
			CALL    VAL0
			POP     IX
			POP     AF
			CALL    STORE
			JR      INPUT5
INPUT4:			CALL    STACCS
INPUT5:			POP     HL
			POP     BC
INPUT3:			RES     0,C
			JR      INPUT1
;
REFILL:			BIT     0,C
			JR      NZ,REFIL0       ;NO PROMPT
			LD      A,'?'
			CALL    OUTCHR          ;PROMPT
			LD      A,' '
			CALL    OUTCHR
REFIL0:			LD      HL,BUFFER
			PUSH    BC
			PUSH    HL
			PUSH    IX
			CALL    OSLINE
			POP     IX
			POP     HL
			POP     BC
			LD      B,A             ;POS AT ENTRY
			XOR     A
			LD      (COUNT),A
			CP      B
			RET     Z
REFIL1:			LD      A,(HL)
			CP      CR
			RET     Z
			INC     HL
			DJNZ    REFIL1
			RET
;
;READ var[,var...]
;
READ:			CP      '#'
			JP      Z,INPUTN
			LD      HL,(DATPTR)
READ0:			LD      A,(HL)
			INC     HL              ;SKIP COMMA OR "DATA"
			CP      CR              ;END OF DATA STMT?
			CALL    Z,GETDAT
			PUSH    HL
			CALL    VAR_
			POP     HL
			OR      A
			JP      M,READ1         ;STRING
			PUSH    HL
			EX      (SP),IY
			PUSH    AF              ;SAVE TYPE
			PUSH    IX
			CALL    EXPRN
			POP     IX
			POP     AF
			CALL    STORE
			EX      (SP),IY
			JR      READ2
READ1:			CALL    FETCHS
			PUSH    HL
			CALL    STACCS
READ2:			POP     HL
			LD      (DATPTR),HL
			CALL    NXT
			CP      ','
			JP      NZ,XEQ
			INC     IY
			CALL    NXT
			JR      READ0
;
GETDAT:			LD      A,DATA_
			CALL    SEARCH
			INC     HL
			RET     NC
			LD      A,42
ERROR4:			JP      ERROR_           ;"Out of DATA"
;
;IF expr statement
;IF expr THEN statement [ELSE statement]
;IF expr THEN line [ELSE line]
;
IF_:		CALL    EXPRI
			CALL    TEST
			JR      Z,IFNOT         ;FALSE
			LD      A,(IY)
			CP      THEN
			JP      NZ,XEQ
			INC     IY              ;SKIP "THEN"
IF1:			CALL    NXT
			CP      LINO
			JP      NZ,XEQ          ;STATEMENT FOLLOWS
			JP      GOTO            ;LINE NO. FOLLOWS
IFNOT:			LD      A,(IY)
			CP      CR
			INC     IY
			JP      Z,XEQ0          ;END OF LINE
			CP      ELSE_
			JR      NZ,IFNOT
			JR      IF1
;
;CLS
;
CLS:		CALL    CLRSCN
			XOR     A
			LD      (COUNT),A
			JP      XEQ
;
;STOP
;
STOP:			CALL    TELL
			DB    CR
			DB    LF
			DB    TSTOP
			DB    0
			CALL    SETLIN          ;FIND CURRENT LINE
			CALL    SAYLN
			CALL    CRLF
			JP      CLOOP
;
;REPORT
;
REPOR:			CALL    REPORT
			JP      XEQ
;
;CLEAR
;
CLR:			CALL    CLEAR
			LD      HL,(PAGE_)
			JR      RESTR1
;
;RESTORE [line]
;
RESTOR:			LD      HL,(PAGE_)
			CALL    TERMQ
			JR      Z,RESTR1
			CALL    ITEMI
			EXX
			CALL    FINDL           ;SEARCH FOR LINE
			LD      A,41
			JR      NZ,ERROR4       ;"No such line"
RESTR1:			LD      A,DATA_
			CALL    SEARCH
			LD      (DATPTR),HL
			JP      XEQ
;
;PTR#channel=expr
;PAGE=expr
;TIME=expr
;LOMEM=expr
;HIMEM=expr
;
PTR:			CALL    CHANEL
			CALL    EQUALS
			LD      A,E
			PUSH    AF
			CALL    EXPRI
			PUSH    HL
			EXX
			POP     DE
			POP     AF
			CALL    PUTPTR
			JP      XEQ
;
PAGEV:			CALL    EQUALS
			CALL    EXPRI
			EXX
			LD      L,0
			LD      (PAGE_),HL
			JP      XEQ
;
TIMEV:			CP      '$'
			JR      Z,TIMEVS
			CALL    EQUALS
			CALL    EXPRI
			PUSH    HL
			EXX
			POP     DE
			CALL    PUTIME
			JP      XEQ
;
TIMEVS:			INC     IY              ;SKIP '$'
			CALL    EQUALS
			CALL    EXPRS
			CALL    PUTIMS
			JP      XEQ
;
LOMEMV:			CALL    EQUALS
			CALL    EXPRI
			CALL    CLEAR
			EXX
			LD      (LOMEM),HL
			LD      (FREE),HL
			JP      XEQ
;
HIMEMV:			CALL    EQUALS
			CALL    EXPRI
			EXX
			LD      DE,(FREE)
			INC     D
			XOR     A
			SBC     HL,DE
			ADD     HL,DE
			JP      C,ERROR_         ;"No room"
			LD      DE,(HIMEM)
			LD      (HIMEM),HL
			EX      DE,HL
			SBC     HL,SP
			JP      NZ,XEQ
			EX      DE,HL
			LD      SP,HL           ;LOAD STACK POINTER
			JP      XEQ
;
;WIDTH expr
;
WIDTHV:			CALL    EXPRI
			EXX
			LD      A,L
			LD      (WIDTH),A
			JP      XEQ
;
;TRACE ON
;TRACE OFF
;TRACE line
;
TRACE:			INC     IY
			LD      HL,0
			CP      TON
			JR      Z,TRACE0
			CP      OFF_
			JR      Z,TRACE1
			DEC     IY
			CALL    EXPRI
			EXX
TRACE0:			DEC     HL
TRACE1:			LD      (TRACEN),HL
			JP      XEQ
;
;VDU expr,expr;....
;
VDU:			CALL    EXPRI
			EXX
			LD      A,L
			CALL    OSWRCH
			LD      A,(IY)
			CP      ','
			JR      Z,VDU2
			CP      ';'
			JR      NZ,VDU3
			LD      A,H
			CALL    OSWRCH
VDU2:			INC     IY
VDU3:			CALL    TERMQ
			JR      NZ,VDU
			JP      XEQ
;
;CLOSE channel number
;
CLOSE:			CALL    CHANEL
			CALL    OSSHUT
			JP      XEQ
;
;BPUT channel,byte
;
BPUT:			CALL    CHANEL          ;CHANNEL NUMBER
			PUSH    DE
			CALL    COMMA
			CALL    EXPRI           ;BYTE
			EXX
			LD      A,L
			POP     DE
			CALL    OSBPUT
			JP      XEQ
;
;CALL address[,var[,var...]]
;
CALL_:			CALL    EXPRI           ;ADDRESS
			EXX
			PUSH    HL              ;SAVE IT
			LD      B,0             ;PARAMETER COUNTER
			LD      DE,BUFFER       ;VECTOR
CALL1:			CALL    NXT
			CP      ','
			JR      NZ,CALL2
			INC     IY
			INC     B
			CALL    NXT
			PUSH    BC
			PUSH    DE
			CALL    VAR_
			POP     DE
			POP     BC
			INC     DE
			LD      (DE),A          ;PARAMETER TYPE
			INC     DE
			EX      DE,HL
			LD      (HL),E          ;PARAMETER ADDRESS
			INC     HL
			LD      (HL),D
			EX      DE,HL
			JR      CALL1
CALL2:			LD      A,B
			LD      (BUFFER),A      ;PARAMETER COUNT
			POP     HL              ;RESTORE ADDRESS
			CALL    USR1
			JP      XEQ
;
;USR(address)
;
USR:			CALL    ITEMI
			EXX
USR1:			PUSH    HL              ;ADDRESS ON STACK
			EX      (SP),IY
			INC     H               ;PAGE &FF?
			LD      HL,USR2         ;RETURN ADDRESS
			PUSH    HL
			LD      IX,STAVAR
			CALL    Z,OSCALL        ;INTERCEPT PAGE &FF
			LD      C,(IX+24)
			PUSH    BC
			POP     AF              ;LOAD FLAGS
			LD      A,(IX+4)        ;LOAD Z80 REGISTERS
			LD      B,(IX+8)
			LD      C,(IX+12)
			LD      D,(IX+16)
			LD      E,(IX+20)
			LD      H,(IX+32)
			LD      L,(IX+48)
			LD      IX,BUFFER
			JP      (IY)            ;OFF TO USER ROUTINE
USR2:			POP     IY
			XOR     A
			LD      C,A
			RET
;
;PUT port,data
;
PUT:			CALL    EXPRI           ;PORT ADDRESS
			EXX
			PUSH    HL
			CALL    COMMA
			CALL    EXPRI           ;DATA
			EXX
			POP     BC
			OUT     (C),L           ;OUTPUT TO PORT BC
			JP      XEQ
;
;SUBROUTINES:
;
;ASSIGN - Assign a numeric value to a variable.
;Outputs: NC,  Z - OK, numeric.
;         NC, NZ - OK, string.
;          C, NZ - illegal
;
ASSIGN:			CALL    GETVAR          ;VARIABLE
			RET     C               ;ILLEGAL VARIABLE
			CALL    NZ,PUTVAR
			OR      A
			RET     M               ;STRING VARIABLE
			PUSH    AF              ;NUMERIC TYPE
			CALL    EQUALS
			PUSH    HL
			CALL    EXPRN
			POP     IX
			POP     AF
STORE:			BIT     0,A
			JR      Z,STOREI
			CP      A               ;SET ZERO
STORE5:		LD      (IX+4),C
STORE4:		EXX
			LD      (IX+0),L
			LD      (IX+1),H
			EXX
			LD      (IX+2),L
			LD      (IX+3),H
			RET
STOREI:			PUSH    AF
			INC     C               ;SPEED - & PRESERVE F'
			DEC     C               ; WHEN CALLED BY FNEND0
			CALL    NZ,SFIX         ;CONVERT TO INTEGER
			POP     AF
			CP      4
			JR      Z,STORE4
			CP      A               ;SET ZERO
STORE1:			EXX
			LD      (IX+0),L
			EXX
			RET
;
STACCS:			LD      HL,ACCS
STORES:			RRA
			JR      NC,STORS3       ;FIXED STRING
			PUSH    HL
			CALL    LOAD4
			LD      A,E             ;LENGTH OF STRING
			EXX
			LD      L,A
			LD      A,H             ;LENGTH ALLOCATED
			EXX
			CP      E
			JR      NC,STORS1       ;ENOUGH ROOM
			EXX
			LD      H,L
			EXX
			PUSH    HL
			LD      B,0
			LD      C,A
			ADD     HL,BC
			LD      BC,(FREE)
			SBC     HL,BC           ;IS STRING LAST?
			POP     HL
			SCF
			JR      Z,STORS1
			LD      H,B
			LD      L,C
STORS1:			CALL    STORE4          ;PRESERVES CARRY!
			LD      B,0
			LD      C,E
			EX      DE,HL
			POP     HL
			DEC     C
			INC     C
			RET     Z               ;NULL STRING
			LDIR
			RET     NC              ;STRING REPLACED
			LD      (FREE),DE
CHECK:			PUSH    HL
			LD      HL,(FREE)
			INC     H
			SBC     HL,SP
			POP     HL
			RET     C
			XOR     A
			JP      ERROR_           ;"No room"
;
STORS3:			LD      C,E
			PUSH    IX
			POP     DE
			XOR     A
			LD      B,A
			CP      C
			JR      Z,STORS5
			LDIR
STORS5:			LD      A,CR
			LD      (DE),A
			RET
;
;ARGUE: TRANSFER FN OR PROC ARGUMENTS FROM THE
; CALLING STATEMENT TO THE DUMMY VARIABLES VIA
; THE STACK.  IT MUST BE DONE THIS WAY TO MAKE
; PROCFRED(A,B)    DEF PROCFRED(B,A)     WORK.
;   Inputs: DE addresses parameter list 
;           IY addresses dummy variable list
;  Outputs: DE,IY updated
; Destroys: Everything
;
ARGUE:			LD      A,-1
			PUSH    AF              ;PUT MARKER ON STACK
ARGUE1:			INC     IY              ;BUMP PAST ( OR ,
			INC     DE
			PUSH    DE
			CALL    NXT
			CALL    GETVAR
			JR      C,ARGERR
			CALL    NZ,PUTVAR
			POP     DE
			PUSH    HL              ;VARPTR
			OR      A               ;TYPE
			PUSH    AF
			PUSH    DE
			EX      (SP),IY
			JP      M,ARGUE2        ;STRING
			CALL    EXPRN           ;PARAMETER VALUE
			EX      (SP),IY
			POP     DE
			POP     AF
			EXX
			PUSH    HL
			EXX
			PUSH    HL
			LD      B,A
			PUSH    BC
			CALL    CHECK           ;CHECK ROOM
			JR      ARGUE4
ARGUE2:			CALL    EXPRS
			EX      (SP),IY
			EXX
			POP     DE
			EXX
			POP     AF
			CALL    PUSHS
			EXX
ARGUE4:			CALL    NXT
			CP      ','
			JR      NZ,ARGUE5
			LD      A,(DE)
			CP      ','
			JR      Z,ARGUE1        ;ANOTHER
ARGERR:			LD      A,31
			JP      ERROR_           ;"Arguments"
ARGUE5:			CALL    BRAKET
			LD      A,(DE)
			CP      ')'
			JR      NZ,ARGERR
			INC     DE
			EXX
ARGUE6:			POP     BC
			LD      A,B
			INC     A
			EXX
			RET     Z               ;MARKER POPPED
			EXX
			DEC     A
			JP      M,ARGUE7        ;STRING
			POP     HL
			EXX
			POP     HL
			EXX
			POP     IX
			CALL    STORE           ;WRITE TO DUMMY
			JR      ARGUE6
ARGUE7:			CALL    POPS
			POP     IX
			CALL    STACCS
			JR      ARGUE6
;
;SAVLOC: SUBROUTINE TO STACK LOCAL PARAMETERS
;  OF A FUNCTION OR PROCEDURE.
;THERE IS A LOT OF STACK MANIPULATION - CARE!!
;   Inputs: IY is parameters pointer
;  Outputs: IY updated
; Destroys: A,B,C,D,E,H,L,IX,IY,F,SP
;
SAVLOC:			POP     DE              ;RETURN ADDRESS
SAVLO1:			INC     IY              ;BUMP PAST ( OR ,
			CALL    NXT
			PUSH    DE
			EXX
			PUSH    BC
			PUSH    DE
			PUSH    HL
			EXX
			CALL    VAR_             ;DUMMY VARIABLE
			EXX
			POP     HL
			POP     DE
			POP     BC
			EXX
			POP     DE
			OR      A               ;TYPE
			JP      M,SAVLO2        ;STRING
			EXX
			PUSH    HL              ;SAVE H'L'
			EXX
			LD      B,A             ;TYPE
			CALL    LOADN
			EXX
			EX      (SP),HL
			EXX
			PUSH    HL
			PUSH    BC
			JR      SAVLO4
SAVLO2:			PUSH    AF              ;STRING TYPE
			PUSH    DE
			EXX
			PUSH    HL
			EXX
			CALL    LOADS
			EXX
			POP     HL
			EXX
			LD      C,E
			POP     DE
			CALL    CHECK
			POP     AF              ;LEVEL STACK
			LD      HL,0
			LD      B,L
			SBC     HL,BC
			ADD     HL,SP
			LD      SP,HL
			LD      B,A             ;TYPE
			PUSH    BC
			JR      Z,SAVLO4
			PUSH    DE
			LD      DE,ACCS
			EX      DE,HL
			LD      B,L
			LDIR                    ;SAVE STRING ON STACK
			POP     DE
SAVLO4:			PUSH    IX              ;VARPTR
			CALL    SAVLO5
LOCCHK:			EQU     $
SAVLO5:			CALL    CHECK
			CALL    NXT
			CP      ','             ;MORE?
			JR      Z,SAVLO1
			EX      DE,HL
			JP      (HL)            ;"RETURN"
;
DELIM:			LD      A,(IY)          ;ASSEMBLER DELIMITER
			CP      ' '
			RET     Z
			CP      ','
			RET     Z
			CP      ')'
			RET     Z
TERM:			CP      ';'             ;ASSEMBLER TERMINATOR
			RET     Z
			CP      '\'
			RET     Z
			JR      TERM0
;
TERMQ:			CALL    NXT
			CP      ELSE_
			RET     NC
TERM0:			CP      ':'             ;ASSEMBLER SEPARATOR
			RET     NC
			CP      CR
			RET
;
SPAN:			CALL    TERMQ
			RET     Z
			INC     IY
			JR      SPAN
;
EQUALS:			CALL    NXT
			INC     IY
			CP      '='
			RET     Z
			LD      A,4
			JP      ERROR_           ;"Mistake"
;
FORMAT:			CP      TAB
			JR      Z,DOTAB
			CP      SPC
			JR      Z,DOSPC
			CP      '''
			RET     NZ
			CALL    CRLF
			XOR     A
			RET
;
DOTAB:			PUSH    BC
			CALL    EXPRI
			EXX
			POP     BC
			LD      A,(IY)
			CP      ','
			JR      Z,DOTAB1
			CALL    BRAKET
			LD      A,L
TABIT:			LD      HL,COUNT
			CP      (HL)
			RET     Z
			PUSH    AF
			CALL    C,CRLF
			POP     AF
			SUB     (HL)
			JR      FILL
DOTAB1:			INC     IY
			PUSH    BC
			PUSH    HL
			CALL    EXPRI
			EXX
			POP     DE
			POP     BC
			CALL    BRAKET
			CALL    PUTCSR
			XOR     A
			RET
;
DOSPC:			PUSH    BC
			CALL    ITEMI
			EXX
			LD      A,L
			POP     BC
FILL:			OR      A
			RET     Z
			PUSH    BC
			LD      B,A
FILL1:			LD      A,' '
			CALL    OUTCHR
			DJNZ    FILL1
			POP     BC
			XOR     A
			RET
;
PTEXT:			LD      HL,ACCS
			INC     E
PTEXT1:			DEC     E
			RET     Z
			LD      A,(HL)
			INC     HL
			CALL    OUTCHR
			JR      PTEXT1
;
FETCHS:			PUSH    AF
			PUSH    BC
			PUSH    HL
			EX      (SP),IY
			CALL    XTRACT
			CALL    NXT
			EX      (SP),IY
			POP     HL
			POP     BC
			POP     AF
			RET
;
LINES:			LD      DE,ACCS
LINE1S:			LD      A,(HL)
			LD      (DE),A
			CP      CR
			RET     Z
			INC     HL
			INC     E
			JR      LINE1S
;
XTRACT:			CALL    NXT
			CP      34		;ASCII ""
			INC     IY
			JP      Z,CONS
			DEC     IY
			LD      DE,ACCS
XTRAC1:			LD      A,(IY)
			LD      (DE),A
			CP      ','
			RET     Z
			CP      CR
			RET     Z
			INC     IY
			INC     E
			JR      XTRAC1
;
SEARCH:			LD      B,0
SRCH1:			LD      C,(HL)
			INC     C
			DEC     C
			JR      Z,SRCH2         ;FAIL
			INC     HL
			INC     HL
			INC     HL
			CP      (HL)
			RET     Z
			DEC     C
			DEC     C
			DEC     C
			ADD     HL,BC
			JP      SRCH1
SRCH2:			DEC     HL              ;POINT TO CR
			SCF
			RET
;
X4OR5:			CP      5
			LD      H,D
			LD      L,E
			ADD     HL,HL
			RET     C
			ADD     HL,HL
			RET     C
			EX      DE,HL
			RET     NZ
			ADD     HL,DE
			EX      DE,HL
			RET
;
MUL16:			EX      DE,HL
			LD      HL,0
			LD      A,16
MUL161:			ADD     HL,HL
			RET     C               ;OVERFLOW
			SLA     E
			RL      D
			JR      NC,MUL162
			ADD     HL,BC
			RET     C
MUL162:			DEC     A
			JR      NZ,MUL161
			RET
;
CHANEL:		CALL    NXT
			CP      '#'
			LD      A,45
			JP      NZ,ERROR_        ;"Missing #"
CHNL:			INC     IY              ;SKIP '#'
			CALL    ITEMI
			EXX
			EX      DE,HL
			RET
;
;ASSEMBLER:
;LANGUAGE-INDEPENDENT CONTROL SECTION:
; Outputs: A=delimiter, carry set if syntax error.
;
ASSEM:			CALL    SKIP
			INC     IY
			CP      ':'
			JR      Z,ASSEM
			CP      ']'
			RET     Z
			CP      CR
			RET     Z
			DEC     IY
			LD      IX,(PC)         ;PROGRAM COUNTER
			LD      HL,LISTON
			BIT     6,(HL)
			JR      Z,ASSEM0
			LD      IX,(OC)         ;ORIGIN of CODE
ASSEM0:			PUSH    IX
			PUSH    IY
			CALL    ASMB
			POP     BC
			POP     DE
			RET     C
			CALL    SKIP
			SCF
			RET     NZ
			DEC     IY
ASSEM3:			INC     IY
			LD      A,(IY)
			CALL    TERM0
			JR      NZ,ASSEM3
			LD      A,(LISTON)
			PUSH    IX
			POP     HL
			OR      A
			SBC     HL,DE
			EX      DE,HL           ;DE= NO. OF BYTES
			PUSH    HL
			LD      HL,(PC)
			PUSH    HL
			ADD     HL,DE
			LD      (PC),HL         ;UPDATE PC
			BIT     6,A
			JR      Z,ASSEM5
			LD      HL,(OC)
			ADD     HL,DE
			LD      (OC),HL         ;UPDATE OC
ASSEM5:			POP     HL              ;OLD PC
			POP     IX              ;CODE HERE
			BIT     4,A
			JR      Z,ASSEM
			LD      A,H
			CALL    HEX
			LD      A,L
			CALL    HEXSP
			XOR     A
			CP      E
			JR      Z,ASSEM2
ASSEM1:			LD      A,(COUNT)
			CP      17
			LD      A,5
			CALL    NC,TABIT        ;NEXT LINE
			LD      A,(IX)
			CALL    HEXSP
			INC     IX
			DEC     E
			JR      NZ,ASSEM1
ASSEM2:			LD      A,18
			CALL    TABIT
			PUSH    IY
			POP     HL
			SBC     HL,BC
ASSEM4:			LD      A,(BC)
			CALL    OUT_
			INC     BC
			DEC     L
			JR      NZ,ASSEM4
			CALL    CRLF
			JP      ASSEM
;
HEXSP:			CALL    HEX
			LD      A,' '
			JR      OUTCH1
HEX:			PUSH    AF
			RRCA
			RRCA
			RRCA
			RRCA
			CALL    HEXOUT
			POP     AF
HEXOUT:			AND     0FH
			ADD     A,90H
			DAA
			ADC     A,40H
			DAA
OUTCH1:			JP      OUT_
;
;PROCESSOR-SPECIFIC TRANSLATION SECTION:
;
;REGISTER USAGE: B - TYPE OF MOST RECENT OPERAND
;                C - OPCODE BEING BUILT
;                D - (IX) OR (IY) FLAG
;                E - OFFSET FROM IX OR IY
;               HL - NUMERIC OPERAND VALUE
;               IX - CODE DESTINATION
;               IY - SOURCE TEXT POINTER
;   Inputs: A = initial character
;  Outputs: Carry set if syntax error.
;
ASMB:			CP      '.'
			JR      NZ,ASMB1
			INC     IY
			PUSH    IX
			CALL    VAR_
			PUSH    AF
			CALL    ZERO
			EXX
			LD      HL,(PC)
			EXX
			POP     AF
			CALL    STORE
			POP     IX
ASMB1:			CALL    SKIP
			RET     Z
			CP      TCALL
			LD      C,0C4H
			INC     IY
			JP      Z,GRPC
			DEC     IY
			LD      HL,OPCODS
			CALL    FIND
			RET     C
			LD      C,B     ;ROOT OPCODE
			LD      D,0     ;CLEAR IX/IY FLAG
;
;GROUP 0 - TRIVIAL CASES REQUIRING NO COMPUTATION
;GROUP 1 - AS GROUP 0 BUT WITH "ED" PREFIX
;
			SUB     39
			JR      NC,GROUP2
			CP      15-39
			CALL    NC,ED
			JR      BYTE0
;
;GROUP 2 - BIT, RES, SET
;GROUP 3 - RLC, RRC, RL, RR, SLA, SRA, SRL
;
GROUP2:			SUB     10
			JR      NC,GROUP4
			CP      3-10
			CALL    C,BIT_
			RET     C
			CALL    REGLO
			RET     C
			CALL    CB
			JR      BYTE0
;
;GROUP 4 - PUSH, POP, EX (SP)
;
GROUP4:			SUB     3
			JR      NC,GROUP5
G4:			CALL    PAIR
			RET     C
			JR      BYTE0
;
;GROUP 5 - SUB, AND, XOR, OR, CP
;GROUP 6 - ADD, ADC, SBC
;
GROUP5:			SUB     8+2
			JR      NC,GROUP7
			CP      5-8
			LD      B,7
			CALL    NC,OPND
			LD      A,B
			CP      7
			JR      NZ,G6HL
G6:			CALL    REGLO
			LD      A,C
			JR      NC,BIND1
			XOR     46H
			CALL    BIND
DB_:			CALL    NUMBER
			JR      VAL8
;
G6HL:			AND     3FH
			CP      12
			SCF
			RET     NZ
			LD      A,C
			CP      80H
			LD      C,9
			JR      Z,G4
			XOR     1CH
			RRCA
			LD      C,A
			CALL    ED
			JR      G4
;
;GROUP 7 - INC, DEC
;
GROUP7:			SUB     2
			JR      NC,GROUP8
			CALL    REGHI
			LD      A,C
BIND1:			JP      NC,BIND
			XOR     64H
			RLCA
			RLCA
			RLCA
			LD      C,A
			CALL    PAIR1
			RET     C
BYTE0:			LD      A,C
			JR      BYTE2
;
;GROUP 8 - IN
;GROUP 9 - OUT
;
GROUP8:			SUB     2
			JR      NC,GROUPA
			CP      1-2
			CALL    Z,CORN
			EX      AF,AF'
			CALL    REGHI
			RET     C
			EX      AF,AF'
			CALL    C,CORN
			INC     H
			JR      Z,BYTE0
			LD      A,B
			CP      7
			SCF
			RET     NZ
			LD      A,C
			XOR     3
			RLCA
			RLCA
			RLCA
			CALL    BYTE_
			JR      VAL8
;
;GROUP 10 - JR, DJNZ
;
GROUPA:			SUB     2
			JR      NC,GROUPB
			CP      1-2
			CALL    NZ,COND_
			LD      A,C
			JR      NC,GRPA
			LD      A,18H
GRPA:			CALL    BYTE_
			CALL    NUMBER
			LD      DE,(PC)
			INC     DE
			SCF
			SBC     HL,DE
			LD      A,L
			RLA
			SBC     A,A
			CP      H
TOOFAR:			LD      A,1
			JP      NZ,ERROR_        ;"Out of range"
VAL8:			LD      A,L
			JR      BYTE2
;
;GROUP 11 - JP
;
GROUPB:			LD      B,A
			JR      NZ,GROUPC
			CALL    COND_
			LD      A,C
			JR      NC,GRPB
			LD      A,B
			AND     3FH
			CP      6
			LD      A,0E9H
			JR      Z,BYTE2
			LD      A,0C3H
GRPB:		CALL    BYTE_
			JR		ADDR_
;
;GROUP 12 - CALL
;
GROUPC:			DJNZ    GROUPD
GRPC:			CALL    GRPE
ADDR_:			CALL    NUMBER
VAL16:			CALL    VAL8
			LD      A,H
			JR      BYTE2
;
;GROUP 13 - RST
;
GROUPD:			DJNZ    GROUPE
			CALL    NUMBER
			AND     C
			OR      H
			JR      NZ,TOOFAR
			LD      A,L
			OR      C
BYTE2:  		JR      BYTE1
;
;GROUP 14 - RET
;
GROUPE:			DJNZ    GROUPF
GRPE:			CALL    COND_
			LD      A,C
			JR      NC,BYTE1
			OR      9
			JR      BYTE1
;
;GROUP 15 - LD
;
GROUPF:			DJNZ    MISC
			CALL    LDOP
			JR      NC,LDA
			CALL    REGHI
			EX      AF,AF'
			CALL    SKIP
			CP      '('
			JR      Z,LDIN
			EX      AF,AF'
			JP      NC,G6
			LD      C,1
			CALL    PAIR1
			RET     C
			LD      A,14
			CP      B
			LD      B,A
			CALL    Z,PAIR
			LD      A,B
			AND     3FH
			CP      12
			LD      A,C
			JR      NZ,GRPB
			LD      A,0F9H
			JR      BYTE1
;
LDIN:			EX      AF,AF'
			PUSH    BC
			CALL    NC,REGLO
			LD      A,C
			POP     BC
			JR      NC,BIND
			LD      C,0AH
			CALL    PAIR1
			CALL    LD16
			JR      NC,GRPB
			CALL    NUMBER
			LD      C,2
			CALL    PAIR
			CALL    LD16
			RET     C
			CALL    BYTE_
			JR      VAL16
;
;OPT - SET OPTION
;
OPT:			DEC     B
			JP      Z,DB_
			DJNZ    ADDR_
			CALL    NUMBER
			LD      HL,LISTON
			LD      C,A
			RLD
			LD      A,C
			RRD
			RET
;
LDA:			CP      4
			CALL    C,ED
			LD      A,B
BYTE1:			JR      BYTE_
;
;MISC - DB, DEFW, DB
;
MISC:			DJNZ    OPT
			PUSH    IX
			CALL    EXPRS
			POP     IX
			LD      HL,ACCS
DB1:			XOR     A
			CP      E
			RET     Z
			LD      A,(HL)
			INC     HL
			CALL    BYTE_
			DEC     E
			JR      DB1
;
;SUBROUTINES:
;
LD16:			LD      A,B
			JR      C,LD8
			LD      A,B
			AND     3FH
			CP      12
			LD      A,C
			RET     Z
			CALL    ED
			LD      A,C
			OR      43H
			RET
;
LD8:			CP      7
			SCF
			RET     NZ
			LD      A,C
			OR      30H
			RET
;
CORN:			PUSH    BC
			CALL    OPND
			BIT     5,B
			POP     BC
			JR      Z,NUMBER
			LD      H,-1
ED:			LD      A,0EDH
			JR      BYTE_
;
CB:			LD      A,0CBH
BIND:			CP      76H
			SCF
			RET     Z               ;REJECT LD (HL),(HL)
			CALL    BYTE_
			INC     D
			RET     P
			LD      A,E
			JR      BYTE_
;
OPND:			PUSH    HL
			LD      HL,OPRNDS
			CALL    FIND
			POP     HL
			RET     C
			BIT     7,B
			RET     Z
			BIT     3,B
			PUSH    HL
			CALL    Z,OFFSET
			LD      E,L
			POP     HL
			LD      A,0DDH
			BIT     6,B
			JR      Z,OP1
			LD      A,0FDH
OP1:			OR      A
			INC     D
			LD      D,A
			RET     M
BYTE_:			LD      (IX),A
			INC     IX
			OR      A
			RET
;
OFFSET:			LD      A,(IY)
			CP      ')'
			LD      HL,0
			RET     Z
NUMBER:			CALL    SKIP
			PUSH    BC
			PUSH    DE
			PUSH    IX
			CALL    EXPRI
			POP     IX
			EXX
			POP     DE
			POP     BC
			LD      A,L
			OR      A
			RET
;
REG:			CALL    OPND
			RET     C
			LD      A,B
			AND     3FH
			CP      8
			CCF
			RET
;
REGLO:			CALL    REG
			RET     C
			JR      ORC
;
REGHI:			CALL    REG
			RET     C
			JR      SHL3
;
COND_:			CALL    OPND
			RET     C
			LD      A,B
			AND     1FH
			SUB     16
			JR      NC,SHL3
			CP      -15
			SCF
			RET     NZ
			LD      A,3
			JR      SHL3
;
PAIR:			CALL    OPND
			RET     C
PAIR1:			LD      A,B
			AND     0FH
			SUB     8
			RET     C
			JR      SHL3
;
BIT_:			CALL    NUMBER
			CP      8
			CCF
			RET     C
SHL3:			RLCA
			RLCA
			RLCA
ORC:			OR      C
			LD      C,A
			RET
;
LDOP:			LD      HL,LDOPS
FIND:			CALL    SKIP
EXIT_:			LD      B,0
			SCF
			RET     Z
			CP      DEF_
			JR      Z,FIND0
			CP      TOR+1
			CCF
			RET     C
FIND0:			LD      A,(HL)
			OR      A
			JR      Z,EXIT_
			XOR     (IY)
			AND     01011111B
			JR      Z,FIND2
FIND1:			BIT     7,(HL)
			INC     HL
			JR      Z,FIND1
			INC     HL
			INC     B
			JR      FIND0
;
FIND2:			PUSH    IY
FIND3:			BIT     7,(HL)
			INC     IY
			INC     HL
			JR      NZ,FIND5
			CP      (HL)
			CALL    Z,SKIP0
			LD      A,(HL)
			XOR     (IY)
			AND     01011111B
			JR      Z,FIND3
FIND4:			POP     IY
			JR      FIND1
;
FIND5:			CALL    DELIM
			CALL    NZ,SIGN
			JR      NZ,FIND4
FIND6:			LD      A,B
			LD      B,(HL)
			POP     HL
			RET
;
SKIP0:			INC     HL
SKIP:			CALL    DELIM
			RET     NZ
			CALL    TERM
			RET     Z
			INC     IY
			JR      SKIP
;
SIGN:			CP      '+'
			RET     Z
			CP      '-'
			RET
;
;.XLIST
OPCODS:			DB    'NO'
			DB    'P'+80H
			DB    0
			DB    'RLC'
			DB    'A'+80H
			DB    7
			DB    'EX'
			DB    0
			DB    'AF'
			DB    0
			DB    'AF'
			DB    '''+80H
			DB    8
			DB    'RRC'
			DB    'A'+80H
			DB    0FH
			DB    'RL'
			DB    'A'+80H
			DB    17H
			DB    'RR'
			DB    'A'+80H
			DB    1FH
			DB    'DA'
			DB    'A'+80H
			DB    27H
			DB    'CP'
			DB    'L'+80H
			DB    2FH
			DB    'SC'
			DB    'F'+80H
			DB    37H
			DB    'CC'
			DB    'F'+80H
			DB    3FH
			DB    'HAL'
			DB    'T'+80H
			DB    76H
			DB    'EX'
			DB    'X'+80H
			DB    0D9H
			DB    'EX'
			DB    0
			DB    'DE'
			DB    0
			DB    'H'
			DB    'L'+80H
			DB    0EBH
			DB    'D'
			DB    'I'+80H
			DB    0F3H
			DB    'E'
			DB    'I'+80H
			DB    0FBH
;
			DB    'NE'
			DB    'G'+80H
			DB    44H
			DB    'IM'
			DB    0
			DB    '0'+80H
			DB    46H
			DB    'RET'
			DB    'N'+80H
			DB    45H
			DB    'RET'
			DB    'I'+80H
			DB    4DH
			DB    'IM'
			DB    0
			DB    '1'+80H
			DB    56H
			DB    'IM'
			DB    0
			DB    '2'+80H
			DB    5EH
			DB    'RR'
			DB    'D'+80H
			DB    67H
			DB    'RL'
			DB    'D'+80H
			DB    6FH
			DB    'LD'
			DB    'I'+80H
			DB    0A0H
			DB    'CP'
			DB    'I'+80H
			DB    0A1H
			DB    'IN'
			DB    'I'+80H
			DB    0A2H
			DB    'OUT'
			DB    'I'+80H
			DB    0A3H
			DB    'LD'
			DB    'D'+80H
			DB    0A8H
			DB    'CP'
			DB    'D'+80H
			DB    0A9H
			DB    'IN'
			DB    'D'+80H
			DB    0AAH
			DB    'OUT'
			DB    'D'+80H
			DB    0ABH
			DB    'LDI'
			DB    'R'+80H
			DB    0B0H
			DB    'CPI'
			DB    'R'+80H
			DB    0B1H
			DB    'INI'
			DB    'R'+80H
			DB    0B2H
			DB    'OTI'
			DB    'R'+80H
			DB    0B3H
			DB    'LDD'
			DB    'R'+80H
			DB    0B8H
			DB    'CPD'
			DB    'R'+80H
			DB    0B9H
			DB    'IND'
			DB    'R'+80H
			DB    0BAH
			DB    'OTD'
			DB    'R'+80H
			DB    0BBH
;
			DB    'BI'
			DB    'T'+80H
			DB    40H
			DB    'RE'
			DB    'S'+80H
			DB    80H
			DB    'SE'
			DB    'T'+80H
			DB    0C0H
;
			DB    'RL'
			DB    'C'+80H
			DB    0
			DB    'RR'
			DB    'C'+80H
			DB    8
			DB    'R'
			DB    'L'+80H
			DB    10H
			DB    'R'
			DB    'R'+80H
			DB    18H
			DB    'SL'
			DB    'A'+80H
			DB    20H
			DB    'SR'
			DB    'A'+80H
			DB    28H
			DB    'SR'
			DB    'L'+80H
			DB    38H
;
			DB    'PO'
			DB    'P'+80H
			DB    0C1H
			DB    'PUS'
			DB    'H'+80H
			DB    0C5H
			DB    'EX'
			DB    0
			DB    '(S'
			DB    'P'+80H
			DB    0E3H
;
			DB    'SU'
			DB    'B'+80H
			DB    90H
			DB    'AN'
			DB    'D'+80H
			DB    0A0H
			DB    'XO'
			DB    'R'+80H
			DB    0A8H
			DB    'O'
			DB    'R'+80H
			DB    0B0H
			DB    'C'
			DB    'P'+80H
			DB    0B8H
			DB    TAND
			DB    0A0H
			DB    TOR
			DB    0B0H
;
			DB    'AD'
			DB    'D'+80H
			DB    80H
			DB    'AD'
			DB    'C'+80H
			DB    88H
			DB    'SB'
			DB    'C'+80H
			DB    98H
;
			DB    'IN'
			DB    'C'+80H
			DB    4
			DB    'DE'
			DB    'C'+80H
			DB    5
;
			DB    'I'
			DB    'N'+80H
			DB    40H
			DB    'OU'
			DB    'T'+80H
			DB    41H
;
			DB    'J'
			DB    'R'+80H
			DB    20H
			DB    'DJN'
			DB    'Z'+80H
			DB    10H
;
			DB    'J'
			DB    'P'+80H
			DB    0C2H
;
			DB    'CAL'
			DB    'L'+80H
			DB    0C4H
;
			DB    'RS'
			DB    'T'+80H
			DB    0C7H
;
			DB    'RE'
			DB    'T'+80H
			DB    0C0H
;
			DB    'L'
			DB    'D'+80H
			DB    40H
;
			DB    DEF_ & 7FH
			DB    'M'+80H
			DB    0
;
			DB    DEF_ & 7FH
			DB    'B'+80H
			DB    0
;
			DB    'OP'
			DB    'T'+80H
			DB    0
;
			DB    DEF_ & 7FH
			DB    'W'+80H
			DB    0
;
			DB    0
;
OPRNDS:			DB    'B'+80H
			DB    0
			DB    'C'+80H
			DB    1
			DB    'D'+80H
			DB    2
			DB    'E'+80H
			DB    3
			DB    'H'+80H
			DB    4
			DB    'L'+80H
			DB    5
			DB    '(H'
			DB    'L'+80H
			DB    6
			DB    'A'+80H
			DB    7
			DB    '(I'
			DB    'X'+80H
			DB    86H
			DB    '(I'
			DB    'Y'+80H
			DB    0C6H
;
			DB    'B'
			DB    'C'+80H
			DB    8
			DB    'D'
			DB    'E'+80H
			DB    10
			DB    'H'
			DB    'L'+80H
			DB    12
			DB    'I'
			DB    'X'+80H
			DB    8CH
			DB    'I'
			DB    'Y'+80H
			DB    0CCH
			DB    'A'
			DB    'F'+80H
			DB    14
			DB    'S'
			DB    'P'+80H
			DB    14
;
			DB    'N'
			DB    'Z'+80H
			DB    16
			DB    'Z'+80H
			DB    17
			DB    'N'
			DB    'C'+80H
			DB    18
			DB    'P'
			DB    'O'+80H
			DB    20
			DB    'P'
			DB    'E'+80H
			DB    21
			DB    'P'+80H
			DB    22
			DB    'M'+80H
			DB    23
;
			DB    '('
			DB    'C'+80H
			DB    20H
;
			DB    0
;
LDOPS:			DB    'I'
			DB    0
			DB    'A'+80H
			DB    47H
			DB    'R'
			DB    0
			DB    'A'+80H
			DB    4FH
			DB    'A'
			DB    0
			DB    'I'+80H
			DB    57H
			DB    'A'
			DB    0
			DB    'R'+80H
			DB    5FH
			DB    '(BC'
			DB    0
			DB    'A'+80H
			DB    2
			DB    '(DE'
			DB    0
			DB    'A'+80H
			DB    12H
			DB    'A'
			DB    0
			DB    '(B'
			DB    'C'+80H
			DB    0AH
			DB    'A'
			DB    0
			DB    '(D'
			DB    'E'+80H
			DB    1AH
;
			DB    0
;
; .LIST
;
LF:			EQU     0AH
CR:			EQU     0DH