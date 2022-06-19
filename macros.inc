;
; Title:	BBC Basic Interpreter - Z80 version
;		Useful macros
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:

EXREG:			MACRO	rp1, rp2
			PUSH 	rp1 
			POP	rp2
			ENDMACRO 
	
ADD8U_DE:		MACRO 	reg
			ADD	A, E 
			LD	E, A 
			ADC	A, D
			SUB	E
			LD	D, A 
			ENDMACRO 

ADD8U_HL:		MACRO 	reg
			ADD	A, L 
			LD	L, A 
			ADC	A, H
			SUB	L
			LD	H, A 
			ENDMACRO 

VDU:			MACRO	VAL
			LD	A, VAL
			CALL	OSWRCH
			ENDMACRO

SET_GPIO:		MACRO	REG, VAL
			IN0	A,(REG)
			OR	VAL
			OUT0	(REG),A
			ENDMACRO
				
RES_GPIO:		MACRO	REG, VAL
			PUSH	BC
			LD	A, VAL
			CPL
			LD	C, A
			IN0	A,(REG)
			AND	C
			OUT0	(REG),A
			POP	BC
			ENDMACRO
			