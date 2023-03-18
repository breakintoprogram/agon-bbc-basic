;
; Title:	BBC Basic for AGON - Initialisation Code
;		Initialisation Code
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	17/03/2023
;
; Modinfo:
; 14/07/2022:	Modified to run in MOS
; 15/10/2022:	Added RST_08 and RST_10 handlers
; 22/11/2022:	Added MOS header block
; 12/01/2023:	Added MOS C-style parameter processing routines
; 17/03/2023:	Added RST_18 handler

			SEGMENT __VECTORS
			
			XREF	_main
		
			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"
			
argv_ptrs_max:		EQU	16				; Maximum number of arguments allowed in argv
			
;
; Start in mixed mode. Assumes MBASE is set to correct segment
;	
			JP	_start				; Jump to start
			DS	5

RST_08:			RST.LIS	08h				; API call
			RET
			DS 	5
			
RST_10:			RST.LIS 10h				; Output
			RET
			DS	5
			
RST_18:			RST.LIS	18h				; Block Output
			RET
			DS	5
			
RST_20:			DS	8
RST_28:			DS	8
RST_30:			DS	8	

;	
; The NMI interrupt vector (not currently used by AGON)
;
RST_38:			EI
			RETI
;
; The header stuff is from byte 64 onwards
;
			ALIGN	64
			
			DB	"MOS"				; Flag for MOS - to confirm this is a valid MOS command
			DB	00h				; MOS header version 0
			DB	00h				; Flag for run mode (0: Z80, 1: ADL)
			
_exec_name:		DB	"BBCBASIC.BIN", 0		; The executable name, only used in argv	
	
;
; And the code follows on immediately after the header
;
_start:			PUSH.LIL	IY			; Preserve IY

			LD		IY, 0			; Preserve SPS
			ADD		IY, SP
			PUSH.LIL	IY
			LD		SP, 8000h		; And set to 8000h, top of the MOS command area
	
			PUSH		AF			; Preserve the rest of the registers
			PUSH.LIL	BC
			PUSH.LIL	DE
			PUSH.LIL	IX

			LD		A, MB			; Segment base
			LD		IX, argv_ptrs		; The argv array pointer address
			CALL		_set_aix24		; Convert to a 24-bit address			
			PUSH.LIL	IX
			CALL		_parse_params		; Parse the parameters
			POP.LIL		IX			; IX: argv
			LD		B, 0			;  C: argc
			CALL		_main			; Start user code

			POP.LIL		IX			; Restore the registers
			POP.LIL		DE
			POP.LIL		BC
			POP		AF

			POP.LIL		IY			; Get the preserved SPS
			LD		SP, IY			; Restore the SP
			
			POP.LIL		IY			; Restore IY
			RET.L					; Return to MOS
			
; Parse the parameter string into a C array
; Parameters
; -   A: Segment base
; - HLU: Address of parameter string
; - IXU: Address for array pointer storage
; Returns:
; -   C: Number of parameters parsed
;
_parse_params:		LD		BC, _exec_name		; Get the address of the app name in this segment			
			CALL		_set_abc24		; Convert it to a 24-bit address based upon segment base
			LD.LIL		(IX+0), BC		; ARGV[0] = the executable name
			INC.LIL		IX
			INC.LIL		IX
			INC.LIL		IX
			CALL		_skip_spaces		; Skip HL past any leading spaces
;
			LD		BC, 1			; C: ARGC = 1 - also clears out top 16 bits of BCU
			LD		B, argv_ptrs_max - 1	; B: Maximum number of argv_ptrs
;
_parse_params_1:	PUSH		BC			; Stack ARGC	
			PUSH.LIL	HL			; Stack start address of token
			CALL		_get_token		; Get the next token
			LD		A, C			; A: Length of the token in characters
			POP.LIL		DE			; Start address of token (was in HL)
			POP		BC			; ARGC
			OR		A			; Check for A=0 (no token found) OR at end of string
			RET		Z
;
			LD.LIL		(IX+0), DE		; Store the pointer to the token
			PUSH.LIL	HL			; DE=HL
			POP.LIL		DE
			CALL		_skip_spaces		; And skip HL past any spaces onto the next character
			XOR		A
			LD.LIL		(DE), A			; Zero-terminate the token
			INC.LIL		IX
			INC.LIL		IX
			INC.LIL		IX			; Advance to next pointer position
			INC		C			; Increment ARGC
			LD		A, C			; Check for C >= A
			CP		B
			JR		C, _parse_params_1	; And loop
			RET

; Get the next token
; Parameters:
; - HL: Address of parameter string
; Returns:
; - HL: Address of first character after token
; -  C: Length of token (in characters)
;
_get_token:		LD		C, 0			; Initialise length
$$:			LD.LIL		A, (HL)			; Get the character from the parameter string
			OR		A			; Exit if 0 (end of parameter string in MOS)
			RET 		Z
			CP		13			; Exit if CR (end of parameter string in BBC BASIC)
			RET		Z
			CP		' '			; Exit if space (end of token)
			RET		Z
			INC.LIL		HL			; Advance to next character
			INC 		C			; Increment length
			JR		$B
	
; Skip spaces in the parameter string
; Parameters:
; - HL: Address of parameter string
; Returns:
; - HL: Address of next none-space character
;    F: Z if at end of string, otherwise NZ if there are more tokens to be parsed
;
_skip_spaces:		LD.LIL		A, (HL)			; Get the character from the parameter string	
			CP		' '			; Exit if not space
			RET		NZ
			INC.LIL		HL			; Advance to next character
			JR		_skip_spaces		; Increment length
			
; Set the MSB of BC (U) to A
; Parameters:
; - BC: 16-bit address
; -  A: Value to stick in U of BC
; Returns:
; - BCU
;
_set_abc24:		PUSH.LIL	HL			; Preserve HL
			PUSH.LIL	BC			; Stick BC onto SPL
			LD.LIL		HL, 2			; HL: SP+2
			ADD.LIL		HL, SP
			LD.LIL		(HL), A			; Store A in it
			POP.LIL		BC			; Fetch ammended BC
			POP.LIL		HL			; Restore HL
			RET

; Set the MSB of BC (U) to A
; Parameters:
; - IX: 16-bit address
; -  A: Value to stick in U of BC
; Returns:
; - IXU
;
_set_aix24:		PUSH.LIL	IX			; Stick IX onto SPL
			LD.LIL		IX, 2			; IX: SP+2
			ADD.LIL		IX, SP
			LD.LIL		(IX), A			; Store A in it
			POP.LIL		IX			; Fetch ammended IX
			RET
			
; Storage for the argv array pointers
;
argv_ptrs:		BLKP	argv_ptrs_max, 0		; Storage for the argv array pointers
