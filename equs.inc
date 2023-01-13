;
; Title:	BBC Basic for AGON - Equs
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	12/01/2023
;
; Modinfo:
; 12/01/2023:	Removed some redundant entries

			XREF		STAVAR
			XREF		ACCS
			
RAM_Top:		EQU		0FF00h
Stack_Top:		EQU		00000h	; Stack at top
	
; For GPIO
; PA not available on eZ80L92
;
PA_DR:			EQU		96h
PA_DDR:			EQU		97h
PA_ALT1:		EQU		98h
PA_ALT2:		EQU		99h
PB_DR:          	EQU		9Ah
PB_DDR:        	 	EQU		9Bh
PB_ALT1:        	EQU		9Ch
PB_ALT2:        	EQU		9Dh
PC_DR:          	EQU		9Eh
PC_DDR:         	EQU		9Fh
PC_ALT1:        	EQU		A0h
PC_ALT2:        	EQU		A1h
PD_DR:          	EQU		A2h
PD_DDR:			EQU		A3h
PD_ALT1:		EQU		A4h
PD_ALT2:		EQU		A5h
	
GPIOMODE_OUT:		EQU		0	; Output
GPIOMODE_IN:		EQU		1	; Input
GPIOMODE_DIO:		EQU		2	; Open Drain IO
GPIOMODE_SIO:		EQU		3	; Open Source IO
GPIOMODE_INTD:		EQU		4	; Interrupt, Dual Edge
GPIOMODE_ALTF:		EQU		5;	; Alt Function
GPIOMODE_INTAL:		EQU		6	; Interrupt, Active Low
GPIOMODE_INTAH:		EQU		7	; Interrupt, Active High
GPIOMODE_INTFE:		EQU		8	; Interrupt, Falling Edge
GPIOMODE_INTRE:		EQU		9	; Interrupt, Rising Edge
	
; Originally in ram.asm
;
OC:			EQU     STAVAR+15*4     ; CODE ORIGIN (O%)
PC:			EQU     STAVAR+16*4     ; PROGRAM COUNTER (P%)
VDU_BUFFER:		EQU	ACCS		; Storage for VDU commands

; Originally in main.asm
;
CR:			EQU     0DH
LF:			EQU     0AH
ESC:			EQU     1BH
