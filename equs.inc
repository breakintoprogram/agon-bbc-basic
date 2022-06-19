;
; Title:	BBC Basic for AGON - Equs
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:

			XREF	STAVAR
			XREF	ACCS
				
RAM_Top:		EQU		0FF00h
Stack_Top:		EQU		00000h	; Stack at top
CHAR_COLS:		EQU		40	; Screen width in chars
CHAR_ROWS:		EQU		30	; Screen height in chars
	
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
	
; For serial.asm
; 
BASE_CLOCK		EQU	24000000	; It's actually 48000000 in the Project Settings

BAUD_500000		EQU	BASE_CLOCK / (16 * 500000)
BAUD_250000		EQU	BASE_CLOCK / (16 * 250000)
BAUD_125000		EQU	BASE_CLOCK / (16 * 125000)
BAUD_19200		EQU	BASE_CLOCK / (16 * 19200)	

; For interrupts.asm
;

;UARTs
;
UART0_IVECT		EQU		18h
UART1_IVECT		EQU		1Ah

;Ports
;
PB0_IVECT   		EQU   	30h	; AGON ITRP Interrupt   (Pin 28/IO17 of the ESP32)
PB1_IVECT  	  	EQU  	32h	; AGON VBLANK Interrupt (Pin 23/IO15 of the ESP32)
PB2_IVECT  	  	EQU   	34h
PB3_IVECT  	  	EQU   	36h
PB4_IVECT    		EQU   	38h
PB5_IVECT    		EQU   	3Ah
PB6_IVECT    		EQU   	3Ch
PB7_IVECT    		EQU   	3Eh
                       
PC0_IVECT    		EQU   	40h
PC1_IVECT    		EQU   	42h
PC2_IVECT    		EQU   	44h
PC3_IVECT    		EQU   	46h
PC4_IVECT    		EQU   	48h
PC5_IVECT    		EQU   	4Ah
PC6_IVECT    		EQU   	4Ch
PC7_IVECT    		EQU   	4Eh
                       
PD0_IVECT    		EQU   	50h
PD1_IVECT    		EQU   	52h
PD2_IVECT    		EQU   	54h
PD3_IVECT    		EQU   	56h
PD4_IVECT    		EQU   	58h
PD5_IVECT    		EQU   	5Ah
PD6_IVECT    		EQU   	5Ch
PD7_IVECT    		EQU   	5Eh

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
