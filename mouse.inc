;***************************************************************************
;                                                                          *
;   Copyright (C) 1983,1984 by Microsoft Inc.                              *
;                                                                          *
;***************************************************************************

;***************************************************************************
;                                                                          *
;   Header file for Microsoft mouse device driver.  Contains all machine   *
;   specific constant and data structure definitions.                      *
;                                                                          *
;***************************************************************************

extrn	__ROMBIOS	:abs

wptr		equ	word ptr
bptr		equ	byte ptr
dptr		equ	dword ptr


WF_PMODE	equ	01h		;Running in protect mode.


MAX_INT_SIZE	equ	210		;Max. size of interrupt routine


ACK_PORT	equ	20h		;8259 master acknowledge port
MASK_PORT	equ	21h
ACK_SLAVE_PORT	equ	0A0h		;8259 slave acknowledge port
MASK_SLAVE_PORT equ	0A1h
EOI		equ	20h		;8259 end of interrupt
INT_REQUEST	equ	0ah		;8259 interrupt request register select
IN_SERVICE	equ	0bh		;8259 in service register select value


;	Symbolic equates for contents of the MOUSEINFO data structure

NUMBER_BUTTONS	equ	2
X_SPEED 	equ	2
Y_SPEED 	equ	2


io_delay macro
	jmp	short $+2
	jmp	short $+2
	jmp	short $+2
	endm



BIOSDataSeg	segment at 40h		;BIOS data areas of the IBM PC/XT/AT
rs232_data	dw	4    dup (?)
		org	6Ch
bios_time	db	?
BIOSDataSeg	ends



;	Definitions of the status word passed to the event_proc
;	in the AX register.  The registers passed to the event_proc
;	are as follows:
;
;		AX = status as defined below
;		BX = delta X or normalized abs
;		CX = delta Y or normalized abs
;		DX = number of buttons
;
;	If the SF_ABSOLUTE bit is clear in the status word, then the
;	BX and CX are reporting relative motion from the last reported
;	position.  If this bit is set, then BX and CX contain normalized
;	absolute coordinates between 0 and +65535, which will be mapped
;	by the event_proc onto the display surface


SF_MOVEMENT	equ	0001h		;Movement occured
SF_B1_DOWN	equ	0002h		;Button 1 (SW1) changed to down
SF_B1_UP	equ	0004h		;Button 1 (SW1) changed to up
SF_B2_DOWN	equ	0008h		;Button 2 (SW3) changed to down
SF_B2_UP	equ	0010h		;Button 2 (SW3) changed to up
SF_ABSOLUTE	equ	8000h		;BX,CX are normalized absolute coords


;	Definitions of the flag byte mouse_flags

MF_ENABLED	equ	00000001b	;Mouse is enabled
MF_INT33H	equ	00000010b	;Int 33h mouse found
;		equ	00000100b	;
;		equ	00001000b	;
;		equ	00010000b	;
;		equ	00100000b	;
MF_ON_SLAVEPIC  equ	01000000b	;Mouse is on slave PIC
MF_MOUSE_EXISTS equ	10000000b	;Mouse was found at boot time


;	Definitions of mouse types for mouse_type variable

MT_NO_MOUSE	equ	0		;Zero only if MF_MOUSE_EXISTS = 0
MT_BUS		equ	1
MT_SERIAL	equ	2
MT_INPORT	equ	3
MT_PS2		equ	4
MT_HP		equ	5

;	Windows/386 mouse VxD device ID.

VMD_DEVICE_ID	equ	0000Ch


display_int_size  macro   s
if2
%out size of interrupt handler is s bytes
endif
endm


;	INT 33h (Mouse) Function Numbers

MOUSE_SYS_VEC	equ	33h		;Software int to call MOUSE.SYS driver
CALL_ON_ANY_INT equ	1Fh		;  Call us on any interrupt
CLEAR_COUNTERS	equ	11		;  Clear dX and dY counters
BASH_INT_PROC	equ	12		;  Overwrite the interrupt routine
SWAP_INT_PROC	equ	20		;  Swap interrupt routine
INT33H_RESET	equ	00		;  Reset mouse
INT33H_ENABLE	equ	32		;  Enable mouse
INT33H_GETINFO  equ	24h		;  Get mouse information

; type of the mouse as reported by the INT 33H driver

INT33H_BUS	equ	1		;  bus mouse
INT33H_SERIAL	equ	2		;  bus mouse
INT33H_INPORT	equ	3		;  bus mouse
INT33H_PS_2	equ	4		;  bus mouse
INT33H_HP	equ	5		;  bus mouse
