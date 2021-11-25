	page	,132
;-----------------------------Module-Header-----------------------------;
; Module Name:	BUS8255.ASM
;
; Windows mouse driver data and initialization routines for using an
; 8255 Bus Mouse for Windows
;
; Created: 21-Aug-1987
; Author:  Mr. Mouse [mickeym], Walt Moore [waltm], and a supporting
;	   cast of thousands
;
; 10-Jan-1989 -by- Amit Chatterjee [amitc]
; Modified the 'check_bus_irq' routine to get the IRQ using the INT 33
; driver if one was loaded.
;
; 18-Oct-1989 -by- Amit Chatterjee [amitc]
; In the 'check_bus_irq' routine added a loop with the mouse interrupts
; disabled to look for stray interrupts coming in (like network ones)
; in the actual loop which follows (with the mouse interrupts enabled)
; these stray interrupts if they come in again are ignored. This fixes
; the old green-eyed mouse problems (windows would not detect them)
;
; Copyright (c) 1986,1987  Microsoft Corporation
;
; Exported Functions:
;	None
; Public Functions:
;	bus_enable
;	bus_disable
;	bus_search
; Public Data:
;	None
; General Description:
;	This module contains the functions to find, enable, disable,
;	and process interrupts for an 8255 Bus Mouse.
;-----------------------------------------------------------------------;

	title	8255 Bus Mouse Hardware Dependent Code

	.xlist
	include cmacros.inc
	include mouse.inc
	.list

	??_out	bus8255


	externNP hook_us_in		;Hook us into our interrupt
	externNP unhook_us		;Hook us out of our interrupt
	externNP enable_our_int 	;Enable us at the 8259



;	Definitions for the 8255A, as it is used for the Bus Mouse.
;
;	Port A of the 8255 will be used to read the motion deltas
;	and button state of the mouse.	It will be configured as
;	an input device.
;
;	Port B will not be used, except as a test register to
;	determine if there really is an 8255 out there.
;
;	Port C will be used be split.  The upper half of port C
;	will be configured as output.  This port will control
;	the mouse (see CTRL_PORT below for bit definitions).
;	The lower half of port C will be configured as input.
;	The state of IRQs 2,3,4 and 5 can be read at this port.


PORT_8255_A	equ	0		;8-bit I/O port A
PORT_8255_B	equ	1		;8-bit I/O port B
PORT_8255_C	equ	2		;8-bit I/O port C or 2 4-bit ports
PORT_8255_C_LOW equ	2		;8-bit I/O port C or 2 4-bit ports
PORT_8255_C_HI	equ	2		;8-bit I/O port C or 2 4-bit ports
PORT_8255_CTRL	equ	3		;Control register


;	Definitions for the 8255 control port

MODE_SET_CMD	equ	10000000b	;Mode set command
					;  Group B definitions
PORT_C_LOW_IN	equ	00000001b	;    Port C[D3:D0] is input
PORT_C_LOW_OUT	equ	00000000b	;    Port C[D3:D0] is output
PORT_B_IN	equ	00000010b	;    Port B is input
PORT_B_OUT	equ	00000000b	;    Port B is output
GROUP_B_MODE_0	equ	00000000b	;    Group B is mode 0
GROUP_B_MODE_1	equ	00000100b	;    Group B is mode 1
					;  Group A definitions
PORT_C_HI_IN	equ	00001000b	;    Port C[D7:D4] is input
PORT_C_HI_OUT	equ	00000000b	;    Port C[D7:D4] is output
PORT_A_IN	equ	00010000b	;    Port A is input
PORT_A_OUT	equ	00000000b	;    Port A is output
GROUP_A_MODE_0	equ	00000000b	;    Group B is mode 0
GROUP_A_MODE_1	equ	00100000b	;    Group B is mode 1
GROUP_A_MODE_2	equ	01100000b	;    Group B is mode 2

BIT_SET_CMD	equ	00000000b	;Bit set command
SET_C_BIT	equ	00000001b	;  Set	 a bit in port C
RESET_C_BIT	equ	00000000b	;  Reset a bit in port C
C_BIT_INDEX	equ	00001110b	;  Index of bit goes here


MOUSE_MODE	=	MODE_SET_CMD+PORT_C_LOW_IN+PORT_B_OUT+GROUP_B_MODE_0
MOUSE_MODE	=	MOUSE_MODE+PORT_C_HI_OUT+PORT_A_IN+GROUP_A_MODE_0



;	Definitions for the bus mouse control and data ports

MOUSE_CTRL_PORT equ	PORT_8255_C_HI	;Control port
CMD_DISABLE_INT equ	0001b shl 4	;  Disable interrupts
CMD_ENABLE_INT	equ	0000b shl 4	;  Enable interrupts
CMD_SELECT_HIGH equ	0010b shl 4	;  Select high nibble of count
CMD_SELECT_LOW	equ	0000b shl 4	;  Select low nibble of count
CMD_SELECT_Y	equ	0100b shl 4	;  Select Y counter
CMD_SELECT_X	equ	0000b shl 4	;  Select X counter
CMD_HOLD	equ	1000b shl 4	;  Hold counter for reading
CMD_CLEAR_N_CNT equ	0000b shl 4	;  Clear counters and start counting

CTRL_GET_LOW_X	equ	CMD_HOLD+CMD_SELECT_X+CMD_SELECT_LOW+CMD_DISABLE_INT
CTRL_GET_HIGH_X equ	CMD_HOLD+CMD_SELECT_X+CMD_SELECT_HIGH+CMD_DISABLE_INT
CTRL_GET_LOW_Y	equ	CMD_HOLD+CMD_SELECT_Y+CMD_SELECT_LOW+CMD_DISABLE_INT
CTRL_GET_HIGH_Y equ	CMD_HOLD+CMD_SELECT_Y+CMD_SELECT_HIGH+CMD_DISABLE_INT
CTRL_RUN	equ	CMD_CLEAR_N_CNT+CMD_ENABLE_INT

MOUSE_DATA_PORT equ	PORT_8255_A	;Mouse Data Port
MOTION_DELTA	equ	00001111b	;  Deltas are lower nibble
BUS_B3_UP	equ	00100000b	;  Button 3 is up (right)
BUS_B2_UP	equ	01000000b	;  Button 2 is up (middle button)
BUS_B1_UP	equ	10000000b	;  Button 1 is up (left)

MOUSE_IRQ_PORT	equ	PORT_8255_C_LOW ;IRQs can be read from this port
BUS_IRQ_5	equ	0001b		;  IRQ 5 state
BUS_IRQ_4	equ	0010b		;  IRQ 4 state
BUS_IRQ_3	equ	0100b		;  IRQ 3 state
BUS_IRQ_2	equ	1000b		;  IRQ 2 state


IRQ_8259_BITS	equ	00111100b	;8259 IRQs 5,4,3,2
IRQ_MASK_BITS	equ	BUS_IRQ_5+BUS_IRQ_4+BUS_IRQ_3+BUS_IRQ_2



;	Address of port to search for the mouse.  We'll search
;	from lower address to higher address.

FIRST_BUS_MOUSE_ADDR	equ	23Ch	;Addr of first port to search
LAST_BUS_MOUSE_ADDR	equ	23Ch	;Addr of last port to search
ADDR_INCREMENT		equ	4	;Increment to next port



sBegin	Data

externW WinFlags			;Windows exported flags
externB vector				;Vector # of mouse interrupt
externB mask_8259			;8259 interrupt enable mask
externB mouse_flags			;Various flags as follows
externW io_base 			;Mouse port base address
externW enable_proc			;Address of routine to	enable mouse
externW disable_proc			;Address of routine to disable mouse
externB device_int			;Start of mouse specific int handler
externW interrupt_rate			;Maximum interrupt rate of mouse
externD event_proc			;Mouse event procedure when enabled

OriginalCommPortAddr	dw	0	;addr of port whose int we will use
OriginalCommAddrLoc	dw	0	;location in 40: for above address

sEnd	Data


sBegin	Code
assumes cs,Code
page

;	This is the start of the data which will be copied into
;	the device_int area reserved in the data segment.

BUS_START	equ	this word


;--------------------------Interrupt-Routine----------------------------;
; bus_int - Mouse Interrupt Handler for the 8255 Bus Mouse
;
; This is the handler for the interrupt generated by the 8255 Bus
; mouse.  It will reside in the Data segment.
;
; Entry:
;	DS = Data
; Returns:
;	AX = status
;	BX = delta X
;	CX = delta Y
; Error Returns:
;	None
; Registers Preserved:
;	SI,DS,ES,BP
; Registers Destroyed:
;	AX,BX,CX,DX,DI,FLAGS
; Calls:
;	None
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data
	assumes es,nothing
	assumes ss,nothing


BUS_PROC_START	equ	$-BUS_START	;Delta to this procedure
		.errnz	BUS_PROC_START	;Must be first

bus_int proc	far

	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp
	push	ds
	push	es
	mov	ax,_DATA
	mov	ds,ax
	assumes ds,Data

	mov	di,io_base
	lea	dx,[di].MOUSE_CTRL_PORT ;DX = command port
;	lea	di,[di].MOUSE_DATA_PORT ;DI = data port
	.errnz	MOUSE_DATA_PORT
	mov	cl,4			;Will be shifting nibbles a lot

	mov	al,CTRL_GET_LOW_X	;Ask for low X
	out	dx,al
	io_delay
	xchg	dx,di			;DX = Data port
	in	al,dx			;D3:D0 = low X delta
	ror	ax,cl			;Save low X delta in AX[D15:D12]

	xchg	dx,di			;DX = command port
	mov	al,CTRL_GET_HIGH_X	;Ask for high X
	out	dx,al
	io_delay
	xchg	dx,di			;DX = Data port
	in	al,dx			;D3:D0 = high X delta
	rol	ax,cl			;AL is delta X
	cbw				;AX is delta X
	xchg	bx,ax			;Save delta X in BX

	xchg	dx,di			;DX = command port
	mov	al,CTRL_GET_LOW_Y	;Ask for low Y
	out	dx,al
	io_delay
	xchg	dx,di			;DX = Data port
	in	al,dx			;D3:D0 = low Y delta
	ror	ax,cl			;Save low Y delta in AX[D15:D12]

	xchg	dx,di			;DX = command port
	mov	al,CTRL_GET_HIGH_Y	;Ask for high Y and button info
	out	dx,al
	io_delay
	xchg	dx,di			;DX = Data port
	in	al,dx			;D3:D0 = high Y delta
	rol	ax,cl			;AL is delta Y, AH has button info
	mov	cx,ax			;Save button info
	cbw				;AX is delta Y
	xchg	cx,ax			;Save delta Y in CX, AH has button info

	.errnz	MOTION_DELTA-00001111b	;The above code assumes this

	xchg	dx,di			;DX = command port
	mov	al,CTRL_RUN		;Clear counters, turn on interrupts
	out	dx,al
	io_delay
	mov	al,EOI			;Acknowledge 8259 interrupt
	test	mouse_flags,MF_ON_SLAVEPIC
	jz	no_slave		;mouse not on slave PIC
	out	ACK_SLAVE_PORT,al
no_slave:
	out	ACK_PORT,al

	and	ah,(BUS_B3_UP+BUS_B1_UP) shr 4	;Isolate the button states
	mov	al,ah
	shr	al,1			;Place current status in D2 and D0
	xchg	al,device_int[BUTTON_STATE]
	or	al,ah			;Combine previous and current to give
	xor	ah,ah			;  an index into the state table
	xchg	ax,bx
	mov	bl,device_int[bx][STATE_XLATE]
	xchg	ax,bx
	mov	dx,bx			;Set 'C' if motion
	or	dx,cx
	neg	dx
	adc	ax,ax			;Move in movement flag, set 'Z' if 0
	.errnz	SF_MOVEMENT-00000001b

	jz	bus8255_no_data 	;Only call out if something happened
	mov	dx,NUMBER_BUTTONS
        xor     si,si		; 0 ExtraMessageInfo for 3.1
        xor     di,di		; 0 ExtraMessageInfo for 3.1

	sti
	call	event_proc

bus8255_no_data:
	pop	es
	pop	ds
	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	iret

bus_int endp
page

;	old_status contains the state of the mouse the last time we
;	were called to process an interrupt.

BUTTON_STATE	equ	$-BUS_START	;Delta to this byte
		db	0		;Old status will go here


;-----------------------------------------------------------------------;
; state_xlate
;
;	state_xlate is used to translate the current and previous
;	button state information into the values required by
;	Windows.  It is indexed as follows:
;
;	    cB1 pB1 cB3 pB3
;
;	     |	 |   |	 |
;	     |	 |   |	  --- 0 if button 3 was down, 1 if button 3 was up
;	     |	 |   |
;	     |	 |    ------- 0 if button 3 is	down, 1 if button 3 is	up
;	     |	 |
;	     |	  ----------- 0 if button 1 was down, 1 if button 1 was up
;	     |
;	      --------------- 0 if button 1 is	down, 1 if button 1 is	up
;
;	This table must be copied to the data segment along with the
;	interrupt handler.
;
;-----------------------------------------------------------------------;

STATE_XLATE	equ	$-BUS_START	;delta to this table

	db	0			shr 1
	db	(SF_B2_DOWN)		shr 1
	db	(SF_B2_UP)		shr 1
	db	0			shr 1

	db	 SF_B1_DOWN		shr 1
	db	(SF_B1_DOWN+SF_B2_DOWN) shr 1
	db	(SF_B1_DOWN+SF_B2_UP)	shr 1
	db	 SF_B1_DOWN		shr 1

	db	 SF_B1_UP		shr 1
	db	(SF_B1_UP+SF_B2_DOWN)	shr 1
	db	(SF_B1_UP+SF_B2_UP)	shr 1
	db	 SF_B1_UP		shr 1

	db	0			shr 1
	db	(SF_B2_DOWN)		shr 1
	db	(SF_B2_UP)		shr 1
	db	0			shr 1

	.errnz	NUMBER_BUTTONS-2	;Won't work unless a two button mouse

BUS_INT_LENGTH	= $-BUS_START		;Length of code to copy
	.errnz	BUS_INT_LENGTH gt MAX_INT_SIZE

display_int_size  %BUS_INT_LENGTH
page

irq_mappings	label	word

	db	0,0			;No IRQ found
	db	5+8,11011111b		;IRQ 5 was only IRQ found
	db	4+8,11101111b		;IRQ 4 was only IRQ found
	db	0,0			;4 and 5 responded
	db	3+8,11110111b		;IRQ 3 was only IRQ found
	db	0,0			;3 and 5 responded
	db	0,0			;3 and 4 responded
	db	0,0			;3,4 and 5 all responded
	db	2+8,11111011b		;IRQ 2 was only IRQ found

	.errnz	BUS_IRQ_5-0001b
	.errnz	BUS_IRQ_4-0010b
	.errnz	BUS_IRQ_3-0100b
	.errnz	BUS_IRQ_2-1000b
page

;---------------------------Public-Routine------------------------------;
; bus_search - Search for a 8255 Bus Mouse
;
; A search will be made for a 8255 based mouse.
;
; Entry:
;	None
; Returns:
;	'C' set if found
;	  AX = address of interrupt routine if interrupt vector found
;	  SI = offset within the Code segment of the handler
; Error Returns:
;	'C' clear if not found
; Registers Preserved:
;	DS,BP
; Registers Destroyed:
;	AX,BX,DX,DI,SI,ES,FLAGS
; Calls:
;	check_bus_irq
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

		public	bus_search
bus_search	proc	near

	mov	di,FIRST_BUS_MOUSE_ADDR

next_bus_card:
	lea	dx,[di].PORT_8255_CTRL	;Initialize possible 8255
	mov	al,MOUSE_MODE
	out	dx,al
	io_delay

;	To determine if we have found an 8255, we'll write a
;	signature byte to the B port and read it back.	Because
;	of ringing problems with the PS/2's I/O bus, we must
;	output something else on the I/O bus before reading the
;	signature back.

	mov	al,0A5h 		;Keep signaure in a register so it
	mov	ah,al			;  isn't on the bus when we do the
	lea	dx,[di].PORT_8255_B	; IN
	io_delay
	out	dx,al

	lea	dx,[di].MOUSE_CTRL_PORT ;This is to flush the PS2 I/O bus
	mov	al,CMD_CLEAR_N_CNT+CMD_DISABLE_INT
	io_delay
	out	dx,al

	lea	dx,[di].PORT_8255_B	;Read signature byte
	io_delay
	in	al,dx
	cmp	al,ah
	jne	see_if_next_card
	cCall	check_bus_irq		;Try to find IRQ for bus mouse
	jcxz	see_if_next_card	;None or too many IRQs
	assumes es,nothing

;	Looks like we found ourselves a mouse.	Maybe it would
;	like some cheese?

	mov	io_base,di		;Save base address
	mov	vector,cl		;Save vector number
	mov	mask_8259,ch		;Save 8259 enable mask
	mov	interrupt_rate,34
	mov	enable_proc,CodeOFFSET bus_enable
	mov	disable_proc,CodeOFFSET bus_disable
	mov	si,CodeOFFSET bus_int
	mov	cx,BUS_INT_LENGTH
	stc				;Show we found a card
	ret

see_if_next_card:
	add	di,ADDR_INCREMENT
	cmp	di,LAST_BUS_MOUSE_ADDR
	jb	next_bus_card		;More cards exist
	ret				;'C' is clear to show no card

bus_search	endp
page

;---------------------------Public-Routine------------------------------;
; check_bus_irq - check which irq the bus mouse is on
;
; For all practical purposes, a bus mouse adapter has been
; found.  We'll now attempt to determine which interrupt
; vector it is on.  If more than one interrupt vector or
; no interrupt vector is found, the card will be rejected.
;
; Entry:
;	DI = io_base
;	Bus mouse interrupts disabled
; Returns:
;	CL = vector number    if IRQ found
;	CH = 8259 enable mask if IRQ found
; Error Returns:
;	CX = 0 if no IRQ or too many
; Registers Preserved:
;	DI,DS,BP
; Registers Destroyed:
;	AX,BX,DX,ES,FLAGS
; Calls:
;	None
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

cProc	check_bus_irq,<NEAR,PUBLIC,PASCAL> 

	localB	ByPassNoiseTest		;noise test needed or not

cBegin	

	test	mouse_flags,MF_INT33H
	jz	int33_not_present	;it is not present

	mov	ax,INT33H_GETINFO	;get info about mouse
	int	MOUSE_SYS_VEC		;CH has type & CL has IRQ
	cmp	ch,INT33H_BUS		;make sure it is bus
	jne	int33_not_present	;may be old driver
	or	cl,cl			;do not deal with IRQ = 0
	jz	int33_not_present	;do alternate search
	mov	ch,1			;initial position for IRQ mask
	shl	ch,cl			;get inverse of the mask
	not	ch			;get the correct mak
	add	cl,08h			;convert IRQ to INT number
	push	si			;setup stack for exit code
	jmp	bus_irq_found

int33_not_present:

	push	si			;Will try the entire procedure
	mov	si,4			;  this many time if we have to
	mov	ByPassNoiseTest,0	;first have noise test enabled.

check_bus_try_again:
	dec	si			;Another retry?
	jnz	check_bus_retry		;yes.
	cmp	ByPassNoiseTest,0ffh	;was the test done w/o moisetest ?
	jnz	noise_bypass_cycle	;no,search once more bypassing noise
	jmp	check_bus_exit		;all done trying

noise_bypass_cycle:

	mov	ByPassNoiseTest,0ffh	;retry again with noise test disabled
	mov	si,3			;3 more attemts

check_bus_retry:

	mov	bx,BIOSDataSeg
	mov	es,bx
	assumes es,BIOSDataSeg
	xor	bx,bx

	cli				;Save the current IRQ mask
	in	al,MASK_PORT		;  and disable those IRQs that
	mov	bh,al			;  the mouse may be on
	or	al,IRQ_8259_BITS
	out	MASK_PORT,al
	sti

	lea	dx,[di].PORT_8255_CTRL	;The only way I've ever gotton this
	mov	al,MOUSE_MODE		;  to work is by reinitializing the
	out	dx,al			;  stupid thing
	io_delay

; first disable the mouse interrupts and look for other noise in the line.

	push	bx			;save PIC mask in BL
	mov	bh,0ffh			;no known noise at this point
	cmp	ByPassNoiseTest,0ffh	;bypass the noise test ?
	jz	NoiseTestDone		;yes

;----------------------------------------------------------------------------;
; The inboard bus mouse on commodore machines fail the mouse noise test. The ;
; mouse interrupts are registered even when mouse activity is disabled. To   ;
; take care of this we will first search the mouse with the irq noise test   ;
; enabled and then with the IRQ noise test disabled.			     ;
;----------------------------------------------------------------------------;

	lea	dx,[di].MOUSE_CTRL_PORT	;mouse control port
	mov	al,CMD_DISABLE_INT	;disable mouse activity
	out	dx,al

; get the activity code on the line

	mov	ch,5			;watch 5 ticks go by
	call	GetChangeInState	;BL has code
	mov	bh,bl			;get the state
	not	bh			;prepare the mask to mask them out

NoiseTestDone:

; now enable mouse ints and get the activity on the line

	lea	dx,[di].MOUSE_CTRL_PORT
	mov	al,CMD_CLEAR_N_CNT+CMD_ENABLE_INT
	out	dx,al
	io_delay

	xor	bl,bl			;clear out for change of state
	mov	ch,3			;watch 3 ticks go by
	call	GetChangeInState	;see what interrupts are coming in
	pop	dx			;get pushed bx into dx
	mov	bh,dh			;get the initial PIC state

;	OK, we now have BL = IRQs which interrupted when they should have.

;	Disable interrupts at the card until we get the real enable call.
;	This is also a good idea incase we detect multiple interrupts.

	lea	dx,[di].MOUSE_CTRL_PORT
	mov	al,CMD_DISABLE_INT
	out	dx,al

	cli				;Restore the 8259 IRQ mask
	in	al,MASK_PORT
	and	al,not IRQ_8259_BITS	;Save current state of unaltered bits
	and	bh,IRQ_8259_BITS	;Get old state of altered bits
	or	al,bh			;Combine into new IRQ mask
	out	MASK_PORT,al
	sti

	and	bx,IRQ_MASK_BITS	;IRQs we want to support
	xor	cx,cx			;Assume not found
	cmp	bl,8			;If greater than 8, more than
	ja	check_bus_irq_done	;  one IRQ responded
	shl	bx,1			;Indexing into words
	mov	cx,irq_mappings[bx]	;Get IRQ mask and vector number

check_bus_irq_done:

	or	cx,cx			;bus IRQ found ?
	jnz	bus_irq_found		;yes
	jmp	check_bus_try_again	;try again

bus_irq_found:

	cmp	cl,0ah			;IRQ 2 ?
	jnz	check_bus_exit		;no.
	test	WinFlags,WF_PMODE	;in pmode ?
	jz	check_bus_exit		;no
	mov	cl,71h			;we need to hook IRQ 9.
	or	mouse_flags,MF_ON_SLAVEPIC;mouse is on slave PIC

check_bus_exit:
	pop	si

check_bus_ret:

cEnd
page

;----------------------------------------------------------------------------;
; GetChangeInState:							     ;
;									     ;
; This routine tests to see what interrupt activity is on the bus and rets   ;
; a coded value in BL. To understand what BL means, look into irq_mapping    ;
; table which is indexed by 2*BL.					     ;
;									     ;
; On entry DX must have the MOUSE_CTRL_PORT address and CX,AX,BL are destroy-;
; -ed. On entry BH has a value of which bits are to be ignored in the line.  ;
; BH will be used to ignore known line noise.				     ;
; Also on entry BL should be 0 and CH has the no of iterations.		     ;
;----------------------------------------------------------------------------;

	public	GetChangeInState

GetChangeInState  proc near

; ch has no of tics to let go 

	in	al,dx
	mov	ah,al			
	and	ah,bh			;ignore known noise

bus_irq_loop_2:

	in	al,dx			;Get current states of irq 2-5
	and	al,bh			;ignore known noise
	xor	ah,al			;AH = 1 where changed
	or	bl,ah			;Mark any changes
	mov	ah,al			;Previous := current
	cmp	cl,bios_time		;Wait specified milliseconds
	je	bus_irq_loop_2
	mov	cl,bios_time
	dec	ch
	jnz	bus_irq_loop_2
	ret

GetChangeInState endp
;---------------------------Private-Routine-----------------------------;
; bus_enable - Enable Bus Mouse
;
; The Bus mouse will be initialized, the interrupt vector hooked,
; the old interrupt mask saved, and our interrupt enabled at the 8259.
;
; Entry:
;	None
; Returns:
;	None
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,ES,BP
; Registers Destroyed:
;	AX,BX,CX,DX,FLAGS
; Calls:
;	hook_us_in
;	enable_our_int
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes ds,Data
	assumes es,nothing
	assumes ss,nothing

		public	bus_enable	;Public for debugging
bus_enable	proc	near

	call	hook_us_in		;Hook us into the interrupt

; if the vector that we are using for the Mouse INT matches either of the 
; serial int vectors 0bh or 0ch, we will then have to NULL out the 
; corresponding serial port address in the BIOS data area so that no one
; will try to use those ports.

	push	es			;save
	mov	bx,BIOSDataSeg		;segment for BIOS data
	mov	es,bx			;es: points to BIOS data
	assumes es,BIOSDataSeg
	xor	ax,ax			;will put in zero there
	mov	bx,offset rs232_data	;Start of serial ports
	cmp	vector,0ch		;using COMM 1's int ?
	jz	zero_out_comm_port_addr	;yes
	add	bx,2			;may be comm 2
	cmp	vector,0bh		;COMM 2's int ?
	jnz	save_comm_port_addr	;do not do anything

zero_out_comm_port_addr:

	xchg	es:[bx],ax		;get previous port addr too.

save_comm_port_addr:

	mov	OriginalCommPortAddr,ax	;if zero then was not swapped
	mov	OriginalCommAddrLoc,bx	;also save the location in 40:
	pop	es			;restore
	assumes	es,nothing

	mov	bx,io_base		;Restart the mouse, with
	lea	dx,[bx].PORT_8255_CTRL	;  interrupts enabled and
	mov	al,MOUSE_MODE		;  counters cleared
	out	dx,al
	io_delay
	lea	dx,[bx].MOUSE_CTRL_PORT
	mov	al,CMD_CLEAR_N_CNT+CMD_ENABLE_INT
	out	dx,al
	call	enable_our_int
	ret

bus_enable	endp
page

;---------------------------Private-Routine-----------------------------;
; bus_disable - Disable Bus Mouse
;
; The interrupt vector will be restored, the old interrupt mask
; restored at the 8259.  If the old mask shows that the mouse was
; previously enabled, it will remain enabled, else we will disable
; interrupts at the mouse itself.
;
; Entry:
;	None
; Returns:
;	None
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,ES,BP
; Registers Destroyed:
;	AX,BX,CX,DX,FLAGS
; Calls:
;	unhook_us
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes ds,Data
	assumes es,nothing
	assumes ss,nothing

		public	bus_disable	;Public for debugging
bus_disable	proc	near

;	Disable interrupts first so no interrupt will hit if
;	only the BIOS default handler is around.

	mov	dx,io_base		;Disable IRQs at the mouse
	add	dl,MOUSE_CTRL_PORT
	mov	al,CMD_DISABLE_INT
	out	dx,al

; now if we had zeroed out one of the comm port addresses in the bios data
; area, restore it.

	mov	ax,OriginalCommPortAddr	;get the original address ?
	or	ax,ax			;was it zero or not put at all ?
	jz	@f			;yes,no need to put it back
	push	es			;save
	mov	bx,BIOSDataSeg		;seg value for BIOS data area
	mov	es,bx			;es: points to BIOS data area
	assumes es,BIOSDataSeg
	mov	bx,OriginalCommAddrLoc	;location of the address
	mov	es:[bx],ax		;restore it
	pop	es			;restore
	assumes	es,nothing
@@:

	call	unhook_us		;Restore everything to what it was
	jnz	bus_disable_exit	;IRQ was previously disabled

	mov	bx,io_base		;Restart the mouse, with
	lea	dx,[bx].PORT_8255_CTRL	;  interrupts enabled and
	mov	al,MOUSE_MODE		;  counters cleared
	out	dx,al
	io_delay
	lea	dx,[bx].MOUSE_CTRL_PORT
	mov	al,CMD_CLEAR_N_CNT+CMD_ENABLE_INT
	out	dx,al

bus_disable_exit:
	ret

bus_disable  endp


sEnd	Code
end
