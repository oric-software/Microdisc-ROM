; When microdisc is connected
; Romdis is enabled
; microdisc Eprom enabled (8 K) top of memory
; When Oric Boots :
; so $c000 to $dfff -> ram overlay
; $e000 to $ffff -> Microdisc eprom

; When Oric boots, 6502 jumps to RESET vector on microdisc Eprom 
;
; Disassembled by F. Frances
; comments added by jede
; Xa source code by jede

#include "fdc1793.h"
; 2 bytes
#define adress_buffer_loading_saving $FE
; 2 bytes
#define buffer_adress_buffer_loading_saving $c003 

*=$e000
eprom_microdisc

	JMP $E5C2	; initializes some parameters
	JMP $E20C	; FDC routine
	JMP $EB34
	JMP $E4CE	; loads a file
	JMP $EAD3	; searches a file
	JMP $E4DE	; error "File not found"
	JMP $EAFC
	JMP $E117	; lets the user type a command in TIB
	JMP $E11F	; waits for a keypress
	JMP $E7DC	; error routine
	JMP $E816	; dummy, points to a RTS
	JMP $E817	; writes a sector
	JMP $E825	; reads sector
	JMP $E82B	; reads boot sector
	JMP $E846	; checks drive number
	JMP $E854	; prints string pointed by ($0C)
	JMP $E980	; points to next directory entry
	JMP $E872	; reads system parameters from boot sector
	JMP $E881	; writes system parameters to boot sector
	JMP $E893	; adds a directory entry (not used)
	JMP $E8B7
	JMP $E8C5
	JMP $E8F6
	JMP $E950
	JMP $E9A8	; reads boot sector
	JMP $E9CA
	JMP $EAB8	; limits a char to alphanumeric
	JMP $EAEA
	JMP $E127	; new line
	JMP $E12E	; prints char
	JMP $E070	; calls a routine in ROM Basic
	JMP $E16A	; interprets a decimal or hex number
	JMP $E163	; reads a non-blank char
	JMP $E4C7
	JMP $0000
	JMP $0000	
	JMP $EB45	; checks no '?' wildcard is used
	.byt  $00

;******************************************************************************
; calls a routine in Basic ROM:
;   grabs the two addresses after the JSR and selects one depending on the version
;******************************************************************************
;E070
	PHP 
	PHA 
	TXA 
	PHA 
	TYA 
	PHA 
	TSX 
	LDA $0105,X
	CLC 
	STA $0E
	ADC #$04
	STA $0105,X
	LDA $0106,X
	STA $0F
	ADC #$00
	STA $0106,X
	LDY #$01
	LDA $C007
	BEQ $E095
	LDY #$03
	LDA ($0E),Y
	STA $0485
	INY 
	LDA ($0E),Y
	STA $0486
	LDA #$06
	STA $0481
	PLA 
	TAY 
	PLA 
	TAX 
	PLA 
	PLP 
	JMP $0490    

;******************************************************************************
;           NMI : handler
;******************************************************************************  
NMI_START
; E0ae
	pha
        lda     $0481
        pha
        lda     $0485
        pha
        lda     $0486
        pha
        lda     $0480
        and     #$FE
        sta     $0480
        sta     MICRODISC_FDC
        lda     #$00
        sta     $0485
        lda     #$00
        sta     $0486
        lda     #$06
        sta     $0481
        jsr     $0490
        pla
        sta     $0486
        pla
        sta     $0485
        pla
        sta     $0481
        pla
        rti
;******************************************************************************
;           IRQ wrapper 
;        -> switch to ROM and exec the Basic IRQ handler
;******************************************************************************
irq_wrapper
;
; $e0e6	
	pha
        txa
        pha
        lda     $0481
        pha
        lda     $0485
        pha
        lda     $0486
        pha
        lda     #$8A
        sta     $0485
        lda     #$04
        sta     $0486
        lda     #$06
        sta     $0481
        jsr     $0490
        pla
        sta     $0486
        pla
        sta     $0485
        pla
        sta     $0481
        pla
        tax
        pla
        rti    

;******************************************************************************
; waits for the user to type a command in TIB
;

	JSR $E05A
	.byt $A2,$C5,$92,$C5 ; ??? FIXME
	RTS

;******************************************************************************
; waits for a keypress, ascii code returned in A
;

	JSR $E05A
	.byt $F8,$C5,$E8,$C5
	RTS


;*****************************************************************************
; prints carriage return + line feed
; 

	LDA #$0D
	JSR $E12E
	LDA #$0A
;
; prints char
;
	PHP 
	STX $C151
	TAX 
	PHA 
	LDA $0C
	PHA 
	LDA $0D
	PHA 
	JSR $E05A		; calls Basic's output routine
	.byt $3F,$F7,$7C,$F7
	PLA 
	STA $0D
	PLA 
	STA $0C
	PLA 
	LDX $C151
	PLP 
	RTS 
  
;******************************************************************************
; prints a hex byte
;
  
	PHA 
	LSR 
	LSR 
	LSR 
	LSR 
	JSR $E156
	PLA 
	AND #$0F
	ORA #$30
	CMP #$3A
	BCC $E12E
	ADC #$06
	BNE $E12E
;******************************************************************************
; reads next non-blank char 

	INY 

	LDA ($E9),Y
	CMP #$20
	BEQ $E162
	RTS 


	

;******************************************************************************
; interprets decimal and hexadecimal numbers

	LDA #$00	; initializes the number read
	STA $C145
	STA $C146
	LDA ($E9),Y	; skips any blanks
	INY 
	CMP #$20
	BEQ $E172
	CMP #$23
	BNE $E1A1	; is it a '#' ?

	LDA ($E9),Y	; yes, reads the hex number
	JSR $E1F1	
	BCC $E19F	; is it a hex digit ?
	INY 		; yes, computes the hex number read so far
	LDX #$04
	ASL $C145
	ROL $C146
	DEX 
	BNE $E187
	CLC
	ADC $C145
	STA $C145
	BCC $E17D
	INC $C146
	JMP $E17D
	SEC 		; no, returns C=1
	RTS 

	DEY 		; first char was not a '!', goes back on it
	JSR $E1E6	; and reads a decimal number
	BCC $E1A0
	INY 
	PHA 
	LDA $C146
	PHA 
	LDA $C145
	ASL $C145
	ROL $C146
	ASL $C145
	ROL $C146
	CLC 
	ADC $C145
	STA $C145
	PLA 
	ADC $C146
	STA $C146
	ASL $C145
	ROL $C146
	PLA 
	CLC 
	ADC $C145
	STA $C145
	BCC $E1DD
	INC $C146
	LDA ($E9),Y
	JSR $E1E6
	BCS $E1A7
	SEC 
	RTS 

; checks for a decimal digit returns C=1 if success
;
	SEC 
	SBC #$30
	BCC $E1EF
	CMP #$0A	
	BCC $E1E4
	CLC 
	RTS 
;$e1f1

; checks for a hex digit returns C=1 if success
;
	JSR $E1E6
	BCS $E1E4
	SBC #$06
	CMP #$10
	BCS $E1EF
	CMP #$09
	RTS 

;******************************************************************************
; switch to Basic (no return)
;
	JSR $E05A
	.byt $A3,$C4,$96,$C4

;******************************************************************************
;  write sector command
;******************************************************************************
	LDX #$A0
	BNE $E20C
;******************************************************************************
;  read sector command
;******************************************************************************
	LDX #$80
;******************************************************************************
;  FDC routine: command specified in register X
;******************************************************************************
	JSR $E3E3	; disables timer1 interrupts
	JSR start_fdc	; the FDC routine itself
	PHP 
	TXA 
	PHA 
	JSR $E3EB	; enables timer1 interrupts
	PLA 
	TAX 
	PLP 
	RTS 

;******************************************************************************
;  FDC routine heart command specified in register X
;  the routine may call itself recursively,
;  thus callers have to save and restore some global variables (C005, C008,...)
;******************************************************************************
start_fdc	
	STX $C005 ; We store the command (Read or write)
	PHA ; we save A
	TYA 
	PHA  ; we save Y
	LDA #$00
	STA $04FE
	LDA #$07
	STA $C008
	JSR run_fdc_command	; recognizes and executes the command
	BEQ $E247	; error ?
	TAY 		; yes...
	ROR 
	BCS $E28A	; busy ?
	LDA #$20
	BIT $C005
	BPL $E251	; was it a type I command ?
	BVC $E267	; or a type II command ?
	BNE $E28A	; or else a read/write track command ?
	LDA #$10
	BIT $C005
	BEQ $E282	; or else a Read address id command ?
	; no, just a Force Interrupt...

	LDX #$00	; forgets the error
	CLC 
	STX $04FE
	PLA 
	TAY 
	PLA 
	RTS 

;******************************************************************************
; got an error in a type I command...
;
	TYA 		
	AND #$18
	BEQ $E247	; takes care of seek and crc errors only
	CPY #$18
	BEQ $E28A	; returns error #1 if both seek and crc errors
	LDA $C005	; so, only one of these...
	CMP #$20
	BCS $E28A	; returns error #1 if step command
	CMP #$10
	BCC $E282	; but retries if Restore track 0
	BCS $E277

;******************************************************************************
; got an error in a type II command...
;
	TYA 
	AND #$40
	BNE $E28A	; returns error #1 if Write protect flag
	CPY #$10
	BCC $E282	; retries if CRC error (or lost data)
	LDA $C005
	AND #$10
	BNE $E247	; forgets a record not found in multiple sectors operations
					; so, a record was not found when reading
	LDY $C005
	JSR $E364	; read first address id encountered
	STY $C005
	BCS $E287	; can't even read an address id ? gives up...
	DEC $C008	; decrements retry counter and tries again
	BPL $E22C
	JSR $E3B2	; restores track 0

	LDX #$01	; returns an error 1
	SEC 
	BCS $E24D


;******************************************************************************
;  type I commands
;
; e28f

	CPY #$20
	BCS $E2BC	; step commands ? issue them ...
	CPY #$10
	BCC $E2BC	; restore track 0 command ? issue it...
	LDA $C001	; no, then it is a seek command
	AND #$7F
	STA MICRODISC_FDC_DATA	; programs the track wanted
	JMP $E2BC

;******************************************************************************
; updates the track register if needed, then recognizes the command
;

; e2a2	
run_fdc_command
	LDY $C005
	JSR update_track	; updates the track register if needed
	BCS $E325
					; now, recognizes the command:
	LDA #$20
	BIT $C005
	BPL $E28F	; type I commands ?
	BVC $E2C3	; type II commands ?
	BNE read_write_command	; read/write track commands ?
	LDA #$10
	BIT $C005
	BEQ $E2E6	; read address id command ?
					; no, so it is a force interrupt command

;******************************************************************************
; issues the FDC command and waits for its completion (interrupt raised)
; the interrupt handler will return to the caller routine
;
	JSR execute_fdc_command ; issues the command
	CLC 
	CLI 
	BCC $E2C1	; waits

;******************************************************************************
;  type II commands : read or write a sector
;
; e2c3

read_write_sector
	LDA $C001
	AND #$7F
	NOP 	
	NOP 
	CMP MICRODISC_FDC_TRACK
	BEQ $E2E0	; is the head already on the right track ?
					; no, seeks the right track first
	LDA $C008
	LDX #$1C
	JSR $E21C
	STA $C008
	STY $C005
	BCS $E32D
	NOP 
					; ok, the head is on the right track
	LDA $C002
	STA MICRODISC_FDC_SECTOR	; programs the wanted sector
	TYA 
	AND #$20
	BNE $E305	; write sector command ?
					; no
	JSR execute_fdc_command 	; issues the read sector command
	CLI 		; and gets the bytes, 
.(	
; $e2ef
loop
	LDA $0318	; the final interrupt will exit from here
	BMI loop
	LDA MICRODISC_FDC_DATA ; Reading on Fdc register
	STA (adress_buffer_loading_saving),Y ; We store the bytes loaded
	INY ; increment Y register for the next byte
	BNE loop ; Did we reach 256 bytes ?
	INC $FF ; We jump a page
	BNE loop
.)	
	BEQ $E31A
	NOP 
	NOP 
	NOP 
					
	JSR execute_fdc_command	; issues the write sector command
	CLI
.(	 		
loop			; and sends the bytes,
	LDA $0318	; the final interrupt will exit from here
	BMI loop
.)
	LDA (adress_buffer_loading_saving),Y
	STA MICRODISC_FDC_DATA
	INY 
	BNE $E309
	INC $FF
	BNE $E309
	BEQ $E31A


;******************************************************************************
; read/write track commands
; handles them like read/write sector commands
;
;$E31C
read_write_command
	LDA $C005
	AND #$10
	BEQ $E2EB
	BNE $E305

;******************************************************************************
; address id read failed, what now ?
; the recursivity bug shows here : 
; the JSR never returns if the restore track 0 fails and the stack fills up !
;
	JSR $E3B2	; restores track 0 
	LDA $04FE	; and returns status of the previous command...
	CLI
	RTS

;******************************************************************************
; seek track command failed, returns interesting bits of the status
;
	LDA $04FE
	AND #$BB
	STA $04FE
	CLI 
	RTS 

;*******************************************************************************
; updates the track register if needed (i.e the selected drive/side changes)
;
; e337
update_track
	LDA $C000
	AND #$03
	TAX 
	LDA $E3F3,X
	BIT $C001
	BPL $E347
	ORA #$10
	STA MICRODISC_FDC	; programs drive and side numbers
	TAX 
	LDA $0480
	STX $0480
	AND #$7E
	STA adress_buffer_loading_saving
	TXA 
	AND #$7E
	CMP adress_buffer_loading_saving
	BEQ $E38D	; were the drive/side numbers the same ?
					; no, checks the drive
	CPY #$10	; unless it is a seek command (no need to move twice)
	BCC $E38D
	CPY #$F0	; or a format command
	BEQ $E38D
	
	LDA buffer_adress_buffer_loading_saving+1	; reads the first address id encountered
	PHA 
	LDA #$C3
	STA buffer_adress_buffer_loading_saving+1
	LDA $C008
	LDX #$C0
	JSR $E21C
	STA $C008
	PLA 
	STA buffer_adress_buffer_loading_saving+1
	STY $C005
	
	LDA $04FE
	BNE $E38F
	LDA MICRODISC_FDC_SECTOR	; gets the track number
	NOP 
	NOP 
	NOP 
	STA MICRODISC_FDC_TRACK	; and updates the track register
	CLC 
	RTS 
	SEC 
	RTS 
	NOP 
	NOP 

;*******************************************************************************
; issue the effective FDC command specified in Y
;
;$E393
execute_fdc_command
	SEI 
	STY $C005
	LDA buffer_adress_buffer_loading_saving
	STA adress_buffer_loading_saving ; Loading/saving ram vector
	LDA buffer_adress_buffer_loading_saving+1
	STA adress_buffer_loading_saving+1
	STY MICRODISC_FDC_COMMAND
	LDA $0480
	ORA #$01
	STA MICRODISC_FDC
	STA $0480
	LDY #$00
	RTS 
	
;******************************************************************************
; restore track 0 (preserving status of previous command)
; heart of the bug is here...
; command should be 0 (no load head flag)
; this way, the command wouldn't fail when no disk is in drive
;
	LDA $04FE
	LDX #$08
	JSR $E21C	
	STA $04FE
	RTS 
	NOP 
	NOP 


IRQ_START
;E3e0    
        pha
        lda     MICRODISC_FDC
        bmi     microdisc_irq_not_from_disc 	; checks if the IRQ comes from disk
        pla ; ...yes, continue here and pull 
        lda     $0480
        and     #$FE
        sta     $0480
        sta     MICRODISC_FDC
        pla ; get rid of the IRQ context !!
        pla
        pla
        lda     MICRODISC_FDC_COMMAND ; so, we are now in the interrupted routine !
        and     #$5D
        sta     $04FE ; store FDC's status (only interesting flags)
        cli ; enable interrupts
        rts; and return to the *caller* of the interrupted routine
					; (not the interrupted routine itself !)
microdisc_irq_not_from_disc					
	pla ; IRQ doesn't come from disk,
        jmp     irq_wrapper ; go to the normal IRQ handler
        pha
	; disables timer1 interrupts
LE404:  lda     #$40
        sta     $030E
        pla
        rts
; enables timer1 interrupts	
        pha
        lda     #$C0
        sta     $030E
        pla
        rts



;******************************************************************************
	.byt 04,$24,$44,$64			; drive numbers

;******************************************************************************
;  interpreter routine to load a program... not used
;
	JSR $E006
	JSR $E04B
	JSR $EB45
	JSR $E000
	DEY 
	INY 
	JSR $E060	; reads a non-blank char
	JSR $0000	; incomplete !!
	BEQ $E462	; end of command ?
	CMP #$2C	; is it a ',' ?
	BNE $E426
	INY 		; yes, reads next char
	LDA ($E9),Y
	CMP #$4E	; is it a 'N' ?
	BNE $E41D
	STA $C14F
	BPL $E404
	CMP #$44	; is it a 'D' ?
	BNE $E42B
	STA $C150
	BPL $E404
	LDX #$01	; invalid command end
	JMP $E01B
	CMP #$4A	; is it a 'J' ?
	BNE $E444
	STA $C141	; yes: Join
	LDA $9C
	SEC 
	SBC #$02
	STA $C14D
	LDA $9D
	SBC #$00
	STA $C14E
	JMP $E404
	CMP #$41	; is it a 'A' ?
	BNE $E426
	STA $C14F
	STA $C141
	INY 
	JSR $E05D	; reads a number
	BCC $E426
	LDA $C146
	STA $C14E
	LDA $C145
	STA $C14D
	BCS $E405
					; execs the command, ie loads specified file
	TYA 
	PHA 
	JSR $E009	; loads file
	PLA 
	TAY 
	LDA $C14C
	BEQ $E476
	LDA $C14F
	BPL $E476
	JMP ($C14B)	; auto-run

	LDA $C14B
	BNE $E47E
	JMP $E069	; uncomplete ! (points to 0000)
	CMP #$03	 
	BCS $E47B

	JSR $E05A	; links Basic program lines
	.byt $6F,$C5,$5F,$C5
	LDA $92
	STA $9D
	CLC 
	LDA $91
	ADC #$02
	STA $9C
	BCC $E498
	INC $9D
	STA $9E
	STA $A0
	LDA $9D
	STA $9F
	STA $A1
	LDA $A6
	STA $A2
	LDA $A7
	STA $A3
	JSR $E05A	; Basic's RESTORE command
	.byt $1F,$C9,$52,$C9
        JSR $E05A	; let's the interpreter points to the basic program
	.byt $65,$C7,$3A,$C7
	LDA $C14B
	CMP #$01
	BEQ $E4C7
	BIT $C14F
	BPL $E4C7
	JMP $E069	; uncomplete, points to 0000
	JSR $E05A	; runs Basic interpreter
	.byt $B5,$C4,$A8,$C4

;******************************************************************************
; loads a file

	LDA $C12B
	STA $C000
	JSR $E02A	; checks drive number
	JSR $E00C	; searches the file
	CPX #$00
	BNE $E4E3
	LDX #$00	; File not found
	JMP $E01B
	LDA $C02F,X	; File found, reads first sector of it
	STA $C001
	LDA $C02E,X
	JSR $E83F
	LDX #$00
	LDY #$02
	BPL $E4F7
	TXA 
	TAY 
	LDA $C141
	BNE $E508	; is it a 'Join' ?
	LDA $C025,Y	; no, uses first start address as global address
	STA $C14D
	LDA $C026,Y
	STA $C14E
	SEC 		; computes end address of record
	LDA $C14D
	SBC $C025,Y
	STA $C025,Y
	LDA $C14E
	SBC $C026,Y
	STA $C026,Y
	CLC 
	LDA $C025,Y
	ADC $C027,Y
	STA $C027,Y
	LDA $C026,Y
	ADC $C028,Y
	STA $C028,Y
	CPX #$00
	BNE $E53E
	LDA $C02A,Y
	STA $C14C
	LDA $C029,Y
	STA $C14B
	
	LDA $C150
	BMI $E579	; is trace required ?
	LDA $C14E	; if yes, prints addresses
	JSR $E14D
	LDA $C14D
	JSR $E14D
	LDA #$20
	JSR $E057
	LDA $C028,Y
	JSR $E14D
	LDA $C027,Y
	JSR $E14D
	LDA $C141
	BNE $E576
	LDA #$20
	JSR $E057
	LDA $C02A,Y
	JSR $E14D
	LDA $C029,Y
	JSR $E14D
	JSR $E054
	
	LDA $C14D
	STA $0C
	LDA $C14E
	STA $0D
	CLC 
	TYA 
	ADC #$08
	TAX 
	BEQ $E5AF
	LDA $C023,X	; taille du record
	BEQ $E5AC
	CMP #$FF
	BNE $E596
	JMP $E4F5
	STA $C141
	LDY #$00
	INX 
	LDA $C023,X
	STA ($0C),Y
	INC $0C
	BNE $E5A7
	INC $0D
	DEC $C141
	BNE $E59B
	INX 
	BNE $E58A
	LDA $C023
	STA $C001
	LDA $C024
	BEQ $E5C1
	JSR $E83F
	LDX #$02
	BPL $E58A
	RTS 

;******************************************************************************
; initializes some parameters
	LDA #$FF
	STA $C14F
	STA $C150
	STA $C13C
	LDA #$00
	STA $C14D
	STA $C14E
	STA $C141
	RTS 

	; $e5d9
eprom_microdisc_str_file_not_found
	.asc "File not found",0
eprom_microdisc_str_invalid_command_end
	.asc "Invalid command end",0
eprom_microdisc_str_no_drive_number
	.asc "No drive number",0
eprom_microdisc_str_bad_drive_number	
	.asc "Bad drive number",0
eprom_microdisc_str_invalid_filename
	.asc "Invalid filename",0
eprom_microdisc_str_disc_error
	.asc "Disc error",0
eprom_microdisc_str_illegal_attribute
	.asc "Illegal attribute",0
	.asc "Wildcard(s) not allowed",0
	.asc "File already exists",0
	.asc "Insufficient disk space",0
	.asc "Start address missing",0
	.asc "Illegal quantity",0
	.asc "End address missing",0
	.asc "Start address > end address",0
	.asc "Missing 'TO'",0
	.asc "Renamed file not on same disk",0
	.asc "Missing comma",0
	.asc "Source and destination drives must be same",0
	.asc "Destination not specified",0
	.asc "Cannot merge and overwrite",0
	.asc "Single destination file not allowed",0
	.asc "Syntax error",0
;$e7b0
 
    ; addresses of the messages above (low bytes in the first line) :
    .byt  <eprom_microdisc_str_file_not_found
    .byt  <eprom_microdisc_str_invalid_command_end
    .byt  <eprom_microdisc_str_no_drive_number
    .byt  <eprom_microdisc_str_bad_drive_number
    .byt  <eprom_microdisc_str_invalid_filename,$2e,$39,$4b,$63,$77,$8f,$a5,$b6,$ca,$e6,$f3
    .byt  $11,$1f,$4a,$64,$7f,$a3
    .byt  >eprom_microdisc_str_file_not_found
    .byt  >eprom_microdisc_str_invalid_command_end
    .byt  >eprom_microdisc_str_no_drive_number
    .byt  >eprom_microdisc_str_bad_drive_number
    .byt >eprom_microdisc_str_invalid_filename,$e6,$e6,$e6,$e6,$e6
    .byt  $e6,$e6,$e6,$e6,$e6,$e6,$e7,$e7,$e7,$e7,$e7,$e7
    
  
    ;******************************************************************************
; error routine
;
	INX
	STX $04FF
	JMP ($C149)	; clearly, this instruction has been added
						; the error routine is below but not used

	DEX 
	LDA $04FD
	AND #$01
	BEQ $E7EE
	JMP $E069	; uncomplete, points to 0000
	CPX #$16
	BCS $E807
	LDA $E7B0,X
	STA $0C
	LDA $E7C6,X
	STA $0D
	JSR $E02D	; prints message
	LDA #$3A
	JSR $E057
	JMP $E813
	
	TXA 		; prints the error number
	JSR $E14D
	LDA $04FE
	BEQ $E813
	JSR $E14D
	JMP $E1FF	; switch to Basic
	RTS 

;******************************************************************************
; writes a sector
	JSR $E206
	LDA $04FE
	BEQ $E824
	LDX #$05	; Disc error
	JMP $E01B
	RTS 
  
;******************************************************************************
; reads a sector
	JSR $E20A
	JMP $E81A
	
;******************************************************************************
; reads boot sector
	LDA #$23
	STA buffer_adress_buffer_loading_saving
	LDA #$C0
	STA buffer_adress_buffer_loading_saving+1
	LDA #$00
	STA $C001
	STA $C00A
	LDA #$01
	STA $C002
	JSR $E024
	RTS 
  
;******************************************************************************
; checks drive number
;
	LDX $C000
	LDA $C013,X
	BEQ $E84F
	RTS 
	LDX #$03	; bad drive number
	JMP $E01B

;******************************************************************************
; prints string pointed by ($0C)
;
	LDY #$00
	LDA ($0C),Y
	BEQ $E860
	JSR $E057
	INY 
	BPL $E856
	RTS 

;******************************************************************************
; not used
	LDA $C146
	BNE $E86F
	LDA $C145
	BMI $E86F
	CMP #$04
	BMI $E871
	LDA #$FF
	RTS 

;******************************************************************************
; reads system parameters from boot sector
	JSR $E027
	LDX #$07
	LDA $C033,X
	STA $C123,X
	DEX 
	BPL $E877
	RTS 


;******************************************************************************
; writes system parameters to boot sector
	JSR $E027
	LDX #$07
	LDA $C123,X
	STA $C033,X
	DEX 
	BPL $E886
	JSR $E021
	RTS 

;******************************************************************************
; adds a directory entry (no used)
	LDA $C13E
	STA $C001
	LDA $C13D
	JSR $E83F	; reads sector (C13D) track (C13E)
	LDX #$00
	LDY $C13F
	LDA $C12C,X
	STA $C023,Y
	INY 
	INX 
	CPX #$10
	BNE $E8A4
	INC $C025
	JSR $E021
	RTS 

;******************************************************************************
	JSR $E03F
	BEQ $E8C4
	INC $C129
	BNE $E8C4
	INC $C12A
	RTS 

;******************************************************************************
	LDA $C123
	BEQ $E8F5
	STA $C002
	LDA $C124
	STA $C001
	JSR $E024
	LDA $C024
	STA $C123
	LDA $C023
	STA $C124
	SEC 
	LDA $C127
	SBC #$01
	STA $C127
	LDA $C128
	SBC #$00
	STA $C128
	LDA #$01
	RTS 
;******************************************************************************
; finds a free directory entry
	JSR $E024
	LDA $C025
	CMP #$0F
	BNE $E931	; this directory sector is full ?
	LDA $C024	; yes
	BEQ $E911	; is it the last dir sector ?
	STA $C002	; no, reads next one
	LDA $C023
	STA $C001
	JMP $E8F6
	LDA $C123	; yes,
	BEQ $E94F
	STA $C024
	LDA $C124
	STA $C023
	JSR $E021
	JSR $E03F
	LDA #$00
	TAX 	
	STA $C023,X
	INX 
	BNE $E928
	JSR $E021
	LDX #$03	; looks for a free entry
	LDA $C023,X
	BEQ $E93F
	TXA 
	CLC 
	ADC #$10
	TAX 
	BNE $E933
	TXA 		; and returns it
	STA $C13F
	LDA $C001
	STA $C13E
	LDA $C002
	STA $C13D
	RTS 
;******************************************************************************
	JSR $E024
	LDX $C13F
	BNE $E980
	JSR $E024
	LDX #$03
	LDA #$26
	STA $0C
	LDA #$C0
	STA $0D
	LDY #$00
	LDA ($0C),Y
	BEQ $E980
	LDY #$08
	LDA $C12C,Y
	CMP #$3F
	BEQ $E978
	CMP ($0C),Y
	BNE $E980
	DEY 
	BPL $E96D
	TXA 
	STA $C13F
	RTS 
;******************************************************************************
; points to next directory entry
	TXA 
	CLC 
	ADC #$10
	BCS $E994	; need to read next directory sector ?
	TAX 	
	LDA $0C
	ADC #$10
	STA $0C
	BCC $E965
	INC $0D
	JMP $E965
	LDA $C024	; yes, gets it...
	BEQ $E9A5
	STA $C002
	LDA $C023
	STA $C001
	JMP $E958
	LDX #$00
	RTS 

;******************************************************************************
; reads boot sector
read_boot_sector	
	LDA $C013
	BNE $E9A7
	STA $C000
	LDA #$13 ; Loading boot sector at $c013 save it in  'buffer_adress_buffer_loading_saving' 
	STA buffer_adress_buffer_loading_saving
	LDA #$C0
	STA buffer_adress_buffer_loading_saving+1
	JMP $E835
;******************************************************************************
; location intended to store a command (not used, how would you write to an eprom ?)
; 
    
    .byt  $20,$20,$20
    .byt  $20,$20,$20,$20,$20,$20,$20,$20,$20,$00

  
   

;E9CA******************************************************************************

	LDX #$0B
	LDA #$20
	STA $E9BD,X
	DEX 
	BPL $E9CE
	JSR $E060
	JSR $0000
	BEQ $EA45
	SEC 
	SBC #$30
	CMP #$04
	BCS $E9F2
	INY 
	STA $C12B
	LDX #$09
	LDA #$20
	STA $C12B,X
	DEX 
	BNE $E9EB
	RTS 
	

	LDA $EA
	PHA 
	LDA $E9
	PHA 
	TYA 
	CLC 
	ADC $E9
	STA $E9
	BCC $EA02
	INC $EA
	JSR $E05A	; evaluates a Basic expression, result on ACC0
	.byt  $8B,$CE,$17,$CF

	BIT $28
	BPL $EA63	; is it a string ?
	JSR $E05A	; yes, gets it
	.byt $15,$D7,$D0,$D7
	
	CMP #$0C	; stores the first 12 chars in E9BD
	BCC $EA1A
	LDA #$0C
	TAY 
	DEY 
	BMI $EA26
	LDA ($91),Y
	STA $E9BD,Y
	JMP $EA1B

	LDA $E9
	PHA 
	LDA #$BD
	STA $E9
	LDA #$E9
	STA $EA
	INY 
	JSR $EA45
	PLA 
	STA $EA
	PLA 
	CLC 
	STA $E9
	SBC $EA
	EOR #$FF
	TAY 
	PLA 
	STA $EA
	RTS 

  
	LDA $C00C
	STA $C12B
	JSR $E9E7
	INY 
	LDA ($E9),Y
	DEY 
	CMP #$CD
	BEQ $EA5A
	CMP #$2D
	BNE $EA6D
	LDA ($E9),Y
	SEC 
	SBC #$30
	CMP #$04
	BCC $EA68
	LDX #$04	; invalid filename
	JMP $E01B
	STA $C12B
	INY 
	INY 
	LDX #$00
	LDA #$06
	JSR $EA8C
	LDA ($E9),Y
	CMP #$2E
	BNE $EA82
	INY 
	LDX #$06
	LDA #$03
	JSR $EA8C
	JSR $0000
	BEQ $EA8B
	CMP #$20
	BNE $EA63
	RTS 


	STA $C141
	LDA ($E9),Y
	CMP #$2A	
	BEQ $EAAB
	CMP #$3F
	BEQ $EAA0
	JSR $E04E
	CMP #$00
	BEQ $EAAA
	STA $C12C,X
	INX 
	INY 
	DEC $C141
	BNE $EA8F
	RTS 
	LDA #$3F
	STA $C12C,X
	INX 
	DEC $C141
	BNE $EAAD
	INY 
	RTS 

	; limits a char to alphanumeric
	CMP #$30
	BCC $EAD0
	CMP #$3A
	BCC $EAD2
	CMP #$41
	BCC $EAD0
	CMP #$5B
	BCC $EAD2
	CMP #$61
	BCC $EAD0
	CMP #$7B
	BCC $EAD2
	LDA #$00
	RTS 


;******************************************************************************
; reads first directory sector
;
	JSR $E033
	LDA $C126
	STA $C001
	LDA $C125
	STA $C002
	LDA #$00
	STA $C13F
	JMP $E045

;******************************************************************************
	LDX #$09
	LDY $C13F
	LDA $C02C,Y
	STA $C12C,X
	INY 
	INX 
	CPX #$10
	BNE $EAEF
	RTS 
;******************************************************************************
	LDX $C13F
	LDY #$06
	LDA $C023,X
	CMP #$20
	BNE $EB0B
	JSR $E057
	INX 
	DEY 
	BNE $EB01
	LDX $C13F
	LDY #$06
	LDA $C023,X
	CMP #$20
	BEQ $EB1E
	JSR $E057
	INX 
	DEY 
	BNE $EB14
	LDA #$2E
	JSR $E057
	LDY #$03
	LDA $C023,X
	JSR $E057
	INX 
	DEY 
	BNE $EB29
	RTS 

;******************************************************************************
	LDA $0C
	STA $C147
	LDA $0D
	STA $C148
	TSX 	
	INX 
	INX 
	STX $C140
	RTS 

;******************************************************************************
; checks no '?' wildcard is used
	LDX #$08
	LDA $C12C,X
	CMP #$3F
	BEQ $EB79
	DEX 
	BPL $EB47
	RTS 

	.byt $43,$4F,$4D ; COM

;******************************************************************************
	JSR $E006
	LDY #$00
	TYA 
	JSR $EA48
	LDA $C132
	CMP #$20
	BNE $EB70
	LDX #$02
	LDA $EB52,X
	STA $C132,X
	DEX 
	BPL $EB67
	JSR $EB45
	JSR $E000 ; Jump to first vector
	JMP $E462
;******************************************************************************
	LDX #$07	; prints "wildcards not allowed"
	JMP $E01B
	


L0000           = $0000
L2065           = $2065
L6469           = $6469
L6765           = $6765
L6966           = $6966
L6C61           = $6C61
L6E65           = $6E65
L6F63           = $6F63
L6F66           = $6F66
L7264           = $7264
L7265           = $7265
L7461           = $7461
L776F           = $776F
LC14B           = $C14B
LE000           = $E000
LE006           = $E006
LE009           = $E009
LE01B           = $E01B
LE04B           = $E04B
LE054           = $E054
LE057           = $E057
LE05A           = $E05A
LE05D           = $E05D
LE060           = $E060
LE069           = $E069
;LE0E6           = $E0E6
LE14D           = $E14D
LE83F           = $E83F
LEB45           = $EB45
L3156           = $3156
L3931           = $3931
L4142           = $4142
L4154           = $4154
L5246           = $5246
L5845           = $5845
L5942           = $5942
L6461           = $6461
L6572           = $6572
L6964           = $6964
L6E6F           = $6E6F
L7973           = $7973
LBFE0           = $BFE0
LBFF8           = $BFF8
LD45A           = $D45A

LE003           = $E003


LE00C           = $E00C
LE02D           = $E02D
LE048           = $E048

LEE92           = $EE92
LEEA3           = $EEA3
LEEAE           = $EEAE
        


LED5A	= $ED5A
LED3F =$ED3F

RESET_START
	; Here we go !
	sei	; Stop Interrupts, inits cpu then waits
        cld	; Decimal mode
        ldx     #$FF	; Set stack pointer
        txs	
        inx	; X =0 
        txa	;  a = 0
        tay	; Y= 0 
LEB86:  dex	; X=0xff
        bne     LEB86 ; Loop something needs to be waiting ?
        dey
        bne     LEB86 ; Loop again with Y
LEB8C:  sta     $C000,x ; Set $c000 to 0, X=0 when it enters, clears some critical pages
        sta     $C100,x ; Set ram overlay to 0 for area from C000 to $c1ff
        sta     $00,x ;  clear page zero with 0
        sta     $0200,x ; Clear page 2 also
        dex ; Let's loop
        bne     LEB8C
        ldx     #$7A ; We initialize page 4 with datas in $eeed
LEB9C:  lda     $EEED,x ; 
        sta     $0480,x ; Store it in page 4; transfers switching routines in page 4
        dex	
        bpl     LEB9C ; Lets loop to copy 7b bytes
        jsr     LEEAE ; We test RAM ? , if not correct it display an error,  checks overlay ram
        ldx     #$0C
.(
loop
	lda     $EF68,x ; Copy 13 bytes from $ef68, copies a routine in BFE0
        sta     LBFE0,x ; Some kind of wrapper, to read rom location C002
        dex
        bpl     loop ; we loop
.)
        jsr     LBFE0 ; ok let's switch to page 4
	
        cpy     #$EA
        beq     LEBC9 ;  is it a Basic v1.0 ?
        lda     #$01 ; Store 1 to $c007, used as a variable for disc system FIXME
        sta     $C007 ; indicates a Basic v1.1
        lda     #$44
        sta     $04DC
        lda     #$47
        sta     $04E4
LEBC9:  ldx     #$FF ; fakes the Basic's initialization
        stx     $A9
        lda     #$FF ; Set HIMEM ? Something like this (to avoid to crach hires video ram ? 
        ldy     #$97
        sta     $A6
        sty     $A7
        sta     $02C1
        sty     $02C2
        sta     $A2
        sty     $A3
        ldx     #$1C
LEBE1:  lda     $EECF,x ; ok We copy again some datas ($1c bytes)
        sta     $E1,x
        dex
        bne     LEBE1
        lda     $C007
        beq     part_oric_1
        lda     #$B9 ; atmos part
        sta     $F0
        lda     #$EC
        sta     $F1
        lda     #$20
        sta     $024E
        lda     #$04
        sta     $024F
        lda     #$00
        sta     $0260
        ldx     #$12
LEC07:  lda     $EE5D,x
        sta     $0238,x ; copy IRQ handler atmos handler ..., 
        dex
        bpl     LEC07
        lda     #$B0
        ldy     #$CC
        bmi     microdisc_continue_init
; $EC16
part_oric_1
	lda     #$FF ; oric1 part
        ldy     #$BF
        sta     $02E1 ; Store to Basic parameters
        sty     $02E2
LEC20        
	ldx     #$08
LEC22:  lda     $EE54,x ; Set Oric-1 Handler !
        sta     $0228,x
        dex
        bpl     LEC22
        lda     #$ED
        ldy     #$CB
; $EC2F
microdisc_continue_init
	sta     $1B ; both
        sty     $1C
        lda     #$4C
        sta     $1A
        sta     $C3
        sta     $21
        sta     $02FB
        lda     #$A0
        ldy     #$D2
        ldx     $C007
        beq     LEC4B
        lda     #$36
        ldy     #$D3
LEC4B:  sta     $22
        sty     $23
        sta     $02FC
        sty     $02FD
        lda     #$C4
        ldy     #$04
        sta     $02F5 ; Set Vector for command "!" to $04C4
        sty     $02F6
        lda     #$00
        sta     $04FF
        sta     $04FD
        jsr     LE05A ; inits the oric with the NMI routine of Basic
        dey ; Dunno some .byt
        sed; Dunno
        clv; Dunno
        sed; Dunno
        lda     #$50
        sta     $31
        lda     #$30
        sta     $32
        lda     #$03
        sta     $C2
        lda     #$00
        sta     $D7
        sta     $88
        sta     $2F
        pha
        sta     $0500
        sta     $0501
        sta     $0502
        sta     $02F7
        sta     $2E
        sta     $02F1
        sta     $02F2
        sta     $02F4
        lda     #$88
        sta     $85
        lda     #$02 ; We set mode RELEASE/TEXT
        sta     $02C0  ; Variable to set management of video ram
        lda     #$01
        ldy     #$05
        sta     $9A
        sty     $9B
        lda     #$03 ; Set some variables in zero page
        sta     $9C
        sty     $9D
        sta     $9E
        sty     $9F
        sta     $A0
        sty     $A1
        ldx     #$00 ; prints 'insert system disc'
        jsr     LEE92
        ldx     #$09 ; copies SYSTEMDOS filename to C12B
LECC0:  lda     $EE40,x
        sta     $C12B,x
        dex
        bpl     LECC0
        lda     #$8A ; initializes Error address to EE8A
        sta     $C149
        lda     #$EE
        sta     $C14A
        ldx     #$D8 ; 'Force Interrupt' command
        stx     MICRODISC_FDC_COMMAND
        ldx     #$08
        jsr     LE003  ; Restores track 0
        jsr     LE048 ; read sector
        jsr     LE000; initializes some parameters
        jsr     LE009 ; loads SYSTEM.DOS
        jsr     LEEA3 ; clears the top line
        ldx     #$08 ; copies Basic line "!BOOTUP" to TIB
LECEB:  lda     LED5A,x
        sta     $35,x
        dex
        bpl     LECEB
        ldx     #$FF ; prints DOS version on top line
LECF5:  inx
        lda     $9FD0,x
        sta     $BB82,x
        bne     LECF5
        ldx     #$1A ; copies a routine to BFE0
LED00:  lda     LED3F,x
        sta     LBFE0,x
        dex
        bpl     LED00 
        lda     #$AE ; prints Basic copyright
        ldy     #$ED
        ldx     $C007
        beq     LED16
        lda     #$F1
        ldy     #$ED
LED16:  sta     $0C
        sty     $0D
        jsr     LE02D
        ldx     #$09 ; copies filename BOOTUPCOM to C12B
LED1F:  lda     $EE4A,x
        sta     $C12B,x
        dex
        bpl     LED1F
        jsr     LE00C  ;searches BOOTUPCOM in directory
        cpx     #$00
        bne     LED3C ; found BOOTUPCOM ? executes !BOOTUP
        stx     $35 ; no, removes !BOOTUP command from TIB
        lda     #$35
        sta     $0C
        lda     #$EE
        sta     $0D
        jsr     LE02D ; and prints Ready

LED3C:  jmp     LBFE0 ; goes to ram in order to activate overlay ram
 
;******************************************************************************
; ED3F-ED59 : routine copied to BFE0
;              switches to overlay ram and starts the OS
;
; ED3F
	SEI 
	LDA #$84
	STA $0480
	STA MICRODISC_FDC
	JSR $BFF8
	LDX #$34
	LDY #$00
	CLI 
	JSR $D45A	; calls the Basic interpreter (no return)
	.byt $CD,$C4,$BD,$C4
	JMP ($C14B)	; init the OS



    
    	.asc "!BOOTUP"
    	.byt $00,$00
    	; ed63
    	.asc "insert system disc",0
	.byt $0c
    	.asc "No operating system on disc"
    	.byt $08,$00,$0c
    	.asc "RV1 adjustment required"
    	.byt $08,$00,$0c
	.asc "ORIC EXTENDED BASIC V1.0"
	.byt $0d,$0a,$60
    	.asc " 1983 TANGERINE"
	.byt $0d,$0a,$0a,$0a
    	.asc "47870 BYTES FREE"
	.byt $0d,$0a,$0a
    	.byt  $00,$0c
    	.asc "ORIC EXTENDED BASIC V1.1"
    	.byt $0d,$0a,$60
    	.asc " 1983 TANGERINE"
    	.byt $0d,$0a,$0a,$0a
    	.asc " 37631 BYTES FREE"
	.byt  $0d,$0a,$0a,$00,$0d,$0a
    	.asc "Ready "
    	.byt $0d,$0a,$00
    	.byt  $00
    	.asc "SYSTEMDOS",0
    	.asc "BOOTUPCOM"
    ;******************************************************************************
; EE54-EE5C vectors copied to 0228 (oric1)
;******************************************************************************

	JMP $EC03
	JMP $F430
	.byt 1,0
	RTI 

;******************************************************************************
; EE5D-EE6F vectors copied to 0238 (atmos)
;******************************************************************************

	JMP $F77C
	JMP $EB78
	JMP $F5C1
	JMP $F865
	JMP $EE22
	JMP $F8B2
	RTI 



; An error occurs ! We switch to text, and display a message
; This part is coded a little crap , 

;ee70  
  
jump1
        ldx     #$31 ; Error prints 'RV1 adjustment required'
        ldy     #$00 ; and halts the system
        lda     #$1A ; Ok we fill ram to have text mode ... at least a bit Crap because it fills 256*5 bytes
.(
loop	
	sta     $BB80,y ; 1280 bytes are filled but Text mode has only 1120 bytes
        sta     $BC80,y ; This routine smells an optimized code
        sta     $BD80,y ; So it does a 'Buffer overflow', even if it does not crash anything
        sta     $BE80,y
        sta     $BEFE,y
        dey
        bne     loop
.)
        beq    hou


; ee8a

        ldx     #$13 ; index of 'No operating system on disc' 
; ee8c
hou
        jsr     jump14 ; A bit strange !
loop_myself        
	jmp     loop_myself ; We loop on ourself ! An error is displayed ; halt the system
; ee92
jump14
        jsr     eprom_microdisc_fill_space_on_top_line	 ; clears the top line
.(        
	jmp     eprom_microdisc_display_message ; and prints a message on it
;ee98
loop
	inx ; prints a message on the top line
        sta     $BB82,y ; At the top line
        iny
; $ee9d
eprom_microdisc_display_message

        lda     $ED63,x ; Display 'insert system disc !' or something else depends on X is set
        bne     loop ; Was it the End of string ? '0' no we loop again to next char
.)
MYVALUE
;eea2
        rts
eprom_microdisc_fill_space_on_top_line	
;eea3

	ldy     #$1B ; clears the top line
        lda     #$20 ; Fill first line of ram video with space value
.(
loop:	sta     $BB81,y
        dey
        bne     loop
.)
	; Y equal to 0 here
        rts

; Checks overlay ram
    ; eeae
    ; This routines check ram overlay at $C045 
    ; SOme kind of memory test
        ldx     #$00
.(
loop	
	lda     $C0A5,x ; Get initial value at this place
        tay	; We save it in y -> backup
        lda     #$55 ; Fill buffer with #55
	sta     $C0A5,x ; $c045
        cmp     $C0A5,x  ; we catch this value, 
        bne     jump1 ; This written value is not correct ? RAM problem ?
	lda     #$AA ; No We try to write another value
	sta     $C0A5,x
        cmp     $C0A5,x
        bne     jump1; Value written not the same ! Memory error !
	tya	; Get the old value before we began to write
        sta     $C0A5,x	; And we restore iy
        inx	; -> Lets go to the next byte !
	bne loop
.)
        rts
	;*****************************************************************************
; EED0-EEE0 interpreter routine copied to E2
;*****************************************************************************

	
LEA41=	$EA41

j17 
	INC $E9
	BNE j16
	INC $EA
j16
	LDA $EA60
	CMP #$20
	BEQ j17
	JSR $EA41
	RTS 
    

  

   
    .byt $2c,$60,$ea,$2c,$60,$ea,$60,$80,$4f,$c7,$52,$58

;******************************************************************************
;  EEED-EF67 switching routines transfered to page 4 (address 0480)
;******************************************************************************
    
    ; eeed
    ; This part is copied at $480
    .byt $04 ; $480 virtually
    .byt $00 ; $481 virtually
    .byt $00 ; $482 virtually
    ; eef0
    .byt $00 ; $483 virtually ; temporary storage for A and flags
    .byt $4c ; $484 virtually ; jmp $ea60 ; address replaced for indirect jumps
    .byt $60 ; $485 virtually
    .byt $ea ; 486 virtually
; $487      virtually
    jmp $04e6 
;$48a virtually
    jmp $4d6
;$48d     virtually
    jmp $4de
;$490         virtually
	php ; calls a routine in rom or eprom
    	sei ; destination bank specified in 0481
    	sta $482
L0228           = $0228
L022B           = $022B
L0484           = $0484
L0490           = $0490
L04E6           = $04E6
        pla
        sta     $0483
        lda     $0480
        pha
        lda     $0481
        jsr     L04E6
        lda     $0483
        pha
        lda     $0482
        plp
        jsr     L0484
        php
        sei
        sta     $0482
        pla
        sta     $0483
        pla
        jsr     L04E6
        lda     $0483
        pha
        lda     $0482
        plp
        rts
        lda     #$00
        sta     $0481
        lda     #$66
        sta     $0485
        lda     #$D4
        sta     $0486
        jmp     L0490
        php
        tsx
        inc     $0102,x
        jmp     L0228 ; changed to 0247 for a v1.0
        php
        tsx
        inc     $0102,x
        jmp     L022B ; changed to 0244 for a v1.1 
        sei ; enables/disables rom
        and     #$02
        sta     $0481
        lda     $0480
        and     #$FD
        ora     $0481
        sta     MICRODISC_FDC
        sta     $0480
        rts
  ;*************************************
    
;******************************************************************************
; routine transfered in BFE0, just to read rom location C002...
;******************************************************************************
    
    ; $ef68
    ; This bytes are copied to $bfE0 0x0C bytes
    ; Datas Some kind of wrapper to leave eprom
        lda     #$06
        jsr     $0487
        ldy     $C002
        lda     #$00
        jmp     $0487
    
  ;*************************************
 	.dsb 4187,255
	
MICRODISC_ALIGN	
	.asc "Oric DOS V0.6",0
    .byt $00,$00
    .asc "(C) ORIC 1983",0
    .dsb 12,0

NMI_VECTOR        
    .word NMI_START
RESET_VECTOR ; Boot
    .word RESET_START
IRQ_VECTOR    
    .word IRQ_START
END_MICRODISC_EPROM




