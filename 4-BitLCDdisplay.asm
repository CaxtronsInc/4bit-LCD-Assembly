4bit-LCD-Assembly
=================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                                  ;
;                             Title:   LCD using 4 bit mode                                        ;
;                             Author:  Augustine Odiadi                                            ;
;                             Date:    12/01/2010                                                  ;
;                                                                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        #INCLUDE         <P16F84a.INC>
        LIST            p=pic16f84a
        __CONFIG _CP_OFF & _XT_OSC & _PWRTE_ON & _WDT_OFF
        ERRORLEVEL      -302        ;Eliminate bank warning

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                                  ;
;                                    PIC16F84A  Microcontroller                                    ;
;                                            ____ ____                                             ;
;                    O                 RA2 -|    -    |- RA1                    O                  ;
;                    O                 RA3 -|         |- RA0                    O                  ;
;                    O                 RA4 -|         |- RA7                    O                  ;
;                                     MCLR -|         |- RA6                    O                  ;
;                                      VSS -|         |- VDD                                       ;
;                    O                 RB0 -|         |- RB7  LCD bit 7         O                  ;
;                    O        LCD R/S  RB1 -|         |- RB6  LCD bit 6         O                  ;
;                    O                 RB2 -|         |- RB5  LCD bit 5         O                  ;
;                    O          LCD E  RB3 -|_________|- RB4  LCD bit 4         O                  ;
;                                                                                                  ;
;                                                                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Assign labels to General Purpose Registers (SRAM) starting at address '0C' (see datasheet page 8) & Set EQU values
CBLOCK 0x0C                         ; Address to start using General Purpose Registers
GPR1                                ; Countdown Register
DATA1                               ; Temp data store for 4 bit mode
DATA2                               ; Temp data store for 4 bit mode
DELAYGPR1                           ; 
DELAYGPR2                           ; 
DELAYGPR3                           ; 
ENDC 

E EQU 3                             ; Used for PORTB, 'Enable' Line
RS EQU 1                            ; Used for PORTB, 'Register Select' Line


SETIO  ; This rountine sets up the microcontrollers Inputs and Outputs
        BSF     STATUS, RP0         ; Bank 1
        CLRF    TRISA               ; Set all of PORTA to outputs 
        CLRF    TRISB               ; Set all of PORTB to outputs
        BCF     STATUS, RP0         ; Bank 0
        CLRF    PORTA               ; Initialize PORTA by clearing output data latches
        CLRF    PORTB               ; Initialize PORTB by clearing output data latches
        
        BSF     PORTB, E            ; Enable Pin (Active Low). Must be set initially and held high until data is to be clocked in by lowering
        
        
MAIN_PROGRAM
        CALL    INITIALISE_LCD      ; 
        CALL    CLEAR_DISPLAY       ; Clear Display
        CALL    PRINT               ; 
        GOTO    STOP                ; 
        
        
INITIALISE_LCD  ; This routine initialises the LCD so that it is ready to display characters
        BCF     PORTB, RS           ; Register Select (0=Command, 1=Character)
        
        MOVLW   B'00110010'         ; As the LCD is initialised on power up to 8 bit mode, anything that is present on LCD Data Lines D0-D7 is captured on toggling of the enable line.
                                    ; Therefore we only need to toggle the enable line once with the command of 0010xxxx to place it into 4 bit mode. The LCD is now waiting for the next command to be sent as 2 nibbles (hi, then low) 
                                    ; Becasue we have a routine called FOUR_BIT_MODE which breaks the command up into 2 halves and sends them 1 by 1 that is universal to the whole program I want to use this routine to send the command for 4 bit mode.
                                    ; BUT, as soon as we send the first nibble of 0010 (not a typo) the LCD will be placed in 4 bit mode and the second nibble will be treated as the first half of the following command. 
                                    ; To overcome this, the first nibble that is sent (0011+'0000' <-- the 0000 comes from the LCD D0-D3 data lines that are grounded) to place it in 8 bit mode, then the second nibble of 0010+'0000' is sent and the LCD is placed in 4 bit mode
                                    ; So, this instruction of "MOVLW   B'00110010'" should be looked at in 2 halves (ie. 0011+'0000' & 0010+'0000')
                                    ; IN SUMMARY: First nibble initialises LCD to 8-bit (0011 along with the 4 LCD lines D0-D3 that are tied to ground make 00110000, then the lower nibble changes the LCD to 4-bit mode (00100000).
                                    ; 
        CALL    FOUR_BIT_MODE       ; Send upper nibble, followed by lower nibble
        
        MOVLW   B'00101000'         ; Function Set (Command): 4-Bit Mode, 2 Line, 5x7 Dot Matrix
        CALL    FOUR_BIT_MODE       ; Send upper nibble, followed by lower nibble
        
        MOVLW   B'00001111'         ; Display On/Off & Cursor (Command): Display On, Cursor Underline ON, Cursor Blink ON
        CALL    FOUR_BIT_MODE       ; Send upper nibble, followed by lower nibble
        RETURN
        
        
CLEAR_DISPLAY
        BCF     PORTB, RS           ; Register Select (0=Command, 1=Character)
        
        MOVLW   B'00000001'         ; Clear Display (Command): 
        CALL    FOUR_BIT_MODE       ; Send upper nibble, followed by lower nibble
        RETURN
        
        
FOUR_BIT_MODE
        MOVWF   DATA1               ; Move byte to GPR to store it
        ANDLW   B'11110000'         ; 
        MOVWF   DATA2               ; 
        MOVF    PORTB, W            ; 
        ANDLW   B'00001111'         ; 
        IORWF   DATA2, W            ; 
        MOVWF   PORTB               ; 
        CALL    TOGGLE_E            ; 
        
        SWAPF   DATA1, F            ; 
        MOVF    DATA1, W            ; 
        ANDLW   B'11110000'         ; 
        MOVWF   DATA2               ; 
        MOVF    PORTB, W            ; 
        ANDLW   B'00001111'         ; 
        IORWF   DATA2, W            ; 
        MOVWF   PORTB               ; 
        CALL    TOGGLE_E            ; 
        RETURN
        
        
TOGGLE_E  ; This routine is called when a command/character is ready to be entered into the LCD
        BCF     PORTB, E            ; Data is enter on the falling edge, therefore we clear the E line to enter data
        CALL    DELAY_1mS           ; Wait
        CALL    DELAY_1mS           ; Wait
        BSF     PORTB, E            ; Set E line ready for next data string
        RETURN
        
PRINT
        BSF     PORTB, RS           ; Register Select (0=Command, 1=Character)
        MOVLW   D'0'                ; Line of charater table to start printing from (+1)
        MOVWF   GPR1                ; Move Working Register to General Purpose Register
        
        MOVF    GPR1, W             ; Move GPR1 to the Working Register. This number will be the line number of the table we will start printing from (+1)
        CALL    CHARACTER_TABLE     ; Call 'CHARACTER_TABLE' to get character
        MOVWF   DATA1               ; Move Charater byte to GPR
        XORLW   B'00000000'         ; Is this the last line of the charater table? XOR the ASCII Character with '0' (The 'XORLW' intruction affects the 'Zero' bit or the Status Register)
        BTFSC   STATUS, Z           ; Test 'Zero' bit of Status Register
        GOTO    $+4                 ; If the ASCII Character returned is a zero, we have already finished printing the word. This is what the H'0' after every string of characters in the lookup table is. So we skip 5 instructions to the RETURN
        CALL    FOUR_BIT_MODE       ; Enter charater to LCD in 4 bit mode
        INCF    GPR1                ; Increment GPR to print next ASCII Character
        GOTO    $-8                 ; Do it again
        RETURN
        
CHARACTER_TABLE ; This routine holds all the words needed for the LCD display
        ADDWF   PCL,F
        RETLW   A' '                ; 
        RETLW   A'L'                ;  
        RETLW   A'C'                ;  
        RETLW   A'D'                ;  
        RETLW   A' '                ;  
        RETLW   A'4'                ;  
        RETLW   A'-'                ; 
        RETLW   A'B'                ; 
        RETLW   A'I'                ; 
        RETLW   A'T'                ; 
        RETLW   A' '                ; 
        RETLW   A'M'                ; 
        RETLW   A'O'                ; 
        RETLW   A'D'                ;
        RETLW   A'E'                ;
        RETLW   H'0'                ; 
        
        RETURN
        
; DELAY ROUTINES
; Actual delay = 0.001 seconds = 1000 cycles
DELAY_1mS
        MOVLW    0xC6                ;
        MOVWF    DELAYGPR1           ;
        MOVLW    0x01                ;
        MOVWF    DELAYGPR2           ;
DELAY_1mS_0                          ;
        DECFSZ   DELAYGPR1, f        ;
        GOTO     $+2                 ;
        DECFSZ   DELAYGPR2, f        ;
        GOTO     DELAY_1mS_0         ; 993 cycles
        GOTO     $+1                 ;
        NOP                          ; 3 cycles
        RETURN                       ; 4 cycles (including call)
        
        
STOP
        GOTO    STOP
        END
 
 
