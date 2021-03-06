;*******************************************************************************
; a1mon68: A MC68HC11 rewrite of the rewrite of the Apple 1 monitor that ran on
; an MC6800 microprocessor, rather than the MCS6502 microprocessor that was
; standard.
;
; Copyright 2011 Eric Smith <spacewar@gmail.com>
;
; This source code will assemble with the ASM11 Macro Assembler:
; http://www.aspisys.com/asm11.htm
;
; This program is free software; you can redistribute and/or modify it
; under the terms of the GNU General Public License version 3 as
; published by the Free Software Foundation.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;*******************************************************************************

;*******************************************************************************
                    #RAM      $0024
;*******************************************************************************

xam                 rmb       2
st                  rmb       2
h                   rmb       1
l                   rmb       1
                    rmb       1
mode                rmb       1
ysav                rmb       2
inptr               rmb       2

in                  equ       512

;*******************************************************************************

KBD                 equ       $d010
KBD_CR              equ       $d011
DSP                 equ       $d012
DSP_CR              equ       $d013

;*******************************************************************************
                    #ROM      $ff00
;*******************************************************************************

Start               proc
                   ;cld                           ; No decimal mode on 6800, so we don't need
                                                  ; need to clear it.
;                   cli                           ; Disable interrupts - not actually needed on reset.
                    ldb       #$7f                ; Mask for DSP data direction register.
                    stb       DSP                 ; Set it up.
                    ldb       #$a7                ; KBD and DSP control register mask.
                    stb       KBD_CR              ; Enable interrupts, set CA1, CB1, for
                    stb       DSP_CR              ; positive edge sense/output mode.
                    lds       #$01ff              ; On the 6502, the monitor didn't initialize the
          ;--------------------------------------
          ; stack pointer, which was OK because it was guaranteed to be
          ; somewhere in page 1. Not so on the 6800!
          ; Ideally, I'd take advantage of the stack starting right before the
          ; input buffer to save a few bytes, but I haven't yet figured out how
          ; to do it.
          ;
          ; Note that B contains $a7 here, which means that the incb below will
          ; set the negative flag, causing the bpl to fall through into escape.
          ; This saves us a "bra escape" instruction here.
          ;
          ; Get a line of input from the keyboard, echoing to display.
          ; Normally enter at escape or getline.
          ;--------------------------------------
NotCR@@             cmpa      #'_'|$80            ; "_"? [NB back arrow]
                    beq       Backspace@@         ; Yes.
                    cmpa      #$1b|$80            ; ESC?
                    beq       Esc@@               ; Yes.
                    inx                           ; Advance text index.
                    incb
                    bpl       NextChar@@          ; Auto ESC if > 127.

Esc@@               lda       #'\'|$80            ; '\'.
                    jsr       Echo                ; Output it.

GetLine@@           lda       #13|$80             ; CR.
                    jsr       Echo                ; Output it.
                    ldx       #in+1               ; Initiallize [sic] text index.
                    ldb       #1
Backspace@@         dex                           ; Back up text index.
                    decb
                    bmi       GetLine@@           ; Beyond start of line, reinitialize.

NextChar@@          lda       KBD_CR              ; Key ready?
                    bpl       NextChar@@          ; Loop until ready.
                    lda       KBD                 ; Load character. B7 should be '1'.
                    sta       ,x                  ; Add to text buffer.
                    bsr       Echo                ; Display character.
                    cmpa      #13|$80             ; CR?
                    bne       NotCR@@             ; No.
          ;-------------------------------------- ; Process an input line.
                    ldx       #in+256-1           ; Reset text index to in-1, +256 so that
                                                  ; 'inc inptr+1' will result in $0200.
                    stx       inptr
                    clra                          ; For XAM mode. 0->B.

SetBlock@@          asla                          ; Leaves $56 if setting BLOCK XAM mode.
SetMode@@           sta       mode                ; $00 = XAM, $BA = STOR, $56 = BLOK XAM.
BlankSkip@@         inc       inptr+1             ; Advance text index.
NextItem@@          ldx       inptr
                    lda       ,x                  ; Get character.
                    cmpa      #13|$80             ; CR?
                    beq       GetLine@@           ; Yes, done this line.
                    cmpa      #$ae                ; "."?
                    beq       SetBlock@@          ; Set BLOCK XAM mode.
                    bls       BlankSkip@@         ; Skip delimiter.
                    cmpa      #':'|$80            ; ":"?
                    beq       SetMode@@           ; Yes, set STOR mode.
                    cmpa      #'R'|$80            ; "R"?
                    beq       run                 ; Yes, run user program.
                    clr       l                   ; $00->L.
                    clr       h                   ; and H.
                    stx       ysav                ; Save Y for comparison.

NextHex@@           ldx       inptr
                    lda       ,x                  ; Get character for hex test.
                    eora      #$b0                ; Map digits to $0-9.
                    cmpa      #9                  ; Digit?
                    bls       Dig@@               ; Yes.
                    adda      #$89                ; Map letter "A"-"F" to $FA-FF.
                    cmpa      #$f9                ; Hex letter?
                    bls       NotHex@@            ; No, character not hex.

Dig@@               asla:4                        ; Hex digit to MSD of A.

                    ldb       #4                  ; Shift count.
HexShift@@          asla                          ; Hex digit left, MSB to carry.
                    rol       l                   ; Rotate into LSD.
                    rol       h                   ; Rotate into MSD's.
                    decb                          ; Done 4 shifts?
                    bne       HexShift@@          ; No, loop.

                    inc       inptr+1             ; Advance text index.
                    bra       NextHex@@           ; Always taken. Check next character for hex.

NotHex@@            cpx       ysav                ; Check if L, H empty (no hex digits).
                    beq       Esc@@               ; Yes, generate ESC sequence.
                    tst       mode                ; Test MODE byte.
                    bpl       NotStor             ; B6=0 for STOR, 1 for XAM and BLOCK XAM
          ;-------------------------------------- ; STOR mode
                    ldx       st
                    lda       l                   ; LSD's of hex data.
                    sta       ,x                  ; Store at current 'store index'.
                    inx
                    stx       st
?ToNextItem         bra       NextItem@@          ; Get next command item.

;*******************************************************************************

prbyte              proc
                    psha                          ; Save A for LSD.
                    lsra:4                        ; MSD to LSD position.
                    bsr       PrHex               ; Output hex digit.
                    pula                          ; Restore A.
;                   bra       PrHex

;*******************************************************************************

PrHex               proc
                    anda      #$0f                ; Mask LSD for hex print.
                    ora       #'0'|$80            ; Add "0".
                    cmpa      #'9'|$80            ; Digit?
                    bls       Echo                ; Yes, output it.
                    adda      #7                  ; Add offset for letter.
;                   bra       Echo

;*******************************************************************************

Echo                proc
Loop@@              tst       DSP                 ; DA bit (B7) cleared yet?
                    bmi       Loop@@              ; No, wait for display.
                    sta       DSP                 ; Output character. Sets DA.
                    rts                           ; Return.

;*******************************************************************************

run                 proc
                    ldx       xam
                    jmp       ,x                  ; Run at current XAM index.

;*******************************************************************************

NotStor             proc
                    bne       XamNext@@           ; mode = $00 for XAM, $56 for BLOCK XAM.
                    ldx       h                   ; Copy hex data to
                    stx       st                  ; 'store index'.
                    stx       xam                 ; And to 'XAM index'.
                    clra                          ; set Z flag to force following branch.

Loop@@              bne       PrData@@            ; NE means no address to print.
                    lda       #13|$80             ; CR.
                    bsr       Echo                ; Output it.
                    lda       xam                 ; 'Examine index' high-order byte.
                    bsr       prbyte              ; Output it in hex format.
                    lda       xam+1               ; Low-order 'Examine index' byte.
                    bsr       prbyte              ; Output it in hex format.
                    lda       #':'|$80            ; ":".
                    bsr       Echo                ; Output it.

PrData@@            lda       #' '|$80            ; Blank.
                    bsr       Echo                ; Output it.

                    ldx       xam
                    lda       ,x                  ; Get data byte at 'examine index'.
                    bsr       prbyte              ; Output it in hex format.

XamNext@@           clr       mode                ; 0->MODE (XAM mode).
                    ldx       xam                 ; Compare 'examine index' to hex data.
                    cpx       h
                    beq       ?ToNextItem         ; Not less, so more data to output.
                    inx
                    stx       xam
                    lda       xam+1               ; Check low-order 'examine index' byte
                    anda      #$07                ; For MOD 8 = 0
                    bra       Loop@@              ; always taken

;*******************************************************************************
                    #VECTORS  $FFF8               ; vector table
;*******************************************************************************

                    fdb       $0000               ; IRQ
                    fdb       $0000               ; SWI
                    fdb       $f000               ; NMI
                    fdb       Start               ; RESET
