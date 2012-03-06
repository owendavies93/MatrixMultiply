;
; ************************************************************
;       SKELETON: INTEL ASSEMBLER MATRIX MULTIPLY (LINUX)
; ************************************************************
;
;
; --------------------------------------------------------------------------
; class matrix {
;     int ROWS              // ints are 64-bit
;     int COLS
;     int elem [ROWS][COLS]
;
;     void print () {
;	  output.newline ()
;         for (int row=0; row < this.ROWS; row++) {
;             for (int col=0; col < this.COLS; cols++) {
;	          output.tab ()
;                 output.int (this.elem[row, col])
;	      }
;             output.newline ()
;         }
;     }
;
;     void mult (matrix A, matrix B) {
;         for (int row=0; row < this.ROWS; row++) {
;             for (int col=0; col < this.COLS; cols++) {
;                 int sum = 0
;                 for (int k=0; k < A.COLS; k++)
;                     sum = sum + A.elem[row, k] * B.elem[k, col]
;                 this.elem [row, col] = sum
;             }
;         }
;     }
; }
; ---------------------------------------------------------------------------
; main () {
;     matrix matrixA, matrixB, matrixC	; Declare and suitably initialise
;  					  matrices A, B and C
;     matrixA.print ()
;     matrixB.print ()
;     matrixC.mult (matrixA, matrixB)
;     matrixC.print ()
; }
; ---------------------------------------------------------------------------
;
; Notes:
; 1. For conditional jump instructions use the form 'Jxx NEAR label'  if label
;    is more than 127 bytes from the jump instruction Jxx.
;    For example use 'JGE NEAR end_for' instead of 'JGE end_for', if the
;    assembler complains that the label end_for is more than 127 bytes from
;    the JGE instruction with a message like 'short jump is out of  range'
;
;
; ---------------------------------------------------------------------------

segment .text
	global	_start
_start:

main:
          mov  rax, matrixA	; matrixA.print ()
          push rax
          call matrix_print
          add  rsp, 8

          mov  rax, matrixB	; matrixB.print ()
          push rax
          call matrix_print
          add  rsp, 8

          mov  rax, matrixB	; matrixC.mult (matrixA, matrixB)
          push rax
          mov  rax, matrixA
          push rax
          mov  rax, matrixC
          push rax
          call matrix_mult
          add  rsp, 24		; pop parameters & object reference

          mov  rax, matrixC	; matrixC.print ()
          push rax
          call matrix_print
          add  rsp, 8

          call os_return		; return to operating system

; ---------------------------------------------------------------------

matrix_print:   ; void matrix_print ()
        push    rbp                                 ; setup base pointer
        mov     rbp, rsp
        
        push    rax                                 ; address of Matrix
        push    rbx                                 ; for ROWS
        push    rcx                                 ; for COLS
        push    rdx                                 ; outer iterator
        push    r15                                 ; inner iterator
        push    r14                                 ; temp storage  
        push    r13                                 ; temp storage
        
        mov     rax, [rbp + 16]                     ; store address in rax
        mov     rbx, [rax]                          ; rbx = ROWS
        mov     rcx, [rax + 8]                      ; rcx = COLS

forP:
        call    output_newline
        mov     rdx, 0                              ; rdx = 0 (iterator)
        
        
nextP:
        cmp     rdx, rbx                            ; if rdx >= rbx (ROWS)
        jge     endForP                             ; end outer for loop
        
        forInP:
                mov     r15, 0                      ; r15 = 0 (iterator)
                
        nextInP:
                cmp     r15, rcx                    ; if r15 >= rcx
                jge     endInP                      ; end inner for loop
                
                call    output_tab
                
                mov     r13, r15
                imul    r13, 8                      ; r13 = r15 * 8
                
                mov     r14, rdx
                imul    r14, rcx
                imul    r14, 8                      ; r12 = rdx * rcx * 8
                
                add     r14, r13                    ; r12 += r13
                    
                push    qword [rax + 16 + r14]      ; r14 = rax + 16 + r12
                call    output_int
                add     rsp, 8
                
                inc     r15                         ; r15++
                jmp     nextInP                     ; loop
                
        endInP:
                call    output_newline  
                inc     rdx                         ; rdx++
                jmp     nextP                       ; loop
          
endForP:
        pop     r13
        pop     r14
        pop     r15
        pop     rdx
        pop     rcx
        pop     rbx
        pop     rax                                 ; pop all the registers
        
        pop     rbp                                 ; restore bp & return
        ret

;  --------------------------------------------------------------------------

matrix_mult:    ; void matix_mult (matrix A, matrix B)
        push    rbp                                 ; setup base pointer
        mov     rbp, rsp
        
        push    rax                                 ; address of Matrix A
        push    rbx                                 ; address of Matrix B
        push    rcx                                 ; address of Matrix C
        push    rdx                                 ; for outer iterator
        push    r15                                 ; for middle iterator
        push    r14                                 ; for inner iterator
        push    r13                                 ; for this.ROWS
        push    r12                                 ; for this.COLS
        push    r11                                 ; for A.COLS
        push    r10                                 ; for sum
        
        mov     rax, [rbp + 16]                     ; rax = this
        mov     r13, [rax]                          ; r13 = this.ROWS
        mov     r12, [rax + 8]                      ; r12 = this.COLS

forM:
        mov     rdx, 0                              ; rdx = 0 (iterator)
        
nextM:   
        cmp     rdx, r13                            ; if rdx >= r13
        jge     endForM                             ; end outer for loop
        
        forMidM:
                mov     r15, 0                      ; r15 = 0 (iterator)
                
        nextMidM:
                cmp     r15, r12                    ; if r15 >= r12
                jge     endMidM                     ; end middle for loop
                mov     r10, 0                      ; sum = 0
                
                forInM:
                        mov     r14, 0              ; r14 = 0 (iterator)
                        
                nextInM:
                        cmp     r14, r11            ; if r14 >= r11
                        jge     endInM              ; end inner for loop
                        
                        ;sum = sum + A.elem[row,k] * B.elem[k, col]
                        
                        inc     r14
                        jmp     nextInM
                        
                endInM:
                        ; this.elem[row, col] = sum
                        
                inc     r15
                jmp     nextMidM
                
        endMidM:
                inc     rdx
                jmp     nextM
                
endForM:        
        pop     r10
        pop     r11
        pop     r12
        pop     r13
        pop     r14
        pop     r15
        pop     rdx
        pop     rcx
        pop     rbx
        pop     rax                                 ; pop all the registers

        pop     rbp                                 ; restore bp & return
        ret


; ---------------------------------------------------------------------
;                    ADDITIONAL METHODS

CR     	equ     13		; carriage-return
LF     	equ     10		; line-feed
TAB	equ	9		; tab
MINUS	equ	'-'		; minus

LINUX  	equ     80H		; interupt number for entering Linux kernel
EXIT   	equ     1		; Linux system call 1 i.e. exit ()
WRITE	equ	4		; Linux system call 4 i.e. write ()
STDOUT	equ	1		; File descriptor 1 i.e. standard output

; ------------------------

os_return:
	mov  rax, EXIT		; Linux system call 1 i.e. exit ()
	mov  rbx, 0		; Error code 0 i.e. no errors
	int  LINUX		; Interrupt Linux kernel

output_char:			; void output_char (ch)
         push rax
         push rbx
         push rcx
         push rdx
	 push r8		; r8..r11 are altered by Linux kernel interrupt
	 push r9
	 push r10
	 push r11
	 push qword [octetbuffer] ; (just to make output_char() re-entrant...)

	mov  rax, WRITE		; Linux system call 4; i.e. write ()
	mov  rbx, STDOUT	; File descriptor 1 i.e. standard output
	mov  rcx, [rsp+80]	; fetch char from non-I/O-accessible segment
	mov  [octetbuffer], rcx	; load into 1-octet buffer
	lea  rcx, [octetbuffer]	; Address of 1-octet buffer
	mov  rdx, 1		; Output 1 character only
	int  LINUX		; Interrupt Linux kernel

	 pop qword [octetbuffer]
	 pop  r11
	 pop  r10
	 pop  r9
	 pop  r8
         pop  rdx
         pop  rcx
         pop  rbx
         pop  rax
         ret

; ------------------------

output_newline:			; void output_newline ()
	push qword LF
	call output_char
	add rsp, 8
         ret

; ------------------------

output_tab:			; void output_tab ()
	push qword TAB
	call output_char
	add  rsp, 8
         ret

; ------------------------

output_minus:			; void output_minus()
	push qword MINUS
	call output_char
	add  rsp, 8
         ret

; ------------------------

output_int:                     ; void output_int (int N)
         push rbp
         mov  rbp, rsp

         ; rax=N then N/10, rdx=N%10, rbx=10

         push rax                ; save registers
         push rbx
         push rdx

         cmp  qword [rbp+16], 0 ; minus sign for negative numbers
         jge  L88

         call output_minus
         neg  qword [rbp+16]

L88:
         mov  rax, [rbp+16]       ; rax = N
         mov  rdx, 0              ; rdx:rax = N (unsigned equivalent of "cqo")
         mov  rbx, 10
         idiv rbx                ; rax=N/10, rdx=N%10

         cmp  rax, 0              ; skip if N<10
         je   L99

         push rax                ; output.int (N / 10)
         call output_int
         add  rsp, 8

  L99:
         add  rdx, '0'           ; output char for digit N % 10
         push rdx
	call output_char
	add  rsp, 8

         pop  rdx                ; restore registers
         pop  rbx
         pop  rax
         pop  rbp
         ret


; ---------------------------------------------------------------------

segment .data

	; Declare test matrices
matrixA	DQ 2    		; ROWS
	DQ 3    		; COLS
	DQ 1, 2, 3     		; 1st row
	DQ 4, 5, 6   		; 2nd row

matrixB DQ 3    		; ROWS
	DQ 2    		; COLS
	DQ 1, 2     		; 1st row
	DQ 3, 4			; 2nd row
	DQ 5, 6			; 3rd row

matrixC	DQ 2    		; ROWS
	DQ 2    		; COLS
	DQ 0, 0			; space for ROWS*COLS ints
	DQ 0, 0			; (for filling in with matrixA*matrixB)

; ---------------------------------------------------------------------

	; The following is used by output_char - do not disturb
	;
	; space in I/O-accessible segment for 1-octet output buffer
octetbuffer	DQ 0		; (qword as choice of size on stack)
