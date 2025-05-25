_start:
    li x5, 5
    addi x2, x2, -4
    sw x2, 0, x5
    jal x1, factorial
    add x5, x10, x0
    addi x2, x2, 4
    addi x2, x2, -4
    sw x2, 0, x5
    add x10, x5, x0
    li x20, 1
    jal x1, runtime_puts
    li x5, 0
    addi x2, x2, 4
factorial:
    addi x2, x2, -4
    sw x2, 0, x8
    addi x2, x2, -4
    sw x2, 0, x1
    add x8, x2, x0
    lw x5, x8, 8
    li x5, 1
    sw x8, -4, x5
    jal x0, loop_start_1
loop_body_2:
    lw x5, x8, -4
    add x7, x5, x0
    lw x5, x8, 8
    add x6, x5, x0
    add x5, x7, x0
    mul x5, x5, x6
    sw x8, -4, x5
    lw x5, x8, 8
    li x6, 1
    sub x5, x5, x6
    sw x8, -8, x5
loop_start_1:
    lw x5, x8, 8
    li x6, 0
    slt x5, x6, x5
    bne x5, x0, loop_body_2
loop_end_3:
    lw x5, x8, -4
    add x10, x5, x0
    jal x0, func_epilogue_0
    li x10, 0
func_epilogue_0:
    add x2, x8, x0
    lw x1, x2, 0
    addi x2, x2, 4
    lw x8, x2, 0
    addi x2, x2, 4
    jalr x0, x1, 0
    ebreak


# --- Runtime Helper Functions ---
runtime_puts_newline:
    li x5, 10
    ewrite x5
    jalr x0, x1, 0
runtime_puts_string:
    lw x5, x10, 0
    beq x5, x0, runtime_puts_newline
    ewrite x5
    addi x10, x10, 1
    jal x0, runtime_puts_string
runtime_puts_int:
    addi x2, x2, 1488
    add x5, x10, x0       # x5 = input number
    li x6, 0              # x6 = digit count
    beq x5, x0, runtime_zero      # Handle zero directly
    li x7, 0              # x7 = sign flag (0=positive)
    bge x5, x0, runtime_extract   # Skip if positive
    sub x5, x0, x5        # Make number positive
    li x7, 1              # Set sign flag
runtime_extract:
    li x28, 10            # Divisor = 10
runtime_loop:
    rem x29, x5, x28      # x29 = x5 % 10
    addi x29, x29, 48     # Convert to ASCII
    addi x2, x2, -1       # Allocate stack cell
    sw x2, 0, x29         # Push digit
    addi x6, x6, 1        # Increment digit count
    div x5, x5, x28       # x5 = x5 / 10
    bne x5, x0, runtime_loop      # Repeat until x5=0
    beq x7, x0, runtime_print     # Skip '-' if positive
    li x5, 45             # Load '-'
    ewrite x5             # Print '-'
runtime_print:
    beq x6, x0, runtime_done      # Exit when all digits printed
    lw x5, x2, 0          # Pop digit
    ewrite x5             # Print it
    addi x2, x2, 1        # Free stack cell
    addi x6, x6, -1       # Decrement count
    jal x0, runtime_print         # Repeat
runtime_zero:
    li x5, 48             # '0'
    ewrite x5
runtime_done:
    li x5, 10             # Newline
    ewrite x5
    jalr x0, x1, 0        # Return
runtime_puts:
    beq x20, x0, runtime_puts_string  # String if x20=0
    jal x0, runtime_puts_int          # Else, print integer
