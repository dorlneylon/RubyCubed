_start:
    li x5, str_0
    sw x3, 25000, x5
    li x5, 42
    sw x3, 25004, x5
    lw x5, x3, 25000
    addi x2, x2, -4
    sw x2, 0, x5
    add x10, x5, x0
    li x20, 0
    jal x1, runtime_puts
    li x5, 0
    addi x2, x2, 4
    lw x5, x3, 25004
    addi x2, x2, -4
    sw x2, 0, x5
    add x10, x5, x0
    li x20, 1
    jal x1, runtime_puts
    li x5, 0
    addi x2, x2, 4
    ebreak

str_0:
    data 73 * 1
    data 32 * 1
    data 97 * 1
    data 109 * 1
    data 32 * 1
    data 97 * 1
    data 32 * 1
    data 115 * 1
    data 116 * 1
    data 114 * 1
    data 105 * 1
    data 110 * 1
    data 103 * 1
    data 0 * 1

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
