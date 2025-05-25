.text
.globl _start

_start:

factorial:
    addi sp, sp, -4
    sw fp, 0(sp)
    addi sp, sp, -4
    sw ra, 0(sp)
    mv fp, sp
    lw t0, 8(fp)
    mv t2, t0
    li t0, 1
    mv t1, t0
    mv t0, t2
    sle t0, t0, t1
    beq t0, zero, else_0
    li t0, 1
    mv a0, t0
    jal zero, epilogue_factorial
    jal zero, endif_1
else_0:
    lw t0, 8(fp)
    mv t2, t0
    lw t0, 8(fp)
    mv t2, t0
    li t0, 1
    mv t1, t0
    mv t0, t2
    sub t0, t0, t1
    addi sp, sp, -4
    sw t0, 0(sp)
    jal ra, factorial
    addi sp, sp, 4
    mv t0, a0
    mv t1, t0
    mv t0, t2
    mul t0, t0, t1
    mv a0, t0
    jal zero, epilogue_factorial
endif_1:
epilogue_factorial_2:
    mv sp, fp
    lw ra, 0(sp)
    addi sp, sp, 4
    lw fp, 0(sp)
    addi sp, sp, 4
    jalr zero, ra, 0
    li t0, 0  # Unknown variable: factorial
    mv a0, t0
    jal ra, runtime_puts
    li t0, 0
    li t0, 10
    ebreak


# --- Runtime Helper Functions ---
runtime_puts:
    # Check if the argument is a string or integer
    blt a0, zero, runtime_puts_int  # Negative means integer (hacky but works for demo)
    beq a0, zero, runtime_puts_done
runtime_puts_str:
    # Print string
    lb t0, 0(a0)
    beq t0, zero, runtime_puts_newline
    ewrite t0
    addi a0, a0, 1
    jal zero, runtime_puts_str
runtime_puts_int:
    # Print integer
    mv t0, a0
    li t1, 0  # Sign flag
    bge t0, zero, runtime_puts_int_positive
    li t1, 1  # Negative number
    sub t0, zero, t0  # Make positive
runtime_puts_int_positive:
    # Convert to string (simplified for demo)
    addi sp, sp, -12  # Space for digits
    mv t2, sp  # Buffer pointer
    li t3, 10  # Divisor
runtime_puts_int_loop:
    rem t4, t0, t3  # Get last digit
    addi t4, t4, 48  # Convert to ASCII
    sb t4, 0(t2)  # Store digit
    addi t2, t2, 1  # Move buffer pointer
    div t0, t0, t3  # Divide by 10
    bne t0, zero, runtime_puts_int_loop
    # Print minus sign if negative
    beq t1, zero, runtime_puts_int_print
    li t0, 45  # ASCII for '-'
    ewrite t0
runtime_puts_int_print:
    # Print digits in reverse order
    addi t2, t2, -1  # Move back to last digit
runtime_puts_int_print_loop:
    lb t0, 0(t2)  # Load digit
    ewrite t0  # Print digit
    blt t2, sp, runtime_puts_int_done
    addi t2, t2, -1  # Move to previous digit
    jal zero, runtime_puts_int_print_loop
runtime_puts_int_done:
    addi sp, sp, 12  # Restore stack
runtime_puts_newline:
    li t0, 10  # Newline character
    ewrite t0
runtime_puts_done:
    jalr zero, ra, 0  # Return
