.text
.globl _start

_start:
factorial:
    addi sp, sp, -4
    sw ra, 0(sp)
    addi sp, sp, -4
    sw fp, 0(sp)
    mv fp, sp
    li t0, 1
    sw t0, -4(fp)
    jal zero, loop_start_0
loop_body_1:
    lw t0, -4(fp)
    mv t2, t0
    lw t0, 8(fp)
    mv t1, t0
    mv t0, t2
    mul t0, t0, t1
    sw t0, -4(fp)
    lw t0, 8(fp)
    li t1, 1
    sub t0, t0, t1
    sw t0, -8(fp)
loop_start_0:
    lw t0, 8(fp)
    li t1, 0
    slt t0, t1, t0
    bne t0, zero, loop_body_1
loop_end_2:
    lw t0, -4(fp)
    mv a0, t0
    jal zero, func_epilogue_2
    li a0, 0
func_epilogue_3:
    mv sp, fp
    lw fp, 0(sp)
    addi sp, sp, 4
    lw ra, 0(sp)
    addi sp, sp, 4
    jalr zero, ra, 0
    li t0, 10
    addi sp, sp, -4
    sw t0, 0(sp)
    jal ra, factorial
    mv t0, a0
    addi sp, sp, 4
    addi sp, sp, -4
    sw t0, 0(sp)
    mv a0, t0
    jal ra, runtime_puts
    li t0, 0
    addi sp, sp, 4
    ebreak


# --- Runtime Helper Functions ---
runtime_puts:
    # a0 contains the address of the string or an integer
    li t1, 0x10000000
    bgeu a0, t1, runtime_puts_string
runtime_puts_int:
    mv t0, a0
    li t1, 0
    bgez t0, runtime_puts_int_process
    neg t0, t0
    li t1, 1
runtime_puts_int_process:
    # Convert integer to string
    addi sp, sp, -40
    mv t2, sp
    li t3, 10
runtime_puts_int_loop:
    rem t4, t0, t3
    addi t4, t4, 48
    sb t4, 0(t2)
    addi t2, t2, 1
    div t0, t0, t3
    bnez t0, runtime_puts_int_loop
    beqz t1, runtime_puts_int_print
    li t4, 45
    sb t4, 0(t2)
    addi t2, t2, 1
runtime_puts_int_print:
    addi t2, t2, -1
runtime_puts_int_print_loop:
    lb t0, 0(t2)
    ewrite t0
    addi t2, t2, -1
    bge t2, sp, runtime_puts_int_print_loop
    li t0, 10
    ewrite t0
    addi sp, sp, 40
    jalr zero, ra, 0
runtime_puts_string:
    lb t0, 0(a0)
    beqz t0, runtime_puts_newline
    ewrite t0
    addi a0, a0, 1
    j runtime_puts_string
runtime_puts_newline:
    li t0, 10
    ewrite t0
    jalr zero, ra, 0
