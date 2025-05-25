.text
.globl _start

_start:
    li t0, 5
    mv t2, t0
    li t0, 10
    li t1, 2
    add t0, t0, t1
    mv t1, t0
    mv t0, t2
    mul t0, t0, t1
    li t1, 2
    div t0, t0, t1
    sw t0, 0(gp)
    lw t0, 0(gp)
    li t1, 2
    xor t0, t0, t1
    seqz t0, t0
    beq t0, zero, else_0
    li t0, 3
    li t1, 0
times_loop_start_2:
    bge t1, t0, times_loop_end_3
    lw t0, 0(gp)
    mv t2, t0
    lw t0, 0(gp)
    mv t1, t0
    mv t0, t2
    mul t0, t0, t1
    sw t0, 0(gp)
    addi t1, t1, 1
    jal zero, times_loop_start_2
times_loop_end_3:
    lw t0, 0(gp)
    addi sp, sp, -4
    sw t0, 0(sp)
    mv a0, t0
    jal ra, runtime_puts
    li t0, 0
    addi sp, sp, 4
    jal zero, endif_1
else_0:
    la t0, str_0
    addi sp, sp, -4
    sw t0, 0(sp)
    mv a0, t0
    jal ra, runtime_puts
    li t0, 0
    addi sp, sp, 4
    lw t0, 0(gp)
    addi sp, sp, -4
    sw t0, 0(sp)
    mv a0, t0
    jal ra, runtime_puts
    li t0, 0
    addi sp, sp, 4
endif_1:
    ebreak

.data
str_0: .string "unsure"

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
