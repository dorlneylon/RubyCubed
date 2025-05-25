_start:
    li x5, 5
    add x7, x5, x0
    li x5, 10
    li x6, 2
    add x5, x5, x6
    add x6, x5, x0
    add x5, x7, x0
    mul x5, x5, x6
    li x6, 2
    div x5, x5, x6
    sw x3, 0, x5
    lw x5, x3, 0
    li x6, 2
    seq x5, x5, x6
    beq x5, x0, else_0
    li x5, 3
    li x6, 0
times_loop_start_2:
    bge x6, x5, times_loop_end_3
    lw x5, x3, 0
    add x7, x5, x0
    lw x5, x3, 0
    add x6, x5, x0
    add x5, x7, x0
    mul x5, x5, x6
    sw x3, 0, x5
    addi x6, x6, 1
    jal x0, times_loop_start_2
times_loop_end_3:
    lw x5, x3, 0
    addi x2, x2, -4
    sw x2, 0, x5
    add x10, x5, x0
    jal x1, runtime_puts
    li x5, 0
    addi x2, x2, 4
    jal x0, endif_1
else_0:
    li x5, str_0
    addi x2, x2, -4
    sw x2, 0, x5
    add x10, x5, x0
    jal x1, runtime_puts
    li x5, 0
    addi x2, x2, 4
    lw x5, x3, 0
    addi x2, x2, -4
    sw x2, 0, x5
    add x10, x5, x0
    jal x1, runtime_puts
    li x5, 0
    addi x2, x2, 4
endif_1:
    ebreak

str_0:
    data 117 * 1
    data 110 * 1
    data 115 * 1
    data 117 * 1
    data 114 * 1
    data 101 * 1
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
runtime_puts_int_loop:
    rem x29, x5, x28
    addi x29, x29, 48
    sw x7, 0, x29
    addi x7, x7, 1
    div x5, x5, x28
    bne x5, x0, runtime_puts_int_loop
    beq x6, x0, runtime_puts_int_print
    li x29, 45
    sw x7, 0, x29
    addi x7, x7, 1
runtime_puts_int_print_loop:
    lw x5, x7, 0
    ewrite x5
    addi x7, x7, -1
    bge x7, x2, runtime_puts_int_print_loop
    li x5, 10
    ewrite x5
    addi x2, x2, 40
    jalr x0, x1, 0
runtime_puts_int_process:
    # Convert integer to string
    addi x2, x2, -40
    add x7, x2, x0
    li x28, 10
runtime_puts_int_print:
    addi x7, x7, -1
runtime_puts_int:
    add x5, x10, x0
    li x6, 0
    bge x5, x0, runtime_puts_int_process
    sub x5, x0, x5
    li x6, 1
runtime_puts:
    # x10 contains the address of the string or an integer
    li x6, 268435456
    bge x10, x6, runtime_puts_string
