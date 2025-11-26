section .data
    message db "Hello, World!", 10
    msg_len equ $ - message

section .text
    global _start

_start:
    call print_message
    call exit_program

print_message:
    mov eax, 4
    mov ebx, 1
    mov ecx, message
    mov edx, msg_len
    int 0x80
    ret

exit_program:
    mov eax, 1
    xor ebx, ebx
    int 0x80

