section .data
    hello db 'HELLO WORLD!', 0

section .text
    global _start

_start:
    ; write the string to stdout
    mov eax, 4          ; syscall number for sys_write
    mov ebx, 1          ; file descriptor 1 is stdout
    mov ecx, hello      ; pointer to the string
    mov edx, 12         ; length of the string
    int 0x80            ; call kernel

    ; exit the program
    mov eax, 1          ; syscall number for sys_exit
    xor ebx, ebx        ; exit code 0
    int 0x80            ; call kernel
