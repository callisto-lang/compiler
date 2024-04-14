org 0x0000
mov si, __stack
jmp __func_end__printch
__func__printch:
sub si, 2
mov ax, [si]
mov ah, 0x0E
int 0x10
__func_return__printch:
add sp, 0
ret
__func_end__printch:
jmp __func_end____equal__
__func____equal__:
sub si, 2
mov ax, [si]
sub si, 2
mov bx, [si]
cmp ax, bx
je .push_1
mov word [si], 0
add si, 2
ret
.push_1:
mov word [si], 0xFFFF
add si, 2
__func_return____equal__:
add sp, 0
ret
__func_end____equal__:
jmp __func_end____right__
__func____right__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
cmp ax, bx
jg .push_1
mov word [si], 0
add si, 2
ret
.push_1:
mov word [si], 0xFFFF
add si, 2
__func_return____right__:
add sp, 0
ret
__func_end____right__:
jmp __func_end____right____equal__
__func____right____equal__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
cmp ax, bx
jge .push_1
mov word [si], 0
add si, 2
ret
.push_1:
mov word [si], 0xFFFF
add si, 2
__func_return____right____equal__:
add sp, 0
ret
__func_end____right____equal__:
jmp __func_end____left__
__func____left__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
cmp ax, bx
jl .push_1
mov word [si], 0
add si, 2
ret
.push_1:
mov word [si], 0xFFFF
add si, 2
__func_return____left__:
add sp, 0
ret
__func_end____left__:
jmp __func_end____left____equal__
__func____left____equal__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
cmp ax, bx
jle .push_1
mov word [si], 0
add si, 2
ret
.push_1:
mov word [si], 0xFFFF
add si, 2
__func_return____left____equal__:
add sp, 0
ret
__func_end____left____equal__:
jmp __func_end____at__
__func____at__:
sub si, 2
mov bx, [si]
mov ax, [bx]
mov [si], ax
add si, 2
__func_return____at__:
add sp, 0
ret
__func_end____at__:
jmp __func_end____exclaim__
__func____exclaim__:
sub si, 2
mov di, [si]
sub si, 2
mov bx, [si]
mov [di], bx
__func_return____exclaim__:
add sp, 0
ret
__func_end____exclaim__:
jmp __func_end__c__at__
__func__c__at__:
sub si, 2
mov bx, [si]
xor ah, ah
mov al, [bx]
mov [si], ax
add si, 2
__func_return__c__at__:
add sp, 0
ret
__func_end__c__at__:
jmp __func_end__c__exclaim__
__func__c__exclaim__:
sub si, 2
mov di, [si]
sub si, 2
mov bx, [si]
mov [di], bl
__func_return__c__exclaim__:
add sp, 0
ret
__func_end__c__exclaim__:
jmp __func_end__dup
__func__dup:
sub si, 2
mov ax, [si]
mov [si], ax
add si, 2
mov [si], ax
add si, 2
__func_return__dup:
add sp, 0
ret
__func_end__dup:
jmp __func_end__drop
__func__drop:
sub si, 2
__func_return__drop:
add sp, 0
ret
__func_end__drop:
jmp __func_end__swap
__func__swap:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
mov [si], bx
add si, 2
sub si, 2
mov ax, [si]
__func_return__swap:
add sp, 0
ret
__func_end__swap:
jmp __func_end____plus__
__func____plus__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
add ax, bx
mov [si], ax
add si, 2
__func_return____plus__:
add sp, 0
ret
__func_end____plus__:
jmp __func_end__s__plus__
__func__s__plus__:
call __func____plus__
__func_return__s__plus__:
add sp, 0
ret
__func_end__s__plus__:
jmp __func_end____dash__
__func____dash__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
sub ax, bx
mov [si], ax
add si, 2
__func_return____dash__:
add sp, 0
ret
__func_end____dash__:
jmp __func_end__s__dash__
__func__s__dash__:
call __func____dash__
__func_return__s__dash__:
add sp, 0
ret
__func_end__s__dash__:
jmp __func_end____star__
__func____star__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
mul bx
mov [si], ax
add si, 2
__func_return____star__:
add sp, 0
ret
__func_end____star__:
jmp __func_end__s__star__
__func__s__star__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
imul bx
mov [si], ax
add si, 2
__func_return__s__star__:
add sp, 0
ret
__func_end__s__star__:
jmp __func_end____slash__
__func____slash__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
xor dx, dx
div bx
mov [si], ax
add si, 2
__func_return____slash__:
add sp, 0
ret
__func_end____slash__:
jmp __func_end__s__slash__
__func__s__slash__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
xor dx, dx
idiv bx
mov [si], ax
add si, 2
__func_return__s__slash__:
add sp, 0
ret
__func_end__s__slash__:
jmp __func_end____percent__
__func____percent__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
xor dx, dx
div bx
mov [si], dx
add si, 2
__func_return____percent__:
add sp, 0
ret
__func_end____percent__:
jmp __func_end__s__percent__
__func__s__percent__:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
xor dx, dx
idiv bx
mov [si], dx
add si, 2
__func_return__s__percent__:
add sp, 0
ret
__func_end__s__percent__:
jmp __func_end__and
__func__and:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
and ax, bx
mov [si], ax
add si, 2
__func_return__and:
add sp, 0
ret
__func_end__and:
jmp __func_end__or
__func__or:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
or ax, bx
mov [si], ax
add si, 2
__func_return__or:
add sp, 0
ret
__func_end__or:
jmp __func_end__xor
__func__xor:
sub si, 2
mov bx, [si]
sub si, 2
mov ax, [si]
xor ax, bx
mov [si], ax
add si, 2
__func_return__xor:
add sp, 0
ret
__func_end__xor:
jmp __func_end__not
__func__not:
sub si, 2
mov ax, [si]
not ax
mov [si], ax
add si, 2
__func_return__not:
add sp, 0
ret
__func_end__not:
jmp __func_end____left____left__
__func____left____left__:
sub si, 2
mov cx, [si]
sub si, 2
mov ax, [si]
shl ax, cl
mov [si], ax
add si, 2
__func_return____left____left__:
add sp, 0
ret
__func_end____left____left__:
jmp __func_end____right____right__
__func____right____right__:
sub si, 2
mov cx, [si]
sub si, 2
mov ax, [si]
shr ax, cl
mov [si], ax
add si, 2
__func_return____right____right__:
add sp, 0
ret
__func_end____right____right__:
jmp __func_end__a__left__
__func__a__left__:
push 0
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
mov word [si], 4
add si, 2
call __func____plus__
call __func____exclaim__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
mov word [si], 2
add si, 2
call __func____plus__
call __func____exclaim__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
call __func____exclaim__
__func_return__a__left__:
add sp, 2
ret
__func_end__a__left__:
jmp __func_end__a__at__
__func__a__at__:
push 0
push 0
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
mov word [si], 2
add si, 2
call __func____plus__
call __func____at__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
call __func____star__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
mov word [si], 4
add si, 2
call __func____plus__
call __func____at__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
call __func____plus__
call __func____at__
__func_return__a__at__:
add sp, 4
ret
__func_end__a__at__:
jmp __func_end__a__exclaim__
__func__a__exclaim__:
push 0
push 0
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
mov word [si], 2
add si, 2
call __func____plus__
call __func____at__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
call __func____star__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
mov word [si], 4
add si, 2
call __func____plus__
call __func____at__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
call __func____plus__
call __func____exclaim__
__func_return__a__exclaim__:
add sp, 4
ret
__func_end__a__exclaim__:
jmp __func_end__allength
__func__allength:
sub sp, 6
call __func__a__left__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
__func_return__allength:
add sp, 6
ret
__func_end__allength:
jmp __func_end__printdec__underscore__loop
__func__printdec__underscore__loop:
call __func__dup
sub si, 2
mov ax, [si]
cmp ax, 0
je __if_1_1
call __func__dup
mov word [si], 10
add si, 2
call __func____slash__
call __func__printdec__underscore__loop
mov word [si], 10
add si, 2
call __func____percent__
mov word [si], 48
add si, 2
call __func____plus__
call __func__printch
jmp __if_1_end
__if_1_1:
call __func__drop
__if_1_end:
__func_return__printdec__underscore__loop:
add sp, 0
ret
__func_end__printdec__underscore__loop:
jmp __func_end__printdec
__func__printdec:
call __func__dup
sub si, 2
mov ax, [si]
cmp ax, 0
je __if_2_1
call __func__printdec__underscore__loop
jmp __if_2_end
__if_2_1:
call __func__drop
mov word [si], 48
add si, 2
call __func__printch
__if_2_end:
__func_return__printdec:
add sp, 0
ret
__func_end__printdec:
jmp __func_end__printstr
__func__printstr:
push 0
push 0
push 0
mov di, sp
add di, 4
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 4
mov [si], di
add si, 2
call __func____at__
call __func____at__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____exclaim__
jmp __while_3_condition
__while_3:
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
mov di, sp
add di, 4
mov [si], di
add si, 2
call __func____at__
call __func__a__at__
call __func__printch
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
mov word [si], 1
add si, 2
call __func____plus__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____exclaim__
__while_3_condition:
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
call __func____left__
sub si, 2
mov ax, [si]
cmp ax, 0
jne __while_3
__while_3_end:
__func_return__printstr:
add sp, 6
ret
__func_end__printstr:
jmp __func_end__printlstr
__func__printlstr:
sub sp, 6
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func__a__left__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func__printstr
__func_return__printlstr:
add sp, 6
ret
__func_end__printlstr:
jmp __func_end__tak
__func__tak:
push 0
push 0
push 0
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 4
mov [si], di
add si, 2
call __func____exclaim__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
call __func____left__
sub si, 2
mov ax, [si]
cmp ax, 0
je __if_4_1
mov di, sp
add di, 4
mov [si], di
add si, 2
call __func____at__
mov word [si], 1
add si, 2
call __func____dash__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
call __func__tak
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
mov word [si], 1
add si, 2
call __func____dash__
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
mov di, sp
add di, 4
mov [si], di
add si, 2
call __func____at__
call __func__tak
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
mov word [si], 1
add si, 2
call __func____dash__
mov di, sp
add di, 4
mov [si], di
add si, 2
call __func____at__
mov di, sp
add di, 2
mov [si], di
add si, 2
call __func____at__
call __func__tak
call __func__tak
add sp, 6
ret
jmp __if_4_end
__if_4_1:
mov di, sp
add di, 0
mov [si], di
add si, 2
call __func____at__
add sp, 6
ret
__if_4_end:
__func_return__tak:
add sp, 6
ret
__func_end__tak:
mov word [si], 7
add si, 2
mov word [si], 4
add si, 2
mov word [si], 8
add si, 2
call __func__tak
call __func__printdec
mov word [si], 10
add si, 2
call __func__printch
ret
__stack: times 512 dw 0
