	org 100h
	
	mov ax,cs
	mov ds,ax
	mov es,ax
	mov fs,ax
	mov ss,ax
	mov sp,100h-4
	mov ax,0B800h
	mov gs,ax	;gs为显存地址
	call cls	;	清屏
start:

	call mode_choose
	call check_in
restart:
	call cls
	call string 
	call initial_game

	call game
	call exit
	call cls
	int 21h 
	

exitstr: db 'Press any button to Exit'
exitlen equ $ - exitstr
;---------------------------------------
exit:
	mov ah,0fh 
	mov bp,exitstr
	mov cx,exitlen
	mov di,3256 
	call showstr 
	
.1  mov ah,0 
	int 16h 
    ret
;========================================
;cls   清屏例程(调用int 10h中断)
;========================================
cls:
	mov ah,06h
	mov al,0
	mov cl,0
	mov ch,0
	mov dh,24
	mov dl,79
	mov bh,0fh
	int 10h
	
	ret
;========================================
;show info  （显示游戏操作信息）
;========================================
mode_choose:	
	mov cx,str1len
	mov bp,str1
	push ax
	push di
	mov di,800
	mov ah,0fh
	call showstr
	pop di
	pop ax
	ret
;----------------------------------------
;showstr（cx bp ah di 为入口参数）
;----------------------------------------
showstr:
	
	mov al,byte[es:bp]
	mov word[gs:di],ax
	add di,2 
	inc bp 
	loop showstr
	ret
mode: db 0
fail_enter: db '                          Please choose mode !'
fail_enterlen equ $ - fail_enter
;==========================================
;check_in
;==========================================
check_in:
	mov ah,00h 
	int 16h
	cmp al,'1'
	je .r 
	cmp al,'2'
	je .r
	mov cx,fail_enterlen
	mov bp,fail_enter
	mov di,160*20
	push ax 
	mov ah,00000100b
	call showstr
	pop ax
	jmp check_in
.r: 
	mov byte[mode],al
	ret

;--------------------------------------------------
string:
	push di 
	mov ah,0fh
	
	cmp byte[mode],'2' 
	je .2 
.1	mov cx,str2len 
	mov bp,str2 
	mov di,160
	call showstr

	mov ax,word[s1]
	mov dx,p
	call todec
	mov ah,0fh 
	mov cx,str3len 
	mov bp,str3 
;	mov di,320
	call showstr
	jmp .r 
	
.2:
	push bx 
	xor bx,bx 
	mov ax,word[s1]
	mov dx,p1 
	call todec 
	

	mov ax,word[s2]
	mov dx,p2
	call todec 

	mov bp,str4
	mov cx,str4len
	mov di,160
	mov ah,0fh
	call showstr 
	pop bx
.r:	pop di 
	ret
;-----------------------------------------------------------------------
todec:;ax入口参数
	push bx 
	push di
	push bp
	push dx
	mov bp,buf 
	xor dx,dx
	mov bx,10
.d:
	div bx 
;	mov byte[gs:4],'s'
	mov byte[bp],dl 
	inc bp
	xor dx,dx
	cmp ax,0 
	je .1
	jmp .d 
	
.1	pop dx
	mov di,dx
.i:	dec bp
	mov al,byte[bp]
	add al,30h;转化ASCII码
	mov byte[di],al
	inc di
	cmp bp,buf 
	jne .i
	pop bp 
	pop di
	pop bx
	ret
	
;-----------------------------------------------------------------------

	
show_wall:
	mov al,'+'
	mov ah,01110111b
	push di
	
.1:	mov cx,79
	mov di,480
.u:	mov word[gs:di],ax
	add di,2 
	loop .u
	
    mov cx,21
.r:	mov	word [gs:di],ax
	add di,160
	loop .r
	
	mov cx,79
.d:	mov word[gs:di],ax 
	sub di,2 
	loop .d 
	
	mov cx,21
.l: mov word[gs:di],ax 
	sub di,160 
	loop .l 
	cmp byte[mode],'1'
	je .re
	
	mov cx,20
	mov di,718
.m: mov word[gs:di],ax
	add di,160 
	loop .m 
.re	pop di
	ret
;设置蛇头的起始位置，食物的位置，采用随机数决定
;===========================================
;initial_game
;===========================================
initial_game:;;//////////////////
	mov byte[result],2
	call show_wall
	
	mov dh,0 
	mov dl,38
	call putfood
	mov byte[dir],'d' ;right
	mov bh,6
	mov bl,10 
	mov byte[snakehx],bh
	mov byte[snakehy],bl
	mov bh,byte[snakehx]
	mov bl,byte[snakehy]
	call moving
	mov byte[snaketx],bh 
	mov byte[snakety],bl
	
	inc bl
	mov byte[snakehy],bl
	call moving 
	
	inc bl
	mov byte[snakehy],bl 
	call moving 
	mov word[snakelen],3
	
	cmp byte[mode],'1'
	je .r
	mov dh,40 
	mov dl,79
	call putfood
	mov byte[dir2],'d'
	mov bh,6
	mov bl,50 
	mov byte[snake2hx],bh
	mov byte[snake2hy],bl
	call moving2
	mov byte[snake2tx],bh 
	mov byte[snake2ty],bl
	
	inc bl
	mov byte[snake2hy],bl
	call moving2 
	
	inc bl
	mov byte[snake2hy],bl 
	call moving2 
	mov word[snake2len],3
	
.r: ret
;-------------------------------------------
;toCoordinate （bx作为入口参数）
;-------------------------------------------
toCoordinate:
	push ax
	push bx 
	push dx
	mov ax,word[randomNum]
	mov dl,25
	div dl
	mov byte[bx],ah
	inc bx
	
	mov ax,word[randomNum]
	mov dl,80
	div dl
	mov byte[bx],ah
	
	pop dx
	pop bx
	pop ax
	ret
;--------------------------------------------
;random (用CMOS RAM产生随机数)cpu运行过快时随机数会出现相同的
;minute*60+second = randomNum
;--------------------------------------------
random:
	push ax
	push bx
	xor bx,bx
	xor ax,ax
	
	mov al,2	
	out 70h,al
	in al,71h
	mov bl,60
	mul bl
	push ax
	
	xor ax,ax
	mov al,0
	out 70h,al
	in al,71h
	pop bx
	add ax,bx
	
	mov word[randomNum],ax
	pop bx
	pop ax
	ret

;===========================================
;game   (游戏主程序)
;===========================================
game:
	call resetTimer
	call setInt
	call check_status
	call setMusic
	call recoverInt
	ret
	
check_status:
	push dx
	push ax
	

.1	mov dl,byte[result]
	cmp dl,2
	je .1 
	cmp dl,1 
	je .w 
	cmp dl,0 
	je .l 
	cmp dl,4 
	je .p1
	cmp dl,5 
	je .p2
	cmp dl,6 
	je .equal 
	cmp dl,7 
	je .esc
	jmp .1
.w  mov bp,str6 
	mov cx,str6len 
	mov di,1990
	mov ah,byte[warnColor]
	call showstr
	jmp .r
.l  mov bp,str7 
	mov cx,str7len 
	mov di,1990 
	mov ah,byte[warnColor]
	call showstr 
	jmp .r 
.p1 mov bp,str8 
	mov cx,str8len 
	mov di,1990 
	mov ah,byte[warnColor]
	call showstr 
	jmp .r 
.p2 mov bp,str9 
	mov cx,str9len 
	mov di,1990 
	mov ah,byte[warnColor]
	call showstr 
	jmp .r 
.equal:
	mov bp,str10 
	mov cx,str10len 
	mov di,1990 
	mov ah,byte[warnColor]
	call showstr
	jmp .r 
.r: 
	cmp byte[f1],1
	jb .re
	call cls 
	mov di,13*80
	shl di,1
	mov ah,00001100b
	cmp byte[f1],2
	;je .fastv1
	je .fastv2
	
.fastv1:
	mov cx,v1len 
	mov bp,v1
	call showstr 
	jmp .re
.fastv2:
	mov cx,v2len 
	mov bp,v2 
	call showstr
	jmp .re
.esc:
	call reset
	int 21h 
	ret
	
.re:
	pop ax
	pop dx
	ret
;--------------------------------------------
;new_Timer
;--------------------------------------------
new_Timer:
	
	pusha 
	sti
	
	dec byte[count]
	cmp byte[count],0 
	jnz .0
	call move
	mov al,byte[delay]
	mov byte[count],al
	call is_win

.0:
	mov al,20h
	out 20h,al
	out 0a0h,al
	popa
	cli
	iret
.1: mov byte[count],1 
	jmp .0 
;---------------------------------------
is_win:
	push ax
	cmp byte[mode],'1'
	je .1 
	cmp byte[mode],'2'
	je .2 

.1 cmp word[snakelen],78*20 
	je .w 
    jmp .r 
.2  cmp byte[error1],1 
    je .lose1
	cmp byte[error2],1 
	je .lose2 
	cmp word[snakelen],20*38
	je .w1 
	cmp word[snake2len],20*38
	je .w2
	jmp .r 
.w mov byte[result],1 
	jmp .r 
.lose1:
	mov ax,word[s1]
	cmp ax,word[s2]
	ja  .w1
	je  .goon
.w2	mov byte[result],5
	jmp .r
.lose2:
	mov ax,word[s2]
	cmp ax,word[s1]
	ja  .w2
	jmp .w1
.w1 cmp word[snake2len],20*38
	je .equal
	mov byte[result],4 
	jmp .r 
.goon:
	cmp byte[error2],1 
	je .equal
	mov byte[result],5
	jmp .r 
.equal:
	mov byte[result],6
	jmp .r 
	
.r  pop ax
	ret

;------------------------------------------
;move 动画效果的移动
;------------------------------------------
move:
	mov bh,byte[snakehx]
	mov bl,byte[snakehy]

	mov al,byte[dir]
 	cmp al,'w'
	je up
	cmp  al,'d'
	je right
	cmp al,'s'
	je down
	cmp al,'a'
	je left
up:
	dec bh
	jmp check
right:
	inc bl 
	jmp check
down: 
	inc bh
	jmp check
left:
	dec bl
	jmp check
check:
	push bx
	pop bx
	je .0
	push bx
	mov ax,bx
	call toAddr
	mov ax,word[gs:bx]
	pop bx
	cmp al,'a';
	je  .0
	cmp al,'w'
	je .0 
	cmp al,'s'
	je .0 
	cmp al,'d'
	je .0 
	cmp al,'+'
	je .0
	cmp al,'*';进食
	je .1
	jmp .2
.0: 
	cmp byte[mode],'2'
	je .e2
	mov byte[result],0
	jmp .r
.e2:mov byte[error1],1
	jmp .r 

.1:
	call eaten
	call moving 
	mov dh,0 
	cmp byte[mode],'2'
	je .f2
	;mov dx,word[snakelen]
	add word[s1],3
	call string 
	inc word[snakelen]
	mov dl,79 
	call putfood
	jmp .r
.f2:
	mov dl,38 
	call putfood
	mov dx,word[snakelen]
	add word[s1],dx
	call string
	inc word[snakelen]
	jmp .r 
.2:
    call moving
	call hideTail
	jmp .r 
.r: cmp byte[mode],'1' 
	je re 
	
	mov bh,byte[snake2hx]
	mov bl,byte[snake2hy]
	;judge direction
	mov al,byte[dir2]
	cmp al,'w'
	je up2
	cmp  al,'d'
	je right2
	cmp al,'s'
	je down2
	cmp al,'a'
	je left2
up2:
	dec bh
	jmp check2
right2:
	inc bl 
	jmp check2
down2: 
	inc bh
	jmp check2
left2:
	dec bl
	jmp check2
check2:
	push bx
	pop bx
	je .02
	push bx
	mov ax,bx
	call toAddr
	mov ax,word[gs:bx]
	pop bx
	cmp al,'a';
	je  .02
	cmp al,'w'
	je .02 
	cmp al,'s'
	je .02 
	cmp al,'d'
	je .02 
	cmp al,'+'
	je .02
	cmp al,'*';进食
	je .12
	jmp .22
.02: 
	mov byte[error2],1
	jmp re 

.12:
	call eaten
	call moving2
	mov dh,40 
	mov dl,79
	call putfood
	;mov dx,word[snake2len]
	add word[s2],3
	call string
	inc word[snake2len]
	jmp re 
.22:call moving2
	call hideTail2
re: ret
;-----------------------------------------
;toAddr ax入口参数
;-------------------------------------------
toAddr:
	push dx
	push ax 
	xor dh,dh
	mov dl,al
	mov bl,80
	mov al,ah
	xor ah,ah
	mul bl
	add ax,dx
	shl ax,1 ;线性地址*2
	mov bx,ax
	pop ax
	pop dx
	ret
;bx 是下一个蛇头坐标
moving:
	push bx
	push ax
	push bx 
	
	mov bh,byte[snakehx]
	mov bl,byte[snakehy]
	mov ax,bx
	call toAddr
	mov ah,byte[snakeColor]
	mov al,byte[dir]
	mov word[gs:bx],ax
	pop bx
	mov byte[snakehx],bh
	mov byte[snakehy],bl
	mov ax,bx
	call toAddr
	mov ah,byte[snakeColor]
	mov al,byte[dir]
	mov word[gs:bx],ax
	pop ax
	pop bx
	ret
moving2:
	push bx
	push ax
	push bx 
	
	mov bh,byte[snake2hx]
	mov bl,byte[snake2hy]
	mov ax,bx
	call toAddr
	mov ah,byte[snakeColor]
	mov al,byte[dir2]
	mov word[gs:bx],ax
	pop bx
	mov byte[snake2hx],bh
	mov byte[snake2hy],bl
	mov ax,bx
	call toAddr
	mov ah,byte[snakeColor]
	mov al,byte[dir2]
	mov word[gs:bx],ax
	pop ax
	pop bx
	ret	
hideTail:
	push bx
	push ax
	push dx
	xor dx,dx
	mov bh,byte[snaketx]
	mov bl,byte[snakety]
	mov ax,bx
	push bx
	mov ax,bx
	call toAddr
	mov al,0
	mov ah,00h
	mov dx,word[gs:bx]
	mov word[gs:bx],ax
	pop bx
	cmp dl,'w'
	je .up
	cmp dl,'d'
	je .right
	cmp dl,'s'
	je .down
	cmp dl,'a'
	je .left
	jmp .0
.up:
	dec bh
	jmp .0
.right:
	inc bl
	jmp .0
.down:
	inc bh
	jmp .0
.left:
	dec bl 
	jmp .0 
.0: mov byte[snaketx],bh
	mov byte[snakety],bl
	pop dx
	pop ax
	pop bx
	ret
	
hideTail2:
	push bx
	push ax
	push dx
	xor dx,dx
	mov bh,byte[snake2tx]
	mov bl,byte[snake2ty]
	mov ax,bx
	push bx
	mov ax,bx
	call toAddr
	mov al,0
	mov ah,00h
	mov dx,word[gs:bx]
	mov word[gs:bx],ax
	pop bx
	cmp dl,'w'
	je .up
	cmp dl,'d'
	je .right
	cmp dl,'s'
	je .down
	cmp dl,'a'
	je .left
	jmp .0
.up:
	dec bh
	jmp .0
.right:
	inc bl
	jmp .0
.down:
	inc bh
	jmp .0
.left:
	dec bl 
	jmp .0 
.0: mov byte[snake2tx],bh
	mov byte[snake2ty],bl
	pop dx
	pop ax
	pop bx
	ret
putfood:
	push bx
	push ax
	push dx
	call random
	mov bx,foodx
	call toCoordinate
	cmp byte[foody],dh ;y的上下界dh<dl
	jb .1 
	cmp  byte[foody],dl 
	ja .1 
	mov ah,byte[foodx]
	mov al,byte[foody]
	cmp ah,3 
	jb .1
	;push ax
	call toAddr
	mov ax,word[gs:bx]
	cmp al,'w'
	je .1 
	cmp al,'a'
	je .1 
	cmp al,'s'
	je .1 
	cmp al,'d'
	je .1 
	cmp al,'+'
	je .1
	cmp al,'*'
	je .1
	mov al,'*'
	mov ah,0fh
	mov word[gs:bx],ax
	jmp .0
.1:
	add word[randomNum],60
	mov bx,foodx
	call toCoordinate
	cmp  byte[foody],dh ;y的上下界dh<dl
	jb .1 
	cmp  byte[foody],dl 
	ja .1 
	mov ah,byte[foodx]
	mov al,byte[foody]
	cmp ah,3 
	jb .1
	call toAddr
	mov ax,word[gs:bx]
	cmp al,'w'
	je .1 
	cmp al,'a'
	je .1 
	cmp al,'s'
	je .1 
	cmp al,'d'
	je .1
	cmp al,'+'
	je .1
	mov al,'*'
	mov ah,0fh
	mov word[gs:bx],ax
	jmp .0
.0:
    pop dx
	pop ax
	pop bx
	ret
eaten:
; 打开蜂鸣器
; 设置计数器
	
;	call SetTimer				; 调用设置计数器函数
	call open
	call setRate
	call stop
	ret

stop:	
	mov al,byte[s8255]					; 恢复8255B端口的原状态值
	out 61h,al					; 关闭蜂鸣器
	call resetTimer
	ret						; 返回
open:	
	mov al,0B6h				; 设控制字值
	out 43h,al					; 写控制字到控制字寄存器
	in al,61h					; 读8255B端口状态
	mov dl,al					; 保存8255B端口的原状态值
	mov byte[s8255],dl
	or al,3					; 使状态值的低2位为1
	out 61h,al					; 打开蜂鸣器
	ret
SetTimer:	; 设置计数器函数
	out 43h,al					; 写控制字到控制字寄存器
	mov ax,1193182/100 		; 每秒100次中断（10ms一次）
	out 40h,al					; 写计数器0的低字节
	mov al,ah					; AL=AH
	out 40h,al					; 写计数器0的高字节
	ret
resetTimer:
	push eax 
	push edx 
	push ebx
	mov al,34h				; 设控制字值
	out 43h,al					; 写控制字到控制字寄存器
	xor edx,edx
	mov eax,1193182
	xor ebx,ebx
	mov bx,word[rate]
	div ebx 
	;mov ax,word[rate] 		; 每秒100次中断（10ms一次）
	out 40h,al					; 写计数器0的低字节
	mov al,ah					; AL=AH
	out 40h,al					; 写计数器0的高字节
	pop ebx 
	pop edx 
	pop eax
	ret	
setRate:
	mov cx,2000 
A:
	mov ax,1193182/784
	out 42h,al					; 写计数器2的低字节
	mov al,ah					; AL=AH
	out 42h,al					; 写计数器2的高字节
	loop A
	mov cx,1000
B:
	mov ax,1193182/1046
	out 42h,al					; 写计数器2的低字节
	mov al,ah					; AL=AH
	out 42h,al					; 写计数器2的高字节
	loop B
	mov cx,1000
C: 
	mov ax,1193182/1318
	out 42h,al					; 写计数器2的低字节
	mov al,ah					; AL=AH
	out 42h,al					; 写计数器2的高字节
	loop C
	
	mov cx,2000
D:
	mov ax,1193182/1046
	out 42h,al					; 写计数器2的低字节
	mov al,ah					; AL=AH
	out 42h,al					; 写计数器2的高字节
	loop D
	mov cx,1000
F:
	mov ax,1193182/1318
	out 42h,al					; 写计数器2的低字节
	mov al,ah					; AL=AH
	out 42h,al					; 写计数器2的高字节
	loop F
	mov cx,500 
G:
	mov ax,1193182/1568
	out 42h,al					; 写计数器2的低字节
	mov al,ah					; AL=AH
	out 42h,al					; 写计数器2的高字节
	loop G
	ret
;-------------------------------------------
;new_int9  键盘的实时监测
;-------------------------------------------
new_int9:
	pusha
	cli
	
	;检测键盘
	; 读取8042的状态寄存器
	in al, 64h
	test al, 0E0h	; 奇偶错误或超时？
	jnz .return		; 出错时返回
	test al, 1		; 输出缓冲器满？
	jz .return		; 为空时返回
	
	; 读取键盘扫描码
	in al, 60h
	; 判断是否出错
	cmp al, 0FFh	; AL = FFh ?
	je	.return		; 错误码
	test al, al		; AL = 0 ?
	jz	.return		; 错误码
	
	cmp al,081h		;Esc断开码
	je .return
	;判断输入方向
	cmp al,4eh;'+'
	je .plus
	cmp al,4ah;'-'
	je .minus
	cmp al,3bh
	je .f1
	cmp al,3ch
	je .f2
	
	cmp al,11h ;'w'上
	je  .0 
	
	cmp al,20h ;'d'右
	je .1 
	
	cmp al,1fh; 's' 下
	je .2 
	
	cmp al,1eh;'a'左
	je .3 
	 
	cmp al,01h;esc 接通码
	je .e
	
	;player2
    cmp al,48h ;'8'上
	je  .02 
	
	cmp al,4dh ;'6'右
	je .12 
	
	cmp al,50h; '2' 下
	je .22 
	
	cmp al,4bh;'4'左
	je .32 
	
	jmp .return
	
.0: cmp byte[dir],'s'
	je .return
	mov byte[dir],'w'
	jmp .return 
.1: cmp byte[dir],'a'
	je .return
	mov byte[dir],'d'
	jmp .return 
	
.2: cmp byte[dir],'w'
	je .return
	mov byte[dir],'s'
	jmp .return
.3: cmp byte[dir],'d'
	je .return
	mov byte[dir],'a'
	jmp .return
.e:
	mov byte[result],7 
	jmp .return

.02: cmp byte[dir2],'s'
	je .return
	mov byte[dir2],'w'
	jmp .return 
.12: cmp byte[dir2],'a'
	je .return
	mov byte[dir2],'d'
	jmp .return 
	
.22: cmp byte[dir2],'w'
	je .return
	mov byte[dir2],'s'
	jmp .return
.32: cmp byte[dir2],'d'
	je .return
	mov byte[dir2],'a'
	jmp .return
	
.plus:
	cmp word[rate],0ffffh
	jae .return 
	add word[rate],10
	call resetTimer
	jmp .return 
.minus:
	cmp word[rate],20
	je .return 
	sub word[rate],10
	call resetTimer
	jmp .return
.f1:
	mov byte[result],1
	cmp byte[mode],'1'
	je .f11 
	mov byte[f1],2 
	jmp .return 
.f11:
	mov byte[f1],1 
	jmp .return 
.f2:
	cmp byte[mode],'1'
	je .return
	call put_wall
	jmp .return
	
.return:
	; 发送EOI给8259A
	mov al, 0x20	;中断结束命令EOI 
	out 0xa0, al	;向从片发送 
	out 0x20, al	;向主片发送 
	sti
		; 恢复保存的寄存器值
	popa			; 从栈中恢复通用寄存器
	iret			; 从中断返回
;-------------------------------------------
put_wall:
	cmp word[s1],10 
	jb .r
	mov al,'+'
	mov ah,01110111b
	call random
	mov bx,randomWall
	call toCoordinate
	mov ah,byte[randomWall]
	mov al,byte[randomWall+1]
	cmp ah,3 
	jbe .again
	cmp al,35
	jbe .again
	cmp al,78 
	jae .again 
	
	call toAddr
	mov ax,word[gs:bx]
	cmp al,'+'
	je .again
	cmp al,'a'
	je .again 
	cmp al,'w'
	je .again
	cmp al,'s'
	je .again 
	cmp al,'d'
	je .again
	cmp al,'*'
	je .again
	jmp .return 
.again:
	add word[randomNum],60 
	mov bx,randomWall
	call toCoordinate
	mov ah,byte[randomWall]
	mov al,byte[randomWall+1]
	cmp ah,3 
	jbe .again
	call toAddr
	mov ax,word[gs:bx]
	cmp al,'+'
	je .again
	cmp al,'a'
	je .again 
	cmp al,'w'
	je .again
	cmp al,'s'
	je .again 
	cmp al,'d'
	je .again
	cmp al,'*'
	je .again
.return:
	mov al,'+'
	mov ah,01110111b
	mov word[gs:bx],ax
	sub word[s1],10
.r:
	ret

;----------------------------------------
;setInt
;----------------------------------------
setInt:
	
	cli;关闭中断
	pusha
	push es
	xor ax,ax 
	mov es,ax 
	mov eax,dword[es:20h]
	mov dword[old_Timer],eax
	mov word[es:20h],new_Timer
	mov ax,cs
	mov [es:22h],ax 
	
	mov eax,dword[es:24h]
	mov dword[old_int9],eax
	mov word[es:24h],new_int9
	mov ax,cs
	mov word[es:26h],ax
	pop es
	popa
	;mov byte[gs:2],0
	sti
	ret
setMusic:
	
	cli 
	pusha 
	push es 
	xor ax,ax
	mov es,ax 
	xor bx,bx
	mov word[es:20h],music
	mov ax,cs
	mov word[es:22h],ax 
	call SetTimer
	
	mov eax,dword[old_int9]
	mov dword[es:24h],eax
	pop es
	popa 
	;xor bx,bx
	call open
	sti 
	ret
;------------------------------------------
;music timer;(si做参数)
;------------------------------------------
music:
	;push bx
    pusha
	sti
	dec word[countm]			; 递减计数变量
	;mov byte[gs:2],'1'
	jnz .end					; >0：跳转
	mov bx,word[phz]
	mov ax,word[delaym+bx]
	mov word[countm],ax		; 重置计数变量=初值delay
; 修改发声的频率
	mov bx,word[phz]
	xor edx,edx	
	xor ecx,ecx
	mov eax,1193182
	mov cx,word[hz+bx]
	add word[phz],2
	div ecx
	;mov byte[gs:0],al
	;mov byte[gs:1],0fh
	out 42h,al					; 写计数器2的低字节
	mov al,ah					; AL=AH
	out 42h,al					; 写计数器2的高字节
.end:
	
	mov al,20h				; AL = EOI
	out 20h,al					; 发送EOI到主8529A
	out 0A0h,al				; 发送EOI到从8529A
	popa
	;pop bx
	cli
	iret						; 从中断返回

;-------------------------------------------
;recoverInt
;-------------------------------------------
recoverInt:
	cmp word[phz],16
	jb recoverInt
	
	call stop
reset:
	cli
	pusha
	push es
	xor eax,eax
	mov es,ax
	mov eax,dword[old_Timer]
	mov dword[es:20h],eax
	
	mov eax,dword[old_int9]
	mov dword[es:24h],eax
	pop es
	popa
	sti
	ret


snakeColor db 10010001b	
delay  db 5   ; 计时器延迟计数
count  db 5
str1:
      db '                                                                                '
	  db '                                                                                '
	  db '                          1.single          2.double                            '
	  db '                                                                                '
	  db '                       choose a mode to start the game                          '   

str1len equ $ - str1	
str2: db '                      Player 1'
str2len equ $ - str2 
str3: db '             Score:       '
str3len equ $ - str3 
p equ str3 + 20
str4: db '             Player 1:                VS              Player 2:                 '
str4len equ $ - str4 
p1 equ str4 + 24
p2 equ str4 + 65
s1: dw 0 
s2: dw 0 
buf db 0,0,0,0
str6: db ' You Win !'
str6len equ $ - str6 
str7: db 'You Lose !'
str7len equ $ - str7 
str8: db 'Player 1 Win '
str8len equ $ - str8 
str9: db 'Player 2 Win '
str9len equ $ - str9 
str10: db '   Tie     '
str10len equ $ - str10
v1: db '                   you WIN this game! Your Score is  666 _(:3 > )_              '
v1len equ $ - v1
v2: db '           hiahiahia~      Player 1 successfully beats Player 2   OVO           '
v2len equ $ - v2
warnColor db 00001100b


foodx: db 0
foody: db 0
snakehx: db 6
snakehy: db 10
snaketx: db 6
snakety: db 10
snakelen: dw 3

snake2hx: db 0
snake2hy: db 0
snake2tx: db 0
snake2ty: db 0
snake2len: dw 3
randomNum: dw 0
randomWall db 0,0
error1: db 0 ;1=player 1 2= player2
error2: db 0 
dir: db 'd'	;0上，1右，2下，3左
dir2: db 'd'	;0上，1右，2下，3左
result: db 0  ;0  fail, 1 win, 2 on game 4 player1 win ,5 player2 win ,6 tie 7 esc
f1 db 0
rate: dw 50
old_Timer dd 0
old_int9 dd 0
delaym: dw 10,20,10,10,20,10,10,10
countm: dw 1
s8255: db 0
hz: dw 784,523,659,784,523,659,784,659
phz: dw 0