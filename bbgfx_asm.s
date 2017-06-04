@      TITLE("larrys_graphics")
@++
@ BitBank Graphics Library (BBGFX)
@ Copyright (c) 2009-2017 BitBank Software, Inc.
@ Written by Larry Bank
@ bitbank@pobox.com
@
@ todo - handle FF case more efficiently in BBGBitPattern
@ changes:
@ 7/2/2010 - fixed bug in BBGFill which didn't fill the "remaining" pixels in the "fast" case
@ 7/5/2010 - optimized BBGBlitAlpha for ARM11 (branch prediction), gains 10-15% speed
@ 7/5/2010 - Added more error checking to BBGFill so that it will reject negative width/height dest rectangles
@ 8/17/2010 - Added translucent option to BBGFill
@ 8/25/2010 - added support for 32bpp images (most features)
@ 3/7/2011 - fixed a problem with BBGPixelConvert and odd width bitmaps
@ 5/30/2017 - converted to Linux (GAS) format
@
@ This program is free software: you can redistribute it and/or modify
@ it under the terms of the GNU General Public License as published by
@ the Free Software Foundation, either version 3 of the License, or
@ (at your option) any later version.
@
@ This program is distributed in the hope that it will be useful,
@ but WITHOUT ANY WARRANTY; without even the implied warranty of
@ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@ GNU General Public License for more details.
@
@ You should have received a copy of the GNU General Public License
@ along with this program.  If not, see <http://www.gnu.org/licenses/>.
@

@ BBGFX_DC structure offsets
.equ BM_SRC_PTR, 0 
.equ BM_DST_PTR, 20 
.equ BM_SRC_PITCH, 4 
.equ BM_DST_PITCH, 24 
.equ BM_SRC_WIDTH, 8 
.equ BM_DST_WIDTH, 28 
.equ BM_SRC_HEIGHT, 12 
.equ BM_DST_HEIGHT, 32 
.equ BM_SRC_BPP, 16 
.equ BM_DST_BPP, 36 
.equ RECT_SRC_LEFT, 40 
.equ RECT_SRC_TOP, 44 
.equ RECT_SRC_RIGHT, 48 
.equ RECT_SRC_BOT, 52 
.equ RECT_DST_LEFT, 56 
.equ RECT_DST_TOP, 60 
.equ RECT_DST_RIGHT, 64 
.equ RECT_DST_BOT, 68 
.equ DST_COLOR, 72 
.equ DST_COLOR2, 76 
.equ DST_NEWCOLOR, 80 
.equ DST_PATTERN, 84 
.equ DST_TRANS, 85 @ translucency (0-255)
.equ DST_ANGLE, 86 @ drawing angle (0,90,180,270)
.equ TEMP_SPACE, 88 @ use for temp vars
  .global  DrawScan90  
  .global  DrawScan270  
  .global  BBGHLine  
  .global  BBGVLine  
  .global  BBGFill  
  .global  BBGGradientFill  
  .global  BBGBlitTransparent  
  .global  BBGBlitFast  
  .global  BBGStretchBlit  
  .global  BBGStretchAlpha  
  .global  BBGStretchPattern  
  .global  BBGRotate  
  .global  BBGRotate90  
  .global  BBGBlitColorSwap  
  .global  BBGBlitPattern  
  .global  BBGBlitAlpha  
  .global  BBGBlitChar  
  .global  BBGLine  
  .global  BBGCircle  
  .global  BBGHFlip  
  .global  BBGVFlip  
  .global  BBGPixelConvert  
  .global  BBGIsTranslucentBitmap  
  .global  BBG565Gray  
@
@ unsigned divide
@ On entry
@ r5 = dividend
@ r3 = divisor
@
@ On exit
@ r5 = quotient
@ r3 = remainder
@
@ trashes r2, r4
divide:   
    cmp r3,#0    @ divide by 0?
    moveq pc,lr    @ return with no result
@ Divide r4 by r2 (unsigned)
    mov r4,r5
    mov r2,r3
    mov r1,#0
@ shift up divisor until it's about to pass numerator
divu32_shift:   
    cmp r2,r4,LSR #1
    movls r2,r2,LSL #1
    bcc divu32_shift
divu32_add:   
    cmp r4,r2
    adc r1,r1,r1    @ double and inc if carry
    subcs r4,r4,r2
    cmp r2,r3
    movne r2,r2,LSR #1
    bne divu32_add
    mov r5,r1    @ get quotient in r5
    mov r3,r4    @ get remainder in r3
    mov pc,lr    @ return
  .ltorg    
@
@ Convert a color RGB565 bitmap into RGB565 grayscale
@ call from C as BBG565Gray(BBGFX_DC *pDC, unsigned char ucBias)@
@ uses destination bitmap and destination rectangle
@
BBG565Gray:   
    ldr r2,[r0,#BM_DST_BPP]    @ make sure dest surface is RGB565
    cmp r2,#16
    movne pc,lr
    
    stmfd sp!,{r4-r12,lr}
    
    ldr r10,[r0,#RECT_DST_LEFT]    @ get x1
    ldr r2,[r0,#RECT_DST_TOP]  @ get y1
    ldr r3,[r0,#RECT_DST_RIGHT] @ get x2
    ldr r4,[r0,#RECT_DST_BOT]  @ get y2
    ldr r5,[r0,#BM_DST_PTR]
    ldr r14,[r0,#BM_DST_PITCH]
@ calculate source pointer
    mul r6,r2,r14
    add r5,r5,r10,LSL #1
    add r5,r5,r6                @ now R6 points to dest bitmap
    mov r12,#0x1f                @ 5-bit color mask
    mov r0,#5                    @ for multiplying green
    cmp r1,#0
    movle r1,#-1                @ make it negative to disable bias
    cmp  r1,#2
    movgt r1,#-1                @ out of range values also disabled
    cmp  r1,#1                    @ lighten?
    moveq r1,#256
    cmp  r1,#2                    @ darken?
    moveq r1,#0
    sub  r3,r3,r10
    adds r3,r3,#1                @ width
    ble  bbg565_exit            @ width <= 0
    sub  r4,r4,r2
    adds r4,r4,#1                @ height
    ble  bbg565_exit            @ height <= 0
bbg565_top:   
    mov  r6,r5                    @ src/dest pointer in R6
    mov  r7,r3
bbg565_loop:   
    tst  r6,#2                    @ dword aligned?
    beq  bbg565_fast            @ do it a word at a time
    ldrh r8,[r6]                @ read a pixel
    sub  r7,r7,#1
    and  r9,r8,r12                @ get blue
    and  r10,r12,r8,LSR #6        @ top 5 bits of green
    and  r11,r12,r8,LSR #11        @ isolate red
    add  r9,r9,r11,LSL #1        @ b+2*r
    mla  r11,r0,r10,r9            @ 5g + b + 2r
    orrs r1,r1,r1                @ bias black or white?
    addpl r11,r11,r1
    movpl r11,r11,LSR #1        @ add and shift back to original level
@ we now have 8 significant bits of gray@ refactor into RGB565 format
    mov  r9,r11,LSR #2            @ 6 bits for G
    mov  r11,r11,LSR #3            @ 5 bits for R/B
    orr  r11,r11,r11,LSL #11    @ R+B
    orr  r11,r11,r9,LSL #5        @ G
    strh r11,[r6],#2            @ store gray pixel
bbg565_fast:   
    cmp  r7,#2                    @ narrow image? less than 2 pixels left?
    blt  bbg565_slow            @ last pixel
    ldr  r8,[r6]                @ get a pair of pixels
    sub  r7,r7,#2
    and  r9,r8,r12                @ get blue
    and  r10,r12,r8,LSR #6        @ top 5 bits of green
    and  r2,r12,r8,LSR #11        @ isolate red
    add  r9,r9,r2,LSL #1        @ b+2*r
    mla  r11,r0,r10,r9            @ 5g + b + 2r
    orrs r1,r1,r1                @ bias black or white?
    addpl r11,r11,r1
    movpl r11,r11,LSR #1        @ add and shift back to original level
@ we now have 8 significant bits of gray@ refactor into RGB565 format
    mov  r9,r11,LSR #2            @ 6 bits for G
    mov  r11,r11,LSR #3            @ 5 bits for R/B
    orr  r11,r11,r11,LSL #11    @ R+B
    orr  r11,r11,r9,LSL #5        @ G
@ prepare high word
    and  r9,r12,r8,LSR #16        @ get blue
    and  r10,r12,r8,LSR #22        @ top 5 bits of green
    and  r8,r12,r8,LSR #27        @ isolate red
    add  r9,r9,r8,LSL #1        @ b+2*r
    mla  r8,r0,r10,r9            @ 5g + b + 2r
    orrs r1,r1,r1                @ bias black or white?
    addpl r8,r8,r1
    movpl r8,r8,LSR #1            @ add and shift back to original level
@ we now have 8 significant bits of gray@ refactor into RGB565 format
    mov  r9,r8,LSR #2            @ 6 bits for G
    mov  r8,r8,LSR #3            @ 5 bits for R/B
    orr  r8,r8,r8,LSL #11        @ R+B
    orr  r8,r8,r9,LSL #5        @ G
    orr  r11,r11,r8,LSL #16        @ both pixels
    str  r11,[r6],#4
    b    bbg565_fast
@ if there is an odd pixel remaining...
bbg565_slow:   
    cmp  r7,#1                    @ any pixels remaining?
    blt  bbg565_bot                @ nope, skip it
    ldrh r8,[r6]                @ read a pixel
    subs r7,r7,#1
    and  r9,r8,r12                @ get blue
    and  r10,r12,r8,LSR #6        @ top 5 bits of green
    and  r11,r12,r8,LSR #11        @ isolate red
    add  r9,r9,r11,LSL #1        @ b+2*r
    mla  r11,r0,r10,r9            @ 5g + b + 2r
    orrs r1,r1,r1                @ bias black or white?
    addpl r11,r11,r1
    movpl r11,r11,LSR #1        @ add and shift back to original level
@ we now have 8 significant bits of gray@ refactor into RGB565 format
    mov  r9,r11,LSR #2            @ 6 bits for G
    mov  r11,r11,LSR #3            @ 5 bits for R/B
    orr  r11,r11,r11,LSL #11    @ R+B
    orr  r11,r11,r9,LSL #5        @ G
    strh r11,[r6],#2            @ store gray pixel    
bbg565_bot:   
    add  r5,r5,r14                @ ptr+=pitch
    subs r4,r4,#1                @ height--
    bne  bbg565_top
bbg565_exit:   
    ldmia sp!,{r4-r12,pc}
@
@ Rotate an image "n" degrees around a center point from source to dest
@ call from C as BBGRotate(BBGFX_DC *pDC, int iAngle)@
@
BBGRotate:   
         stmfd sp!,{r4-r12,lr}
@ since we work from dest back to source
@ convert angle to 360-angle
         cmp   r1,#0        @ angle 0 stays 0
         rsbne r1,r1,#360
         ldr   r12,=sine @ point to sine/cosine table
         mov   r1,r1,LSL #1    @ indexing words
         ldrsh r2,[r12,r1]    @ get the sine
         add   r1,r1,#180    @ point to cosine
         ldrsh r3,[r12,r1]
@ pre-calculate the sine/cosine from -512 to 511
         add  r4,r0,#TEMP_SPACE    @ point to sine table
         add  r5,r4,#2048    @ point to cosine table
         mov  r8,#-256        @ starting value
         mov  r8,r8,LSL #1    @ -512
BBGRotateTab:   
         muls r6,r8,r2        @ sin * x
         muls r7,r8,r3        @ cos * x
         mov  r6,r6,ASR #15    @ adjust and retain sign
         mov  r7,r7,ASR #15    @ ditto
         strh r6,[r4],#2    @ save sin value
         strh r7,[r5],#2    @ save cos value
         add  r8,r8,#1
         cmp  r8,#512        @ loop until done
         bne  BBGRotateTab
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_LEFT] @ center X
         ldr  r4,[r0,#RECT_SRC_TOP]    @ src center Y coordinate
         ldr  r8,[r0,#BM_DST_PITCH]
         orr  r3,r3,r4,LSL #16    @ combine src center x/y in R3
         ldr  r6,[r0,#RECT_DST_LEFT] @ dst center X
         ldr  r7,[r0,#RECT_DST_TOP]    @ dst center Y
         orr  r2,r2,r8,LSL #16    @ keep src and dst pitch in R2
         orr  r4,r6,r7,LSL #16    @ combine dst center x/y in r4
         ldr  r9,[r0,#BM_SRC_WIDTH]    @ get source bitmap size here
         ldr  r10,[r0,#BM_SRC_HEIGHT]
         ldr  r7,[r0,#BM_DST_PTR]    @ destination pointer
         orr  r5,r9,r10,LSL #16    @ combine cx/cy in R5
         ldr  r10,[r0,#BM_DST_WIDTH]
         ldr  r11,[r0,#BM_DST_HEIGHT]
         ldr  r12,[r0,#BM_SRC_BPP]    @ 1 or 16 bpp?
         orr  r6,r10,r11,LSL #16    @ combine cx/cy in R6
@ walk through pixels of destination and rotate them
@ back from source (to not leave gaps)
@ R1 = source bitmap pointer
@ R2 = src pitch (b), dst pitch (t)
@ R3 = src center X (b), src center Y (t)
@ R5 = src width (b), src height (t)
@ R6 = dst width (b), dst height (t)
@ R7 = destination bitmap pointer
@ R8 = pointer to x*sin(theta)
@ R9 = pointer to y*sin(theta)
@ R10 = current x/y (destination)
@ R4, R11, R12, R14 = temp
        mov  r10,#0        @ current x/y (in destination)
        cmp  r12,#1        @ 1bpp bitmap?
        beq  BBGRotate1bpp0
BBGRotate0:   
@ do it line by line
        ldr  r4,[r0,#RECT_DST_TOP]    @ dst center Y
        add  r9,r0,#TEMP_SPACE    @ point to start of sin table
        add  r9,r9,#1024        @ point to middle (for +/- offset)
        mov  r11,r10,LSR #16    @ get current Y
        sub  r11,r11,r4            @ get current Y - center Y
        add  r9,r9,r11,LSL #1    @ now r9 points to y*sin
        ldr  r4,[r0,#RECT_DST_LEFT]    @ dst center X
        mov  r8,r10,LSL #16        @ get X
        mov  r8,r8,LSR #16
        sub  r8,r8,r4            @ minus center X        
        add  r11,r0,#TEMP_SPACE    @ point to start of sin table
        add  r11,r11,#1024        @ point to middle of table
        add  r8,r11,r8,LSL #1    @ now r8 points to x*sin
BBGRotate1:   
@ translate current position into relative x,y through dest center point
        ldrsh r4,[r9]        @ sin*y
        ldrsh r11,[r8]        @ sin*x
        mov   r12,#2048        @ offset to cosine table
        ldrsh r14,[r8,r12]    @ cos*x
        ldrsh r12,[r9,r12]    @ cos*y
        sub  r4,r14,r4        @ x' = cos*x - sin*y
        add  r11,r11,r12    @ y' = sin*x + cos*y
@ now we have the rotated X/Y, translate through the source center point
@ check if it's out of bounds
@        mov  r14,#0xf800
@        orr  r14,r14,#0x1f    @ put transparent color in default src pixel
        mov  r12,r3,LSL #16    @ isolate src center x
        adds r4,r4,r12,LSR #16        @ add source center x
        addpls r11,r11,r3,LSR #16    @ add source center y
        bmi  BBGRotate2    @ off the page
        cmp  r11,r5,LSR #16    @ past bottom
        bge  BBGRotate2
        mov  r12,r5,LSL #16
        mov  r12,r12,LSR #16    @ isolate source width
        cmp  r4,r12
        bge  BBGRotate2        @ point is off the page, skip it
@ calculate source address
        ldr  r12,[r0,#BM_SRC_BPP]        @ 16/32bpp?
        mov  r14,r2,LSL #16    @ get src pitch
        mov  r14,r14,LSR #16
        cmp  r12,#32
        mul  r12,r14,r11        @ y*pitch
        add  r12,r12,r1        @ +pbitmap
        addne  r12,r12,r4,LSL #1    @ + (x<<1)
        addeq  r12,r12,r4,LSL #2    @ + (x<<2)
        ldrneh r14,[r12]        @ get rotated source pixel
        ldreq r14,[r12]
@ set destination pixel    
        mov  r4,r10,LSR #16    @ get dest y
        mov  r11,r2,LSR #16    @ dest pitch
        mul  r12,r4,r11
        add  r12,r12,r7        @ + dest bitmap ptr
        mov  r4,r10,LSL #16    @ get dest x
        mov  r4,r4,LSR #16
        addne  r12,r12,r4,LSL #1    @ now we have it    
        addeq  r12,r12,r4,LSL #2
        strneh r14,[r12]        @ store destination pixel
        streq  r14,[r12]
BBGRotate2:   
@ advance x and then y
        add  r8,r8,#2        @ point to next x*sin(theta)
        add  r10,r10,#1        @ x++
        mov  r11,r10,LSL #16
        mov  r11,r11,LSR #16    @ isolate x
        mov  r12,r6,LSL #16
        mov  r12,r12,LSR #16    @ get dest width
        cmp  r11,r12        @ hit right edge?
        blt  BBGRotate1        @ not yet
        mov  r10,r10,LSR #16    @ x = 0
        add  r10,r10,#1            @ y++
        add  r9,r9,#2        @ y*sine(theta) inc
        cmp  r10,r6,LSR #16    @ hit bottom edge?
        beq  BBGRotateExit    @ yes, time to go
        mov  r10,r10,LSL #16    @ reset y to top position
        b    BBGRotate0        @ keep looping
  .ltorg    
@
@ 1 bpp version
@
BBGRotate1bpp0:   
@ do it line by line
        ldr  r4,[r0,#RECT_DST_TOP]    @ dst center Y
        add  r9,r0,#TEMP_SPACE    @ point to start of sin table
        add  r9,r9,#512            @ point to middle (for +/- offset)
        mov  r11,r10,LSR #16    @ get current Y
        sub  r11,r11,r4            @ get current Y - center Y
        add  r9,r9,r11,LSL #1    @ now r9 points to y*sin
        ldr  r4,[r0,#RECT_DST_LEFT]    @ dst center X
        mov  r8,r10,LSL #16        @ get X
        mov  r8,r8,LSR #16
        sub  r8,r8,r4            @ minus center X        
        add  r11,r0,#TEMP_SPACE    @ point to start of sin table
        add  r11,r11,#512    @ point to middle of table
        add  r8,r11,r8,LSL #1    @ now r8 points to x*sin
BBGRotate1bpp1:   
@ translate current position into relative x,y through dest center point
        ldrsh r4,[r9]        @ sin*y
        ldrsh r11,[r8]        @ sin*x
        mov   r12,#1024        @ offset to cosine table
        ldrsh r14,[r8,r12]    @ cos*x
        ldrsh r12,[r9,r12]    @ cos*y
        sub  r4,r14,r4        @ x' = cos*x - sin*y
        add  r11,r11,r12    @ y' = sin*x + cos*y
@ now we have the rotated X/Y, translate through the source center point
@ check if it's out of bounds
        mov  r12,r3,LSL #16    @ isolate src center x
        adds r4,r4,r12,LSR #16        @ add source center x
        addpls r11,r11,r3,LSR #16    @ add source center y
        bmi  BBGRotate1bpp2    @ off the page
        cmp  r11,r5,LSR #16    @ past bottom
        bge  BBGRotate1bpp2
        mov  r12,r5,LSL #16
        mov  r12,r12,LSR #16    @ isolate source width
        cmp  r4,r12
        bge  BBGRotate1bpp2        @ point is off the page, skip it
@ calculate source address
        mov  r14,r2,LSL #16    @ get src pitch
        mov  r14,r14,LSR #16
        mla  r12,r14,r11,r1        @ y*pitch + pbitmap
        add  r12,r12,r4,LSR #3    @ + (x>>3)
        ldrb r14,[r12]        @ get rotated source pixel
        and  r12,r4,#7        @ get bit number
        mov  r14,r14,LSL r12    @ shift pixel into bit 7 (left-most pixel)
        tst  r14,#0x80        @ pixel on?
@ set destination pixel    
        mov  r4,r10,LSR #16    @ get dest y
        mov  r14,r2,LSR #16    @ dest pitch
        mul  r12,r4,r14
        add  r11,r12,r7
        mov  r12,r10,LSL #16    @ get dest x
        mov  r12,r12,LSR #16
        add  r11,r11,r12,LSR #3    @ now we have it    
        ldrb r12,[r11]        @ get current byte of destination
        and  r4,r10,#7        @ get destination bit number
        mov  r14,#0x80        @ prepare bit mask to affect dest pixel
        mov  r14,r14,LSR r4    @ shift down pixel
        biceq  r12,r12,r14    @ clear it
        orrne  r12,r12,r14    @ set it
        strb r12,[r11]        @ store it back
BBGRotate1bpp2:   
        add  r8,r8,#2        @ point to next x*sin(theta)
        add  r10,r10,#1        @ x++
        mov  r11,r10,LSL #16
        cmp  r11,r6,LSL #16    @ compare to dest width (hit right edge)?
        blt  BBGRotate1bpp1    @ not yet
        mov  r10,r10,LSR #16    @ x = 0
        add  r10,r10,#1            @ y++
        add  r9,r9,#2        @ y*sine(theta) inc
        cmp  r10,r6,LSR #16    @ hit bottom edge?
        beq     BBGRotateExit    @ yes, time to go
        mov  r10,r10,LSL #16    @ reset y to top position
        b    BBGRotate1bpp0        @ keep looping
BBGRotateExit:   
        ldmia sp!,{r4-r12,pc}
@ draw the 8 pixels in the circle
@ R0 = pixel color
@ R1,R2,R3 = temp
@ R4 = current X
@ R5 = current Y
@ R6 = must preserve
@ R7 = center X
@ R8 = center Y
@ R9 = dest bitmap ptr
@ R10 = dest pitch
@ R11 = X fraction
@ R12 = Y fraction
BBGCirclePixels:   
        cmp  r11,r12        @ true circle?  can draw it faster
        bne  circlepixels1
        add  r2,r4,r7        @ new X
        add  r3,r5,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #1    @ x offset
        strh r0,[r1]        @ plot (cx+x,cy+y)
        sub  r1,r1,r4,LSL #2
        strh r0,[r1]        @ plot (cx-x,cy+y)
        sub  r3,r3,r5,LSL #1    @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #1    @ add x
        strh r0,[r1]        @ (cx+x,cy-y)
        sub  r1,r1,r4,LSL #2
        strh r0,[r1]        @ (cx-x,cy-y)
@ swap x/y deltas
        add  r2,r5,r7        @ new X
        add  r3,r4,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #1    @ x offset
        strh r0,[r1]        @ plot (cx+y,cy+x)
        sub  r1,r1,r5,LSL #2
        strh r0,[r1]        @ plot (cx-y,cy+x)
        sub  r3,r3,r4,LSL #1        @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #1    @ add x
        strh r0,[r1]        @ (cx+y,cy-x)
        sub  r1,r1,r5,LSL #2
        strh r0,[r1]        @ (cx-y,cy-x)
        bx   lr
@ first, scale the pixels to the "elipse" fractions
circlepixels1:   
        mul  r2,r4,r11
        mul  r3,r5,r12
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r2    @ new X
        add  r8,r8,r3    @ new Y
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
        strh r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r2,LSL #2
        strh r0,[r1]        @ plot (cx-x, cy+y)
        sub  r8,r8,r3,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
        strh r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r2,LSL #2
        strh r0,[r1]        @ plot (cx-x, cy-y)
        add  r8,r8,r3        @ restore center Y
        sub  r7,r7,r2        @ restore center X
@ swap x/y deltas
        mul  r2,r4,r12        @ swap scaling of x,y
        mul  r3,r5,r11
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r3    @ new X (centx + cy)
        add  r8,r8,r2    @ new Y (centy + cx)
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
        strh r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r3,LSL #2
        strh r0,[r1]        @ plot (cx-x, cy+y)
        sub  r8,r8,r2,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9       @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
        strh r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r3,LSL #2
        strh r0,[r1]        @ plot (cx-x, cy-y)
        add  r8,r8,r2        @ restore center Y
        sub  r7,r7,r3        @ restore center X
        bx   lr
@ draw a filled in circle
@ R0 = pixel color
@ R1,R2,R3 = temp
@ R4 = current X
@ R5 = current Y
@ R6 = must preserve
@ R7 = center X
@ R8 = center Y
@ R9 = dest bitmap ptr
@ R10 = dest pitch
@ R11 = X fraction
@ R12 = Y fraction
BBGFCirclePixels:   
        stmfd sp!,{r14}
        cmp  r11,r12        @ true circle?  can draw it faster
        bne  fcirclepixels1
        add  r2,r4,r7        @ new X
        add  r3,r5,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #1    @ x offset
@        strh r0,[r1]        @ plot (cx+x,cy+y)
        sub  r1,r1,r4,LSL #2
        mov  r14,r4,LSL #1
fcirclepixels0_0:   
        strh r0,[r1],#2        @ plot (cx-x,cy+y) to (cx+x,cy+y)
        subs r14,r14,#1
        bge  fcirclepixels0_0
        sub  r3,r3,r5,LSL #1    @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #1    @ add x
@        strh r0,[r1]        @ (cx+x,cy-y)
        sub  r1,r1,r4,LSL #2
        mov  r14,r4,LSL #1
fcirclepixels0_1:   
        strh r0,[r1],#2        @ plot (cx-x,cy-y) to (cx+x,cy-y)
        subs r14,r14,#1
        bge  fcirclepixels0_1
@ swap x/y deltas
        add  r2,r5,r7        @ new X
        add  r3,r4,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #1    @ x offset
@        strh r0,[r1]        @ plot (cx+y,cy+x)
        sub  r1,r1,r5,LSL #2
        mov  r14,r5,LSL #1
fcirclepixels0_2:   
        strh r0,[r1],#2        @ plot (cx-y,cy+x) to (cx+y,cy+x)
        subs r14,r14,#1
        bge  fcirclepixels0_2
        sub  r3,r3,r4,LSL #1        @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #1    @ add x
@        strh r0,[r1]        @ (cx+y,cy-x)
        sub  r1,r1,r5,LSL #2
        mov  r14,r5,LSL #1
fcirclepixels0_3:   
        strh r0,[r1],#2        @ plot (cx-y,cy-x) to (cx+y,cy-x)
        subs r14,r14,#1
        bge  fcirclepixels0_3
        ldmfd sp!,{pc}
@ first, scale the pixels to the "elipse" fractions
fcirclepixels1:   
        mul  r2,r4,r11
        mul  r3,r5,r12
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r2    @ new X
        add  r8,r8,r3    @ new Y
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
@        strh r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r2,LSL #2
        mov  r14,r2,LSL #1
fcirclepixels1_0:   
        strh r0,[r1],#2        @ plot (cx-x, cy+y)
        subs r14,r14,#1
        bge  fcirclepixels1_0
        sub  r8,r8,r3,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
@        strh r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r2,LSL #2
        mov  r14,r2,LSL #1
fcirclepixels1_1:   
        strh r0,[r1],#2        @ plot (cx-x, cy-y)
        subs r14,r14,#1
        bge  fcirclepixels1_1
        add  r8,r8,r3        @ restore center Y
        sub  r7,r7,r2        @ restore center X
@ swap x/y deltas
        mul  r2,r4,r12        @ swap scaling of x,y
        mul  r3,r5,r11
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r3    @ new X (centx + cy)
        add  r8,r8,r2    @ new Y (centy + cx)
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
@        strh r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r3,LSL #2
        mov  r14,r3,LSL #1
fcirclepixels1_2:   
        strh r0,[r1],#2        @ plot (cx-x, cy+y)
        subs r14,r14,#1
        bge  fcirclepixels1_2
        sub  r8,r8,r2,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9       @ destination pixel
        add  r1,r1,r7,LSL #1    @ now we point to pixel
@        strh r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r3,LSL #2
        mov  r14,r3,LSL #1
fcirclepixels1_3:   
        strh r0,[r1],#2        @ plot (cx-x, cy-y)
        subs r14,r14,#1
        bge  fcirclepixels1_3
        add  r8,r8,r2        @ restore center Y
        sub  r7,r7,r3        @ restore center X
        ldmfd sp!,{pc}
@ draw the 8 pixels in the circle
@ R0 = pixel color
@ R1,R2,R3 = temp
@ R4 = current X
@ R5 = current Y
@ R6 = must preserve
@ R7 = center X
@ R8 = center Y
@ R9 = dest bitmap ptr
@ R10 = dest pitch
@ R11 = X fraction
@ R12 = Y fraction
BBGCircle32Pixels:   
        cmp  r11,r12        @ true circle?  can draw it faster
        bne  circle32pixels1
        add  r2,r4,r7        @ new X
        add  r3,r5,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #2    @ x offset
        str  r0,[r1]        @ plot (cx+x,cy+y)
        sub  r1,r1,r4,LSL #3
        str  r0,[r1]        @ plot (cx-x,cy+y)
        sub  r3,r3,r5,LSL #1    @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #2    @ add x
        str  r0,[r1]        @ (cx+x,cy-y)
        sub  r1,r1,r4,LSL #3
        str  r0,[r1]        @ (cx-x,cy-y)
@ swap x/y deltas
        add  r2,r5,r7        @ new X
        add  r3,r4,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #2    @ x offset
        str  r0,[r1]        @ plot (cx+y,cy+x)
        sub  r1,r1,r5,LSL #3
        str  r0,[r1]        @ plot (cx-y,cy+x)
        sub  r3,r3,r4,LSL #1        @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #2    @ add x
        str  r0,[r1]        @ (cx+y,cy-x)
        sub  r1,r1,r5,LSL #3
        str  r0,[r1]        @ (cx-y,cy-x)
        bx   lr
@ first, scale the pixels to the "elipse" fractions
circle32pixels1:   
        mul  r2,r4,r11
        mul  r3,r5,r12
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r2    @ new X
        add  r8,r8,r3    @ new Y
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
        str  r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r2,LSL #3
        str  r0,[r1]        @ plot (cx-x, cy+y)
        sub  r8,r8,r3,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9       @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
        str  r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r2,LSL #3
        str  r0,[r1]        @ plot (cx-x, cy-y)
        add  r8,r8,r3        @ restore center Y
        sub  r7,r7,r2        @ restore center X
@ swap x/y deltas
        mul  r2,r4,r12        @ swap scaling of x,y
        mul  r3,r5,r11
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r3    @ new X (centx + cy)
        add  r8,r8,r2    @ new Y (centy + cx)
        mla  r1,r8,r10,r9    @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
        str  r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r3,LSL #3
        str  r0,[r1]        @ plot (cx-x, cy+y)
        sub  r8,r8,r2,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9       @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
        str  r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r3,LSL #3
        str  r0,[r1]        @ plot (cx-x, cy-y)
        add  r8,r8,r2        @ restore center Y
        sub  r7,r7,r3        @ restore center X
        bx   lr
@ draw the horizontal pixels for a filled circle
@ R0 = pixel color
@ R1,R2,R3 = temp
@ R4 = current X
@ R5 = current Y
@ R6 = must preserve
@ R7 = center X
@ R8 = center Y
@ R9 = dest bitmap ptr
@ R10 = dest pitch
@ R11 = X fraction
@ R12 = Y fraction
BBGFCircle32Pixels:   
        stmfd sp!,{lr}
        cmp  r11,r12        @ true circle?  can draw it faster
        bne  fcircle32pixels1
        add  r2,r4,r7        @ new X
        add  r3,r5,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #2    @ x offset
@        str  r0,[r1]        @ plot (cx+x,cy+y)
        sub  r1,r1,r4,LSL #3
        mov  r14,r4,LSL #1
fcircle32pixels0_0:   
        str  r0,[r1],#4        @ plot (cx-x,cy+y) to (cx+x,cy+y)
        subs r14,r14,#1
        bge  fcircle32pixels0_0
        sub  r3,r3,r5,LSL #1    @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #2    @ add x
@        str  r0,[r1]        @ (cx+x,cy-y)
        sub  r1,r1,r4,LSL #3
        mov  r14,r4,LSL #1
fcircle32pixels0_1:   
        str  r0,[r1],#4        @ (cx-x,cy-y)
        subs r14,r14,#1
        bge  fcircle32pixels0_1
@ swap x/y deltas
        add  r2,r5,r7        @ new X
        add  r3,r4,r8        @ new Y
        mla  r1,r3,r10,r9    @ y offset + bitmap
        add  r1,r1,r2,LSL #2    @ x offset
@        str  r0,[r1]        @ plot (cx+y,cy+x)
        sub  r1,r1,r5,LSL #3
        mov  r14,r5,LSL #1
fcircle32pixels0_2:   
        str  r0,[r1],#4        @ plot (cx-y,cy+x) to (cy+y,cy+x)
        subs r14,r14,#1
        bge  fcircle32pixels0_2
        sub  r3,r3,r4,LSL #1        @ -y*2
        mla  r1,r3,r10,r9
        add  r1,r1,r2,LSL #2    @ add x
@        str  r0,[r1]        @ (cx+y,cy-x)
        sub  r1,r1,r5,LSL #3
        mov  r14,r5,LSL #1
fcircle32pixels0_3:   
        str  r0,[r1],#4        @ (cx-y,cy-x)
        subs r14,r14,#1
        bge  fcircle32pixels0_3
        ldmfd sp!,{pc}
@ first, scale the pixels to the "elipse" fractions
fcircle32pixels1:   
        mul  r2,r4,r11
        mul  r3,r5,r12
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r2    @ new X
        add  r8,r8,r3    @ new Y
        mla  r1,r8,r10,r9        @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
@        str  r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r2,LSL #3
        mov  r14,r2,LSL #1
fcircle32pixels1_0:   
        str  r0,[r1],#4        @ plot (cx-x, cy+y)
        subs r14,r14,#1
        bge  fcircle32pixels1_0
        sub  r8,r8,r3,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9       @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
@        str  r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r2,LSL #3
        mov  r14,r2,LSL #1
fcircle32pixels1_1:   
        str  r0,[r1],#4        @ plot (cx-x, cy-y)
        subs r14,r14,#1
        bge  fcircle32pixels1_1
        add  r8,r8,r3        @ restore center Y
        sub  r7,r7,r2        @ restore center X
@ swap x/y deltas
        mul  r2,r4,r12        @ swap scaling of x,y
        mul  r3,r5,r11
        mov  r2,r2,LSR #16  @ new X
        mov  r3,r3,LSR #16    @ new Y
        add  r7,r7,r3    @ new X (centx + cy)
        add  r8,r8,r2    @ new Y (centy + cx)
        mla  r1,r8,r10,r9    @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
@        str  r0,[r1]        @ plot (cx+x, cy+y)
        sub  r1,r1,r3,LSL #3
        mov  r14,r3,LSL #1
fcircle32pixels1_2:   
        str  r0,[r1],#4        @ plot (cx-x, cy+y)
        subs r14,r14,#1
        bge  fcircle32pixels1_2
        sub  r8,r8,r2,LSL #1    @ mirror across Y
        mla  r1,r8,r10,r9       @ destination pixel
        add  r1,r1,r7,LSL #2    @ now we point to pixel
@        str  r0,[r1]        @ plot (cx+x, cy-y)
        sub  r1,r1,r3,LSL #3
        mov  r14,r3,LSL #1
fcircle32pixels1_3:   
        str  r0,[r1],#4        @ plot (cx-x, cy-y)
        subs r14,r14,#1
        bge  fcircle32pixels1_3
        add  r8,r8,r2        @ restore center Y
        sub  r7,r7,r3        @ restore center X
        ldmfd sp!,{pc}
@
@ Draw a circle
@ call from C as BBGCircle(BBGFX_DC *pDC)@
@
BBGCircle:   
         stmfd sp!,{r4-r12,lr}
         ldr r1,[r0,#RECT_DST_LEFT]    @ get x1
         ldr r2,[r0,#RECT_DST_TOP]  @ get y1
         ldr r3,[r0,#RECT_DST_RIGHT] @ get x2
         ldr r4,[r0,#RECT_DST_BOT]  @ get y2
         ldr r9,[r0,#BM_DST_WIDTH]
         ldr r10,[r0,#BM_DST_HEIGHT]
@ make sure the circle fits within the bounds of the dest bitmap
         cmp r1,#0
         blt BBGCircleExit
         cmp r1,r9
         bge BBGCircleExit
         cmp r3,#0
         blt BBGCircleExit
         cmp r3,r9
         bge BBGCircleExit
         cmp r2,#0
         blt BBGCircleExit
         cmp r2,r10
         bge BBGCircleExit
         cmp r4,#0
         blt BBGCircleExit
         cmp r4,r10
         bge BBGCircleExit
@ get the horizontal and vertical radii from the dest rect
         sub r9,r3,r1
         add r9,r9,#1        @ get diameter
         mov r9,r9,LSR #1    @ /2 = radius
         cmp r9,#0
         ble BBGCircleExit
         sub r10,r4,r2
         add r10,r10,#1        @ get diameter
         mov r10,r10,LSR #1    @ /2 = radius
         cmp r10,#0
         ble BBGCircleExit
@ get the center point
         add r7,r3,r1
         mov r7,r7,LSR #1    @ center x
         add r8,r4,r2
         mov r8,r8,LSR #1    @ center y
@ see which radius is larger and use it as the "reference"
         mov r11,#0x10000        @ assume 1:1
         mov r12,#0x10000
         cmp r9,r10            @ compare x/y radii
         beq bbgcircle0        @ perfectly round
         bgt bbgcircle1
@ y is larger, get fraction for x
         mov r5,r9,LSL #16    @ x fraction = (x*65536)/y
         mov r3,r10
         bl  divide
         mov r11,r5        @ get fraction in r11
         mov r9,r10        @ keep "bigger" radius in R9
         b   bbgcircle0
@ x is larger, get fraction for y
bbgcircle1:   
         mov r5,r10,LSL #16    @ y fraction = (y*65536)/x
         mov r3,r9
         bl  divide
         mov r12,r5        @ get fraction in r12
         ldr r14,[r0,#DST_COLOR]
@ Draw the circle
@ R4 = current X
@ R5 = current Y
@ R6 = error
@ R7 = center X
@ R8 = center Y
@ R9 = "bigger" radius, then dest bitmap ptr
@ R10 = pitch
@ R11 = X fraction
@ R12 = Y fraction
@ R14 = pixel color
bbgcircle0:   
        mov r4,#0        @ x = 0
        mov r5,r9        @ y = radius
        mov r6,r9,LSL #2    @ error = 3-radius*4
        rsb r6,r6,#3
        ldr r9,[r0,#BM_DST_PTR]
        ldr r1,[r0,#BM_DST_BPP]
        ldr r10,[r0,#BM_DST_PITCH]
        ldrb r14,[r0,#DST_TRANS]        @ translucency 255=filled, !=255 = outline
        ldr r0,[r0,#DST_COLOR]
        cmp r1,#32        @ 32bpp destination?
        beq bbgcircle32
        cmp r14,#255    @ filled circle?
        beq bbgfilledcircle
bbgcircle2: @ while (y >= x)
        bl  BBGCirclePixels    @ draw the 8 pixels around the elipse
        add r4,r4,#1        @ x++
        cmp r5,r4
        blt BBGCircleExit    @ break out of while loop
        orrs r6,r6,r6        @ error < 0?
        bpl  bbgcircle3
        add  r6,r6,r4,LSL #2    @ error += x*4 + 6
        add  r6,r6,#6
        b    bbgcircle2        @ loop
bbgcircle3:   
        sub  r2,r5,r4        @ temp = y-x
        mov  r2,r2,LSL #2    @ *4
        sub  r2,r2,#10        @ - 10
        sub  r6,r6,r2        @ error -= ((y-x)*4-10)
        sub  r5,r5,#1        @ y--
        b    bbgcircle2        @ loop
bbgfilledcircle: @ while (y >= x)
        bl  BBGFCirclePixels    @ draw the 8 pixels around the elipse
        add r4,r4,#1        @ x++
        cmp r5,r4
        blt BBGCircleExit    @ break out of while loop
        orrs r6,r6,r6        @ error < 0?
        bpl  bbgfilledcircle1
        add  r6,r6,r4,LSL #2    @ error += x*4 + 6
        add  r6,r6,#6
        b    bbgfilledcircle @ loop
bbgfilledcircle1:   
        sub  r2,r5,r4        @ temp = y-x
        mov  r2,r2,LSL #2    @ *4
        sub  r2,r2,#10        @ - 10
        sub  r6,r6,r2        @ error -= ((y-x)*4-10)
        sub  r5,r5,#1        @ y--
        b    bbgfilledcircle     @ loop
BBGCircleExit:   
        ldmia sp!,{r4-r12,pc}
bbgcircle32:   
        cmp r14,#255    @ filled circle?
        beq bbgfilledcircle32
bbgcircle32_0:   
        bl  BBGCircle32Pixels    @ draw the 8 pixels around the elipse
        add r4,r4,#1        @ x++
        cmp r5,r4
        blt BBGCircleExit    @ break out of while loop
        orrs r6,r6,r6        @ error < 0?
        bpl  bbgcircle32_3
        add  r6,r6,r4,LSL #2    @ error += x*4 + 6
        add  r6,r6,#6
        b    bbgcircle32_0      @ loop
bbgcircle32_3:   
        sub  r2,r5,r4        @ temp = y-x
        mov  r2,r2,LSL #2    @ *4
        sub  r2,r2,#10        @ - 10
        sub  r6,r6,r2        @ error -= ((y-x)*4-10)
        sub  r5,r5,#1        @ y--
        b    bbgcircle32_0   @ loop
bbgfilledcircle32:   
        bl  BBGFCircle32Pixels    @ draw the 8 pixels around the elipse
        add r4,r4,#1        @ x++
        cmp r5,r4
        blt BBGCircleExit    @ break out of while loop
        orrs r6,r6,r6        @ error < 0?
        bpl  bbgcircle32_4
        add  r6,r6,r4,LSL #2    @ error += x*4 + 6
        add  r6,r6,#6
        b    bbgfilledcircle32      @ loop
bbgcircle32_4:   
        sub  r2,r5,r4        @ temp = y-x
        mov  r2,r2,LSL #2    @ *4
        sub  r2,r2,#10        @ - 10
        sub  r6,r6,r2        @ error -= ((y-x)*4-10)
        sub  r5,r5,#1        @ y--
        b    bbgfilledcircle32   @ loop
@
@ Draw a line
@ call from C as BBGLine(BBGFX_DC *pDC)@
@ the line start/end points defined by the destination rectangle
@
BBGLine:   
         stmfd sp!,{r4-r12,lr}
         ldr r1,[r0,#RECT_DST_LEFT]    @ get x1
         ldr r2,[r0,#RECT_DST_TOP]  @ get y1
         ldr r3,[r0,#RECT_DST_RIGHT] @ get x2
         ldr r4,[r0,#RECT_DST_BOT]  @ get y2
         ldr r10,[r0,#BM_DST_PTR]
         ldr r11,[r0,#BM_DST_PITCH]
         ldr r14,[r0,#DST_COLOR]
         ldr r9,[r0,#BM_DST_BPP]
@ check that the lines are within the bounds of the destination bitmap
         ldr r5,[r0,#BM_DST_WIDTH]
         ldr r6,[r0,#BM_DST_HEIGHT]
         cmp r9,#16
         blt BBGLineExit    @ only valid for 16/24/32bpp
         cmp r5,r1
         bls BBGLineExit
         cmp r5,r3
         bls BBGLineExit
         cmp r6,r2
         bls BBGLineExit
         cmp r6,r4
         bls BBGLineExit
         cmp r2,r4    @ y1 > y2?
         movgt r5,r1    @ need to swap p1 with p2
         movgt r1,r3    @ to keep the slope positive
         movgt r3,r5
         movgt r5,r2
         movgt r2,r4
         movgt r4,r5
         mla  r12,r2,r11,r10   @ point to starting pixel
         ldrb r10,[r0,#DST_TRANS]   @ translucency
         cmp  r9,#32    @ 32bpp destination?
         beq  BBGLine32
         add  r12,r12,r1,LSL #1    @ r12 points to pixel
         subs r5,r3,r1    @ dx = x2-x1
         rsbmi r5,r5,#0    @ if dx<0 dx = -dx
         movmi r6,#-2    @ xinc = -2
         movpl r6,#2    @ xinc = 2
         b    BBGLineInit
BBGLine32:   
         add  r12,r12,r1,LSL #2    @ r12 points to pixel
         subs r5,r3,r1    @ dx = x2-x1
         rsbmi r5,r5,#0    @ if dx<0 dx = -dx
         movmi r6,#-4    @ xinc = -4
         movpl r6,#4    @ xinc = 4
BBGLineInit:   
         subs r7,r4,r2    @ dy = y2-y1
         rsbmi r7,r7,#0    @ if dy<0 dy = -dy
         rsbmi r8,r11,#0    @ yinc = -iPitch
         movpl r8,r11    @ yinc = +iPitch
@ there are 5 possible line draw cases
         cmp  r7,#0        @ dy == 0 horizontal line?
         bne  BBGLineType0
@ horizontal line
BBGLineHLoop0:   
         cmp  r9,#32        @ 32bpp?
         beq  BBGLineHLoop32
         cmp  r10,#255      @ opaque?
         bne  BBGLineHLoop2
BBGLineHLoop1:   
         strh r14,[r12],r6   @ draw the current pixel, x += xinc
         subs r5,r5,#1       @ dx--@
         bne  BBGLineHLoop1
         b    BBGLineExit    @ done, leave
@ 16bpp translucent
BBGLineHLoop2:   
         mov   r10,r10,LSR #3    @ convert trans to 0-31
         mov   r9,#0x7e00000   @ create mask
         orr   r9,r9,#0xf800
         orr   r9,r9,#0x1f
 @ prepare the pixel color we're storing
         mov   r14,r14,LSL #16      @ make sure top half 0
         orr   r14,r14,r14,LSR #16
         and   r14,r14,r9       @ masked
         mul   r4,r14,r10   @ color * translucency
         rsb   r10,r10,#32  @ invert translucency for destination
         mov   r14,r4       @ keep color in r14
BBGLineHLoop2_0:   
         ldrh  r3,[r12]     @ get dest pixels
         orr   r3,r3,r3,LSL #16 @ double it
         and   r3,r3,r9     @ mask it
         mla   r4,r3,r10,r14    @ multiply and combine
         and   r4,r9,r4,LSR #5  @ shift and mask
         orr   r4,r4,r4,LSR #16 @ merge G + RB
         strh  r4,[r12],r6   @ draw the current pixel, x += xinc
         subs  r5,r5,#1       @ dx--@
         bne   BBGLineHLoop2_0
         b     BBGLineExit    @ done, leave
BBGLineHLoop32:   
@         cmp  r10,#255   @ translucent?
@         bne  BBGLineHLoop32T
BBGLineHLoop32_0:   
         str  r14,[r12],r6   @ draw the current pixel, x += xinc
         subs r5,r5,#1       @ dx--@
         bne  BBGLineHLoop32_0
         b    BBGLineExit    @ done, leave
@ 32bpp translucent
BBGLineHLoop32T:   
         mov   r10,r14,LSR #24 @ get alpha
         mov   r9,#0xff0000   @ create R+B mask
         orr   r9,r9,#0xff
 @ prepare the pixel color we're storing
         and   r1,r14,r9    @ masked
         and   r14,r14,#0xff00  @ green
         mul   r4,r1,r10   @ color * trans (r+b)
         mul   r3,r14,r10   @ color * trans (g)
         rsb   r10,r10,#256 @ invert translucency for destination
         mov   r1,r4       @ keep r+b in r1
         mov   r14,r3      @ and g in r14
BBGLineHLoop32T_0:   
         ldr   r2,[r12]     @ dest pixel
         and   r3,r2,r9     @ mask r+b
         mla   r4,r3,r10,r1 @ r4 now has sum of r+b
         and   r2,r2,#0xff00 @ g
         mla   r3,r2,r10,r14    @ r3 now has sum of g
         and   r4,r9,r4,LSR #8  @ shift and mask r+b
         mov   r3,r3,LSR #8
         and   r3,r3,#0xff00  @ now g is ready
         orr   r4,r4,r3      @ pixel ready
@         rsb   r3,r10,#256   @ get original alpha back
@         orr   r4,r4,r3,LSL #24 @ include in pixel (ARGB)
         orr  r4,r4,#0xff000000      @ set alpha = ff
         str  r4,[r12],r6    @ draw the current pixel, x += xinc
         subs r5,r5,#1       @ dx--@
         bne  BBGLineHLoop32T_0
         b    BBGLineExit    @ done, leave
BBGLineType0:   
         cmp  r5,#0          @ dx == 0 vertical line?
         bne  BBGLineType1
@ vertical line
BBGLineVLoop0:   
         cmp  r9,#32         @ 32bpp?
         beq  BBGLineVLoop32
         cmp  r10,#255       @ translucent?
         bne  BBGLineVLoop2
BBGLineVLoop1:   
         strh r14,[r12],r8   @ draw the current pixel, y += yinc
         subs r7,r7,#1       @ dy--@
         bne  BBGLineVLoop1
         b    BBGLineExit    @ done, leave
@ 16bpp translucent
BBGLineVLoop2:   
         mov   r10,r10,LSR #3    @ convert trans to 0-31
         mov   r9,#0x7e00000   @ create mask
         orr   r9,r9,#0xf800
         orr   r9,r9,#0x1f
 @ prepare the pixel color we're storing
         mov   r14,r14,LSL #16      @ make sure top half 0
         orr   r14,r14,r14,LSR #16
         and   r14,r14,r9       @ masked
         mul   r4,r14,r10   @ color * translucency
         rsb   r10,r10,#32  @ invert translucency for destination
         mov   r14,r4       @ keep color in r14
BBGLineVLoop2_0:   
         ldrh  r3,[r12]     @ get dest pixels
         orr   r3,r3,r3,LSL #16 @ double it
         and   r3,r3,r9     @ mask it
         mla   r4,r3,r10,r14    @ multiply and combine
         and   r4,r9,r4,LSR #5  @ shift and mask
         orr   r4,r4,r4,LSR #16 @ merge G + RB
         strh  r4,[r12],r8   @ draw the current pixel, y += yinc
         subs  r7,r7,#1       @ dy--@
         bne   BBGLineVLoop2_0
         b     BBGLineExit    @ done, leave
BBGLineVLoop32:   
         cmp  r10,#255       @ translucent?
         bne  BBGLineVLoop32T
BBGLineVLoop32_0:   
         str  r14,[r12],r8   @ draw the current pixel, y += yinc
         subs r7,r7,#1       @ dy--@
         bne  BBGLineVLoop32_0
         b    BBGLineExit    @ done, leave
@ 32bpp translucent
BBGLineVLoop32T:   
         mov   r10,r14,LSR #24 @ get alpha
         mov   r9,#0xff0000   @ create R+B mask
         orr   r9,r9,#0xff
 @ prepare the pixel color we're storing
         and   r1,r14,r9    @ masked
         and   r14,r14,#0xff00  @ green
         mul   r4,r1,r10   @ color * trans (r+b)
         mul   r3,r14,r10   @ color * trans (g)
         rsb   r10,r10,#256 @ invert translucency for destination
         mov   r1,r4       @ keep r+b in r1
         mov   r14,r3      @ and g in r14
BBGLineVLoop32T_0:   
         ldr   r2,[r12]     @ dest pixel
         and   r3,r2,r9     @ mask r+b
         mla   r4,r3,r10,r1 @ r4 now has sum of r+b
         and   r2,r2,#0xff00 @ g
         mla   r3,r2,r10,r14    @ r3 now has sum of g
         and   r4,r9,r4,LSR #8  @ shift and mask r+b
         mov   r3,r3,LSR #8
         and   r3,r3,#0xff00  @ now g is ready
         orr   r4,r4,r3      @ pixel ready
@         rsb   r3,r10,#256   @ get original alpha back
@         orr   r4,r4,r3,LSL #24 @ include in pixel (ARGB)
         orr r4,r4,#0xff000000       @ set alpha = FF
         str  r4,[r12],r8   @ draw the current pixel, y += yinc
         subs r7,r7,#1       @ dy--@
         bne  BBGLineVLoop32T_0
         b    BBGLineExit    @ done, leave
BBGLineType1:   
         cmp  r5,r7        @ dy == dx special case for diagonal line
         bne  BBGLineType2
@ diagonal line
         add  r6,r6,r8        @ add xinc and yinc
BBGLineDLoop0:   
         cmp  r9,#32        @ 32bpp?
         beq  BBGLineDLoop32
         cmp  r10,#255        @ opaque?
         bne  BBGLineDLoop2
BBGLineDLoop1:   
         strh r14,[r12],r6    @ draw the current pixel, update x,y
         subs r7,r7,#1        @ dy--@
         bne  BBGLineDLoop1
         b    BBGLineExit    @ done
@ 16bpp translucent
BBGLineDLoop2:   
         mov   r10,r10,LSR #3    @ convert trans to 0-31
         mov   r9,#0x7e00000   @ create mask
         orr   r9,r9,#0xf800
         orr   r9,r9,#0x1f
 @ prepare the pixel color we're storing
         mov   r14,r14,LSL #16      @ make sure top half 0
         orr   r14,r14,r14,LSR #16
         and   r14,r14,r9       @ masked
         mul   r4,r14,r10   @ color * translucency
         rsb   r10,r10,#32  @ invert translucency for destination
         mov   r14,r4       @ keep color in r14
BBGLineDLoop2_0:   
         ldrh  r3,[r12]     @ get dest pixels
         orr   r3,r3,r3,LSL #16 @ double it
         and   r3,r3,r9     @ mask it
         mla   r4,r3,r10,r14    @ multiply and combine
         and   r4,r9,r4,LSR #5  @ shift and mask
         orr   r4,r4,r4,LSR #16 @ merge G + RB
         strh r4,[r12],r6     @ draw the current pixel, update x,y
         subs r7,r7,#1        @ dy--@
         bne  BBGLineDLoop2_0
         b    BBGLineExit    @ done
BBGLineDLoop32:   
         cmp  r10,#255       @ opaque?
         bne  BBGLineDLoop32T
BBGLineDLoop32_0:   
         str  r14,[r12],r6    @ draw the current pixel, update x,y
         subs r7,r7,#1        @ dy--@
         bne  BBGLineDLoop32_0
         b    BBGLineExit    @ done
BBGLineDLoop32T:   
         mov   r10,r14,LSR #24 @ get alpha
         mov   r9,#0xff0000   @ create R+B mask
         orr   r9,r9,#0xff
 @ prepare the pixel color we're storing
         and   r1,r14,r9    @ masked
         and   r14,r14,#0xff00  @ green
         mul   r4,r1,r10   @ color * trans (r+b)
         mul   r3,r14,r10   @ color * trans (g)
         rsb   r10,r10,#256 @ invert translucency for destination
         mov   r1,r4       @ keep r+b in r1
         mov   r14,r3      @ and g in r14
BBGLineDLoop32T_0:   
         ldr   r2,[r12]     @ dest pixel
         and   r3,r2,r9     @ mask r+b
         mla   r4,r3,r10,r1 @ r4 now has sum of r+b
         and   r2,r2,#0xff00 @ g
         mla   r3,r2,r10,r14    @ r3 now has sum of g
         and   r4,r9,r4,LSR #8  @ shift and mask r+b
         mov   r3,r3,LSR #8
         and   r3,r3,#0xff00  @ now g is ready
         orr   r4,r4,r3      @ pixel ready
@         rsb   r3,r10,#256   @ get original alpha back
@         orr   r4,r4,r3,LSL #24 @ include in pixel (ARGB)
         orr  r4,r4,#0xff000000      @ set alpha = ff
         str  r4,[r12],r6     @ draw the current pixel, update x,y
         subs r7,r7,#1        @ dy--@
         bne  BBGLineDLoop32T_0
         b    BBGLineExit    @ done
@ line is not horizontal, vertical or diagonal
BBGLineType2:   
         cmp  r7,r5            @ dy > dx y major line
         blt  BBGLineType3
@ y-major line
         mov  r11,r7,LSR #1    @ ErrorAcc = dy / 2
         mov  r0,r7            @ count along delta y
BBGLineLoop0:   
         cmp  r9,#32        @ 32bpp?
         beq  BBGLineLoop32
         cmp  r10,#255      @ opaque?
         bne  BBGLineLoop0T
BBGLineLoop0_0:   
         strh  r14,[r12],r8  @ draw the current pixel, y += yinc (always increment y)
         subs  r11,r11,r5    @ error -= dx@
         addmi r11,r11,r7    @ error += dy@
         addmi r12,r12,r6    @ x += xinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop0_0
         b     BBGLineExit
BBGLineLoop0T: @ 16bpp translucent line
         mov   r10,r10,LSR #3    @ convert trans to 0-31
         mov   r9,#0x7e00000   @ create mask
         orr   r9,r9,#0xf800
         orr   r9,r9,#0x1f
 @ prepare the pixel color we're storing
         mov   r14,r14,LSL #16      @ make sure top half 0
         orr   r14,r14,r14,LSR #16
         and   r14,r14,r9       @ masked
         mul   r4,r14,r10   @ color * translucency
         rsb   r10,r10,#32  @ invert translucency for destination
         mov   r14,r4       @ keep color in r14
BBGLineLoop0T_0:   
         ldrh  r3,[r12]     @ get dest pixels
         orr   r3,r3,r3,LSL #16 @ double it
         and   r3,r3,r9     @ mask it
         mla   r4,r3,r10,r14    @ multiply and combine
         and   r4,r9,r4,LSR #5  @ shift and mask
         orr   r4,r4,r4,LSR #16 @ merge G + RB
         strh  r4,[r12],r8   @ draw the current pixel, y += yinc (always increment y)
         subs  r11,r11,r5    @ error -= dx@
         addmi r11,r11,r7    @ error += dy@
         addmi r12,r12,r6    @ x += xinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop0T_0
         b     BBGLineExit
BBGLineLoop32:   
         cmp   r10,#255  @ opaque?
         bne   BBGLineLoop32T
BBGLineLoop32_0:   
         str   r14,[r12],r8  @ draw the current pixel, y += yinc (always increment y)
         subs  r11,r11,r5    @ error -= dx@
         addmi r11,r11,r7    @ error += dy@
         addmi r12,r12,r6    @ x += xinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop32_0
         b     BBGLineExit
BBGLineLoop32T: @ 32bpp translucent line
         mov   r10,r14,LSR #24 @ get alpha
         mov   r9,#0xff0000   @ create R+B mask
         orr   r9,r9,#0xff
 @ prepare the pixel color we're storing
         and   r1,r14,r9    @ masked
         and   r14,r14,#0xff00  @ green
         mul   r4,r1,r10   @ color * trans (r+b)
         mul   r3,r14,r10   @ color * trans (g)
         rsb   r10,r10,#256 @ invert translucency for destination
         mov   r1,r4       @ keep r+b in r1
         mov   r14,r3      @ and g in r14
BBGLineLoop32T_0:   
         ldr   r2,[r12]     @ dest pixel
         and   r3,r2,r9     @ mask r+b
         mla   r4,r3,r10,r1 @ r4 now has sum of r+b
         and   r2,r2,#0xff00 @ g
         mla   r3,r2,r10,r14    @ r3 now has sum of g
         and   r4,r9,r4,LSR #8  @ shift and mask r+b
         mov   r3,r3,LSR #8
         and   r3,r3,#0xff00  @ now g is ready
         orr   r4,r4,r3      @ pixel ready
@         rsb   r3,r10,#256   @ get original alpha back
@         orr   r4,r4,r3,LSL #24 @ include in pixel (ARGB)
         orr   r4,r4,#0xff000000     @ set alpha = ff
         str   r4,[r12],r8   @ draw the current pixel, y += yinc (always increment y)
         subs  r11,r11,r5    @ error -= dx@
         addmi r11,r11,r7    @ error += dy@
         addmi r12,r12,r6    @ x += xinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop32T_0
         b     BBGLineExit
BBGLineType3:   
@ x-major line
         mov   r11,r5,LSR #1    @ Error Acc = dx/2
         mov   r0,r5        @ count along delta x
BBGLineLoop1:   
         cmp  r9,#32        @ 32bpp?
         beq  BBGLineLoop32_1
         cmp  r10,#255  @ opaque?
         bne  BBGLineLoop1T
BBGLineLoop1_0:   
         strh  r14,[r12],r6  @ draw the current pixel, x += xinc (always increment x)
         subs  r11,r11,r7    @ error -= dy@
         addmi r11,r11,r5    @ error += dx@
         addmi r12,r12,r8    @ y += yinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop1_0
         b     BBGLineExit
BBGLineLoop1T: @ 16bpp translucent
         mov   r10,r10,LSR #3    @ convert trans to 0-31
         mov   r9,#0x7e00000   @ create mask
         orr   r9,r9,#0xf800
         orr   r9,r9,#0x1f
 @ prepare the pixel color we're storing
         mov   r14,r14,LSL #16      @ make sure top half 0
         orr   r14,r14,r14,LSR #16
         and   r14,r14,r9       @ masked
         mul   r4,r14,r10   @ color * translucency
         rsb   r10,r10,#32  @ invert translucency for destination
         mov   r14,r4       @ keep color in r14
BBGLineLoop1T_0:   
         ldrh  r3,[r12]     @ get dest pixels
         orr   r3,r3,r3,LSL #16 @ double it
         and   r3,r3,r9     @ mask it
         mla   r4,r3,r10,r14    @ multiply and combine
         and   r4,r9,r4,LSR #5  @ shift and mask
         orr   r4,r4,r4,LSR #16 @ merge G + RB
         strh  r4,[r12],r6   @ draw the current pixel, x += xinc (always increment x)
         subs  r11,r11,r7    @ error -= dy@
         addmi r11,r11,r5    @ error += dx@
         addmi r12,r12,r8    @ y += yinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop1T_0
         b     BBGLineExit
BBGLineLoop32_1:   
         cmp   r10,#255  @ opaque?
         bne   BBGLineLoop32T_1
BBGLineLoop32_2:   
         str   r14,[r12],r6  @ draw the current pixel, x += xinc (always increment x)
         subs  r11,r11,r7    @ error -= dy@
         addmi r11,r11,r5    @ error += dx@
         addmi r12,r12,r8    @ y += yinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop32_2
         b     BBGLineExit
BBGLineLoop32T_1:   
         mov   r10,r14,LSR #24 @ get alpha
         mov   r9,#0xff0000   @ create R+B mask
         orr   r9,r9,#0xff
 @ prepare the pixel color we're storing
         and   r1,r14,r9    @ masked
         and   r14,r14,#0xff00  @ green
         mul   r4,r1,r10   @ color * trans (r+b)
         mul   r3,r14,r10   @ color * trans (g)
         rsb   r10,r10,#256 @ invert translucency for destination
         mov   r1,r4       @ keep r+b in r1
         mov   r14,r3      @ and g in r14
BBGLineLoop32T_2:   
         ldr   r2,[r12]     @ dest pixel
         and   r3,r2,r9     @ mask r+b
         mla   r4,r3,r10,r1 @ r4 now has sum of r+b
         and   r2,r2,#0xff00 @ g
         mla   r3,r2,r10,r14    @ r3 now has sum of g
         and   r4,r9,r4,LSR #8  @ shift and mask r+b
         mov   r3,r3,LSR #8
         and   r3,r3,#0xff00  @ now g is ready
         orr   r4,r4,r3      @ pixel ready
@         rsb   r3,r10,#256   @ get original alpha back
@         orr   r4,r4,r3,LSL #24 @ include in pixel (ARGB)
         orr   r4,r4,#0xff000000     @ set alpha = ff
         str   r4,[r12],r6   @ draw the current pixel, x += xinc (always increment x)
         subs  r11,r11,r7    @ error -= dy@
         addmi r11,r11,r5    @ error += dx@
         addmi r12,r12,r8    @ y += yinc
         subs  r0,r0,#1        @ count--
         bne   BBGLineLoop32T_2
         b     BBGLineExit
BBGLineExit:   
         ldmia sp!,{r4-r12,pc}
@
@ Switch one color for another
@ call from C as BBGColorSwap(BBGFX_DC *pDC)@
@
BBGBlitColorSwap:   
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r1,r1,r4,LSL #1    @ now r1 points to source bitmap
         sub  r6,r6,r3            @ get height to draw
         add  r6,r6,#1
         sub  r7,r7,r4            @ get width to draw
         add  r7,r7,#1
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r10,r8,r4
         add  r3,r3,r10
         add  r3,r3,r9,LSL #1        @ pointer to destination
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r12,[r0,#BM_DST_WIDTH]
         ldrh r14,[r0,#DST_COLOR]        @ transparent color
         ldrh r10,[r0,#DST_COLOR2]        @ color to swap
         ldrh r5,[r0,#DST_NEWCOLOR]        @ new color
@ see if we're out of bounds, and trim to fit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGSwapExit
         cmp  r9,r12        @ starts past right edge?
         bge  BBGSwapExit
         sub  r11,r11,r8        @ see if size is too big
         cmp  r6,r11        @ goes past bottom?
         movgt r6,r11        @ only draw what can fit
         sub  r11,r12,r9
         cmp  r7,r11        @ goes past right edge?
         movgt r7,r11
@ adjust source and destination pitch to move us to the next line
         sub  r2,r2,r7,LSL #1    @ now R2 has delta src pitch
         sub  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
         add  r4,r4,#2        @ make up for pre-dec
BBGSwaploop0: @ top of loop
         mov  r11,r7        @ get width in r10
         sub  r3,r3,#2        @ dec dest to speed things up
BBGSwapLoop1:   
         ldrh r9,[r1],#2    @ source pixel
         add  r3,r3,#2        @ advance dst ptr
         cmp  r9,r10        @ change color?
         moveq r9,r5        @ do it
         cmp  r9,r14        @ transparent
         strneh r9,[r3]
         subs r11,r11,#1    @ decrement h count
         bne  BBGSwapLoop1    @ loop through line
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGSwaploop0
BBGSwapExit:   
         ldmia sp!,{r4-r12,pc}
@
@ Draw a 1bpp pattern with color substitution and translucency
@ call from C as BBGBlitPattern(BBGFX_DC *pDC)@
@
BBGBlitPattern:   
         ldr  r1,[r0,#BM_SRC_BPP]
         cmp  r1,#1        @ don't allow invalid bit depths
         movne pc,lr         
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         ldrb r14,[r0,#DST_TRANS]    @ translucency
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r1,r1,r4,LSR #3    @ now r1 points to source bitmap
         sub  r6,r6,r3            @ get height to draw
         adds r6,r6,#1
         ble  BBGBlitPatExit    @ nothing to draw
         sub  r7,r7,r4            @ get width to draw
         adds r7,r7,#1
         ble  BBGBlitPatExit
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         ldr  r11,[r0,#BM_DST_BPP]
         mul  r10,r8,r4
         add  r3,r3,r10
         cmp  r11,#32
         addne  r3,r3,r9,LSL #1        @ pointer to destination
         addeq  r3,r3,r9,LSL #2
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r12,[r0,#BM_DST_WIDTH]
         ldr  r5,[r0,#DST_COLOR]        @ color to draw
@ see if we're out of bounds, and trim to fit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGBlitPatExit
         cmp  r9,r12        @ starts past right edge?
         bge  BBGBlitPatExit
         sub  r11,r11,r8        @ see if size is too big
         cmp  r6,r11        @ goes past bottom?
         movgt r6,r11        @ only draw what can fit
         sub  r11,r12,r9
         cmp  r7,r11        @ goes past right edge?
         movgt r7,r11
@ adjust source and destination pitch to move us to the next line
         ldr  r11,[r0,#BM_DST_BPP]
         cmp  r11,#32
         subne  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
         addne  r4,r4,#2        @ make up for pre-dec
         subeq  r4,r4,r7,LSL #2    @ now R4 has delta dest pitch
         addeq  r4,r4,#4        @ make up for pre-dec
@ source pitch adjustment is tricky because of the 8-pixels per byte issue
         ldr  r9,[r0,#RECT_SRC_LEFT] @ starting X
         mov  r8,r9,LSR #3    @ get starting byte
         add  r10,r9,r7        @ add width to start
         sub  r10,r10,#1    @ width-1 to get ending pixel
         mov  r10,r10,LSR #3    @ get ending byte
         sub  r9,r10,r8
         add  r9,r9,#1        @ +1
         sub  r2,r2,r9        @ now R2 has delta src pitch
         mov  r7,r7,LSL #16    @ move h-count to upper 16-bits
         ldrb r8,[r0,#RECT_SRC_LEFT]    @ get starting x
         orr  r7,r7,r8        @ stick in bottom of R7
         cmp  r14,#255    @ opaque?
         beq  BBGPatOpaque
@ draw with translucency
         cmp  r11,#32    @ special case for 32bpp
         beq  BBGPatTrans32
@ separate the source color and multiply by the translucent value
@ R9 = multiplied R+G+B
         mov  r14,r14,LSR #3    @ make it a 5-bit number
         mov  r12,#0x7e00000    @ create constant for easy masking
         orr  r12,r12,#0xf800
         orr  r12,r12,#0x1f    @ now we have 07E0F81F
         orr  r5,r5,r5,LSL #16    @ double source color
         and  r5,r5,r12        @ mask G, R+B
         mul  r9,r5,r14        @ ready for combining
         rsb  r14,r14,#32    @ inverted translucency of destination pixel
         mov  r6,r6,LSL #16    @ move v-count to upper 16-bits
BBGPatTrans0:   
         pld  [r1,#32]        @ pre-cache source data
         mov  r10,r7,LSR #16    @ get width in r10
         sub  r3,r3,#2        @ dec dest to speed things up
         tst  r7,#7            @ starting on a byte boundary?
         beq  BBGPatTrans1    @ yes, proceed
         ldrb r8,[r1],#1    @ get partial byte
         mov  r8,r8,LSL #23
         and  r11,r7,#7        @ get shift amount
         mov  r8,r8,LSL r11    @ shift to get to current X
         rsb  r11,r11,#8    @ number of remaining pixels to process
         b    BBGPatTrans1a    @ enter loop
BBGPatTrans1:   
         ldrb r8,[r1],#1    @ get 8 source pixels
         pld  [r3,#32]        @ prepare destination pixels in cache
         movs r8,r8,LSL #23        @ anything to draw?
         mov  r11,#8        @ maximum pixels to draw in this byte
BBGPatTrans1a:   
         cmp  r11,r10        @ more than the pixels left on the line?
         movgt r11,r10        @ just do that many
         sub  r10,r10,r11    @ subtract from line count
         movgt r0,#0x7f000000
         bicgt r8,r8,r0,LSR r11    @ clear unused bits
BBGPatTrans2:   
         movs r8,r8,LSL #1    @ draw this pixel?
         beq  BBGPatTrans2a    @ no more pixels to draw
         add  r3,r3,#2
         sub  r11,r11,#1    @ decrement h count
         bpl  BBGPatTrans2  @ skip this pixel
         ldrh r5,[r3]        @ get destination pixel
@ prepare destination pixel
         orr  r5,r5,r5,LSL #16    @ double it
         and  r5,r5,r12        @ mask it
         mla  r0,r5,r14,r9    @ multiply and add source pixel
         and  r0,r12,r0,LSR #5    @ adjust for mult and mask again
         orr  r0,r0,r0,LSR #16    @ combine G + R + B
         strh r0,[r3]
         b    BBGPatTrans2    @ keep going
BBGPatTrans2a:   
         add  r3,r3,r11,LSL #1    @ add any blank pixels we skipped
         cmp  r10,#0        @ done with the line?
         bne  BBGPatTrans1    @ grab a new set of 8 pixels
BBGPatTrans3:   
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#0x10000    @ decrement v-count
         bne  BBGPatTrans0
         b    BBGBlitPatExit @ time to go
BBGPatTrans32:   
@ separate the source color and multiply by the translucent value
@ R9 = multiplied R+B, R5 = multiplied G
         orr  r4,r4,r2,LSL #16    @ put src+dest pitch in r4
         mov  r12,#0xff0000    @ create constant for easy masking
         orr  r12,r12,#0xff @ now we have 0x00ff00ff
         and  r8,r5,r12        @ mask R+B
         and  r5,r5,#0xff00    @ mask G
         mul  r9,r8,r14        @ R+B
         mul  r10,r5,r14
         mov  r5,r10        @ G
         rsb  r14,r14,#256    @ inverted translucency of destination pixel
         mov  r6,r6,LSL #16    @ move v-count to upper 16-bits
BBGPatTrans32_0:   
         pld  [r1,#32]        @ pre-cache source data
         mov  r10,r7,LSR #16    @ get width in r10
         sub  r3,r3,#4        @ dec dest to speed things up
         tst  r7,#7            @ starting on a byte boundary?
         beq  BBGPatTrans32_1    @ yes, proceed
         ldrb r8,[r1],#1    @ get partial byte
         mov  r8,r8,LSL #23
         and  r11,r7,#7        @ get shift amount
         mov  r8,r8,LSL r11    @ shift to get to current X
         rsb  r11,r11,#8    @ number of remaining pixels to process
         b    BBGPatTrans32_1a    @ enter loop
BBGPatTrans32_1:   
         ldrb r8,[r1],#1    @ get 8 source pixels
         pld  [r3,#32]        @ prepare destination pixels in cache
         movs r8,r8,LSL #23        @ anything to draw?
         mov  r11,#8        @ maximum pixels to draw in this byte
BBGPatTrans32_1a:   
         cmp  r11,r10        @ more than the pixels left on the line?
         movgt r11,r10        @ just do that many
         sub  r10,r10,r11    @ subtract from line count
         movgt r0,#0x7f000000
         bicgt r8,r8,r0,LSR r11    @ clear unused bits
BBGPatTrans32_2:   
         movs r8,r8,LSL #1    @ draw this pixel?
         beq  BBGPatTrans32_2a    @ no more pixels to draw
         add  r3,r3,#4
         sub  r11,r11,#1    @ decrement h count
         bpl  BBGPatTrans32_2  @ skip this pixel
         ldr  r0,[r3]        @ get destination pixel
@ prepare destination pixel
         stmfd sp!,{r1,r3}    @ need more reg vars
         and  r2,r0,r12        @ mask R+B
         and  r0,r0,#0xff00    @ mask G
         mla  r1,r2,r14,r9    @ multiply and add source pixel (R+B)
         and  r1,r12,r1,LSR #8    @ adjust for mult and mask again
         mla  r3,r0,r14,r5    @ multiply and add source pixel (G)
         and  r3,r3,#0xff0000    @ mask off multiplied green
         orr  r0,r1,r3,LSR #8    @ combine G + R + B
         ldmfd sp!,{r1,r3}    @ restore pointers
         str  r0,[r3]        @ store new pixel
         b    BBGPatTrans32_2    @ keep going
BBGPatTrans32_2a:   
         add  r3,r3,r11,LSL #2    @ add any blank pixels we skipped
         cmp  r10,#0        @ done with the line?
         bne  BBGPatTrans32_1    @ grab a new set of 8 pixels
BBGPatTrans32_3:   
         add  r1,r1,r4,LSR #16        @ move to next source line
         mov  r4,r4,ROR #16
         add  r3,r3,r4,LSR #16        @ move to next dest line
         mov  r4,r4,ROR #16
         subs r6,r6,#0x10000    @ decrement v-count
         bne  BBGPatTrans32_0
         b    BBGBlitPatExit @ time to go
@ draw pixels opaque
BBGPatOpaque:   
         ldr  r9,[r0,#BM_DST_BPP]
         cmp  r9,#32
         beq  BBGPatLoop32
BBGPatloop0: @ top of loop
         pld  [r1,#32]        @ pre-cache source data
         mov  r11,r7,LSR #16    @ get width in r10
         sub  r3,r3,#2        @ dec dest to speed things up
         ands r12,r7,#7        @ starting on a byte boundary?
         beq  BBGPatLoop1
         ldrb r9,[r1],#1    @ get partial byte
         mov  r9,r9,LSL r12    @ shift to get to current X
         mov  r9,r9,LSL #23
         rsb  r12,r12,#8    @ number of remaining pixels
         b    BBGPatLoop2    @ enter loop
BBGPatLoop1:   
         ldrb r9,[r1],#1    @ get 8 source pixels
         pld  [r3]            @ preload destination pixels in cache
         mov  r12,#8
         movs r9,r9,LSL #23        @ anything to draw?
         beq  BBGPatLoopSkip8
BBGPatLoop2:   
         add  r3,r3,#2        @ advance dst ptr
         movs  r9,r9,LSL #1        @ draw this pixel?
         strmih r5,[r3]
         subs r11,r11,#1    @ decrement h count
         beq  BBGPatLoop3    @ line is finished
         subs r12,r12,#1    @ any bits left?
         bne  BBGPatLoop2
         b    BBGPatLoop1    @ grab a new set of 8 pixels
@ empty source byte, skip up to 8 pixels
BBGPatLoopSkip8:   
         mov  r8,r11    @ get h count
         cmp  r8,#8        @ do we have at least 8 pixels to work with?
         movgt r8,#8    @ set max at 8
         add  r3,r3,r8,LSL #1    @ advance dest ptr
         subs  r11,r11,r8    @ decrement h count
         beq  BBGPatLoop3    @ line is finished
         b    BBGPatLoop1    @ grab 8 more pixels
BBGPatLoop3:   
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGPatloop0
BBGBlitPatExit:   
         ldmia sp!,{r4-r12,pc}
BBGPatLoop32: @ top of loop
         pld  [r1,#32]        @ pre-cache source data
         mov  r11,r7,LSR #16    @ get width in r10
         sub  r3,r3,#4        @ dec dest to speed things up
         ands r12,r7,#7        @ starting on a byte boundary?
         beq  BBGPatLoop32_1
         ldrb r9,[r1],#1    @ get partial byte
         mov  r9,r9,LSL r12    @ shift to get to current X
         mov  r9,r9,LSL #23
         rsb  r12,r12,#8    @ number of remaining pixels
         b    BBGPatLoop32_2    @ enter loop
BBGPatLoop32_1:   
         ldrb r9,[r1],#1    @ get 8 source pixels
         mov  r12,#8
         movs r9,r9,LSL #23        @ anything to draw?
         beq  BBGPatLoop32_Skip8
BBGPatLoop32_2:   
         add  r3,r3,#4
         movs  r9,r9,LSL #1        @ draw this pixel?
         strmi r5,[r3]
         subs r11,r11,#1    @ decrement h count
         beq  BBGPatLoop32_3    @ line is finished
         subs r12,r12,#1    @ any bits left?
         bne  BBGPatLoop32_2
         b    BBGPatLoop32_1    @ grab a new set of 8 pixels
@ empty source byte, skip up to 8 pixels
BBGPatLoop32_Skip8:   
         mov  r8,r11    @ get h count
         cmp  r8,#8        @ do we have at least 8 pixels to work with?
         movgt r8,#8    @ set max at 8
         add  r3,r3,r8,LSL #2    @ advance dest ptr
         subs  r11,r11,r8    @ decrement h count
         beq  BBGPatLoop32_3    @ line is finished
         b    BBGPatLoop32_1    @ grab 8 more pixels
BBGPatLoop32_3:   
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGPatLoop32
         b    BBGBlitPatExit
  .ltorg    
  .ltorg    
@
@ Draw a 8bpp character (alpha) bitmap to a 16 or 32bpp destination
@ call from C as BBGBlitChar(BBGFX_DC *pDC)@
@
BBGBlitChar:   
         ldr  r1,[r0,#BM_SRC_BPP]
         cmp  r1,#8        @ don't allow invalid bit depths
         movne pc,lr         
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r1,r1,r4            @ now r1 points to source bitmap
         sub  r6,r6,r3            @ get height to draw
         adds r6,r6,#1
         ble  BBGBlitCharExit    @ nothing to draw
         sub  r7,r7,r4            @ get width to draw
         adds r7,r7,#1
         ble  BBGBlitCharExit
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         ldr  r12,[r0,#BM_DST_BPP]
         mul  r10,r8,r4
         add  r3,r3,r10
         cmp  r12,#32
         addne  r3,r3,r9,LSL #1        @ pointer to destination
         addeq  r3,r3,r9,LSL #2
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r12,[r0,#BM_DST_WIDTH]
@ see if we're out of bounds, and trim to fit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGBlitCharExit
         cmp  r9,r12        @ starts past right edge?
         bge  BBGBlitCharExit
         sub  r11,r11,r8        @ see if size is too big
         cmp  r6,r11        @ goes past bottom?
         movgt r6,r11        @ only draw what can fit
         sub  r11,r12,r9
         cmp  r7,r11        @ goes past right edge?
         movgt r7,r11
@ adjust source and destination pitch to move us to the next line
         ldr  r11,[r0,#BM_DST_BPP]
         cmp  r11,#32
         subne  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
         subeq  r4,r4,r7,LSL #2
@ source pitch adjustment
         mov  r7,r7,LSL #16    @ move h-count to upper 16-bits
         ldrh r8,[r0,#RECT_SRC_LEFT]    @ get starting x
         orr  r7,r7,r8        @ stick in bottom of R7
@ draw with per-pixel translucency
@ separate the source color and multiply by the translucent value
@ R9 = multiplied R+G+B
         ldr  r5,[r0,#DST_COLOR]
         cmp  r11,#32        @ special case for 32bpp destination
         beq  BBGBlitChar32
         cmp  r11,#24
         beq  BBGBlitChar24     @ different behavior for 24bpp destination
         mov  r12,#0x7e00000    @ create constant for easy masking
         orr  r12,r12,#0xf800
         orr  r12,r12,#0x1f    @ now we have 07E0F81F
         orr  r5,r5,r5,LSL #16    @ double source color
         and  r5,r5,r12        @ mask G, R+B
         mov  r6,r6,LSL #16    @ move v-count to upper 16-bits
BBGCharTrans0:   
         pld  [r1,#32]        @ pre-cache source data
         mov  r10,r7,LSR #16    @ get width in r10
BBGCharTrans1:   
         ldrb r8,[r1],#1    @ get source alpha
         ldrh r14,[r3]        @ get destination pixel
         subs r10,r10,#1    @ decrement h count
         mov  r8,r8,LSR #3    @ shift down to 0-31 range
         mul  r9,r5,r8        @ ready for combining
         rsb  r8,r8,#32        @ inverted translucency of destination pixel
@ prepare destination pixel
         orr  r14,r14,r14,LSL #16    @ double it
         and  r14,r14,r12        @ mask it
         mla  r0,r8,r14,r9    @ multiply and add source pixel
         and  r0,r12,r0,LSR #5    @ adjust for mult and mask again
         orr  r0,r0,r0,LSR #16    @ combine G + R + B
         strh r0,[r3],#2
         bne  BBGCharTrans1    @ grab a new set of 8 pixels
BBGCharTrans3:   
         sub  r1,r1,r7,LSR #16    @ move back to start of line
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#0x10000    @ decrement v-count
         bne  BBGCharTrans0
BBGBlitCharExit:   
         ldmia sp!,{r4-r12,pc}
BBGBlitChar24:   
@ point r11 to destination alpha
         ldr  r8,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r12,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
     mul  r11,r8,r12
     ldr  r12,[r0,#BM_DST_PTR]
     add  r11,r11,r12
     mov  r8,r8,LSR #1         @ pitch/2 for alpha plane
     ldr  r12,[r0,#RECT_DST_TOP]
     mul  r10,r12,r8        @ offset to start of dest
     add  r11,r11,r10
     ldr  r12,[r0,#RECT_DST_LEFT]
     add  r11,r11,r12        @ now r11 points to dest alpha
         and  r5,r5,#0xffffff    @ remove any alpha value
BBGBlitChar24_0:   
         pld  [r1,#32]        @ pre-cache source data
         mov  r10,r7,LSR #16    @ get width in r10
BBGBlitChar24_1:   
         ldrb r8,[r1],#1    @ get source alpha
         add  r11,r11,#1    @ inc dest alpha
         add  r3,r3,#2        @ inc dest color
         orrs r8,r8,r8        @ 0?
         strneb r8,[r11,#-1]    @ or store if != 0
         strneh r5,[r3, #-2]    @ store color
         subs r10,r10,#1    @ decrement h count
         bne  BBGBlitChar24_1
         subs r6,r6,#1        @ decrement v-count
         sub  r1,r1,r7,LSR #16    @ move back to start of line
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         add  r11,r11,r4,LSR #1    @ update alpha pointer
         bne  BBGBlitChar24_0
         b    BBGBlitCharExit
BBGBlitChar32:   
         and  r5,r5,#0xffffff    @ remove any alpha value
BBGBlitChar32_0:   
         pld  [r1,#32]        @ pre-cache source data
         mov  r10,r7,LSR #16    @ get width in r10
BBGBlitChar32_1:   
         ldrb r8,[r1],#1    @ get source alpha
         subs r10,r10,#1    @ decrement h count
         orr  r8,r5,r8,LSL #24    @ add character alpha to color
         str  r8,[r3],#4
         bne  BBGBlitChar32_1
         subs r6,r6,#1        @ decrement v-count
         sub  r1,r1,r7,LSR #16    @ move back to start of line
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         bne  BBGBlitChar32_0
         b    BBGBlitCharExit
  .ltorg    
  .ltorg    
@
@ call from C as BBGRotate90(BBGFX_DC *pDC)@
@
@ Rotate an image 90 degrees clockwise
@ rotates from src to dest
@
BBGRotate90:   
         stmfd sp!,{r4-r12,lr}
@ get source pointer
     ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
     ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
     ldr  r3,[r0,#BM_SRC_WIDTH]
     ldr  r4,[r0,#BM_SRC_HEIGHT]    @ check that line is within bounds
     ldr  r5,[r0,#BM_DST_PTR]    @ bitmap pointer
     ldr  r6,[r0,#BM_DST_PITCH]    @ pitch
bbgrot90_0:   
     add  r9,r5,r4,LSL #1        @ start in upper right corner of dest
     sub  r9,r9,#2
     mov  r11,r1            @ keep source pointer here
     mov  r12,r3                    @ width counter in R12
bbgrot90_1:   
     ldrh r10,[r11],#2
     subs r12,r12,#1
     strh r10,[r9]
     add  r9,r9,r6    @ down 1 line
     bne  bbgrot90_1
     add  r1,r1,r2            @ next source line
     subs r4,r4,#1            @ V count
     bne  bbgrot90_0
     ldr  r1,[r0,#BM_SRC_BPP]    @ akp
     cmp  r1,#16            @ alpha channel?
     beq  bbgrot90_exit
@ rotate the alpha channel
     ldr  r4,[r0,#BM_SRC_HEIGHT]
     ldr  r7,[r0,#BM_SRC_WIDTH]
     mul  r8,r7,r6
     add  r5,r5,r8          @ now R5 points to dest alpha
     mov  r6,r6,LSR #1        @ dest pitch / 2
     mov  r2,r2,LSR #1      @ src pitch / 2
bbgrot90_2:   
     add  r9,r5,r4           @ start in upper right corner of dest
     sub  r9,r9,#1
     mov  r11,r1            @ keep source pointer here
     mov  r12,r3            @ width counter in R12
bbgrot90_3:   
     ldrb r10,[r11],#1
     subs r12,r12,#1
     strb r10,[r9]
     add  r9,r9,r6    @ down 1 line
     bne  bbgrot90_3
     add  r1,r1,r2            @ next source line
     subs r4,r4,#1            @ V count
     bne  bbgrot90_2
bbgrot90_exit:   
     ldmia sp!,{r4-r12,pc}
@
@ call from C as BBGHFlip(BBGFX_DC *pDC)@
@
@ Flip (mirror) a bitmap horizontally
@ flips the source bitmap in-place
@
BBGHFlip:   
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r12,[r0,#BM_SRC_WIDTH]
         ldr  r11,[r0,#BM_SRC_HEIGHT]    @ check that line is within bounds
         ldr  r3,[r0,#BM_SRC_BPP]
         cmp  r3,#32        @ 32bpp?
         beq  hflip32
hflip0: mov r5,r12,LSR #1        @ h counter = width / 2
         sub  r6,r12,#1            @ width-1 = src offset
         mov  r6,r6,LSL #1       @ x2 to address shorts
         mov  r7,#0            @ dest offset
hflip1: ldrh r8,[r1,r6] @ pull source from right edge
         ldrh r9,[r1,r7]    @ swap with left edge pixel
         subs r5,r5,#1
         strh r8,[r1,r7]
         strh r9,[r1,r6]    @ swap them
         sub  r6,r6,#2        @ move back 1 pixel
         add  r7,r7,#2
         bne  hflip1        @ loop through this line
         add  r1,r1,r2        @ next src line
         subs r11,r11,#1    @ line counter
         bne  hflip0
         ldr  r3,[r0,#BM_SRC_BPP]
         cmp  r3,#24
         bne  hflipexit
@ flip the alpha channel
         ldr  r11,[r0,#BM_SRC_HEIGHT]    @ check that line is within bounds
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r12,[r0,#BM_SRC_WIDTH]
         mul  r3,r2,r11
         add  r1,r1,r3        @ now R1 points to start of alpha
         mov  r2,r2,LSR #1    @ half the pitch
hflip2: mov r5,r12,LSR #1    @ h counter = width/2
         sub  r6,r12,#1        @ width-1 = src offset
         mov  r7,#0            @ dest offset
hflip3: ldrb r8,[r1,r6] @ pull source from right edge
         ldrb r9,[r1,r7]    @ swap with left edge pixel
         subs r5,r5,#1        @ dec counter
         strb r8,[r1,r7]
         strb r9,[r1,r6]    @ swap them
         sub  r6,r6,#1        @ move back 1 pixel
         add  r7,r7,#1
         bne  hflip3
         add  r1,r1,r2        @ next src line
         subs r11,r11,#1    @ line counter
         bne  hflip2
         b    hflipexit
hflip32: mov r5,r12,LSR #1        @ h counter = width / 2
         sub  r6,r12,#1            @ width-1 = src offset
         mov  r6,r6,LSL #2     @ x4 to address longs
         mov  r7,#0            @ dest offset
hflip32_1: ldr r8,[r1,r6] @ pull source from right edge
         ldr  r9,[r1,r7]    @ swap with left edge pixel
         subs r5,r5,#1
         str  r8,[r1,r7]
         str  r9,[r1,r6]    @ swap them
         sub  r6,r6,#4      @ move back 1 pixel
         add  r7,r7,#4
         bne  hflip32_1     @ loop through this line
         add  r1,r1,r2      @ next src line
         subs r11,r11,#1    @ line counter
         bne  hflip32
         b    hflipexit
hflipexit:   
         ldmia sp!,{r4-r12,pc}
@
@ call from C as BBGVFlip(BBGFX_DC *pDC)@
@
@ Flip (mirror) a bitmap vertically
@ flips the source bitmap in-place
@
BBGVFlip:   
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r12,[r0,#BM_SRC_WIDTH]
         ldr  r11,[r0,#BM_SRC_HEIGHT]    @ check that line is within bounds
         ldr  r14,[r0,#BM_SRC_BPP]
         sub  r3,r11,#1        @ get height-1 (point to bottom line)
         mul  r4,r3,r2
         add  r4,r4,r1        @ now r4 points to bottom line
         mov  r11,r11,LSR #1    @ count = height/2
         cmp  r14,#32       @ 32bpp?
         beq  vflip32
vflip0: mov r5,r12 @ h counter = width
         mov  r7,#0            @ src/dest horizontal offset
vflip1: ldrh r8,[r1,r7] @ pull source from top line
         ldrh r9,[r4,r7]    @ swap with bottom line pixel
         subs r5,r5,#1
         strh r8,[r4,r7]
         strh r9,[r1,r7]    @ swap them
         add  r7,r7,#2        @ next horiz pixel
         bne  vflip1        @ loop through this line
         add  r1,r1,r2        @ next top line
         sub  r4,r4,r2        @ next bottom line
         subs r11,r11,#1    @ line counter
         bne  vflip0
         ldr  r3,[r0,#BM_SRC_BPP]
         cmp  r3,#24
         bne  vflipexit
@ flip the alpha channel
         ldr  r11,[r0,#BM_SRC_HEIGHT]    @ check that line is within bounds
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r12,[r0,#BM_SRC_WIDTH]
         mul  r3,r11,r2
         add  r1,r1,r3        @ now R1 points to start of alpha pixels
         mov  r2,r2,LSR #1    @ half the pitch
         sub  r3,r11,#1        @ get height-1 (point to bottom line)
         mul  r4,r3,r2
         add  r4,r4,r1        @ now r4 points to bottom line
         mov  r11,r11,LSR #1    @ count = height/2
vflip2: mov r5,r12 @ h counter = width
         mov  r7,#0            @ src/dest horizontal offset
vflip3: ldrb r8,[r1,r7] @ pull source from top line
         ldrb r9,[r4,r7]    @ swap with bottom line pixel
         subs r5,r5,#1
         strb r8,[r4,r7]
         strb r9,[r1,r7]    @ swap them
         add  r7,r7,#1        @ next horiz pixel
         bne  vflip3        @ loop through this line
         add  r1,r1,r2        @ next top line
         sub  r4,r4,r2        @ next bottom line
         subs r11,r11,#1    @ line counter
         bne  vflip2
         b    vflipexit
vflip32: mov r5,r12 @ h counter = width
         mov  r7,#0            @ src/dest horizontal offset
vflip32_1: ldr r8,[r1,r7] @ pull source from top line
         ldr  r9,[r4,r7]    @ swap with bottom line pixel
         subs r5,r5,#1
         str  r8,[r4,r7]
         str  r9,[r1,r7]    @ swap them
         add  r7,r7,#4      @ next horiz pixel
         bne  vflip32_1     @ loop through this line
         add  r1,r1,r2        @ next top line
         sub  r4,r4,r2        @ next bottom line
         subs r11,r11,#1    @ line counter
         bne  vflip32
         b    vflipexit
vflipexit:   
         ldmia sp!,{r4-r12,pc}
@
@ call from C as BBGBlitFast(BBGFX_DC *pDC)@
@
BBGBlitFast:   
         ldr  r1,[r0,#BM_SRC_BPP]
         ldr  r2,[r0,#BM_DST_BPP]
         cmp  r1,#16
         movlt pc,lr
         cmp  r2,#16
         movlt pc,lr
         cmp  r1,#32
         beq  BBGBlitFast32
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r1,r1,r4,LSL #1    @ now r1 points to source bitmap
         sub  r6,r6,r3            @ get height to draw
         add  r6,r6,#1
         sub  r7,r7,r4            @ get width to draw
         add  r7,r7,#1
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r5,r8,r4
         add  r3,r3,r5
         add  r3,r3,r9,LSL #1        @ pointer to destination
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r12,[r0,#BM_DST_WIDTH]
@ see if we're out of bounds, and trim to fit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGBlitFExit
         cmp  r9,r12        @ starts past right edge?
         bge  BBGBlitFExit
         sub  r5,r11,r8        @ see if size is too big
         cmp  r6,r5            @ goes past bottom?
         movgt r6,r5        @ only draw what can fit
         sub  r5,r12,r9
         cmp  r7,r5            @ goes past right edge?
         movgt r7,r5
@ adjust source and destination pitch to move us to the next line
         mov  r8,r2            @ save src pitch for cache preload
         sub  r2,r2,r7,LSL #1    @ now R2 has delta src pitch
         sub  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
         eor  r10,r1,r3     @ see if both src/dst are dword aligned
         tst  r10,#2          @ both dword-aligned or both not?
         bne  BBGBlitFast0    @ nope
         cmp  r7,#8           @ width is at least 8 pixels?
         bge  BBGBlitFast2    @ can go even faster
BBGBlitFast0: @ top of loop
         mov  r10,r7        @ get width in r10
         pld  [r1,r8]        @ pre cache next src line
BBGBlitFast1:   
         ldrh r9,[r1],#2    @ source pixel
         subs r10,r10,#1    @ decrement h count
         pld  [r1,#32]        @ use wasted cycle to preload next pixels
         strh r9,[r3],#2
         bne  BBGBlitFast1    @ loop through line
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGBlitFast0
         b    BBGBlitFExit
@ source and dest have same alignment and width is at least 8 pixels
BBGBlitFast2:   
         mov  r12,r7        @ get width in r10
         tst  r1,#2         @ dword aligned?
         ldrneh r9,[r1],#2  @ nope, treat first pixel special
         strneh r9,[r3],#2
         subne  r12,r12,#1
         mov  r11,r12,LSR #2 @ get width in r11 (/4)
         sub  r12,r12,r11,LSL #2    @ remaining count in r10
         pld  [r1,r8]        @ pre cache next src line
BBGBlitFast3:   
         ldmia r1!,{r9,r10}    @ 4 source pixels
         subs r11,r11,#1    @ decrement h count
         pld  [r1,#64]        @ use wasted cycle to preload next pixels
         stmia r3!,{r9,r10}
         bne  BBGBlitFast3    @ loop through line
@ see if any odd pixels to draw
         orrs r12,r12,r12
         beq  BBGBlitFast5   @ nothing to draw
BBGBlitFast4:   
         ldrh  r9,[r1],#2
         strh  r9,[r3],#2
         subs  r12,r12,#1
         bne   BBGBlitFast4
BBGBlitFast5:   
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGBlitFast2
BBGBlitFExit:   
         ldmia sp!,{r4-r12,pc}
BBGBlitFast32:   
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r1,r1,r4,LSL #2    @ now r1 points to source bitmap
         sub  r6,r6,r3            @ get height to draw
         add  r6,r6,#1
         sub  r7,r7,r4            @ get width to draw
         add  r7,r7,#1
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r5,r8,r4
         add  r3,r3,r5
         add  r3,r3,r9,LSL #2        @ pointer to destination
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r12,[r0,#BM_DST_WIDTH]
@ see if we're out of bounds, and trim to fit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGBlitFExit
         cmp  r9,r12        @ starts past right edge?
         bge  BBGBlitFExit
         sub  r5,r11,r8        @ see if size is too big
         cmp  r6,r5            @ goes past bottom?
         movgt r6,r5        @ only draw what can fit
         sub  r5,r12,r9
         cmp  r7,r5            @ goes past right edge?
         movgt r7,r5
@ adjust source and destination pitch to move us to the next line
         mov  r8,r2            @ save src pitch for cache preload
         sub  r2,r2,r7,LSL #2    @ now R2 has delta src pitch
         sub  r4,r4,r7,LSL #2    @ now R4 has delta dest pitch
BBGBlitFast32_0:   
         movs r5,r7,LSR #2        @ get width/4 in r11
         beq  BBGBlitFast32_2
BBGBlitFast32_1:   
         ldmia r1!,{r9-r12}    @ 4 source pixels
         subs r5,r5,#1        @ decrement h count
         pld  [r1,#64]        @ use wasted cycle to preload next pixels
         stmia r3!,{r9-r12}
         bne  BBGBlitFast32_1    @ loop through line
BBGBlitFast32_2:   
         ands r11,r7,#3     @ get remaining count r11
         beq  BBGBlitFast32_4
BBGBlitFast32_3:   
         ldr  r9,[r1],#4    @ source pixel
         subs r11,r11,#1    @ decrement h count
         str  r9,[r3],#4
         bne  BBGBlitFast32_3    @ loop through line
BBGBlitFast32_4:   
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGBlitFast32_0
         b    BBGBlitFExit
@
@ call from C as BBGBlitAlpha(BBGFX_DC *pDC)@
@ 2 code paths: adjust alpha / no-adjust alpha
@
BBGBlitAlpha:   
         ldr  r1,[r0,#BM_SRC_BPP]
         cmp  r1,#24        @ don't allow invalid bit depths
         movne pc,lr         
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         ldrb r8,[r0,#DST_TRANS]
         ldr  r9,[r0,#BM_SRC_HEIGHT]
         mov  r12,r1        @ get pointer to alpha channel in r12
         cmp  r8,#7            @ if less than 8, nothing to draw
         ble  BBGBlitAExit
         mul  r10,r9,r2    @ get offset to alpha channel
         add  r12,r12,r10    @ now we point to alpha channel         
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r12,r12,r5,LSR #1    @ alpha is half the pitch
         add  r1,r1,r4,LSL #1    @ now r1 points to source bitmap
         add  r12,r12,r4        @ now r12 points to source alpha
         sub  r6,r6,r3            @ get height to draw
         add  r6,r6,#1
         sub  r7,r7,r4            @ get width to draw
         add  r7,r7,#1
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r5,r8,r4
         add  r3,r3,r5
         add  r3,r3,r9,LSL #1        @ pointer to destination
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r14,[r0,#BM_DST_WIDTH]
@ see if we're out of bounds, and trim to fit
         orrs r8,r8,r8        @ negative Y?
         bmi  BBGBlitAExit    @ DEBUG - maybe we can adjust this
         orrs r9,r9,r9        @ negative X?
         bmi  BBGBlitAExit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGBlitAExit
         cmp  r9,r14        @ starts past right edge?
         bge  BBGBlitAExit
         sub  r5,r11,r8        @ see if size is too big
         cmp  r6,r5            @ goes past bottom?
         movgt r6,r5        @ only draw what can fit
         sub  r5,r14,r9
         cmp  r7,r5            @ goes past right edge?
         movgt r7,r5
@ adjust source and destination pitch to move us to the next line
         sub  r2,r2,r7,LSL #1    @ now R2 has delta src pitch
         sub  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
@ create mask used for alpha blending
         ldrb r8,[r0,#DST_TRANS]
         mov  r14,#0x7e00000    @ create constant for masking
         orr  r14,r14,#0xf800
         orr  r14,r14,#0x1f    @ 0x07E0F81F
         cmp  r8,#255        @ max opaqueness?
         movne r8,r8,LSR #3    @ adjust for 0-31 range
         bne  BBGBlitAlphaA0    @ blit with adjusted alpha
BBGBlitAlpha0: @ top of loop
         mov  r10,r7        @ get width in r10
         pld  [r1,r2]        @ pre cache next src line
BBGBlitAlpha1:   
         ldrb r11,[r12],#1    @ source alpha
         ldrh r9,[r1],#2    @ source pixel
         pld  [r1,r2]        @ use wasted cycle to preload next pixels
         cmp  r11,#0xff        @ fully opaque
         beq  BBGBlitAlpha5    @ store and go
         movs r11,r11,LSR #3    @ convert to 0-31 range
         beq  BBGBlitAlpha4 @ alpha == 0, skip it
         ldrh r0,[r3]        @ dest pixel
@ prepare the source pixel
         orr  r9,r9,r9,LSL #16    @ double it
         rsb  r5,r11,#32        @ inverted translucency for dest pixels
         and  r9,r9,r14        @ mask it
         mul  r11,r9,r11    @ ready
@ prepare the destination pixel
         orr  r0,r0,r0,LSL #16  @ double it
         subs r10,r10,#1    @ decrement h count - at least 4 instr away from bne for branch prediction
         and  r0,r0,r14        @ mask it
         mla  r9,r0,r5,r11    @ multiply and add to src
         and  r9,r14,r9,LSR #5    @ shift down and mask to get final pixel
         orr  r9,r9,r9,LSR #16    @ final pixel ready
         strh r9,[r3],#2
         bne  BBGBlitAlpha1    @ loop through line
BBGBlitAlpha2a:   
         subs r6,r6,#1        @ decrement vcount
         add  r1,r1,r2        @ move to next source line
         add  r12,r12,r2,LSR #1    @ next src alpha
         add  r3,r3,r4        @ move to next dest line
         bne  BBGBlitAlpha0
         b    BBGBlitAExit
BBGBlitAlpha4: @ alpha = 0, skip it
         subs r10,r10,#1    @ h count
         add  r3,r3,#2
         bne  BBGBlitAlpha1
         b    BBGBlitAlpha2a
BBGBlitAlpha5: @ fully opaque
         subs r10,r10,#1    @ decrement h count
         strh r9,[r3],#2
         bne  BBGBlitAlpha1
         b    BBGBlitAlpha2a
BBGBlitAExit:   
         ldmia sp!,{r4-r12,pc}
@ blit with adjusted alpha (slower)
BBGBlitAlphaA0: @ top of loop
         mov  r10,r7        @ get width in r10
         pld  [r1,r2]        @ pre cache next src line
BBGBlitAlphaA1:   
         ldrb r11,[r12],#1    @ source alpha
         ldrh r9,[r1],#2    @ source pixel
         mul  r11,r8,r11    @ multiplied by the new alpha adjustment
         pld  [r1,r2]        @ use wasted cycle to preload next pixels
         movs r11,r11,LSR #8    @ convert to 0-31 range
         addeq r3,r3,#2
         beq  BBGBlitAlphaA2 @ alpha == 0, skip it
         ldrh r0,[r3]        @ dest pixel
         rsb  r5,r11,#32        @ inverted translucency for dest pixels
@ prepare the source pixel
         orr  r9,r9,r9,LSL #16    @ double it
         and  r9,r9,r14        @ mask it
         mul  r11,r9,r11    @ ready
@ prepare the destination pixel
         orr  r0,r0,r0,LSL #16  @ double it
         and  r0,r0,r14        @ mask it
         mla  r9,r0,r5,r11    @ multiply and add to src
         and  r9,r14,r9,LSR #5    @ shift down and mask to get final pixel
         orr  r9,r9,r9,LSR #16    @ final pixel ready
BBGBlitAlphaA3:   
         strh r9,[r3],#2
BBGBlitAlphaA2:   
         subs r10,r10,#1    @ decrement h count
         bne  BBGBlitAlphaA1    @ loop through line
         add  r1,r1,r2        @ move to next source line
         add  r12,r12,r2,LSR #1    @ next src alpha
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGBlitAlphaA0
         b    BBGBlitAExit
@
@ call from C as BBGStretchAlpha(BBGFX_DC *pDC)@
@ 2 code paths: adjust alpha / no-adjust alpha
@
BBGStretchAlpha:   
         ldrb r1,[r0,#DST_TRANS]
         cmp  r1,#7            @ if less than 8, nothing to draw
         movle  pc,lr        @ leave
         ldr  r1,[r0,#BM_SRC_BPP]
         cmp  r1,#24        @ don't allow invalid bit depths
         movne pc,lr         
         stmfd sp!,{r4-r12,lr}
@ calculate the vertical and horizontal scale factors
         ldr  r1,[r0,#RECT_SRC_TOP]
         ldr  r2,[r0,#RECT_SRC_BOT]
         ldr  r3,[r0,#RECT_DST_TOP] 
         ldr  r4,[r0,#RECT_DST_BOT]
         sub  r2,r2,r1
         add  r2,r2,#1
         sub  r4,r4,r3
         add  r4,r4,#1
@ calc source height * 4096 / dest height
         mov  r5,r2,LSL #12
         mov  r3,r4
         bl   divide
         strh r5,[r0,#TEMP_SPACE]    @ keep quotient
@ calc width
         ldr  r1,[r0,#RECT_SRC_LEFT]
         ldr  r2,[r0,#RECT_SRC_RIGHT]
         ldr  r3,[r0,#RECT_DST_LEFT]
         ldr  r4,[r0,#RECT_DST_RIGHT]
         sub  r2,r2,r1
         add  r2,r2,#1
         sub  r4,r4,r3
         add  r4,r4,#1
@ calc source width * 4096 / dest width
         mov  r5,r2,LSL #12
         mov  r3,r4
         bl   divide
         strh r5,[r0,#TEMP_SPACE+2]    @ keep quotient
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         ldr  r9,[r0,#BM_SRC_HEIGHT]
         mov  r12,r1        @ get pointer to alpha channel in r12
         mul  r10,r9,r2    @ get offset to alpha channel
         add  r12,r12,r10    @ now we point to alpha channel         
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r12,r12,r5,LSR #1    @ alpha is half the pitch
         add  r1,r1,r4,LSL #1    @ now r1 points to source bitmap
         add  r12,r12,r4        @ now r12 points to source alpha
         ldr  r6,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r7,[r0,#RECT_DST_LEFT] @ start x
         ldr  r3,[r0,#RECT_DST_BOT]
         ldr  r4,[r0,#RECT_DST_RIGHT]
         sub  r6,r3,r6            @ get height to draw
         add  r6,r6,#1
         sub  r7,r4,r7            @ get width to draw
         add  r7,r7,#1
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r5,r8,r4
         add  r3,r3,r5
         add  r3,r3,r9,LSL #1        @ pointer to destination
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r14,[r0,#BM_DST_WIDTH]
@ see if we're out of bounds, and trim to fit
         orrs r8,r8,r8        @ negative Y?
         bmi  BBGStretchAExit    @ DEBUG - maybe we can adjust this
         orrs r9,r9,r9        @ negative X?
         bmi  BBGStretchAExit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGStretchAExit
         cmp  r9,r14        @ starts past right edge?
         bge  BBGStretchAExit
         sub  r5,r11,r8        @ see if size is too big
         cmp  r6,r5            @ goes past bottom?
         movgt r6,r5        @ only draw what can fit
         sub  r5,r14,r9
         cmp  r7,r5            @ goes past right edge?
         movgt r7,r5
@ adjust source and destination pitch to move us to the next line
@         sub  r2,r2,r7,LSL #1    @ now R2 has delta src pitch
         sub  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
@ create mask used for alpha blending
         ldrb r8,[r0,#DST_TRANS]
         mov  r14,#0x7e00000    @ create constant for masking
         orr  r14,r14,#0xf800
         orr  r14,r14,#0x1f    @ 0x07E0F81F
         cmp  r8,#255        @ max opaqueness?
         movne r8,r8,LSR #3    @ adjust for 0-31 range
         bne  BBGStretchAlphaA00    @ blit with adjusted alpha
         mov  r5,#0            @ v accumulator
         ldr  r8,[r0,#TEMP_SPACE]    @ height increment (low word), width increment (high word)
BBGStretchAlpha0: @ top of loop
         orr  r7,r7,r7,LSL #16        @ get width counter in top of r7
         mov  r10,#0        @ h accumulator
         pld  [r1,r2]        @ pre cache next src line
BBGStretchAlpha1:   
         mov  r11,r10,LSR #11    @ to address shorts
         bic  r11,r11,#1    @ clear odd bit
         ldrh r9,[r1,r11]    @ source pixel
         ldrb r11,[r12,r11,LSR #1]    @ source alpha
         pld  [r1,r2]        @ use wasted cycle to preload next pixels
         cmp  r11,#0xff        @ fully opaque
         beq  BBGStretchAlpha3    @ store and go
         movs r11,r11,LSR #3    @ convert to 0-31 range
         addeq r3,r3,#2
         beq  BBGStretchAlpha2 @ alpha == 0, skip it
         ldrh r0,[r3]        @ dest pixel
@ prepare the source pixel
         orr  r9,r9,r9,LSL #16    @ double it
         and  r9,r9,r14        @ mask it
         mul  r9,r11,r9    @ ready
         rsb  r11,r11,#32        @ inverted translucency for dest pixels
@ prepare the destination pixel
         orr  r0,r0,r0,LSL #16  @ double it
         and  r0,r0,r14        @ mask it
         mla  r9,r0,r11,r9    @ multiply and add to src
         and  r9,r14,r9,LSR #5    @ shift down and mask to get final pixel
         orr  r9,r9,r9,LSR #16    @ final pixel ready
BBGStretchAlpha3:   
         strh r9,[r3],#2
BBGStretchAlpha2:   
         add  r10,r10,r8,LSR #16    @ add h increment to h sum
         sub  r7,r7,#0x10000    @ decrement h count
         cmp  r7,#0x10000    @ finished?
         bgt  BBGStretchAlpha1    @ keep going
         mov  r8,r8,ROR #16    @ rotate v increment into position
         add  r5,r5,r8,LSR #16    @ add v increment to v sum
         mov  r8,r8,ROR #16    @ rotate v increment back
BBGStretchAlpha4:   
         tst  r5,#0xff000    @ any whole parts?  Need to increment Y         
         addne  r1,r1,r2        @ move to next source line
         addne  r12,r12,r2,LSR #1    @ next src alpha
         subne r5,r5,#0x1000
         bne  BBGStretchAlpha4    @ keep going (in case of image shrink) until none left
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGStretchAlpha0
BBGStretchAExit:   
         ldmia sp!,{r4-r12,pc}
BBGStretchAlphaA00:   
@ blit with adjusted alpha (slower)
         orr  r4,r4,r2,LSL #16    @ combine src/dest pitch to free up R2
         mov  r2,r8            @ get adjusted translucency value in R2
         mov  r5,#0            @ v accumulator
         ldr  r8,[r0,#TEMP_SPACE]    @ height increment (low word), width increment (high word)
BBGStretchAlphaA0: @ top of loop
         orr  r7,r7,r7,LSL #16        @ get width counter in top of r7
         mov  r10,#0        @ h accumulator
BBGStretchAlphaA1:   
         mov  r11,r10,LSR #11    @ to address shorts
         bic  r11,r11,#1    @ clear odd bit
         ldrh r9,[r1,r11]    @ source pixel
         ldrb r11,[r12,r11,LSR #1]    @ source alpha
         mul  r11,r2,r11    @ original alpha * adjustment
         movs r11,r11,LSR #8    @ convert to 0-31 range
         addeq r3,r3,#2
         beq  BBGStretchAlphaA2 @ alpha == 0, skip it
         ldrh r0,[r3]        @ dest pixel
@ prepare the source pixel
         orr  r9,r9,r9,LSL #16    @ double it
         and  r9,r9,r14        @ mask it
         mul  r9,r11,r9    @ ready
         rsb  r11,r11,#32        @ inverted translucency for dest pixels
@ prepare the destination pixel
         orr  r0,r0,r0,LSL #16  @ double it
         and  r0,r0,r14        @ mask it
         mla  r9,r0,r11,r9    @ multiply and add to src
         and  r9,r14,r9,LSR #5    @ shift down and mask to get final pixel
         orr  r9,r9,r9,LSR #16    @ final pixel ready
BBGStretchAlphaA3:   
         strh r9,[r3],#2
BBGStretchAlphaA2:   
         add  r10,r10,r8,LSR #16    @ add h increment to h sum
         sub  r7,r7,#0x10000    @ decrement h count
         cmp  r7,#0x10000    @ finished?
         bgt  BBGStretchAlphaA1    @ keep going
         mov  r8,r8,ROR #16    @ rotate v increment into position
         add  r5,r5,r8,LSR #16    @ add v increment to v sum
         mov  r8,r8,ROR #16    @ rotate v increment back
BBGStretchAlphaA4:   
         tst  r5,#0xff000    @ any whole parts?  Need to increment Y         
         addne  r1,r1,r4,LSR #16        @ move to next source line
         addne  r12,r12,r4,LSR #17    @ next src alpha
         subne r5,r5,#0x1000
         bne  BBGStretchAlphaA4    @ keep going (in case of image shrink) until none left
         mov  r4,r4,ROR #16    @ rotate it around to get dest pitch
         add  r3,r3,r4,LSR #16        @ move to next dest line
         mov  r4,r4,ROR #16    @ restore it to original position
         subs r6,r6,#1        @ decrement vcount
         bne  BBGStretchAlphaA0
         b    BBGStretchAExit
@
@ call from C as BBGBlitTransparent(BBGFX_DC *pDC)@
@
BBGBlitTransparent:   
         ldr  r1,[r0,#BM_SRC_BPP]
         cmp  r1,#16        @ don't allow invalid bit depths
         movne pc,lr         
         stmfd sp!,{r4-r12,lr}
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldr  r6,[r0,#RECT_SRC_BOT]
         ldr  r7,[r0,#RECT_SRC_RIGHT]
         ldrb r10,[r0,#DST_TRANS]    @ translucency
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r1,r1,r4,LSL #1    @ now r1 points to source bitmap
         sub  r6,r6,r3            @ get height to draw
         add  r6,r6,#1
         sub  r7,r7,r4            @ get width to draw
         add  r7,r7,#1
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r5,r8,r4
         add  r3,r3,r5
         add  r3,r3,r9,LSL #1        @ pointer to destination
         ldr  r11,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r12,[r0,#BM_DST_WIDTH]
         ldr  r14,[r0,#DST_COLOR]        @ transparent color
@ see if we're out of bounds, and trim to fit
         cmp  r8,r11        @ starts past bottom?
         bge  BBGBlitExit
         cmp  r9,r12        @ starts past right edge?
         bge  BBGBlitExit
         cmp  r10,#5        @ less than 5 means no contribution
         blt  BBGBlitExit
         sub  r5,r11,r8        @ see if size is too big
         cmp  r6,r5            @ goes past bottom?
         movgt r6,r5        @ only draw what can fit
         sub  r5,r12,r9
         cmp  r7,r5            @ goes past right edge?
         movgt r7,r5
         cmp  r7,#0            @ nothing to draw?
         beq  BBGBlitExit
         cmp  r6,#0
         beq  BBGBlitExit
@ adjust source and destination pitch to move us to the next line
         mov  r8,r2            @ save src pitch for cache preload
         sub  r2,r2,r7,LSL #1    @ now R2 has delta src pitch
         sub  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
         add  r4,r4,#2        @ make up for pre-dec
         cmp  r10,#255        @ opaque?
         bne  BBGBlitTrans    @ nope, do translucent
BBGBlitloop0: @ top of loop
         mov  r10,r7        @ get width in r10
         sub  r3,r3,#2        @ dec dest to speed things up
@         pld  [r1,r8]        @ pre cache next src line
BBGBlitLoop1:   
         ldrh r9,[r1],#2    @ source pixel
         pld  [r1,r2]        @ use wasted cycle to preload next pixels
         add  r3,r3,#2        @ advance dst ptr
         cmp  r9,r14        @ transparent?
         strneh r9,[r3]
         subs r10,r10,#1    @ decrement h count
         bne  BBGBlitLoop1    @ loop through line
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGBlitloop0
         b    BBGBlitExit
BBGBlitTrans:   
         mov  r10,r10,LSR #3    @ convert to 0-31 range
         rsb  r5,r10,#32        @ inverted translucency for dest pixels
         mov  r12,#0x7e00000    @ create constant for masking
         orr  r12,r12,#0xf800
         orr  r12,r12,#0x1f    @ 0x07E0F81F
BBGBlitTrans0:   
         mov  r8,r7            @ get width in r8
         sub  r3,r3,#2        @ dec dest to speed things up
BBGBlitTrans1:   
         ldrh r9,[r1],#2    @ source pixel
         pld  [r1,r2]
         add  r3,r3,#2        @ advance dst ptr
         cmp  r9,r14        @ transparent?
         beq  BBGBlitTrans2    @ skip the translucency stuff
         ldrh r0,[r3]        @ dest pixel
@ prepare the source pixel
         orr  r9,r9,r9,LSL #16    @ double it
         and  r9,r9,r12        @ mask it
         mul  r11,r9,r10    @ ready
@ prepare the destination pixel
         orr  r0,r0,r0,LSL #16  @ double it
         and  r0,r0,r12        @ mask it
         mla  r9,r0,r5,r11    @ multiply and add to src
         and  r9,r12,r9,LSR #5    @ shift down and mask to get final pixel
         orr  r9,r9,r9,LSR #16    @ final pixel ready
         strh r9,[r3]        @ store :)
BBGBlitTrans2:   
         subs r8,r8,#1        @ decrement h count
         bne  BBGBlitTrans1    @ loop through line
         add  r1,r1,r2        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGBlitTrans0
BBGBlitExit:   
         ldmia sp!,{r4-r12,pc}
@
@ do a blit with stretching / shrinking
@ call from C as BBGStretchBlit(BBGFX_DC *pDC)@
@
BBGStretchBlit:   
         ldr  r1,[r0,#BM_SRC_BPP]
         cmp  r1,#16        @ don't allow invalid bit depths
         beq  BBGStretchStart
         cmp  r1,#32        @ only 16 or 32bpp images
         movne pc,lr         
BBGStretchStart:   
         stmfd sp!,{r4-r12,lr}
         mov  r11,r1     @ keep bpp in r11
@ calculate the vertical and horizontal scale factors
         ldr  r1,[r0,#RECT_SRC_TOP]
         ldr  r2,[r0,#RECT_SRC_BOT]
         ldr  r3,[r0,#RECT_DST_TOP]
         ldr  r4,[r0,#RECT_DST_BOT]
         sub  r2,r2,r1
         add  r2,r2,#1
         sub  r4,r4,r3
         add  r4,r4,#1
@ calc source height * 256 / dest height
         mov  r5,r2,LSL #8
         mov  r3,r4
         bl   divide
         strh r5,[r0,#TEMP_SPACE]    @ keep quotient
@ calc width
         ldr  r1,[r0,#RECT_SRC_LEFT]
         ldr  r2,[r0,#RECT_SRC_RIGHT]
         ldr  r3,[r0,#RECT_DST_LEFT]
         ldr  r4,[r0,#RECT_DST_RIGHT]
         sub  r2,r2,r1
         add  r2,r2,#1
         sub  r4,r4,r3
         add  r4,r4,#1
@ calc source width * 256 / dest width
         mov  r5,r2,LSL #8
         mov  r3,r4
         bl   divide
         strh r5,[r0,#TEMP_SPACE+2]    @ keep quotient
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
         ldrb r10,[r0,#DST_TRANS]    @ translucency
         mul  r5,r3,r2
         add  r1,r1,r5
         cmp  r11,#16       @ 16bpp?
         addeq  r1,r1,r4,LSL #1    @ now r1 points to source bitmap
         addne  r1,r1,r4,LSL #2
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r5,r8,r4
         add  r3,r3,r5
         cmp  r11,#16       @ 16bpp?
         addeq r3,r3,r9,LSL #1
         addne r3,r3,r9,LSL #2        @ pointer to destination
         ldr  r14,[r0,#RECT_DST_BOT]
         ldr  r12,[r0,#RECT_DST_RIGHT]
         sub  r6,r14,r8            @ get height to draw
         add  r6,r6,#1
         sub  r7,r12,r9            @ get width to draw
         add  r7,r7,#1
         ldr  r14,[r0,#DST_COLOR]        @ transparent color
@ adjust destination pitch to move us to the next line
         cmp  r11,#16       @ 16bpp?
         subeq  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
         subne  r4,r4,r7,LSL #2
         ldrh r5,[r0,#TEMP_SPACE]    @ get v scale factor
         ldrh r8,[r0,#TEMP_SPACE+2]    @ get h scale factor
         cmp  r11,#32       @ 32bpp?
         mov  r11,#0        @ v accumulator
         beq  BBGStretch32
         cmp  r10,#255        @ opaque?
         bne  BBGStretchTrans    @ nope, do translucent
BBGStretch0: @ top of loop
         mov  r12,#0        @ h accumulator
         mov  r10,r7        @ get width in r10
BBGStretch1:   
         mov  r9,r12,LSR #7    @ to address shorts
         bic  r9,r9,#1    @ clear odd bit
         ldrh r9,[r1,r9]    @ source pixel
         add  r12,r12,r8    @ add to h sum
         cmp  r9,r14        @ transparent?
         strneh r9,[r3]
         add  r3,r3,#2
         subs r10,r10,#1    @ decrement h count
         bne  BBGStretch1    @ loop through line
         add  r11,r11,r5    @ add to v sum
         mov  r10,r11,LSR #8    @ get the whole part (deltay)
         mul  r9,r2,r10        @ get the y offset of deltay
         bic  r11,r11,#0xff00    @ clear "whole" part for next time
         add  r1,r1,r9        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGStretch0
         ldmia sp!,{r4-r12,pc}    @ leave
@ translucent stretch
BBGStretchTrans: @ top of loop
         mov  r10,r10,LSR #3    @ make trans a 5-bit number
         mov  r11,#0x7e00000    @ create constant for easy masking
         orr  r11,r11,#0xf800
         orr  r11,r11,#0x1f    @ now we have 07E0F81F
BBGStretchTrans0:   
         bic  r8,r8,#0xff000000    @ zero h accumulator
         bic  r8,r8,#0x00ff0000
         orr  r6,r6,r7,LSL #16    @ get width in top of r6
BBGStretchTrans1:   
         mov  r9,r8,LSR #23    @ to address shorts
         bic  r9,r9,#1    @ clear odd bit
         ldrh r9,[r1,r9]    @ source pixel
         add  r8,r8,r8,LSL #16    @ add to h sum
         cmp  r9,r14        @ transparent?
         beq  BBGStretchTrans2    @ skip it
         ldrh r12,[r3]        @ destination pixel
         orr  r9,r9,r9,LSL #16    @ double source color
         and  r9,r9,r11        @ mask G, R+B
         mul  r9,r10,r9        @ ready for combining
         rsb  r10,r10,#32    @ inverted translucency for dest pixel
         orr  r12,r12,r12,LSL #16    @ prepare dest pixel
         and  r12,r12,r11    @ mask
         mla  r12,r10,r12,r9    @ times translucency + source pixel
         rsb  r10,r10,#32    @ revert to source trans
         and  r12,r11,r12,LSR #5    @ adjust for mult and mask again         
         orr  r12,r12,r12,LSR #16    @ combine G+R+B
         strh r12,[r3]
BBGStretchTrans2:   
         add  r3,r3,#2        @ advance dest pointer
         sub  r6,r6,#0x10000    @ decrement h count
         cmp  r6,#0x10000
         bge  BBGStretchTrans1    @ loop through line
         add  r5,r5,r5,LSL #16    @ add to v sum
         mov  r9,r5,LSR #24    @ get the whole part (deltay)
         mul  r9,r2,r9        @ get the y offset of deltay
         bic  r5,r5,#0xff000000    @ clear "whole" part for next time
         add  r1,r1,r9        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGStretchTrans0
         ldmia sp!,{r4-r12,pc}    @ done, leave
@ 32bpp
BBGStretch32: @ top of loop
         mov  r12,#0        @ h accumulator
         mov  r10,r7        @ get width in r10
BBGStretch32_1:   
         subs r10,r10,#1    @ decrement h count - keep it far from the BNE for better branch prediction
         mov  r9,r12,LSR #6    @ to address longs
         bic  r9,r9,#3      @ clear odd bits
         ldr  r9,[r1,r9]    @ source pixel
         pld  [r1,r2]
         add  r12,r12,r8    @ add to h sum
         str  r9,[r3],#4
         bne  BBGStretch32_1    @ loop through line
         subs r6,r6,#1        @ decrement vcount
         add  r11,r11,r5    @ add to v sum
         mov  r10,r11,LSR #8    @ get the whole part (deltay)
         mul  r9,r2,r10        @ get the y offset of deltay
         bic  r11,r11,#0xff00    @ clear "whole" part for next time
         add  r1,r1,r9        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         bne  BBGStretch32
         ldmia sp!,{r4-r12,pc}    @ leave
@
@ do a blit with stretching / shrinking
@ call from C as BBGStretchPattern(BBGFX_DC *pDC)@
@
BBGStretchPattern:   
         ldr  r1,[r0,#BM_SRC_BPP]
         cmp  r1,#1        @ mske sure it's 1Bpp
         movne pc,lr    @ return if not
         stmfd sp!,{r4-r12,lr}
@ calculate the vertical and horizontal scale factors
         ldr  r1,[r0,#RECT_SRC_TOP]
         ldr  r2,[r0,#RECT_SRC_BOT]
         ldr  r3,[r0,#RECT_DST_TOP]
         ldr  r4,[r0,#RECT_DST_BOT]         
         sub  r2,r2,r1
         add  r2,r2,#1
         sub  r4,r4,r3
         add  r4,r4,#1
@ calc source height * 256 / dest height
         mov  r5,r2,LSL #8
         mov  r3,r4
         bl   divide
         strh r5,[r0,#TEMP_SPACE]    @ keep quotient
@ calc width
         ldr  r1,[r0,#RECT_SRC_LEFT]
         ldr  r2,[r0,#RECT_SRC_RIGHT]
         ldr  r3,[r0,#RECT_DST_LEFT]
         ldr  r4,[r0,#RECT_DST_RIGHT]
         sub  r2,r2,r1
         add  r2,r2,#1
         sub  r4,r4,r3
         add  r4,r4,#1
@ calc source width * 256 / dest width
         mov  r5,r2,LSL #8
         mov  r3,r4
         bl   divide
         strh r5,[r0,#TEMP_SPACE+2]    @ keep quotient
@ get source pointer
         ldr  r1,[r0,#BM_SRC_PTR]    @ bitmap pointer
         ldr  r2,[r0,#BM_SRC_PITCH]    @ pitch
         ldr  r3,[r0,#RECT_SRC_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_SRC_LEFT] @ starting X
@         ldrb r10,[r0,#DST_TRANS]    @ translucency
         mul  r5,r3,r2
         add  r1,r1,r5
         add  r1,r1,r4,LSL #1    @ now r1 points to source bitmap
@ get destination pointer
         ldr  r3,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r4,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r8,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r9,[r0,#RECT_DST_LEFT] @ start x
         mul  r5,r8,r4
         add  r3,r3,r5
         add  r3,r3,r9,LSL #1        @ pointer to destination
         ldr  r14,[r0,#RECT_DST_BOT]
         ldr  r12,[r0,#RECT_DST_RIGHT]
         sub  r6,r14,r8            @ get height to draw
         add  r6,r6,#1
         sub  r7,r12,r9            @ get width to draw
         add  r7,r7,#1
         ldrh r14,[r0,#DST_COLOR]    @ color to draw
@ adjust destination pitch to move us to the next line
         sub  r4,r4,r7,LSL #1    @ now R4 has delta dest pitch
         ldrh r5,[r0,#TEMP_SPACE]    @ get v scale factor
         ldrh r8,[r0,#TEMP_SPACE+2]    @ get h scale factor
BBGStretchPat0: @ top of loop
         mov  r12,#0        @ h accumulator
         mov  r10,r7        @ get width in r10
BBGStretchPat1:   
         mov  r9,#0x80        @ bit offset
         mov  r11,r12,LSR #8    @ get pixel offset
         and  r11,r11,#0x7    @ bit offset
         mov  r9,r9,LSR r11    @ get bit mask
         ldrb r11,[r1,r12,LSR #11]    @ source pixel
         add  r12,r12,r8    @ add to h sum
         tst  r11,r9        @ transparent?
         strneh r14,[r3]
         add  r3,r3,#2
         subs r10,r10,#1    @ decrement h count
         bne  BBGStretchPat1    @ loop through line
         add  r5,r5,r5,LSL #16    @ add to v sum
         mov  r10,r5,LSR #24    @ get the whole part (deltay)
         mul  r9,r2,r10        @ get the y offset of deltay
         bic  r5,r5,#0xff000000    @ clear "whole" part for next time
         add  r1,r1,r9        @ move to next source line
         add  r3,r3,r4        @ move to next dest line
         subs r6,r6,#1        @ decrement vcount
         bne  BBGStretchPat0
         ldmia sp!,{r4-r12,pc}    @ leave
@
@ Draw a horizontal line
@ call from C as BBGHLine(BBGFX_DC *pDC)@
@
BBGHLine:   
         stmfd sp!,{r4-r12,lr}
         ldr  r1,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r2,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r3,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_DST_LEFT] @ start x
         ldr  r5,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r6,[r0,#BM_DST_WIDTH]
         ldr  r7,[r0,#RECT_DST_RIGHT]    @ right side
         ldrb r12,[r0,#DST_PATTERN]        @ bit pattern
         ldr  r14,[r0,#DST_COLOR]        @ pixel color
         ldrb r10,[r0,#DST_TRANS]    @ translucency
         ldrb r9,[r0,#BM_DST_BPP]    @ see if 16 or 32bpp
         mul  r8,r3,r2                @ offset to this line
         add  r1,r1,r8
         cmp  r9,#16
         blt  BBGHLineExit    @ invalid bit depth (only 16/24/32 are valid)
         cmp  r9,#32
         addlt  r1,r1,r4,LSL #1    @ now we're pointing to the starting point
         addeq  r1,r1,r4,LSL #2
         cmp  r3,r5            @ line within bounds?
         bge  BBGHLineExit    @ nope, leave
         cmp  r4,r6            @ line not visible?
         bge  BBGHLineExit
         cmp  r10,#5        @ less than 5 means no contribution
         blt  BBGHLineExit
         cmp  r7,r6            @ right side of line off right edge?
         subge r7,r6,#1        @ trim line to fit in destination bitmap
         subs r8,r7,r4        @ get the number of pixels to draw
         bmi  BBGHLineExit
         add  r8,r8,#1        @ right-left+1
         sub  r11,r5,r3        @ see how many lines to bottom of bitmap
         cmp  r11,#8        @ see if we need to trim pattern lines to less than 8
         movgt r11,#8        @ number of loops through the pattern
         cmp  r9,#32        @ 32bpp?
         beq  BBGHLine32
         cmp  r10,#255        @ no translucency?
         bne  BBGHLineTrans @ need to do it the slow way
         orr  r14,r14,r14,LSL #16    @ double pixel color for faster throughput
BBGHLine0:   
         movs r12,r12,LSR #1
         bcc  BBGHLineBot    @ nothing to draw
         mov  r10,r1        @ destination address
         mov  r9,r8            @ pixel count
         tst  r10,#2        @ dword aligned?
         strneh r14,[r10],#2
         subne r9,r9,#1
         and  r3,r9,#3        @ remainder
         movs  r9,r9,LSR #2    @ do 4 pixels at a time
         mov  r4,r14        @ get pixels here also
         beq  BBGHLine2        @ less than 4 pixels to draw
BBGHLine1:   
         stmia  r10!,{r4,r14}
         subs r9,r9,#1
         bne  BBGHLine1
BBGHLine2:   
         cmp  r3,#0
         beq  BBGHLineBot    @ do leftover pixels
         strh r4,[r10],#2
         sub  r3,r3,#1
         b    BBGHLine2
BBGHLineBot:   
         add  r1,r1,r2        @ skip to next line
         subs r11,r11,#1    @ outer loop count
         bne  BBGHLine0
         b    BBGHLineExit
@ draw an opaque 32bpp line
BBGHLine32:   
         cmp  r10,#255        @ no translucency?
         bne  BBGHLineTrans32 @ need to do it the slow way
BBGHLine32_3:   
         movs r12,r12,LSR #1
         bcc  BBGHLine32Bot    @ nothing to draw
         mov  r10,r1        @ destination address
         mov  r9,r8            @ pixel count
         mov  r4,r14        @ put it in 4 registers for speed
         mov  r5,r14
         mov  r6,r14
         mov  r7,r14
         movs r3,r9,LSR #2    @ try to do 4 at a time
         beq  BBGHLine32_1
BBGHLine32_0:   
         stmia r10!,{r4-r7}
         subs r3,r3,#1
         bne  BBGHLine32_0
BBGHLine32_1:   
         ands r3,r8,#3
         beq  BBGHLine32Bot
BBGHLine32_2:   
         str  r14,[r10],#4
         subs r3,r3,#1
         bne  BBGHLine32_2         
BBGHLine32Bot:   
         add  r1,r1,r2        @ skip to next line
         subs r11,r11,#1    @ outer loop count
         bne  BBGHLine32_3
         b    BBGHLineExit
@ 32-bit line with translucency
BBGHLineTrans32:   
         mov  r7,#0xff0000  @ mask for r+b
         orr  r7,r7,#0xff
         and  r3,r14,r7     @ get r+b masked
         mov  r0,r14,LSR #24    @ get alpha in r0
         and  r14,r14,#0xff00   @ get g
         mul  r4,r3,r0      @ r+b * alpha
         mul  r5,r14,r0     @ g * alpha
         rsb  r0,r0,#256    @ invert alpha for destination
BBGHLineTrans32_1:   
         movs r12,r12,LSR #1    @ test for pattern bit
         bcc  BBGHLine32T_Bot    @ nothing to draw
         mov  r10,r1        @ destination address
         mov  r9,r8            @ pixel count
BBGHLineTrans32_0:   
         ldr  r6,[r10]      @ get dest pixel
         subs r9,r9,#1      @ pixel count --
         and  r14,r7,r6     @ mask r+b
         and  r6,r6,#0xff00 @ mask g
         mla  r3,r14,r0,r4  @ combine src+dest r+b
         mla  r14,r6,r0,r5  @ combine src+dest g
         and  r3,r7,r3,LSR #8   @ mask new r+b
         and  r14,r14,#0xff0000   @ mask new g
         orr  r3,r3,r14,LSR #8    @ combine
         orr  r3,r3,#0xff000000 @ set alpha = ff
         str  r3,[r10],#4
         bne  BBGHLineTrans32_0
BBGHLine32T_Bot:   
         add  r1,r1,r2        @ skip to next line
         subs r11,r11,#1    @ outer loop count
         bne  BBGHLineTrans32_1
         b    BBGHLineExit
@ draw the line with translucency
BBGHLineTrans:   
         mov  r10,r10,LSR #3    @ translucency to 0-31
         mov  r5,#0x7e00000        @ create mask
         orr  r5,r5,#0xf800
         orr  r5,r5,#0x1f
@ prepare the pixel color we're storing
         orr  r14,r14,r14,LSL #16
         and  r14,r14,r5    @ masked
         mul  r6,r14,r10    @ color * translucency
         rsb  r10,r10,#32    @ invert translucency for destination
         mov  r14,r6        @ prepared color in R14
BBGHTrans0:   
         movs r12,r12,LSR #1
         bcc  BBGHTransBot    @ nothing to draw
         mov  r7,r1            @ destination address
         mov  r9,r8            @ pixel count
BBGHTrans1:   
         ldrh r3,[r7]        @ get source pixel
         orr  r3,r3,r3,LSL #16 @ double it
         and  r3,r5,r3        @ mask it
         mla  r4,r3,r10,r14    @ multiply and combine
         and  r4,r5,r4,LSR #5    @ shift and mask
         orr  r4,r4,r4,LSR #16    @ combine
         strh r4,[r7],#2    @ store updated pixel 
         subs r9,r9,#1
         bne  BBGHTrans1
BBGHTransBot:   
         add  r1,r1,r2        @ skip to next line
         subs r11,r11,#1    @ outer loop count
         bne  BBGHTrans0
BBGHLineExit:   
         ldmia sp!,{r4-r12,pc}
@
@ Draw a vertical line
@ call from C as BBGVLine(BBGFX_DC *pDC)@
@
BBGVLine:   
         stmfd sp!,{r4-r12,lr}
         ldr  r1,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r2,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r3,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_DST_LEFT] @ start x
         ldr  r5,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r6,[r0,#BM_DST_WIDTH]
         ldr  r7,[r0,#RECT_DST_BOT]        @ bottom
         ldrb r12,[r0,#DST_PATTERN]        @ bit pattern
         ldr  r14,[r0,#DST_COLOR]        @ pixel color
         ldrb r10,[r0,#DST_TRANS]    @ translucency
         ldr  r9,[r0,#BM_DST_BPP]
         mul  r8,r3,r2                @ offset to this line
         add  r1,r1,r8
         cmp  r9,#16
         blt  BBGVLineExit    @ invalid dest bit depth - only 16/24/32 allowed
         cmp  r9,#32
         addne  r1,r1,r4,LSL #1    @ now we're pointing to the starting point
         addeq  r1,r1,r4,LSL #2
         cmp  r3,r5            @ line within bounds?
         bge  BBGVLineExit    @ nope, leave
         cmp  r4,r6            @ line not visible?
         bge  BBGVLineExit
         cmp  r10,#5        @ less than 5 means no contribution
         blt  BBGVLineExit
         cmp  r7,r5            @ bottom past bottom?
         subge r7,r5,#1        @ trim line to fit in destination bitmap
         sub  r8,r7,r3        @ get the number of pixels to draw
         add  r8,r8,#1        @ bot-top+1
         cmp  r9,#32
         beq  BBGVLine32    @ special version for 32bpp
         cmp  r10,#255        @ no translucency?
         bne  BBGVLineTrans @ need to do it the slow way
BBGVLine0:   
         tst  r12,#1
         strneh r14,[r1]
         tst  r12,#2
         strneh r14,[r1,#2]
         tst  r12,#4
         strneh r14,[r1,#4]
         tst  r12,#8
         strneh r14,[r1,#6]
         tst  r12,#16
         strneh r14,[r1,#8]
         tst  r12,#32
         strneh r14,[r1,#10]
         tst  r12,#64
         strneh r14,[r1,#12]
         tst  r12,#128
         strneh r14,[r1,#14]
         add  r1,r1,r2        @ skip to next line
         subs r8,r8,#1
         bne  BBGVLine0
         b    BBGVLineExit
BBGVLine32:   
         cmp  r10,#255        @ no translucency?
         bne  BBGVLineTrans32 @ need to do it the slow way
BBGVLine32_0:   
         tst  r12,#1
         strne  r14,[r1]
         tst  r12,#2
         strne  r14,[r1,#4]
         tst  r12,#4
         strne r14,[r1,#8]
         tst  r12,#8
         strne r14,[r1,#12]
         tst  r12,#16
         strne r14,[r1,#16]
         tst  r12,#32
         strne r14,[r1,#20]
         tst  r12,#64
         strne r14,[r1,#24]
         tst  r12,#128
         strne r14,[r1,#28]
         add  r1,r1,r2        @ skip to next line
         subs r8,r8,#1
         bne  BBGVLine32_0
         b    BBGVLineExit
BBGVLineTrans32:   
         mov  r7,#0xff0000  @ mask for r+b
         orr  r7,r7,#0xff
         and  r3,r14,r7     @ get r+b masked
         mov  r0,r14,LSR #24    @ get alpha in r0
         and  r14,r14,#0xff00   @ get g
         mul  r4,r3,r0      @ r+b * alpha
         mul  r5,r14,r0     @ g * alpha
         rsb  r0,r0,#256    @ invert alpha for destination
BBGVLineT32_1:   
         orr  r12,r12,#0xff00      @ "end" bit pattern for shift mask
BBGVLineT32_0:   
         movs r12,r12,ROR #1
         bcc  BBGVLineT32_Bot   @ no line to draw
         ldr  r6,[r1]       @ get dest pixel
         and  r14,r7,r6     @ mask r+b
         and  r6,r6,#0xff00 @ mask g
         mla  r3,r14,r0,r4  @ combine src+dest r+b
         mla  r14,r6,r0,r5  @ combine src+dest g
         and  r3,r7,r3,LSR #8   @ mask new r+b
         and  r14,r14,#0xff0000   @ mask new g
         orr  r3,r3,r14,LSR #8    @ combine
         orr  r3,r3,#0xff000000 @ set alpha = ff
         str  r3,[r1]
BBGVLineT32_Bot:   
         add  r1,r1,#4   @ move horizontally
         and  r6,r12,#0xff  @ guard bits == done
         cmp  r6,#0xff
         bne  BBGVLineT32_0
         subs r1,r1,#32 @ move back 8 pixels
         mov  r12,r12,ROR #24   @ roll around for another 8
         add  r1,r1,r2        @ skip to next line
         subs r8,r8,#1
         bne  BBGVLineT32_1
         b    BBGVLineExit
@ draw the 16bpp line with translucency
BBGVLineTrans:   
         mov  r10,r10,LSR #3    @ translucency to 0-31
         mov  r5,#0x7e00000        @ create mask
         orr  r5,r5,#0xf800
         orr  r5,r5,#0x1f
@ prepare the pixel color we're storing
         orr  r14,r14,r14,LSL #16
         and  r14,r14,r5    @ masked
         mul  r6,r14,r10    @ color * translucency
         rsb  r10,r10,#32    @ invert translucency for destination
         mov  r14,r6        @ prepared color in R14
BBGVTrans0:   
         mov  r11,r12        @ get copy of bit pattern
         orr  r11,r11,#0xff0000
         mov  r7,r1        @ destination address
BBGVTrans1:   
         movs r11,r11,LSR #1    @ draw this pixel?
         bcc  BBGVLineBot
         ldrh r3,[r7]        @ get source pixel
         orr  r3,r3,r3,LSL #16 @ double it
         and  r3,r5,r3        @ mask it
         mla  r4,r3,r10,r14    @ multiply and combine
         and  r4,r5,r4,LSR #5    @ shift and mask
         orr  r4,r4,r4,LSR #16    @ combine
         strh r4,[r7]      @ store updated pixel 
BBGVLineBot:   
         add  r7,r7,#2
         cmp  r11,#0xff00    @ done with 8 pixel pattern?
         bne  BBGVTrans1
         add  r1,r1,r2        @ skip to next line
         subs r8,r8,#1
         bne  BBGVTrans0
BBGVLineExit:   
         ldmia sp!,{r4-r12,pc}
@
@ Fill a rectangle
@ call from C as BBGFill(BBGFX_DC *pDC)@
@
BBGFill:   
         stmfd sp!,{r4-r12,lr}
         ldr  r1,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r2,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r3,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_DST_LEFT] @ start x
         ldr  r5,[r0,#BM_DST_HEIGHT]    @ check that line is within bounds
         ldr  r6,[r0,#BM_DST_WIDTH]
         ldr  r7,[r0,#RECT_DST_RIGHT]    @ right side
         ldr  r12,[r0,#RECT_DST_BOT]
         ldr  r14,[r0,#DST_COLOR]        @ pixel color
         ldrb r9,[r0,#DST_TRANS]    @ translucency
         ldr  r10,[r0,#BM_DST_BPP]    @ check if we're using 16 or 32bpp
         cmp  r3,r5
         bhs  BBGFillExit    @ starts below dest
         cmp  r4,r6
         bhs  BBGFillExit   @ starts to the right of dest
         cmp  r12,r5
         bhs  BBGFillExit    @ ends below bottom
         cmp  r7,r6
         bhs  BBGFillExit   @ ends to off the right edge
         mul  r8,r3,r2
         add  r1,r1,r8        @ point to starting line
         subs r12,r12,r3    @ line count-1
         bmi  BBGFillExit    @ dest_bottom is above dest_top
         cmp  r4,r7
         bgt  BBGFillExit    @ dest_right is to the left of dest_left
         add  r12,r12,#1
         cmp  r10,#32    @ 32bpp?
         beq  BBGFill32
         cmp  r9,#255    @ fully opaque?
         bne  BBGFill_slow2
         cmp  r4,#0        @ for edge-to-edge fills, use a faster technique
         bne  BBGFill_slow
         sub  r6,r6,#1
         cmp  r7,r6
         bne  BBGFill_slow
         mul  r10,r12,r2
         mov  r5,r10,LSR #4    @ number of 16-byte blocks to do
         bic  r14,r14,#0xff0000        @ clear alpha (if present)
         orr  r14,r14,r14,LSL #16    @ double pixel color
         mov  r12,r14
         mov  r9,r14
         mov  r11,r14    @ get colors in 4 registers
BBGFill0:   
         stmia r1!,{r9,r11,r12,r14}    @ do 8 pixels at a time
         subs  r5,r5,#1
         bne   BBGFill0        @ loop through rectangle
         ands  r10,r10,#0xe    @ any remaining bytes?
         beq   BBGFill0b
BBGFill0a:   
         strh  r9,[r1],#2
         subs  r10,r10,#2
         bne   BBGFill0a
BBGFill0b: ldr r10,[r0,#BM_DST_BPP] 
         cmp  r10,#24    @ 24bpp?
         beq  BBGFill24    @ need to finish by setting the dest alpha values
BBGFillExit:   
         ldmia sp!,{r4-r12,pc}
@
@ 24-bit destination bitmap
@ if ucTransparency == 255, clear the destination
@ else blend the color with the destination
@
BBGFill24: ldrb r9,[r0,#DST_TRANS] @ see if we're going to clear or blend
         ldr   r4,[r0,#RECT_DST_LEFT]
         ldr   r7,[r0,#RECT_DST_RIGHT]    @ right side
         ldr   r14,[r0,#DST_COLOR]        @ pixel color
         ldr   r12,[r0,#RECT_DST_BOT]
         ldr   r3,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr   r1,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr   r2,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr   r5,[r0,#BM_DST_HEIGHT]
         mul   r11,r2,r5
         add   r1,r1,r11    @ point to alpha plane
         mov   r2,r2,LSR #1    @ pitch /= 2
         mul   r11,r2,r3    @ dest top
         add   r1,r1,r11
         add   r11,r1,r4    @ now r11 points to correct pixel
         sub   r4,r7,r4        @ drawing width-1
         add   r4,r4,#1
         sub   r12,r12,r3    @ line count-1
         add   r12,r12,#1
@ store the alpha value to all pixels
         mov   r14,r14,LSR #16    @ get alpha value in lower 8-bits
         cmp   r9,#255
         bne   BBGFill24_blend
BBGFill24_2:   
         mov   r7,r4        @ horizontal count
         mov   r1,r11        @ dest pointer
BBGFill24_3:   
         strb  r14,[r1],#1
         subs  r7,r7,#1
         bne   BBGFill24_3
         add   r11,r11,r2        @ point to next line
         subs  r12,r12,#1    @ dec line count
         bne   BBGFill24_2
         b     BBGFillExit
@ combine the new alpha value with each pixel
@ DIVIDE_BY_255(thisInput) ((thisInput+(((thisInput+128)>>8)+128))>>8 )
@  tmpVar1 = alpha * *a@
@  *a = alpha + *a - DIVIDE_BY_255(tmpVar1)@
@
BBGFill24_blend:   
         mov   r7,r4        @ horizontal count
         mov   r1,r11        @ dest pointer
BBGFill24_4:   
         ldrb  r3,[r1]        @ get current alpha value
         mul   r5,r3,r14    @ t = Aa * Ab
         add   r6,r5,#128
         add   r5,r5,r6,LSR #8
         add   r5,r5,#128
         sub   r3,r3,r5,LSR #8    @ t = (Aa - t/255)
         add   r3,r3,r14    @ A = Aa + t
         cmp   r3,#0xff
         movgt r3,#0xff        @ don't let it overflow
         strb  r3,[r1],#1
         subs  r7,r7,#1
         bne   BBGFill24_4
         add   r11,r11,r2        @ point to next line
         subs  r12,r12,#1    @ dec line count
         bne   BBGFill24_blend
         b     BBGFillExit
@
@ 32-bit destination bitmap
@ if ucTransparency == 255, clear the destination
@ else blend the color with the destination
@
BBGFill32: ldrb r9,[r0,#DST_TRANS] @ see if we're going to clear or blend
         cmp   r14,#0xff000000
         bge   BBGFill32_opaque  @ if source alpha == 255, just store it
@ future
@         cmp   r9,#255
@         bne   BBGFill32_blend
@ store the 32-bit color to all destination pixels
BBGFill32_opaque:   
         add   r0,r1,r4,LSL #2    @ starting x,y pointer
         sub   r4,r7,r4        @ drawing width-1
         add   r4,r4,#1
BBGFill32_0:   
         mov   r7,r4        @ horizontal count
         mov   r1,r0        @ dest pointer
BBGFill32_1:   
         str   r14,[r1],#4
         subs  r7,r7,#1
         bne   BBGFill32_1
         add   r0,r0,r2        @ point to next line
         subs  r12,r12,#1    @ dec line count
         bne   BBGFill32_0
         b     BBGFillExit
@
@ Blend the source color and the destination pixels
@
BBGFill32_blend:   
         add   r0,r1,r4,LSL #2    @ starting x,y pointer
         sub   r4,r7,r4        @ drawing width-1
         add   r4,r4,#1
         mov   r6,#0xff        @ create mask for RB/AG
         orr   r6,r6,#0xff0000    @ 00FF00FF
BBGFill32_b0:   
         mov   r7,r4        @ horizontal count
         mov   r1,r0        @ dest pointer
BBGFill32_b1:   
         ldr   r5,[r1]        @ get destination pixel
         strh  r14,[r1],#2
         subs  r7,r7,#1
         bne   BBGFill32_b1
         add   r0,r0,r2        @ point to next line
         subs  r12,r12,#1    @ dec line count
         bne   BBGFill32_b0
         b     BBGFillExit
@ The fill is not edge-to-edge, go slower
BBGFill_slow:   
         mov   r14,r14,LSL #16
         orr   r14,r14,r14,LSR #16   @ double the color
         add   r11,r1,r4,LSL #1    @ starting x,y pointer
         sub   r4,r7,r4        @ drawing width-1
         add   r4,r4,#1
BBGFillslow2:   
         mov   r7,r4        @ horizontal count
         mov   r1,r11        @ dest pointer
         tst   r1,#2        @ dword-aligned?
         strneh r14,[r1],#2
         subne r7,r7,#1
         movs  r6,r7,LSR #2 @ do 4 pixels at a time
         beq   BBGFillslow4     @ no dword pairs to do
         mov   r10,r14      @ put color in r10
BBGFillslow3:   
         stmia r1!,{r10,r14}
         subs  r6,r6,#1
         bne   BBGFillslow3
BBGFillslow4:   
         ands  r6,r7,#3    @ do leftover pixels
         beq   BBGFillslow6
BBGFillslow5:   
         strh  r14,[r1],#2
         subs  r6,r6,#1
         bne   BBGFillslow5
BBGFillslow6:   
         add   r11,r11,r2    @ point to next line
         subs  r12,r12,#1    @ dec line count
         bne   BBGFillslow2
         ldr   r10,[r0,#BM_DST_BPP]
         cmp   r10,#24        @ 24bpp?
         beq   BBGFill24    @ finish by setting the alpha values
         b     BBGFillExit        @ leave
@ The fill uses translucency@ even slower
BBGFill_slow2:   
@ separate the source color and multiply by the translucent value
@ R9 = multiplied R+G+B
         cmp  r10,#24        @ is the destination 24bpp?
         moveq r8,r14,LSR #19    @ use the translucent value from the color
         movne r8,r9,LSR #3        @ otherwise use the ucTranslucency value (5-bits)
         cmp  r8,#0x1f      @ fully opaque?
         beq  BBGFill_slow  @ do it faster
         mov  r5,#0x7e00000    @ create constant for easy masking
         orr  r5,r5,#0xf800
         orr  r5,r5,#0x1f    @ now we have 07E0F81F
         bic  r14,r14,#0xff0000        @ clear possible alpha value
         orr  r14,r14,r14,LSL #16    @ double source color
         and  r14,r14,r5    @ mask G, R+B
         mul  r9,r14,r8        @ ready for combining
         rsb  r8,r8,#32     @ inverted translucency of destination pixel
         add   r11,r1,r4,LSL #1    @ starting x,y pointer
         sub   r4,r7,r4        @ drawing width-1
         add   r4,r4,#1
BBGFill4: mov r7,r4 @ horizontal count
         mov   r1,r11        @ dest pointer
BBGFill5: ldrh r10,[r1] @ dest pixel
         subs  r7,r7,#1        @ keep far from BNE for better branch prediction
         orr   r10,r10,r10,LSL #16    @ double it
         and   r10,r10,r5    @ mask
         mla   r3,r10,r8,r9
         and   r3,r5,r3,LSR #5    @ shift down and mask to get final pixel
         orr   r3,r3,r3,LSR #16    @ final pixel ready
         strh  r3,[r1],#2
         bne   BBGFill5
         add   r11,r11,r2        @ point to next line
         subs  r12,r12,#1    @ dec line count
         bne   BBGFill4
         ldr   r10,[r0,#BM_DST_BPP]
         cmp   r10,#24        @ 24bpp?
         beq   BBGFill24    @ finish by setting the alpha values
         b     BBGFillExit        @ leave
@
@ Fill a rectangle with a color gradient (vertical)
@ call from C as BBGGradientFill(BBGFX_DC *pDC)@
@
BBGGradientFill:   
         stmfd sp!,{r4-r12,lr}
         ldr  r1,[r0,#BM_DST_PTR]    @ destination bitmap pointer
         ldr  r2,[r0,#BM_DST_PITCH]    @ destination pitch
         ldr  r3,[r0,#RECT_DST_TOP]    @ starting Y coordinate
         ldr  r4,[r0,#RECT_DST_LEFT] @ start x
         ldr  r5,[r0,#RECT_DST_BOT]
         ldr  r6,[r0,#BM_DST_WIDTH]
         ldr  r7,[r0,#RECT_DST_RIGHT]    @ right side
         ldr  r14,[r0,#DST_COLOR]        @ pixel color
         ldr  r12,[r0,#DST_COLOR2]        @ pixel color 2 for gradient
         ldr  r9,[r0,#BM_DST_BPP]        @ check if we're on 16 or 32bpp
         cmp  r9,#32
         orrne  r14,r14,r14,LSL #16        @ double color
         sub  r0,r7,r4        @ get width
         add  r0,r0,#1        @ right-left+1
         cmp  r9,#32
         moveq r0,r0,LSR #1    @ 32bpp uses 2 pixels per loop
         movne r0,r0,LSR #2    @ divide by 4 pixels per loop
@ calculate fractional slope for RGB
         mul  r8,r3,r2
         add  r1,r1,r8        @ move down to starting y
         add  r1,r1,r4,LSL #1    @ add left edge offset
         mov  r7,#0            @ keep track of pos/neg deltas
         cmp  r9,#32        @ work on ARGB pixels
         beq  BBGGFill32
         and  r8,r14,#0x1f    @ get blue1
         and  r9,r12,#0x1f    @ get blue2
         subs r10,r9,r8        @ get the delta
         rsbmi r10,r10,#0    @ make it positive
         orrmi r7,r7,#1        @ mark blue as negative
         and  r8,r14,#0x7e0    @ get green1
         and  r9,r12,#0x7e0 @ get green2
         subs r11,r9,r8        @ get delta
         rsbmi r11,r11,#0    @ make it positive
         mov  r11,r11,LSR #5    @ shift down to bottom
         orrmi r7,r7,#2        @ mark green as negative
         and  r8,r14,#0xf800    @ get red1
         and  r9,r12,#0xf800    @ get red2
         subs r9,r9,r8        @ get delta
         rsbmi r9,r9,#0        @ make it positive
         orrmi r7,r7,#4        @ mark red as negative
         mov  r9,r9,LSR #11    @ shift down to bottom
@ DEBUG - need to check bounds
         sub  r6,r5,r3        @ get height in count variable
         add  r6,r6,#1
BBGGradFill0:   
         mov  r12,r0    @ loop count
         mov  r8,r14    @ use 2 registers for current color
BBGGradFill1:   
         stmia r1!,{r8,r14}    @ do 4 pixels at a time
         subs  r12,r12,#1
         bne   BBGGradFill1        @ loop through rectangle
         sub   r1,r1,r0,LSL #3    @ back to start of line
         add   r1,r1,r2            @ skip to next line
@ adjust colors and do next line
         add   r10,r10,r10,LSL #16    @ add blue delta
         cmp   r5,r10,LSR #16    @ do this to avoid divides
         sublt r10,r10,r5,LSL #16    @ adjust blue accumulator
         bge   BBGGradFill2
         tst   r7,#1            @ is blue delta negative?
         addeq r14,r14,#0x10000    @ time to increment blue
         addeq r14,r14,#0x1
         subne r14,r14,#0x10000
         subne r14,r14,#0x1
@ add green delta
BBGGradFill2:   
         add   r11,r11,r11,LSL #16    @ add green delta
         cmp   r5,r11,LSR #16    @ do this to avoid divides
         sublt r11,r11,r5,LSL #16    @ adjust green accumulator
         bge   BBGGradFill3
         tst   r7,#2            @ green negative
         addeq r14,r14,#0x200000 @ time to increment green
         addeq r14,r14,#0x20
         subne r14,r14,#0x200000
         subne r14,r14,#0x20
@ add red delta
BBGGradFill3:   
         add   r9,r9,r9,LSL #16    @ add red delta
         cmp   r5,r9,LSR #16    @ do this to avoid divides
         sublt r9,r9,r5,LSL #16    @ adjust red accumulator
         bge   BBGGradFill4
         tst   r7,#4            @ red negative?
         addeq r14,r14,#0x8000000    @ time to increment red
         addeq r14,r14,#0x800
         subne r14,r14,#0x8000000
         subne r14,r14,#0x800
BBGGradFill4:   
         subs  r6,r6,#1        @ line count
         bne   BBGGradFill0         
BBGGradFillEnd:   
         ldmia sp!,{r4-r12,pc}
BBGGFill32:   
         and  r8,r14,#0xff    @ get blue1
         and  r9,r12,#0xff    @ get blue2
         subs r10,r9,r8        @ get the delta
         rsbmi r10,r10,#0    @ make it positive
         orrmi r7,r7,#1        @ mark blue as negative
         and  r8,r14,#0xff00   @ get green1
         and  r9,r12,#0xff00   @ get green2
         subs r11,r9,r8        @ get delta
         rsbmi r11,r11,#0    @ make it positive
         mov  r11,r11,LSR #8    @ shift down to bottom
         orrmi r7,r7,#2        @ mark green as negative
         and  r8,r14,#0xff0000    @ get red1
         and  r9,r12,#0xff0000    @ get red2
         subs r9,r9,r8        @ get delta
         rsbmi r9,r9,#0        @ make it positive
         orrmi r7,r7,#4        @ mark red as negative
         mov  r9,r9,LSR #16    @ shift down to bottom
@ DEBUG - need to check bounds
         sub  r6,r5,r3        @ get height in count variable
         add  r6,r6,#1
BBGGFill32_0:   
         mov  r12,r0    @ loop count
         mov  r8,r14    @ use 2 registers for current color
BBGGFill32_1:   
         stmia r1!,{r8,r14}    @ do 2 pixels at a time
         subs  r12,r12,#1
         bne   BBGGFill32_1        @ loop through rectangle
         sub   r1,r1,r0,LSL #3    @ back to start of line
         add   r1,r1,r2            @ skip to next line
@ adjust colors and do next line
         add   r10,r10,r10,LSL #16    @ add blue delta
         cmp   r5,r10,LSR #16    @ do this to avoid divides
         sublt r10,r10,r5,LSL #16    @ adjust blue accumulator
         bge   BBGGFill32_2
         tst   r7,#1            @ is blue delta negative?
         addeq r14,r14,#0x1        @ time to increment blue
         subne r14,r14,#0x1
@ add green delta
BBGGFill32_2:   
         add   r11,r11,r11,LSL #16    @ add green delta
         cmp   r5,r11,LSR #16    @ do this to avoid divides
         sublt r11,r11,r5,LSL #16    @ adjust green accumulator
         bge   BBGGFill32_3
         tst   r7,#2            @ green negative
         addeq r14,r14,#0x100    @ time to increment green
         subne r14,r14,#0x100
@ add red delta
BBGGFill32_3:   
         add   r9,r9,r9,LSL #16    @ add red delta
         cmp   r5,r9,LSR #16    @ do this to avoid divides
         sublt r9,r9,r5,LSL #16    @ adjust red accumulator
         bge   BBGGFill32_4
         tst   r7,#4            @ red negative?
         addeq r14,r14,#0x10000    @ time to increment red
         subne r14,r14,#0x10000
BBGGFill32_4:   
         subs  r6,r6,#1        @ line count
         bne   BBGGFill32_0
         b     BBGGradFillEnd
@
@ Draw a scanline at 90 degrees rotated
@ call from C as DrawScan90(s, d, pEnd, iScreenPitch)@
@
DrawScan90:   
    stmfd    sp!,{r4-r9}
    add r9,r1,r3,LSL #1     @ add iPitch*2 to another dest pointer
ads90_top:   
   ldmia r0!,{r4-r7}    @ get 8 pixels
   strh r4,[r1]   @ pixel 0
   mov  r4,r4,LSR #16
   strh r4,[r1,r3]   @ pixel 1
   strh r5,[r9]      @ pixel 2
   mov  r5,r5,LSR #16
   strh r5,[r9,r3]   @ pixel 3
   add  r1,r1,r3,LSL #2
   add  r9,r9,r3,LSL #2
   strh r6,[r1]      @ pixel 4
   mov  r6,r6,LSR #16
   strh r6,[r1,r3]      @ pixel 5
   strh r7,[r9]         @ pixel 6
   mov  r7,r7,LSR #16
   strh r7,[r9,r3]      @ pixel 7
   add  r1,r1,r3,LSL #2
   add  r9,r9,r3,LSL #2
   cmp  r0,r2        @ end of line?
   bne  ads90_top
   ldmfd sp!,{r4-r9} @ restore regs
   mov  pc,lr        @ and return
@
@ Draw a scanline at 270 degrees rotated
@ call from C as DrawScan270(s, d, pEnd, iScreenPitch)@
@
DrawScan270:   
    stmfd    sp!,{r4-r9}
    rsb r3,r3,#0      @ make pitch negative
    add r9,r1,r3,LSL #1     @ add iPitch*2 to another dest pointer
ads270_top:   
   ldmia r0!,{r4-r7}    @ get 8 pixels
   strh r4,[r1]   @ pixel 0
   mov  r4,r4,LSR #16
   strh r4,[r1,r3]   @ pixel 1
   strh r5,[r9]      @ pixel 2
   mov  r5,r5,LSR #16
   strh r5,[r9,r3]   @ pixel 3
   add  r1,r1,r3,LSL #2
   add  r9,r9,r3,LSL #2
   strh r6,[r1]      @ pixel 4
   mov  r6,r6,LSR #16
   strh r6,[r1,r3]      @ pixel 5
   strh r7,[r9]         @ pixel 6
   mov  r7,r7,LSR #16
   strh r7,[r9,r3]      @ pixel 7
   add  r1,r1,r3,LSL #2
   add  r9,r9,r3,LSL #2
   cmp  r0,r2        @ end of line?
   bne  ads270_top
   ldmfd sp!,{r4-r9} @ restore regs
   mov  pc,lr        @ and return
@
@ Fast test to see if a bitmap has any translucent pixels
@ if not, then a faster blit routine can be used
@ call from C as BOOL BBGIsTranslucentBitmap(void *pBits, int width, int height, int BitsPerPixel)@
@ only supports 24 and 32bpp
@
BBGIsTranslucentBitmap:   
         stmfd sp!,{r4-r12,lr}
         cmp  r3,#32
         beq  IsTrans32
         cmp  r3,#24
         beq  IsTrans24
         mov  r14,#0xff000000 @ force a FALSE return value for invalid bit depths
         b    IsTransExit   @ invalid bit depth
IsTrans32:   
         mul  r12,r1,r2     @ number of pixels to process
         mov  r11,r12,LSR #2    @ process 4 at a time
         mov  r14,#0xff000000   @ mask to detect alpha != 255
IsTrans32_0:   
         ldmia r0!,{r5-r8}
         cmp  r14,#0xff000000   @ quick exit if we hit a non-opaque pixel
         bne  IsTransExit
         subs r11,r11,#1
         and  r14,r14,r5
         and  r14,r14,r6
         and  r14,r14,r7
         and  r14,r14,r8
         bne  IsTrans32_0
@ see if any odd pixels
         ands r12,r12,#3
         beq  IsTransExit
IsTrans32_1:   
         ldr  r5,[r0],#4
         subs r11,r11,#1
         and  r14,r14,r5
         bne  IsTrans32_1
         b    IsTransExit
@
@ 24bpp planar bitmap
@
IsTrans24:   
         mov  r12,r1,LSL #1     @ calculate source pitch
         add  r12,r12,#2        @ dword-align
         bic  r12,r12,#3
         mov  r14,#-1           @ test mask
         mla  r4,r12,r2,r0      @ point to alpha plane
IsTrans24_0:   
         mov  r5,r4             @ source pointer
         movs r6,r1,LSR #3      @ do 8 pixels at a time
         beq  IsTrans24_2       @ less than 8 pixels wide
IsTrans24_1:   
         ldmia r5!,{r9,r10}     @ grab 8 source pixels
         cmp  r14,#-1           @ break out early if translucent pixels found
         bne  IsTransExit2
         subs r6,r6,#1
         and  r14,r14,r9
         and  r14,r14,r10
         bne  IsTrans24_1
IsTrans24_2:   
         tst  r1,#7
         beq  IsTrans24_4
         and  r6,r1,#7
IsTrans24_3:   
         ldrsb r9,[r5],#1
         subs r6,r6,#1
         and  r14,r14,r9
         bne  IsTrans24_3
IsTrans24_4:   
         add  r4,r4,r12,LSR #1  @ point to next source line
         subs r2,r2,#1          @ decrement y count
         bne  IsTrans24_0
         b    IsTransExit2
IsTransExit:   
         cmp  r14,#0xff000000
         moveq r0,#0            @ false - bitmap is totally opaque
         movne r0,#1            @ true - bitmap has translucent pixels
         ldmia sp!,{r4-r12,pc}
IsTransExit2:   
         cmp  r14,#-1
         moveq r0,#0            @ false - bitmap is totally opaque
         movne r0,#1            @ true - bitmap has translucent pixels
         ldmia sp!,{r4-r12,pc}
@
@ Convert pixels from one format to another
@ call from C as:
@ void BBGPixelConvert(void *pSrc, void *pDest, int iWidth, int iHeight, int iSrcBPP, int iDestBPP)
@ The length is specified in pixels@ e.g. for a 32x32 bitmap, set length = 1024
@ valid source and destination bit depths are 16, 24 (planar) and 32
@
BBGPixelConvert:   
         stmfd sp!,{r4-r12,lr}
         ldr  r4,[sp,#40]    @ srcbpp
         ldr  r5,[sp,#44]    @ destbpp
         cmp  r4,r5            @ source already same as dest?
         beq  PixelConvertExit
         cmp  r2,#1            @ width < 1?
         blt  PixelConvertExit
         cmp  r3,#1            @ height < 1?
         blt  PixelConvertExit
         cmp  r4,#32        @ 32-bpp source
         beq  Pixel32Src
         cmp  r4,#24
         beq  Pixel24Src
         cmp  r4,#16
         beq  Pixel16Src
         b    PixelConvertExit    @ invalid source bit depth
Pixel32Src:   
         mov  r12,r2,LSL #1        @ dest pitch
         add  r12,r12,#2
         bic  r12,r12,#3        @ make sure dword-aligned
         cmp  r5,#24        @ 32->24?
         beq  Pixel32To24
         cmp  r5,#16        @ 32->16?
         beq  Pixel32To16
         b    PixelConvertExit    @ invalid destination bit depth
Pixel32To24:   
@ calculate dest pitch
         mul  r6,r12,r3            @ get pointer to alpha plane
         add  r5,r6,r1            @ point r5 to alpha plane
         sub  r12,r12,r2,LSL #1    @ get pitch-x*2 (delta to next line)
Pixel32To24_0:   
         bics r10,r2,#1        @ make sure horizontal count is an even number
         beq  Pixel32To24_Last @ one pixel wide
Pixel32To24_1:   
         ldmia r0!,{r6,r7}        @ get 2 source pixels
         tst  r5,#1                  @ odd dest alpha address?
         mov  r8,r6,LSR #24        @ get alpha 1
         and  r9,r7,#0xff000000    @ get alpha 2
         orr  r8,r8,r9,LSR #16    @ put in upper byte of short
         streqh r8,[r5],#2        @ store the 2 alphas
         strneb r8,[r5],#1
         movne r8,r8,LSR #8
         strneb r8,[r5],#1
         subs r10,r10,#2        @ minus 2 pixels
         and  r8,r6,#0xf8        @ get blue 1
         mov  r9,r8,LSR #3        @ blue 1 ready
         and  r8,r6,#0xfc00        @ green 1
         orr  r9,r9,r8,LSR #5    @ green 1 ready
         and  r8,r6,#0xf80000    @ red 1
         orr  r9,r9,r8,LSR #8    @ red 1 ready
         and  r8,r7,#0xf8        @ blue 2
         orr  r9,r9,r8,LSL #13
         and  r8,r7,#0xfc00        @ green 2
         orr  r9,r9,r8,LSL #11
         and  r8,r7,#0xf80000    @ red 2
         orr  r9,r9,r8,LSL #8
         str  r9,[r1],#4        @ store 2 RGB565 pixels
         bne  Pixel32To24_1
         tst  r2,#1                @ single odd pixel?
         beq  Pixel32To24_2
@ process the last pixel
Pixel32To24_Last:   
         ldr  r6,[r0],#4        @ get 1 source pixel
         mov  r8,r6,LSR #24        @ get alpha 1
         strb r8,[r5],#1        @ store the alpha
         and  r8,r6,#0xf8        @ get blue 1
         mov  r9,r8,LSR #3        @ blue 1 ready
         and  r8,r6,#0xfc00        @ green 1
         orr  r9,r9,r8,LSR #5    @ green 1 ready
         and  r8,r6,#0xf80000    @ red 1
         orr  r9,r9,r8,LSR #8    @ red 1 ready
         strh r9,[r1],#2        @ store last pixel
Pixel32To24_2:   
         add  r1,r1,r12            @ adjust with pitch delta
         add  r5,r5,r12,LSR #1    @ and alpha plane
         subs r3,r3,#1            @ vcount
         bne  Pixel32To24_0
         b    PixelConvertExit         
Pixel32To16:   
         sub  r12,r12,r2,LSL #1    @ get pitch-x*2 (delta to next line)
Pixel32To16_0:   
         bics  r10,r2,#1        @ make sure horizontal count is an even number
         beq  Pixel32To16_Last    @ one pixel wide
Pixel32To16_1:   
         ldmia r0!,{r6,r7}    @ get 2 source pixels
         subs  r10,r10,#2        @ minus 2 pixels
         and  r8,r6,#0xf8        @ get blue 1
         mov  r9,r8,LSR #3        @ blue 1 ready
         and  r8,r6,#0xfc00        @ green 1
         orr  r9,r9,r8,LSR #5    @ green 1 ready
         and  r8,r6,#0xf80000    @ red 1
         orr  r9,r9,r8,LSR #8    @ red 1 ready
         and  r8,r7,#0xf8        @ blue 2
         orr  r9,r9,r8,LSL #13
         and  r8,r7,#0xfc00        @ green 2
         orr  r9,r9,r8,LSL #11
         and  r8,r7,#0xf80000    @ red 2
         orr  r9,r9,r8,LSL #8
         str  r9,[r1],#4        @ store 2 RGB565 pixels
         bne  Pixel32To16_1
         tst  r2,#1            @ single odd pixel remaining?
         beq  Pixel32To16_2
@ process the last pixel
Pixel32To16_Last:   
         ldr  r6,[r0],#4        @ get 1 source pixel
         and  r8,r6,#0xf8        @ get blue 1
         mov  r9,r8,LSR #3        @ blue 1 ready
         and  r8,r6,#0xfc00        @ green 1
         orr  r9,r9,r8,LSR #5    @ green 1 ready
         and  r8,r6,#0xf80000    @ red 1
         orr  r9,r9,r8,LSR #8    @ red 1 ready
         strh r9,[r1],#2        @ store last pixel
Pixel32To16_2:   
         add  r1,r1,r12            @ adjust destination pointer for dword-alignment
         subs r3,r3,#1            @ vcount
         bne  Pixel32To16_0
         b    PixelConvertExit
Pixel24Src:   
         cmp  r5,#32        @ 24->32?
         bne  PixelConvertExit    @ no other valid conversion for 24bpp src
         mov  r12,r2,LSL #1        @ calculate source pitch
         add  r12,r12,#2
         bic  r12,r12,#3        @ make sure it's dword-aligned
         mul  r6,r12,r3            @ point to alpha plane
         add  r5,r6,r0            @ point r5 to alpha plane
         sub  r12,r12,r2,LSL #1    @ get delta between x*2 and pitch
Pixel24To32_0:   
         bics  r10,r2,#1        @ make sure horizontal count is an even number
         beq   Pixel24To32_Last @ width == 1
Pixel24To32_1:   
         ldr  r6,[r0],#4        @ grab 2 source pixels
         subs r10,r10,#2        @ minus 2 pixels
         and  r8,r6,#0x1f        @ get blue 1
         mov  r9,r8,LSL #3        @ upper 5 bits
         orr  r9,r9,r8,LSR #2    @ lower 3 bits are upper 3 repeated
         and  r8,r6,#0x7e0        @ get green 1
         orr  r9,r9,r8,LSL #5
         and  r8,r8,#0x600        @ upper 2 bits become lower 2
         orr  r9,r9,r8,LSR #1
         and  r8,r6,#0xf800        @ get red 1
         ldrb r7,[r5],#1        @ get first alpha
         orr  r9,r9,r8,LSL #8
         and  r8,r8,#0xe000        @ re-use upper 3 bits
         orr  r9,r9,r8,LSL #3
         orr  r9,r9,r7,LSL #24    @ first pixel ready
         and  r8,r6,#0x1f0000    @ blue 2
         mov  r11,r8,LSR #13
         and  r8,r8,#0x1c0000    @ keep 3 bits
         orr  r11,r11,r8,LSR #18
         and  r8,r6,#0x7e00000    @ green 2
         orr  r11,r11,r8,LSR #11
         and  r8,r8,#0x6000000    @ use 2 msbs
         orr  r11,r11,r8,LSR #17
         and  r8,r6,#0xf8000000    @ red 2
         ldrb r7,[r5],#1            @ get second alpha
         orr  r11,r11,r8,LSR #8
         and  r8,r8,#0xe0000000    @ get 3 MSBs
         orr  r11,r11,r8,LSR #13
         orr  r11,r11,r7,LSL #24
         stmia r1!,{r9,r11}        @ store the 2 pixels
         bne  Pixel24To32_1
         tst  r2,#1                @ odd pixel remaining?
         beq  Pixel24To32_2
@ process the last pixel
Pixel24To32_Last:   
         ldrh r6,[r0],#2           @ grab 1 source pixel
         ldrb r7,[r5],#1           @ grab 1 source alpha
         and  r8,r6,#0x1f        @ get blue 1
         mov  r9,r8,LSL #3        @ upper 5 bits
         orr  r9,r9,r8,LSR #2    @ lower 3 bits are upper 3 repeated
         and  r8,r6,#0x7e0        @ get green 1
         orr  r9,r9,r8,LSL #5
         and  r8,r8,#0x600        @ upper 2 bits become lower 2
         orr  r9,r9,r8,LSR #1
         and  r8,r6,#0xf800        @ get red 1
         orr  r9,r9,r8,LSL #8
         and  r8,r8,#0xe000        @ re-use upper 3 bits
         orr  r9,r9,r8,LSL #3
         orr  r9,r9,r7,LSL #24    @ first pixel ready
         str  r9,[r1],#4        @ store it
Pixel24To32_2:   
         add  r0,r0,r12            @ adjust source pointer
         add  r5,r5,r12,LSR #1    @ adjust alpha plane pointer
         subs r3,r3,#1            @ vcount
         bne  Pixel24To32_0
         b    PixelConvertExit
Pixel16Src:   
         cmp  r5,#32        @ 16->32?
         bne  PixelConvertExit    @ no other valid conversion for 16bpp src
         mov  r12,r2,LSL #1    @ calculate source pitch
         add  r12,r12,#2
         bic  r12,r12,#3    @ dword-align
         sub  r12,r12,r2,LSL #1    @ get delta for next line
Pixel16To32_0:   
         bics  r10,r2,#1        @ make sure horizontal count is an even number
         beq   Pixel16To32_Last    @ width == 1
Pixel16To32_1:   
         ldr  r6,[r0],#4        @ grab 2 source pixels
         subs r10,r10,#2        @ minus 2 pixels
         and  r8,r6,#0x1f        @ get blue 1
         mov  r9,r8,LSL #3        @ upper 5 bits
         orr  r9,r9,r8,LSR #2    @ lower 3 bits are upper 3 repeated
         and  r8,r6,#0x7e0        @ get green 1
         orr  r9,r9,r8,LSL #5
         and  r8,r8,#0x600        @ upper 2 bits become lower 2
         orr  r9,r9,r8,LSR #1
         and  r8,r6,#0xf800        @ get red 1
         orr  r9,r9,r8,LSL #8
         and  r8,r8,#0xe000        @ re-use upper 3 bits
         orr  r9,r9,r8,LSL #3
         orr  r9,r9,#0xff000000    @ first pixel ready
         and  r8,r6,#0x1f0000    @ blue 2
         mov  r11,r8,LSR #13
         and  r8,r8,#0x1c0000    @ keep 3 bits
         orr  r11,r11,r8,LSR #18
         and  r8,r6,#0x7e00000    @ green 2
         orr  r11,r11,r8,LSR #11
         and  r8,r8,#0x6000000    @ use 2 msbs
         orr  r11,r11,r8,LSR #17
         and  r8,r6,#0xf8000000    @ red 2
         orr  r11,r11,r8,LSR #8
         and  r8,r8,#0xe0000000    @ get 3 MSBs
         orr  r11,r11,r8,LSR #13
         orr  r11,r11,#0xff000000    @ second pixel ready
         stmia r1!,{r9,r11}        @ store the 2 pixels
         bne  Pixel16To32_1
         tst  r2,#1                @ odd pixel remaining?
         beq  Pixel16To32_2
@ process the last pixel
Pixel16To32_Last:   
         ldrh r6,[r0],#2        @ grab 1 source pixel
         and  r8,r6,#0x1f        @ get blue 1
         mov  r9,r8,LSL #3        @ upper 5 bits
         orr  r9,r9,r8,LSR #2    @ lower 3 bits are upper 3 repeated
         and  r8,r6,#0x7e0        @ get green 1
         orr  r9,r9,r8,LSL #5
         and  r8,r8,#0x600        @ upper 2 bits become lower 2
         orr  r9,r9,r8,LSR #1
         and  r8,r6,#0xf800        @ get red 1
         orr  r9,r9,r8,LSL #8
         and  r8,r8,#0xe000        @ re-use upper 3 bits
         orr  r9,r9,r8,LSL #3
         orr  r9,r9,#0xff000000    @ pixel ready
         str  r9,[r1],#4        @ store last pixel
Pixel16To32_2:   
         add  r0,r0,r12            @ adjust source pointer
         subs r3,r3,#1            @ vcount
         bne  Pixel16To32_0
         b    PixelConvertExit
PixelConvertExit:   
         ldmia sp!,{r4-r12,pc}
@
@ Table of sine values for 0-360 degrees expressed as a signed 16-bit value
@ from -32768 (-1) to 32767 (1)
@
sine: .hword 0,572, 1144, 1715, 2286, 2856, 3425, 3993, 4560, 5126  @ 0-9
  .hword  5690,  6252, 6813, 7371, 7927, 8481, 9032, 9580, 10126, 10668 @ 10-19
  .hword  11207,  11743, 12275, 12803, 13328, 13848, 14365, 14876, 15384, 15886 @ 20-29
  .hword  16384,  16877, 17364, 17847, 18324, 18795, 19261, 19720, 20174, 20622 @ 30-39
  .hword  21063,  21498, 21926, 22348, 22763, 23170, 23571, 23965, 24351, 24730 @ 40-49
  .hword  25102,  25466, 25822, 26170, 26510, 26842, 27166, 27482, 27789, 28088 @ 50-59
  .hword  28378,  28660, 28932, 29197, 29452, 29698, 29935, 30163, 30382, 30592 @ 60-69
  .hword  30792,  30983, 31164, 31336, 31499, 31651, 31795, 31928, 32052, 32166 @ 70-79
  .hword  32270,  32365, 32440, 32524, 32599, 32643, 32688, 32723, 32748, 32763 @ 80-89
  .hword  32767,  32763, 32748, 32723, 32688, 32643, 32588, 32524, 32449, 32365 @ 90-99
  .hword  32270,  32166, 32052, 31928, 31795, 31651, 31499, 31336, 31164, 30983 @ 100-109
  .hword  30792,  30592, 30382, 30163, 29935, 29698, 29452, 29197, 28932, 28660 @ 110-119
  .hword  28378,  28088, 27789, 27482, 27166, 26842, 26510, 26170, 25822, 25466 @ 120-129
  .hword  25102,  24730, 24351, 23965, 23571, 23170, 22763, 22348, 21926, 21498 @ 130-139
  .hword  21063,  20622, 20174, 19720, 19261, 18795, 18324, 17847, 17364, 16877 @ 140-149
  .hword  16384,  15886, 15384, 14876, 14365, 13848, 13328, 12803, 12275, 11743 @ 150-159
  .hword  11207,  10668, 10126, 9580, 9032, 8481, 7927, 7371, 6813, 6252 @ 160-169
  .hword  5690,  5126, 4560, 3993, 3425, 2856, 2286, 1715, 1144, 572   @ 170-179
  .hword  0,  -572, -1144, -1715, -2286, -2856, -3425, -3993, -4560, -5126 @ 180-189
  .hword  -5690,  -6252, -6813, -7371, -7927, -8481, -9032, -9580, -10126, -10668 @ 190-199
  .hword  -11207,  -11743, -12275, -12803, -13328, -13848, -14365, -14876, -15384, -15886 @ 200-209
  .hword  -16384,  -16877, -17364, -17847, -18324, -18795, -19261, -19720, -20174, -20622 @ 210-219
  .hword  -21063,  -21498, -21926, -22348, -22763, -23170, -23571, -23965, -24351, -24730 @ 220-229
  .hword  -25102,  -25466, -25822, -26170, -26510, -26842, -27166, -27482, -27789, -28088 @ 230-239
  .hword  -28378,  -28660, -28932, -29196, -29452, -29698, -29935, -30163, -30382, -30592 @ 240-249
  .hword  -30792,  -30983, -31164, -31336, -31499, -31651, -31795, -31928, -32052, -32166 @ 250-259
  .hword  -32270,  -32365, -32449, -32524, -32588, -32643, -32688, -32723, -32748, -32763 @ 260-269
  .hword  -32768,  -32763, -32748, -32723, -32688, -32643, -32588, -32524, -32449, -32365 @ 270-279
  .hword  -32270,  -32166, -32052, -31928, -31795, -31651, -31499, -31336, -31164, -30983 @ 280-289
  .hword  -30792,  -30592, -30382, -30163, -29935, -29698, -29452, -29196, -28932, -28660 @ 290-299
  .hword  -28378,  -28088, -27789, -27482, -27166, -26842, -26510, -26170, -25822, -25466 @ 300-309
  .hword  -25102,  -24730, -24351, -23965, -23571, -23170, -22763, -22348, -21926, -21498 @ 310-319
  .hword  -21063,  -20622, -20174, -19720, -19261, -18795, -18234, -17847, -17364, -16877 @ 320-329
  .hword  -16384,  -15886, -15384, -14876, -14365, -13848, -13328, -12803, -12275, -11743 @ 330-339
  .hword  -11207,  -10668, -10126, -9580, -9032, -8481, -7927, -7371, -6813, -6252 @ 340-349
  .hword  -5690,  -5126, -4560, -3993, -3425, -2856, -2286, -1715, -1144, -572@ 350-359
@ an extra 90 degrees to simulate the cosine function
  .hword  0,572,  1144, 1715, 2286, 2856, 3425, 3993, 4560, 5126  @ 0-9
  .hword  5690,  6252, 6813, 7371, 7927, 8481, 9032, 9580, 10126, 10668 @ 10-19
  .hword  11207,  11743, 12275, 12803, 13328, 13848, 14365, 14876, 15384, 15886 @ 20-29
  .hword  16384,  16877, 17364, 17847, 18324, 18795, 19261, 19720, 20174, 20622 @ 30-39
  .hword  21063,  21498, 21926, 22348, 22763, 23170, 23571, 23965, 24351, 24730 @ 40-49
  .hword  25102,  25466, 25822, 26170, 26510, 26842, 27166, 27482, 27789, 28088 @ 50-59
  .hword  28378,  28660, 28932, 29197, 29452, 29698, 29935, 30163, 30382, 30592 @ 60-69
  .hword  30792,  30983, 31164, 31336, 31499, 31651, 31795, 31928, 32052, 32166 @ 70-79
  .hword  32270,  32365, 32440, 32524, 32599, 32643, 32688, 32723, 32748, 32763 @ 80-89
  .end    
