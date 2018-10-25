	dc.l $00000000,	EntryPoint,	ErrorTrap,	ErrorTrap	;00
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;10
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;20
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;30
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;40
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;50
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;60
	dc.l HInt,	ErrorTrap,	VInt,		ErrorTrap	;70
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;80
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;90
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;A0
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;B0
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;C0
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;D0
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;E0
	dc.l ErrorTrap,	ErrorTrap,	ErrorTrap,	ErrorTrap	;F0

check_sum equ	$0000
;------------------------------------------------------------------------
	dc.b	'SEGA GENESIS    '	;100
	dc.b	'(C)T-xx 2008.01 '	;110 release year.month
	dc.b	'Sprite Masking T'	;120 Japan title
	dc.b	'est ROM         '	;130
	dc.b	'                '	;140
	dc.b	'Sprite Masking T'	;150 US title
	dc.b	'est ROM         '	;160
	dc.b	'                '	;170
	dc.b	'GM T-XXXXXX XX'  	;180 product #, version
	dc.w	check_sum         	;18E check sum
	dc.b	'J               '	;190 controller
	dc.l	$00000000,$0007ffff,$00ff0000,$00ffffff		;1A0
	dc.b	'                '	;1B0
	dc.b	'                '	;1C0
	dc.b	'                '	;1D0
	dc.b	'                '	;1E0
	dc.b	'JUE             '	;1F0

ErrorTrap:
	jmp	ErrorTrap
HInt:
VInt:
	rte

EntryPoint:
	;Set the initial VDP register state
	move.w #$8004,$C00004	;Enable the palette
	move.w #$8144,$C00004	;Enable the display/mode 5
	move.w #$8230,$C00004	;Set the scroll A name table base to 0xC000
	move.w #$8405,$C00004	;Set the scroll B name table base to 0xA000
	move.w #$8570,$C00004	;Set the sprite table base to 0xE000
	move.w #$8D3F,$C00004	;Set the hscroll base to 0xFC00
	move.w #$9001,$C00004	;Set the scroll size to V32 H64
	move.w #$8F02,$C00004	;Set the auto-increment data to 2

;Initialize CRAM
	move.l	#$C0000000,$C00004
	move.w	#$3F,D0
	move.l	#CRAMData,A0
	CRAMLoop:
	move.w	(A0)+,$C00000
	dbf	D0,CRAMLoop

;Initialize VSRAM
	move.l	#$40000010,$C00004
	move.w	#$3F,D0
	VSRAMLoop:
	move.w	#$00,$C00000
	dbf	D0,VSRAMLoop

H32SpriteTest:
	;H32 mode: 64 sprites per frame, 16 sprites per line, 256 pixels per line
	move.w #$8C00,$C00004	;Set the screen size to H32

	move.w #$8104,$C00004	;Disable the display
	;Initialize VRAM
	move.l	#$40000000,$C00004
	move.w	#$7FFF,D0
	move.l	#VRAMDataH32,A0
	VRAMLoopH32:
	move.w	(A0)+,$C00000
	dbf	D0,VRAMLoopH32
	move.w #$8144,$C00004	;Enable the display/mode 5

	bsr WaitForStart

H40SpriteTest:
	;H40 mode: 80 sprites per frame, 20 sprites per line, 320 pixels per line
	move.w #$8C81,$C00004	;Set the screen size to H40

	move.w #$8104,$C00004	;Disable the display
	;Initialize VRAM
	move.l	#$40000000,$C00004
	move.w	#$7FFF,D0
	move.l	#VRAMDataH40,A0
	VRAMLoopH40:
	move.w	(A0)+,$C00000
	dbf	D0,VRAMLoopH40
	move.w #$8144,$C00004	;Enable the display/mode 5

	bsr WaitForStart
	jmp	H32SpriteTest


WaitForStart:
	move.b	#0, $A10003
	nop
	nop
	nop
	nop
	WaitForStartButtonRelease:
	btst.b	#5, $A10003
	beq	WaitForStartButtonRelease
	WaitForStartButtonPress:
	btst.b	#5, $A10003
	bne	WaitForStartButtonPress
	rts

CRAMData:
	incbin "cram.bin"

VRAMDataH32:
	incbin "vram - h32.bin"

VRAMDataH40:
	incbin "vram - h40.bin"
