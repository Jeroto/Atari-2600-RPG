    processor 6502

    include "vcs.h"
    include "macro.h"

    seg.u Variables
    org $80

NTSC = 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Room                .byte
Scene               .byte


PlayerHp            .byte
EnemyHp             .byte
PlayerMaxHp         .byte

Level               .byte
PlayerAtk           .byte
PlayerDef           .byte
PlayerSpd           .byte
PlayerExp           .byte
NextLvlExp          .byte

EnemyAtk            .byte
EnemyDef            .byte
EnemySpd            .byte

BattleFlags         .byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Battle Flag Meanings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; D0 = turn
;; D1 = attack selected
;; D2 = player defending this turn
;; D3 = 
;; D4 = Has Sword
;; D5 = Has Shield
;; D6 = Has Armor
;; D7 = Has Trophy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlayerXPos          .byte
PlayerYPos          .byte
ItemYPos            .byte

Random              .byte
Temp                .byte
ItemOffset          .byte
Timer               .byte
TurnSpeed           .byte

PF0Swap             .byte
PF1Swap             .byte
PF2Swap             .byte

NumSpriteOne           .byte        ; used to store bit pattern for player hp
NumSpriteOneTemp       .byte        ; used to store bit pattern for player hp before updating main value
NumSpriteTwo           .byte        ; used to store bit pattern for enemy hp
NumSpriteTwoTemp       .byte        ; used to store bit pattern for enemy hp before updating main value

OnesDigitOffset     .word
TensDigitOffset     .word

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BASE_HP     = $10
BASE_ATK    = $3
BASE_DEF    = $1
BASE_SPD    = $1

BLACK       = $00
DARK_GREY   = $02
GREY        = $06
LIGHT_GREY  = $08
WHITE       = $0E

CASTLE      = $06


    IF NTSC
RED         = $46
LIGHT_RED   = $4A
BLUE        = $84
YELLOW      = $1E
PURPLE      = $66
GREEN       = $C6
LIGHT_GREEN = $CA

GRASS       = $C2
LIGHT_GRASS = GRASS + $02
DARK_GRASS  = GRASS - $02

TREE        = $D0
LIGHT_TREE  = TREE + $02
DARK_TREE   = $10

WATER       = $70
ITEM        = $1E
PIT         = $02

    ELSE
RED         = $64
BLUE        = $B4
YELLOW      = $2E
PURPLE      = $C6
GREEN       = $56
    ENDIF
DARK_BLUE   = BLUE - $04
LIGHT_BLUE  = BLUE + $16


DIGITS_HEIGHT = 5
PLAYER_HEIGHT = 4
ITEM_HEIGHT = 8

    seg Code
    org $F000

Reset:
    CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #68
    sta PlayerXPos
    lda #30
    sta PlayerYPos
    lda #20
    lda #%11010100
    sta Random               ; Random = $D4

    lda #5
    sta Room
    lda #2
    sta Scene
    lda #255
    sta Timer

    lda #0
    sta PlayerExp
    lda #1
    sta Level
    sta NextLvlExp
    lda #BASE_HP
    sta PlayerMaxHp
    sta PlayerHp
    lda #BASE_ATK
    sta PlayerAtk
    lda #BASE_DEF
    sta PlayerDef
    lda #BASE_SPD
    sta PlayerSpd

    lda #%00000010
    sta BattleFlags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table adresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



Frame:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK
    sta VSYNC                ; turn on VSYNC
    
    ldx #3
    jsr SleepForLines        ; display 3 recommended lines of VSYNC

    lda #0
    sta VSYNC                ; turn off VSYNC

    ldx #32
    jsr SleepForLines        ; display the recommended lines of VBLANK (37-5 for calculations)

    
    lda #0
    sta GRP1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed during the VBLANK section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda PlayerXPos
    ldy #1
    jsr SetObjectXPos           ; set P1 horizontal position (1 WSYNC)
    lda #60                     ; Items will always be at the midpoint of the screen
    ldy #0
    jsr SetObjectXPos           ; set P0 horizontal position (1 WSYNC)

    jsr CalculateDigitOffset    ; calculate the scoreboard digit lookup table offset

    sta WSYNC
    sta HMOVE

    ldy Scene

    lda #0
    sta VBLANK                  ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the remaining visible scanlines of our main game (4-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    cpy #0
    bne OtherSceneJump
    
    lda #100
    sta ItemYPos

    lda #%00000111
    sta NUSIZ0

    ldx #YELLOW
    stx COLUP0
    lda #%01000000
    bit BattleFlags
    bne MakePlayerYellow
    ldx #RED
MakePlayerYellow:
    stx COLUP1

    lda #%001100001
    sta CTRLPF              ; enable playfield reflection

    ldy Room
    cpy #32
    beq LakeJump
    cpy #45
    beq PitsJump
    cpy #104
    beq TreeJump
    cpy #109
    beq FountainJump
    cpy #5
    bne ForestJump

    jmp InitCastle

ForestJump:
    jmp InitForest
LakeJump:
    jmp InitLake
PitsJump:
    jmp InitPits
TreeJump:
    jmp InitTree
FountainJump:
    jmp InitFountain
OtherSceneJump:
    cpy #1
    beq FightRendJump
    cpy #3
    beq GameOverJump
    cpy #4
    beq VictoryJump
    jmp LevelSceneRend
FightRendJump:
    jmp FightSceneRend
GameOverJump:
    jmp GameOverSceneRend
VictoryJump:
    jmp VictorySceneRend

InitCastle:
    lda #PIT
    sta COLUBK
    lda #CASTLE
    sta COLUPF

    lda #0                     ; full kernel offset
    jsr DrawBackgroundFullPF

    jmp EndDraw

InitForest:
    cpy #33
    bmi .LightForestColors
    cpy #77
    bpl InitDarkForest
    lda #GRASS
    sta COLUBK
    lda #TREE
    sta COLUPF
    jmp .DrawForest

.LightForestColors:
    lda #LIGHT_GRASS
    sta COLUBK
    lda #LIGHT_TREE
    sta COLUPF
    jmp .DrawForest
.DrawForest:
    lda #48                     ; full kernel offset
    jsr DrawBackgroundFullPF
    jmp EndDraw

InitDarkForest:
    lda #DARK_GRASS
    sta COLUBK
    lda #DARK_TREE
    sta COLUPF
    
    lda #0                     ; full kernel offset
    jsr DrawBackgroundMostPF
    jmp EndDraw

InitLake:
    lda #LIGHT_GRASS
    sta COLUBK
    lda #WATER
    sta COLUPF

    lda #%00010000
    bit BattleFlags
    bne DrawSwordOffscreen
    lda #18
    sta ItemYPos
DrawSwordOffscreen:
    lda #8
    sta ItemOffset
    
    lda #48                     ; full kernel offset
    jsr DrawBackgroundMostPF
    jmp EndDraw
    
InitPits:
    lda #GRASS
    sta COLUBK
    lda #PIT
    sta COLUPF
    
    lda #%00100000
    bit BattleFlags
    bne DrawShieldOffscreen
    lda #21
    sta ItemYPos
DrawShieldOffscreen:
    lda #16
    sta ItemOffset
    
    lda #96                     ; full kernel offset
    jsr DrawBackgroundMostPF
    jmp EndDraw
    
InitTree:
    lda #DARK_GRASS
    sta COLUBK
    lda #DARK_TREE
    sta COLUPF

    lda #%01000000
    bit BattleFlags
    bne DrawArmorOffscreen
    lda #9
    sta ItemYPos
DrawArmorOffscreen:
    lda #24
    sta ItemOffset
    
    lda #144                     ; full kernel offset
    jsr DrawBackgroundMostPF
    jmp EndDraw

    
InitFountain:
    lda #DARK_GRASS
    sta COLUBK
    lda #CASTLE
    sta COLUPF
    
    lda #%10000000
    bit BattleFlags
    bne DrawTrophyOffscreen
    lda #25
    sta ItemYPos
DrawTrophyOffscreen:
    lda #32
    sta ItemOffset
    
    lda #192                     ; full kernel offset
    jsr DrawBackgroundMostPF
    jmp EndDraw

FightSceneRend:
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta COLUBK

    lda #%00000001
    bit BattleFlags
    bne PlayerTurnColors

    lda #GREEN
    sta COLUP0
    lda #LIGHT_RED
    sta COLUP1
    jmp TurnColorsSet
PlayerTurnColors:
    lda #LIGHT_GREEN
    sta COLUP0
    lda #RED
    sta COLUP1
TurnColorsSet:

    lda #%00000010
    sta CTRLPF

    ldx #3
    jsr SleepForLines

    
    ldy #4
.TopTextLoop:
    sta WSYNC
    dey

    jsr DrawBattleTopText
    sta WSYNC
    jsr DrawBattleTopText
    sta WSYNC
    jsr DrawBattleTopText
    sta WSYNC
    jsr DrawBattleTopText

    cpy #0
    beq .EndTopTextLoop
    jmp .TopTextLoop

.EndTopTextLoop

    sta WSYNC


    lda #0
    sta PF0
    sta PF1
    sta PF2

    ldx #4
    jsr SleepForLines

    ldy #5
.HPDrawLoop
    dey
    lda HPText,Y        ; 4
    sta PF1             ; 3
    ldx #4
    jsr SleepForLines
    cpy #0
    bne .HPDrawLoop
    lda #0
    sta PF0
    sta PF1
    sta PF2

    sta WSYNC

    ldy TensDigitOffset         ; get the tens digit offset for the player hp
    lda Digits,Y                ; load the bit pattern from the lookup table
    and #$F0                    ; mask/remove the graphics for the ones digit
    sta NumSpriteOne            ; save the player hp tens digit pattern

    sta WSYNC
    
    ldy OnesDigitOffset         ; get the ones digit offset for the player hp
    lda Digits,Y                ; load the bit pattern from the lookup table
    and #$0F                    ; mask/remove the graphics for the tens digit
    ora NumSpriteOne            ; merge with the saved tens digit sprite
    sta NumSpriteOne            ; and save it

    sta WSYNC
    
    ldy TensDigitOffset+1       ; get the tens digit offset for the enemy hp
    lda Digits,Y                ; load the bit pattern from the lookup table
    and #$F0                    ; mask/remove the graphics for the ones digit
    sta NumSpriteTwo            ; save the enemy hp tens digit pattern

    
    inc TensDigitOffset
    inc TensDigitOffset+1

    sta WSYNC
    
    ldy OnesDigitOffset+1       ; get the ones digit offset for the enemy hp
    lda Digits,Y                ; load the bit pattern from the lookup table
    and #$0F                    ; mask/remove the graphics for the tens digit
    ora NumSpriteTwo            ; merge with the saved tens digit sprite
    sta NumSpriteTwo            ; and save it

    inc OnesDigitOffset
    inc OnesDigitOffset+1

    ; Draw numbers instead of HP text below

    ldx #5
.NumDrawLoop
    lda NumSpriteOne            ; 3  3
    sta PF1                     ; 3  6

    ldy TensDigitOffset         ; 3  9   get the tens digit offset for the playerhp
    lda Digits,Y                ; 4  13   load the bit pattern from the lookup table
    and #$F0                    ; 2  15   mask/remove the graphics for the ones digit
    sta NumSpriteOneTemp        ; 3  18  save the player hp tens digit pattern

    
    lda NumSpriteTwo            ; 3  45
    sta PF1                     ; 3  48

    sta WSYNC

    lda NumSpriteOne            ; 3  3
    sta PF1                     ; 3  6

    ldy OnesDigitOffset         ; 3  9  get the ones digit offset for the player hp
    lda Digits,Y                ; 4  13  load the bit pattern from the lookup table
    and #$0F                    ; 2  15  mask/remove the graphics for the tens digit
    ora NumSpriteOneTemp        ; 3  18  merge with the saved tens digit sprite
    sta NumSpriteOneTemp        ; 3  21  and save it

    jsr Sleep12Cycles           ; 12 33
    jsr Sleep12Cycles           ; 12 30
    
    lda NumSpriteTwo            ; 3  48
    sta PF1                     ; 3  51

    sta WSYNC                   ; wait for the end of scanline

    lda NumSpriteOne            ; 3  3
    sta PF1                     ; 3  6

    ldy TensDigitOffset+1       ; get the tens digit offset for the enemy hp
    lda Digits,Y                ; load the bit pattern from the lookup table
    and #$F0                    ; mask/remove the graphics for the ones digit
    sta NumSpriteTwoTemp        ; save the enemy hp tens digit pattern

    jsr Sleep12Cycles           ; 12 30
    jsr Sleep12Cycles           ; 12 30
    
    lda NumSpriteTwo            ; 3  45
    sta PF1                     ; 3  48

    sta WSYNC

    ldy OnesDigitOffset+1       ; get the ones digit offset for the enemy hp
    lda Digits,Y                ; load the bit pattern from the lookup table
    and #$0F                    ; mask/remove the graphics for the tens digit
    ora NumSpriteTwoTemp        ; merge with the saved tens digit sprite
    sta NumSpriteTwoTemp        ; and save it

    lda NumSpriteOne            ; 3  3
    sta PF1                     ; 3  6

    jsr Sleep12Cycles           ; 12 30

    lda NumSpriteOneTemp        ; 3
    sta NumSpriteOne            ; 3
    
    ldy NumSpriteTwoTemp
    ldy NumSpriteTwoTemp
    
    lda NumSpriteTwo            ; 3  45
    sta PF1                     ; 3  48

    sty NumSpriteTwo

    inc TensDigitOffset
    inc TensDigitOffset+1

    sta WSYNC


    inc OnesDigitOffset
    inc OnesDigitOffset+1

    dex
    cpx #0
    bne .NumDrawLoop

    lda #0
    sta PF0
    sta PF1
    sta PF2

    sta WSYNC

    lda #%00000010
    bit BattleFlags
    bne AttackDark
    
    lda #%00000001
    bit BattleFlags
    beq AttackDark

    lda #WHITE
    jmp AttackColSet
AttackDark:
    lda #DARK_GREY
AttackColSet:
    sta COLUPF
    lda #0
    sta CTRLPF

    ldx #3
    jsr SleepForLines


    ldy #6
.AttackTextLoop:
    dey

    jsr DrawAttackText
    sta WSYNC
    jsr DrawAttackText
    sta WSYNC
    jsr DrawAttackText
    sta WSYNC
    jsr DrawAttackText
    sta WSYNC
    
    cpy #0
    beq .EndAttackTextLoop
    jmp .AttackTextLoop

.EndAttackTextLoop:

    lda #0
    sta PF0
    sta PF1
    sta PF2

    ldx #8
    jsr SleepForLines  

    lda #%00000010
    bit BattleFlags
    beq DefendDark

    lda #%00000001
    bit BattleFlags
    beq DefendDark

    lda #WHITE
    jmp DefendColSet
DefendDark:
    lda #DARK_GREY
DefendColSet:
    sta COLUPF

    ldx #4
    jsr SleepForLines  


    ldy #6
.DefendTextLoop:
    dey

    jsr DrawDefendText
    sta WSYNC
    jsr DrawDefendText
    sta WSYNC
    jsr DrawDefendText
    sta WSYNC
    jsr DrawDefendText
    sta WSYNC

    cpy #0
    beq .EndDefendTextLoop
    jmp .DefendTextLoop

.EndDefendTextLoop:

    lda #0
    sta PF0
    sta PF1
    sta PF2

    ; 15 more sets of 4

    ldx #60
    jsr SleepForLines 

    jmp EndDraw





LevelSceneRend:
    lda #0
    sta COLUBK

    ldx #3
    jsr SleepForLines
    
    lda #LIGHT_GREEN
    sta COLUPF

    sta WSYNC

    ldy #5
LevelUpText:
    dey

    jsr DrawLevelUpText
    sta WSYNC
    jsr DrawLevelUpText
    sta WSYNC
    jsr DrawLevelUpText
    sta WSYNC
    jsr DrawLevelUpText
    sta WSYNC

    cpy #0
    beq EndLevelUpText
    jmp LevelUpText
EndLevelUpText:

    lda #0
    sta PF0
    sta PF1
    sta PF2

    
    lda #GREEN
    sta COLUPF

    ldx #4
    jsr SleepForLines

    ldx Level
    jsr CalculateMiscDigitOffset
    jsr PrepareNumsOverThreeLines
    lda #0
    jsr LevelScreenNumLoop

    lda #RED
    sta COLUPF

    ldx #12
    jsr SleepForLines
    
    ldx PlayerAtk
    jsr CalculateMiscDigitOffset
    jsr PrepareNumsOverThreeLines
    lda #5
    jsr LevelScreenNumLoop
    
    lda #BLUE
    sta COLUPF
    ldx #4
    jsr SleepForLines

    ldx PlayerDef
    jsr CalculateMiscDigitOffset
    jsr PrepareNumsOverThreeLines
    lda #10
    jsr LevelScreenNumLoop
    
    lda #LIGHT_BLUE
    sta COLUPF
    ldx #4
    jsr SleepForLines

    ldx PlayerSpd
    jsr CalculateMiscDigitOffset
    jsr PrepareNumsOverThreeLines
    lda #15
    jsr LevelScreenNumLoop

    
    
    lda #PURPLE
    sta COLUPF
    ldx #4
    jsr SleepForLines

    ldx PlayerMaxHp
    jsr CalculateMiscDigitOffset
    jsr PrepareNumsOverThreeLines
    lda #20
    jsr LevelScreenNumLoop

    ldx #40
    jsr SleepForLines
    jmp EndDraw

GameOverSceneRend:
    lda #0
    jsr DrawVictoryAndGameOver
    jmp EndDraw
    
VictorySceneRend:
    lda #6
    jsr DrawVictoryAndGameOver
    jmp EndDraw

EndDraw:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VBLANK Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK again to display overscan

    ldx #30
    jsr SleepForLines        ; display recommended lines of overscan


    lda #0
    sta VBLANK               ; turn off VBLANK

    ldy Scene
    lda #%10000000
    bit BattleFlags
    beq CheckScene
    ldx Room
    cpx #5
    bne CheckScene
    ldy #4
    sty Scene

CheckScene:
    cpy #0
    beq OverworldCode
    cpy #1
    beq BattleJump
    cpy #3
    beq GameOverCode
    cpy #4
    beq VictoryCode


CheckLevelScreenButton:
    lda #%10000000
    bit INPT4               ; if button is pressed
    bne KeepLevelScene      ; if pattern doesn't match, skip
    lda #0
    sta Scene
KeepLevelScene:
    jmp Frame

GameOverCode:
    lda #%10000000
    bit INPT4               ; if button is pressed
    bne KeepGameOverScene      ; if pattern doesn't match, skip
    jmp Reset
KeepGameOverScene:
    jmp Frame
    
VictoryCode:
    lda #%10000000
    bit INPT4               ; if button is pressed
    bne KeepVictoryScene    ; if pattern doesn't match, skip
    jmp Reset
KeepVictoryScene:
    jmp Frame

BattleJump:
    jmp BattleCode


OverworldCode:

    lda #%10000000
    bit CXPPMM
    beq CheckP0Up

    sta CXCLR

    lda Room
    cmp #45
    beq ShieldCollect
    cmp #104
    beq ArmorCollect
    cmp #109
    beq TrophyCollect

SwordCollect:
    lda #%00010000
    ora BattleFlags
    sta BattleFlags
    lda PlayerAtk
    sed
    clc
    adc #5
    cld
    sta PlayerAtk
    jmp CheckP0Up
    
ShieldCollect:
    lda #%00100000
    ora BattleFlags
    sta BattleFlags
    jmp CheckP0Up

ArmorCollect:
    lda #%01000000
    ora BattleFlags
    sta BattleFlags
    lda PlayerDef
    sed
    clc
    adc #5
    cld
    sta PlayerDef
    jmp CheckP0Up
    
TrophyCollect:
    lda #%10000000
    ora BattleFlags
    sta BattleFlags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000      ; player0 joystick up check pattern
    bit SWCHA
    bne CheckP0Down     ; if pattern doesn't match, skip logic
    
    jsr GetRandomNum
    jsr BeginBattle

    lda PlayerYPos
    cmp #45
    bpl .MoveScreenUp

    inc PlayerYPos
    jmp CheckP0Left
    
.MoveScreenUp:
    ldy Room
    cpy #11
    bmi CheckP0Left
    tya
    sec
    sbc #11
    sta Room
    lda #2
    sta PlayerYPos
    jmp EndInputCheck



CheckP0Down:
    lda #%00100000      ; player0 joystick down check pattern
    bit SWCHA
    bne CheckP0Left     ; if pattern doesn't match, skip logic
    
    jsr GetRandomNum
    jsr BeginBattle

    lda PlayerYPos
    cmp #2
    bmi .MoveScreenDown

    dec PlayerYPos
    jmp CheckP0Left

.MoveScreenDown:
    ldy Room
    cpy #99
    bpl CheckP0Left
    tya
    adc #11
    sta Room
    lda #45
    sta PlayerYPos
    jmp EndInputCheck



CheckP0Left:
    lda #%01000000      ; player0 joystick left check pattern
    bit SWCHA
    bne CheckP0Right    ; if pattern doesn't match, skip logic

    jsr GetRandomNum
    jsr BeginBattle

    lda PlayerXPos
    cmp #1
    beq .MoveScreenLeft

    dec PlayerXPos
    jmp EndInputCheck

.MoveScreenLeft:
    ldy Room
    tya
    sec                      ; make sure carry-flag is set before subtracion
.Div11LeftLoop:
    sbc #11                  ; subtract 11 from accumulator
    bcs .Div11LeftLoop       ; loop until carry-flag is clear
    cmp #245
    beq EndInputCheck

    dec Room

    lda #145
    sta PlayerXPos
    jmp EndInputCheck



CheckP0Right:
    lda #%10000000      ; player0 joystick right check pattern
    bit SWCHA
    bne EndInputCheck   ; if pattern doesn't match, skip logic

    jsr GetRandomNum
    jsr BeginBattle

    lda PlayerXPos
    cmp #145
    beq .MoveScreenRight

    inc PlayerXPos
    jmp EndInputCheck

.MoveScreenRight:
    ldy Room
    iny
    tya
    sec                      ; make sure carry-flag is set before subtracion
.Div11RightLoop:
    sbc #11                  ; subtract 11 from accumulator
    bcs .Div11RightLoop       ; loop until carry-flag is clear
    cmp #245
    beq EndInputCheck

    inc Room

    lda #1
    sta PlayerXPos
    jmp EndInputCheck

EndInputCheck:

    lda Room
    cmp #5
    bne NoRestore
    lda PlayerMaxHp
    sta PlayerHp
NoRestore:

    jmp Frame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


BattleCode:
    lda Timer
    cmp #0
    beq CheckBattleEnd
    dec Timer
    jmp Frame

CheckBattleEnd:
    lda PlayerHp
    cmp #0
    beq PlayerDead
    lda EnemyHp
    cmp #0
    beq BattleWon
    jmp CheckTurn

PlayerDead:
    lda #3
    sta Scene
    jmp Frame

BattleWon:
    lda #255
    sta Timer
    lda #0
    sta Scene

    ldy Room
    lda #%10000000
    bit BattleFlags
    beq KeepRoomExpCalc
    lda #173
    sec
    sbc Room
    tay
KeepRoomExpCalc:
    tya

    lsr
    lsr
    lsr
    jsr DecompressNum
    cmp #0
    bpl KeepGivenExp
    lda #1
KeepGivenExp:
    sed
    adc PlayerExp
    bvc KeepExp
    lda #$99
KeepExp:
    sta PlayerExp
    
CheckLevelUp:
    lda PlayerExp
    cmp NextLvlExp
    bmi KeepLevel

    sbc NextLvlExp
    sta PlayerExp

    lda Level
    clc
    adc #1
    bvc KeepPLvl
    lda #$99
KeepPLvl:
    sta Level

    lda PlayerAtk
    clc
    adc #1
    bvc KeepPAtk
    lda #$99
KeepPAtk:
    sta PlayerAtk
    
    lda PlayerDef
    clc
    adc #1
    bvc KeepPDef
    lda #$99
KeepPDef:
    sta PlayerDef
    
    lda PlayerSpd
    clc
    adc #1
    bvc KeepPSpd
    lda #$99
KeepPSpd:
    sta PlayerSpd
    
    lda PlayerMaxHp
    clc
    adc #1
    bvc KeepPMaxHp
    lda #$99
KeepPMaxHp:
    sta PlayerMaxHp
    sta PlayerHp
    
    lda #%00000001
    bit Level
    lda NextLvlExp
    beq KeepNextExp

    lda NextLvlExp
    clc
    adc #1
    bvc KeepNextExp
    lda #$99
KeepNextExp:
    sta NextLvlExp
    
    lda #2
    sta Scene

    jmp CheckLevelUp

KeepLevel:
    cld

    jmp Frame

CheckTurn:
    lda #%00000001
    bit BattleFlags
    bne PlayerTurn


EnemyTurn:
    sed
    lda EnemyAtk
    sec
    sbc PlayerDef
    bcc .EDamageIsZero

    sta Temp
    lda #%00000100
    bit BattleFlags
    bne .PlayerDefending
    jmp .DamagePlayer

.PlayerDefending:
    lda Temp
    sec
    sbc PlayerDef
    sta Temp
    bcc .EDamageIsZero
    
    lda #%00100000
    bit BattleFlags
    bne .EDamageIsZero

    jmp .DamagePlayer

.EDamageIsZero:
    lda #0
    sta Temp

.DamagePlayer
    lda PlayerHp
    sec
    sbc Temp
    bcs .PlayerAlive
    lda #120
    sta Timer
    lda #0
.PlayerAlive:
    sta PlayerHp
    cld


EndEnemyTurn:
    sed
    lda PlayerSpd
    adc #6
    sta Temp
    lda TurnSpeed
    sec
    sbc Temp
    sta TurnSpeed
    bcs KeepETurn

SwitchETurn:
    lda BattleFlags
    ora #%00000001
    sta BattleFlags
    lda PlayerSpd
    sta TurnSpeed
KeepETurn:
    cld

    lda #15
    sta Timer
    lda BattleFlags
    and #%11111011
    sta BattleFlags

    jmp Frame




PlayerTurn:

CheckBattleDirections:
    lda #%00010000      ; player0 joystick up check pattern
    bit SWCHA
    beq .SwitchSelection     ; if pattern matches, switch selection
    lda #%00100000      ; player0 joystick down check pattern
    bit SWCHA
    beq .SwitchSelection     ; if pattern matches, switch selection
    jmp .CheckButton
.SwitchSelection:
    lda BattleFlags
    eor #%00000010
    sta BattleFlags

    lda #10
    sta Timer
    jmp Frame

.CheckButton:
    lda #%10000000
    bit INPT4           ; if button is pressed
    bne NoButton        ; if pattern doesn't match, skip

    lda #%00000010
    bit BattleFlags
    beq .PlayerAttack

.Defend:
    lda BattleFlags
    ora #%00000100
    sta BattleFlags
    jmp EndPlayerTurn

.PlayerAttack:
    sed
    lda PlayerAtk
    sec
    sbc EnemyDef
    bcc .PDamageIsZero
    cmp #0
    beq .PDamageIsZero
    sta Temp
    jmp .DamageEnemy
    
.PDamageIsZero:
    lda #1
    sta Temp

.DamageEnemy:

    lda EnemyHp
    sec
    sbc Temp
    bcs .EnemyAlive
    lda #120
    sta Timer
    lda #0
.EnemyAlive:
    sta EnemyHp
    cld
    lda BattleFlags
    and #%11111011
    sta BattleFlags

EndPlayerTurn:
    sed
    lda EnemySpd
    adc #6
    sta Temp
    lda TurnSpeed
    sec
    sbc Temp
    sta TurnSpeed
    bcs KeepPTurn

SwitchPTurn:
    lda BattleFlags
    and #%11111110
    sta BattleFlags
    lda EnemySpd
    sta TurnSpeed
KeepPTurn:
    cld
    
    lda #15
    sta Timer

    jmp Frame

NoButton:

    jmp Frame







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw GameOver and Victory screens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;; A is offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawVictoryAndGameOver subroutine
    ldx #84
    jsr SleepForLines

    ldy #6
    sty Temp
    clc
    adc Temp
    sta Temp
    cmp #12
    beq VictoryColors

GameOverColors:
    lda #0
    sta PF0
    ldx #RED
    jmp SetColors

VictoryColors:
    lda #$90
    ldx #GREEN

SetColors:
    sta COLUBK
    stx COLUPF

    lda #00000001
    sta CTRLPF

    dec Temp
    ldx Temp

GameOverVictoryLoop:
    sta WSYNC
    jsr GameOverVictoryLine
    dec Temp
    sta WSYNC
    jsr GameOverVictoryLine
    dey
    sta WSYNC
    jsr GameOverVictoryLine
    sta WSYNC
    jsr GameOverVictoryLine
    ldx Temp
    cpy #0
    bne GameOverVictoryLoop

    lda #0
    sta PF0
    sta PF1
    sta PF2
    
    ldx #84
    jsr SleepForLines
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw GameOver and Victory lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOverVictoryLine subroutine  ; 6  6
    lda GameOverPF1L,X          ; 4  10
    sta PF1                     ; 3  13
    lda GameOverPF2L,X          ; 4  17
    sta PF2                     ; 3  20
    sta PF2                     ; 3  23
    sta PF2                     ; 3  26
    sta PF2                     ; 3  29
    sta PF2                     ; 3  32
    sta PF2                     ; 3  35
    sta PF2                     ; 3  38
    sta PF2                     ; 3  41
    lda GameOverPF2R,X          ; 4  45
    sta PF2                     ; 3  49
    lda GameOverPF1R,X          ; 4  53
    sta PF1                     ; 3  56
    rts                         ; 6  62

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw overworld backgrounds that use PF0, PF1, and PF2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;; A is offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawBackgroundFullPF subroutine

    ldy #48                 ; X counts the number of remaining scanlines..
                            ; ... 192 total, with 48 for four-line kernel
    sty Temp
    clc
    adc Temp
    sta Temp
    dec Temp 

.DrawBKFullPF:    
    ldx Temp

    lda CastlePF0,X
    sta PF0
    lda CastlePF1,X
    sta PF1
    lda CastlePF2,X
    sta PF2

    jsr DrawPlayer
    ldx #3
    jsr SleepForLines
    dec Temp  
    dey        
    cpy #0
    sta WSYNC
    bne .DrawBKFullPF        ; repeat next main game scanline until finished
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw overworld backgrounds that use PF1, and PF2 but not PF0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;; A is offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawBackgroundMostPF subroutine

    ldy #48                 ; X counts the number of remaining scanlines..
                            ; ... 192 total, with 48 for four-line kernel
    sty Temp
    clc
    adc Temp
    sta Temp
    dec Temp 

.DrawBKMostPF:    
    ldx Temp

    lda #0
    sta PF0
    lda DarkForestPF1,X
    sta PF1
    lda DarkForestPF2,X
    sta PF2

    jsr DrawPlayer
    
    sta WSYNC
    jsr DrawItem

    ldx #2
    jsr SleepForLines
    dec Temp  
    dey        
    cpy #0
    sta WSYNC
    bne .DrawBKMostPF        ; repeat next main game scanline until finished
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to do WSYNC several times in a row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;; X is number of scanlines to wait
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SleepForLines subroutine
SleepLoop:
    dex
    cpx #0
    sta WSYNC
    bne SleepLoop
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw 'YOUR      THEIR' text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawBattleTopText subroutine
    lda YourPF0,Y       ; 4
    sta PF0             ; 3
    lda YourPF1,Y       ; 4
    sta PF1             ; 3
    lda YourPF2,Y       ; 4
    sta PF2             ; 3
    sta PF2             ; 3
    sta PF2             ; 3
    lda TheirPF0,Y      ; 4
    sta PF0             ; 3
    lda TheirPF1,Y      ; 4
    sta PF1             ; 3
    lda TheirPF2,Y      ; 4
    sta PF2             ; 3
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw 'ATTACK' text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawAttackText subroutine

    lda #0              ; 2    2
    sta PF0             ; 3    5
    lda AttackPF1L,Y    ; 4    9
    sta PF1             ; 3    12
    lda AttackPF2L,Y    ; 4    16
    sta PF2             ; 3    19
    sta PF2             ; 3    19
    sta PF2             ; 3    19
    lda AttackPF0R,Y    ; 4    35
    sta PF0             ; 3    38
    lda AttackPF1R,Y    ; 4    42
    sta PF1             ; 3    45
    lda #0              ; 2    47
    sta PF2             ; 3    50
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw 'DEFEND' text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawDefendText subroutine
    lda #0              ; 2
    sta PF0             ; 3
    lda DefendPF1L,Y    ; 4
    sta PF1             ; 3
    lda DefendPF2L,Y    ; 4
    sta PF2             ; 3
    sta PF2             ; 3
    sta PF2             ; 3
    lda DefendPF0R,Y    ; 4
    sta PF0             ; 3
    lda DefendPF1R,Y    ; 4
    sta PF1             ; 3
    lda #0              ; 2
    sta PF2             ; 3
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw 'LEVEL UP' text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawLevelUpText subroutine
    lda #0                  ; 2     2
    sta PF0                 ; 3     5
    lda LevelUpPF1L,Y       ; 4     9
    sta PF1                 ; 3     12
    lda LevelUpPF2L,Y       ; 4     16
    sta PF2                 ; 3     19
    sta PF2                 ; 3     19
    lda LevelUpPF0R,Y       ; 4     35
    sta PF0                 ; 3     37
    lda LevelUpPF1R,Y       ; 4     41
    sta PF1                 ; 3     44
    lda #0                  ; 2     46
    sta PF2                 ; 3     51
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw label text and a number on level up screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;; A register is offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LevelScreenNumLoop subroutine

    ldy #5
    sty Temp
    adc Temp
    sta Temp
    
    lda NumSpriteOne
    sta NumSpriteOneTemp

LvlNumLoop:
    dey
    dec Temp

    ldx Temp
    sta WSYNC

    lda NumSpriteOneTemp
    sta NumSpriteOne

    lda #0                  ; 2   2
    sta PF0                 ; 3   5
    lda LvlPF1,X            ; 4   9
    sta PF1                 ; 3   12
    lda LvlPF2,X            ; 4   16
    sta PF2                 ; 3   19
    
    ldx TensDigitOffset     ; 3   21     get the tens digit offset for the num
    lda Digits,X            ; 4   25     load the bit pattern from the lookup table
    and #$F0                ; 2   27     mask/remove the graphics for the ones digit
    sta NumSpriteOneTemp    ; 3   30

    sta NumSpriteOneTemp    ; 3   30

    sta NumSpriteOneTemp    ; 3   30

    lda NumSpriteOne        ; 3   45
    sta PF1                 ; 3   48
    lda #0                  ; 2   50
    sta PF2                 ; 3   53

    sta WSYNC

    ldx Temp

    lda #0                  ; 2   2
    sta PF0                 ; 3   5
    lda LvlPF1,X            ; 4   9
    sta PF1                 ; 3   12
    lda LvlPF2,X            ; 4   16
    sta PF2                 ; 3   19

    ldx OnesDigitOffset     ; 3   21     get the ones digit offset for the num
    lda Digits,X            ; 4   25     load the bit pattern from the lookup table
    and #$0F                ; 2   27     mask/remove the graphics for the tens digit
    ora NumSpriteOneTemp    ; 3   30     merge with the saved tens digit sprite
    sta NumSpriteOneTemp    ; 3   33     and save it

    ldx Temp

    lda NumSpriteOne        ; 3   48
    sta PF1                 ; 3   51
    lda #0                  ; 2   53
    sta PF2                 ; 3   56

    inc TensDigitOffset
    inc OnesDigitOffset

    sta WSYNC
    
    jsr DrawLvlText
    sta WSYNC
    jsr DrawLvlText

    cpy #0
    beq EndLvlNumLoop
    jmp LvlNumLoop
EndLvlNumLoop:

    sta WSYNC
    jsr DrawLvlText
    sta WSYNC
    lda #0
    sta PF0
    sta PF1
    sta PF2

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to draw 'LVL:' text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawLvlText subroutine

    lda #0                  ; 2   2
    sta PF0                 ; 3   5
    lda LvlPF1,X            ; 4   9
    sta PF1                 ; 3   12
    lda LvlPF2,X            ; 4   16
    sta PF2                 ; 3   19
    
    jsr Sleep12Cycles       ; 12  31

    lda NumSpriteOne        ; 3   46
    sta PF1                 ; 3   49
    lda #0                  ; 2   51
    sta PF2                 ; 3   54

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to try beginning battles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creates enemy stats from room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BeginBattle subroutine
    lda Timer
    cmp #0
    beq CheckBattleRand
    dec Timer
    jmp DoNotStartBattle

CheckBattleRand:
    lda #255
    sbc Room
    sta Timer
    lda Random
    cmp #220
    bpl StartBattle
    jmp DoNotStartBattle

StartBattle:

    ldy Room
    lda #%10000000
    bit BattleFlags
    beq KeepRoomCalc
    lda #173
    sec
    sbc Room
    tay

KeepRoomCalc
    tya
    sec
    sbc #20
    bcc SetEHP
    tax
    lda #%10000000
    bit BattleFlags
    txa
    bne KeepEHp
    cmp #5
    bpl KeepEHp
SetEHP:
    lda #5
KeepEHp:
    sty Temp
    ldx Temp
    jsr DecompressNum
    sta EnemyHp
    stx Temp
    ldy Temp
    

    tya
    sec
    sbc #20
    bcc SetEAtk
    tax
    lda #%10000000
    bit BattleFlags
    txa
    bne KeepEAtk
    cmp #2
    bpl KeepEAtk
SetEAtk:
    lda #2
KeepEAtk:
    sty Temp
    ldx Temp
    jsr DecompressNum
    sta EnemyAtk
    stx Temp
    ldy Temp


    tya
    lsr
    lsr
    tax
    lda #%10000000
    bit BattleFlags
    txa
    bne KeepEDef
    cmp #2
    bpl KeepEDef
    lda #2
KeepEDef:
    sty Temp
    ldx Temp
    jsr DecompressNum
    sta EnemyDef
    stx Temp
    ldy Temp
    
    tya
    lsr
    lsr
    sec
    sbc #10
    bcc SetESpd
    tax
    lda #%10000000
    bit BattleFlags
    txa
    bne KeepESpd
    cmp #4
    bpl KeepESpd
SetESpd:
    lda #4
KeepESpd:
    jsr DecompressNum
    sta EnemySpd

    lda #1
    sta Scene

    lda #30
    sta Timer
    
    lda BattleFlags
    ora #%00000011
    and #%11111011
    sta BattleFlags

DoNotStartBattle:
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to try beginning battles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creates enemy stats from room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DecompressNum subroutine
    tay
    and #$0F
    cmp #$0A
    bmi KeepUncompressed
    sec
    sbc #$0A
    sta Temp
    tya
    and #$F0
    adc #$10
    ora Temp
    jmp EndUncompression
KeepUncompressed:
    sta Temp
    tya
    and #$F0
    ora Temp

EndUncompression:
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Register random num
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random num
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomNum subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random          ; performs a series of shifts and bit operations
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC                ; start a fresh new scanline
    sec                      ; make sure carry-flag is set before subtracion
.Div15Loop:
    sbc #15                  ; subtract 15 from accumulator
    bcs .Div15Loop           ; loop until carry-flag is clear
    eor #7                   ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                      ; four shift lefts to get only the top 4 bits
    sta HMP0,Y               ; store the fine offset to the correct HMxx
    sta RESP0,Y              ; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to render the player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; y is scanline num
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawPlayer subroutine
    tya
    sec
    sbc PlayerYPos
    cmp #PLAYER_HEIGHT
    bcc .DrawPlayer
    lda #0
    jmp .SkipPlayerDraw        ; if X != player y pos
.DrawPlayer:
    lda #%11111111              ; else enable player display
.SkipPlayerDraw:
    sta GRP1                   ; store the correct value in the TIA register
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to render overworld items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; y is scanline num
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawItem subroutine
    tya
    sec
    sbc ItemYPos
    cmp #ITEM_HEIGHT
    bcc .DrawItem
    lda #0
    jmp .SkipItemDraw        ; if X != player y pos
.DrawItem:
    dec ItemOffset
    ldx ItemOffset
    lda Sword,X              ; else enable player display
.SkipItemDraw:
    sta GRP0                   ; store the correct value in the TIA register

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of the variables PlayerHp and EnemyHp
;; into the offsets of digit lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes
;; For the low nibble, we need to multiply by 5
;; - We can use left shifts to perform multiplication by 2
;; - For any number N, the value of Nx5 = (Nx2x2)+N
;; For the upper nibble, since it's already times 16, we need to divide it
;; and then multiply by 5
;; - We can use right shifts to perform division by 2
;; - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                  ; x reg is loop counter
.PrepareScoreLoop           ; this will loop twice. First, X=1, and then X=0
    
    lda PlayerHp,X          ; first pass, gets enemy hp (X=1) and gets player hp on second pass (X=2)
    and #$0F                ; removes tens digit by masking 4 bits with 00001111
    sta Temp                ; save the value of A into Temp
    asl
    asl                     ; multiply by 4
    adc Temp                ; add the value saved in Temp (+N)
    sta OnesDigitOffset,X   ; first pass save enemy hp to low byte, second pass save player hp to high

    lda PlayerHp,X          ; load A with enemy hp (X=1) or player hp (X=0)
    and #$F0                ; remove ones digit by masking 4 bits with 11110000
    lsr
    lsr                     ; divide by 4
    sta Temp                ; save to Temp
    lsr
    lsr                     ; divide by 4 more (N/16)
    adc Temp                ; Add value stored in temp (+N/4)
    sta TensDigitOffset,X   ; store A in TensDigitOffset+1 or TensDigitOffset

    dex
    bpl .PrepareScoreLoop

    rts
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of anything in the Y register
;; into the offsets of digit lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes
;; For the low nibble, we need to multiply by 5
;; - We can use left shifts to perform multiplication by 2
;; - For any number N, the value of Nx5 = (Nx2x2)+N
;; For the upper nibble, since it's already times 16, we need to divide it
;; and then multiply by 5
;; - We can use right shifts to perform division by 2
;; - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateMiscDigitOffset subroutine
    txa
    and #$0F                ; removes tens digit by masking 4 bits with 00001111
    sta Temp                ; save the value of A into Temp
    asl
    asl                     ; multiply by 4
    adc Temp                ; add the value saved in Temp (+N)
    sta OnesDigitOffset     ; save value to low byte

    txa
    and #$F0                ; remove ones digit by masking 4 bits with 11110000
    lsr
    lsr                     ; divide by 4
    sta Temp                ; save to Temp
    lsr
    lsr                     ; divide by 4 more (N/16)
    adc Temp                ; Add value stored in temp (+N/4)
    sta TensDigitOffset     ; store A in TensDigitOffset

    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare num sprite one for drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used for code compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrepareNumsOverThreeLines subroutine
    sta WSYNC
    ldy TensDigitOffset     ; get the tens digit offset for the player hp
    lda Digits,Y            ; load the bit pattern from the lookup table
    and #$F0                ; mask/remove the graphics for the ones digit
    sta NumSpriteOne           ; save the player hp tens digit pattern

    sta WSYNC
    
    ldy OnesDigitOffset     ; get the ones digit offset for the player hp
    lda Digits,Y            ; load the bit pattern from the lookup table
    and #$0F                ; mask/remove the graphics for the tens digit
    ora NumSpriteOne           ; merge with the saved tens digit sprite
    sta NumSpriteOne           ; and save it

    sta WSYNC
    inc TensDigitOffset
    inc OnesDigitOffset
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Digits:
    .byte %00100010          ;  #   # 
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %00100010          ;  #   # 

    .byte %00100010          ;  #   #
    .byte %01100110          ; ##  ##
    .byte %00100010          ;  #   #
    .byte %00100010          ;  #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

CastlePF0:
    .byte #%11110000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%11110000
    .byte #%11110000
    .byte #%01110000
    .byte #%11110000
    .byte #%11110000
    .byte #%11110000
    .byte #%11110000
    .byte #%01110000
    .byte #%10110000
    .byte #%10010000
    .byte #%01110000
    .byte #%11110000
    .byte #%11110000
    .byte #%11110000
    .byte #%01110000
    .byte #%11110000
    .byte #%00010000
    .byte #%11110000
    .byte #%11110000
    .byte #%10010000
    .byte #%11110000
    .byte #%01110000
    .byte #%11110000
    .byte #%11110000
ForestPF0:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%11000000
    .byte #%11000000
    .byte #%11000000
    .byte #%10000000
    .byte #%10000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%11000000
    .byte #%11000000
    .byte #%11000000
    .byte #%10000000
    .byte #%10000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

CastlePF1:
    .byte #%11111111
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%11111111
    .byte #%11110111
    .byte #%01100001
    .byte #%10011111
    .byte #%11000111
    .byte #%11011000
    .byte #%11100011
    .byte #%01111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11101111
    .byte #%11110111
    .byte #%01111111
    .byte #%00000111
    .byte #%11111111
    .byte #%11111111
    .byte #%11100111
    .byte #%11100001
    .byte #%00011111
    .byte #%11011110
    .byte #%11111111
ForestPF1:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000011
    .byte #%00000011
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%11000000
    .byte #%11110000
    .byte #%11110000
    .byte #%11110000
    .byte #%11100000
    .byte #%11100000
    .byte #%11000000
    .byte #%11000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%11000000
    .byte #%11110000
    .byte #%11110000
    .byte #%11110000
    .byte #%11100000
    .byte #%11100000
    .byte #%11000000
    .byte #%11000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000

CastlePF2:
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01010101
    .byte #%01010101
    .byte #%11111111
    .byte #%01010101
    .byte #%01010101
    .byte #%01010101
    .byte #%01010101
    .byte #%11111111
    .byte #%01010111
    .byte #%01010111
    .byte #%01011111
    .byte #%01011111
    .byte #%01111111
    .byte #%11111110
    .byte #%11111101
    .byte #%11111001
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%10011001
    .byte #%01111111
    .byte #%00000001
    .byte #%00011110
    .byte #%11111111
ForestPF2:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000011
    .byte #%00001111
    .byte #%00001111
    .byte #%00001111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000011
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000

DarkForestPF1:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00010000
    .byte #%00010000
    .byte #%00110000
    .byte #%01011000
    .byte #%00010100
    .byte #%00010100
    .byte #%01100010
    .byte #%00100010
    .byte #%01010000
    .byte #%01001000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000011
    .byte #%00000101
    .byte #%00001001
    .byte #%00001000
    .byte #%00000000
    .byte #%00000011
    .byte #%00000100
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
LakePF1:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000011
    .byte #%00000111
    .byte #%00001111
    .byte #%00001111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00111111
    .byte #%00111111
    .byte #%00111111
    .byte #%01111111
    .byte #%01111111
    .byte #%01111111
    .byte #%00111111
    .byte #%00111111
    .byte #%00111111
    .byte #%00111111
    .byte #%00111111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00001111
    .byte #%00001111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000011
    .byte #%00000011
    .byte #%00000011
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
PitsPF1:        
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000111
    .byte #%00111111
    .byte #%01111111
    .byte #%01110101
    .byte #%11110000
    .byte #%11110000
    .byte #%11111000
    .byte #%01110000
    .byte #%01100000
    .byte #%01100000
    .byte #%00100000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000011
    .byte #%00000011
    .byte #%00000011
    .byte #%00000011
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00100000
    .byte #%01100000
    .byte #%01100000
    .byte #%01110000
    .byte #%11111000
    .byte #%11110000
    .byte #%11110000
    .byte #%01110101
    .byte #%01111111
    .byte #%00111111
    .byte #%00000111
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
TreePF1:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000111
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000011
    .byte #%00000111
    .byte #%00001111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00001111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
FountainPF1:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000111
    .byte #%00011111
    .byte #%01111111
    .byte #%01111000
    .byte #%01111000
    .byte #%00000111
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000


DarkForestPF2:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000010
    .byte #%00000010
    .byte #%00000110
    .byte #%00001011
    .byte #%00010011
    .byte #%00000010
    .byte #%00001100
    .byte #%00010100
    .byte #%00000110
    .byte #%00000101
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000010
    .byte #%00000010
    .byte #%00000000
    .byte #%00000011
    .byte #%00000001
    .byte #%00000010
    .byte #%00000010
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
LakePF2:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111111
    .byte #%01111111
    .byte #%00111111
    .byte #%00001111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000011
    .byte #%00000001
    .byte #%00000001
    .byte #%01110011
    .byte #%11100001
    .byte #%11100001
    .byte #%11100011
    .byte #%11000011
    .byte #%11000111
    .byte #%10000111
    .byte #%10001111
    .byte #%10001111
    .byte #%00001111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011111
    .byte #%00111111
    .byte #%00111111
    .byte #%00111100
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
PitsPF2:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000111
    .byte #%00001111
    .byte #%00000011
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%11000000
    .byte #%11110000
    .byte #%01101000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000010
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
    .byte #%00001100
    .byte #%11111000
    .byte #%11100000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000011
    .byte #%00001111
    .byte #%00000111
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
TreePF2:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000001
    .byte #%00000011
    .byte #%00000011
    .byte #%00000011
    .byte #%00000011
    .byte #%00000111
    .byte #%00000111
    .byte #%00001111
    .byte #%00011111
    .byte #%00111111
    .byte #%11111110
    .byte #%11111110
    .byte #%11111110
    .byte #%11111100
    .byte #%11111100
    .byte #%11111100
    .byte #%11111110
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111100
    .byte #%11100000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
FountainPF2:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%11111110
    .byte #%11111111
    .byte #%11111111
    .byte #%00000001
    .byte #%11110000
    .byte #%11000000
    .byte #%11000111
    .byte #%11111000
    .byte #%11000000
    .byte #%11000000
    .byte #%11110000
    .byte #%00001100
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

YourPF0:
    .byte #%01000000
    .byte #%01000000
    .byte #%10100000
    .byte #%10100000
YourPF1:
    .byte #%01110111
    .byte #%01010101
    .byte #%01010101
    .byte #%01110101
YourPF2:
    .byte #%00001010
    .byte #%00000110
    .byte #%00001010
    .byte #%00000110

TheirPF0:
    .byte #%00100000
    .byte #%00100000
    .byte #%00100000
    .byte #%01110000
TheirPF1:
    .byte #%10101110
    .byte #%10101000
    .byte #%11101100
    .byte #%10101110
TheirPF2:
    .byte #%00010101
    .byte #%00001101
    .byte #%00010101
    .byte #%00001101

HPText:
    .byte #%10010100
    .byte #%10010100
    .byte #%11110111
    .byte #%10010101
    .byte #%10010111

AttackPF1L:
    .byte #%00000101
    .byte #%00000101
    .byte #%00000111
    .byte #%00000101
    .byte #%00000101
    .byte #%00000010
AttackPF2L:
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
    .byte #%11101110
AttackPF0R:
    .byte #%10100000
    .byte #%10100000
    .byte #%11100000
    .byte #%10100000
    .byte #%10100000
    .byte #%01000000
AttackPF1R:
    .byte #%00110101
    .byte #%01000101
    .byte #%01000101
    .byte #%01000110
    .byte #%01000101
    .byte #%00110101

DefendPF1L:
    .byte #%00000110
    .byte #%00000101
    .byte #%00000101
    .byte #%00000101
    .byte #%00000101
    .byte #%00000110
DefendPF2L:
    .byte #%00101110
    .byte #%00100010
    .byte #%00100010
    .byte #%01100110
    .byte #%00100010
    .byte #%01101110
DefendPF0R:
    .byte #%01110000
    .byte #%00010000
    .byte #%00010000
    .byte #%00110000
    .byte #%00010000
    .byte #%01110000
DefendPF1R:
    .byte #%10010110
    .byte #%10110101
    .byte #%10110101
    .byte #%11010101
    .byte #%11010101
    .byte #%10010110

LevelUpPF1L:
    .byte #%11101110
    .byte #%10001000
    .byte #%10001100
    .byte #%10001000
    .byte #%10001110
LevelUpPF2L:
    .byte #%01110010
    .byte #%00010111
    .byte #%00110101
    .byte #%00010101
    .byte #%01110101
LevelUpPF0R:
    .byte #%01110000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
    .byte #%00010000
LevelUpPF1R:
    .byte #%01110100
    .byte #%01010100
    .byte #%01010111
    .byte #%01010101
    .byte #%01010111

LvlPF1:
    .byte #%00111001
    .byte #%00100010
    .byte #%00100010
    .byte #%00100010
    .byte #%00100010
StrPF1:
    .byte #%00110001
    .byte #%00001001
    .byte #%00010001
    .byte #%00100001
    .byte #%00011011
DefPF1:
    .byte #%00110011
    .byte #%00101010
    .byte #%00101011
    .byte #%00101010
    .byte #%00110011
SpdPF1:
    .byte #%00110010
    .byte #%00001010
    .byte #%00010011
    .byte #%00100010
    .byte #%00011011
MhpPF1:
    .byte #%00101010
    .byte #%00111010
    .byte #%00111011
    .byte #%00111010
    .byte #%00101010
LvlPF2:
    .byte #%00011100
    .byte #%01000101
    .byte #%00000101
    .byte #%01000101
    .byte #%00000101
StrPF2:
    .byte #%00010100
    .byte #%01010100
    .byte #%00001100
    .byte #%01010100
    .byte #%00001101
DefPF2:
    .byte #%00000101
    .byte #%01000100
    .byte #%00001100
    .byte #%01000100
    .byte #%00011101
SpdPF2:
    .byte #%00001100
    .byte #%01010100
    .byte #%00010101
    .byte #%01010101
    .byte #%00001100
MhpPF2:
    .byte #%00000101
    .byte #%01000101
    .byte #%00011101
    .byte #%01010101
    .byte #%00001101

Sword:
    .byte #%10110000
    .byte #%01100000
    .byte #%11110000
    .byte #%10111000
    .byte #%00011100
    .byte #%00001110
    .byte #%00000111
    .byte #%00000011
Shield:
    .byte #%00000000
    .byte #%00011000
    .byte #%00111100
    .byte #%01100110
    .byte #%01000010
    .byte #%01011010
    .byte #%01100110
    .byte #%00000000
Armor:
    .byte #%01000010
    .byte #%00111100
    .byte #%00111100
    .byte #%01111110
    .byte #%01111110
    .byte #%11111111
    .byte #%11100111
    .byte #%00000000
Trophy:
    .byte #%00111100
    .byte #%00011000
    .byte #%00011000
    .byte #%00111100
    .byte #%01111110
    .byte #%10111101
    .byte #%10111101
    .byte #%11000011

GameOverPF1L:
    .byte #%01100101
    .byte #%10010101
    .byte #%10110111
    .byte #%10000101
    .byte #%10010101
    .byte #%01100010
VictoryPF1L:
    .byte #%00001001
    .byte #%00010101
    .byte #%00010101
    .byte #%00010101
    .byte #%00010101
    .byte #%00010101
GameOverPF2L:
    .byte #%10100010
    .byte #%10101010
    .byte #%10101010
    .byte #%10101010
    .byte #%10101010
    .byte #%10010100
VictoryPF2L:
    .byte #%10001100
    .byte #%10010010
    .byte #%10000010
    .byte #%10000010
    .byte #%10010010
    .byte #%11001100

GameOverPF1R:
    .byte #%10101100
    .byte #%10100101
    .byte #%01100101
    .byte #%10101101
    .byte #%10100101
    .byte #%11101101
VictoryPF1R:
    .byte #%00001001
    .byte #%00001001
    .byte #%00001001
    .byte #%00010100
    .byte #%00010101
    .byte #%00010101
GameOverPF2R:
    .byte #%10111001
    .byte #%00101010
    .byte #%00101010
    .byte #%10101010
    .byte #%00101010
    .byte #%10111010
VictoryPF2R:
    .byte #%00111010
    .byte #%00101010
    .byte #%00101010
    .byte #%00101011
    .byte #%00101010
    .byte #%10111011

    org $FFFC
    .word Reset         ; Reset pos
    .word Reset         ; Interrupt