INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]

EntryPoint:
        di ; Disable interrupts. That way we can avoid dealing with them, especially since we didn't talk about them yet :p
        jp Start

REPT $150 - $104
    db 0
ENDR

SECTION "Game code", ROM0

Start:

;; Busy loop to poll until VBlank is reached.
.waitVBlank
  ld a, [rLY]
  cp 144 ; Check if the LCD is past VBlank (vblank starts at line 144)
  jr c, .waitVBlank ; c: carry flag means 144 > [rLY], so we haven't reached 144, so loop.

  ;; Turn off the display.
  xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
  ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.

  ;; Load the tile data from ROM to VRAM at address $8000
  ld bc, TileSet.end - TileSet
  ld hl, _VRAM8000
  ld de, TileSet
  call memcpy_big

  ;; Initialize the ROM bank to $001 and WRAM variables to keep track of it.
  ld a, 0
  ;;ld a, $1 ; for testing
  ld [vCurrentBankHigh], a
  ld [rROMB1], a
  ld a, $01
  ;;ld a, $FF ; for testing
  ld [vCurrentBankLow], a
  ld [rROMB0], a

  ld a, 1
  ld [vWorldColumn], a
  ld a, 0
  ld [vWorldColumn + 1], a
  ld [vWorldRow], a
  ld [vWorldRow + 1], a
  ld d, a
  ld e, a

.tileMapCopyLoop
  call worldToDest
  call worldToSource
  ld a, [vSourceBank]
  ld [rROMB1], a
  ld a, [vSourceBank + 1]
  ld [rROMB0], a
  ld a, [vCopySource]
  ld h, a
  ld a, [vCopySource + 1]
  ld l, a
  ld a, [vCopyDest]
  ld b, a
  ld a, [vCopyDest + 1]
  ld c, a
  ld a, [hl]
  ld [bc], a

  inc e
  ld a, e
  ld [vWorldColumn + 1], a
  cp 22
  jp nz, .tileMapCopyLoop

  ld e, 0
  ld a, e
  ld [vWorldColumn + 1], a
  inc d
  ld a, d
  ld [vWorldRow + 1], a
  cp 20
  jp nz, .tileMapCopyLoop



.finish_setup

  ;; Set BG palette.
  ld hl, $FF47
  ld [hl], %11100100


  ld a, 1
  ld [vWorldColumn], a
  ld [vWorldColumn + 1], a
  ld [vWorldRow + 1], a
  ld a, 0
  ld [vWorldRow], a

  ld a, 8
  ld [rSCX], a
  ld [rSCY], a
  ;; Initialize variable to keep track of the scX and scY values when we last
  ;; copied data. This is to make sure we don't repeatedly run the copy code if
  ;; the screen is kept at a position that triggered a copy.
  ld [vLastSCXCopy], a
  ld [vLastSCYCopy], a
  ;; Disable interupts but set the VBlank IE flag so that I 
  ;; can use halt to wait for VBlank.
  di
  ld a, %00000001
  ld [rIE], a

  ld a, 0
  ld [rLYC], a
  ld a, %01000000
  ld [rSTAT], a ; Used to wait to check the dpad.


  ;; turn the screen back on with background enabled
  ld a, %10010001
  ld [rLCDC], a

.mainLoop

.checkDPad
  ld a, [rSCY]
  ld [vNextSCY], a
  ld a, [rSCX]
  ld [vNextSCX], a
.checkRight
  ld a, P1F_GET_DPAD
  ld [rP1], a
  ld a, [rP1]
  ld a, [rP1]
  ld a, [rP1]
  ld hl, rSCX
  bit 0, a
  jp nz, .checkLeft ; Right wasn't pressed so check left.
  ;; Right was pressed
  ld a, [hl]
  inc a
  ld [vNextSCX], a
  ld b, a
  ld a, [vLastSCXCopy]
  cp b
  jp z, .updateScrollWithHalt
;  jp nz, .mightNeedCopyRight
;  ;; rSCX was the same as vLastSCXCopy, but maybe rSCY has changed.
;  ld a, [rSCY]
;  ld c, a
;  ld a, [vLastSCYCopy]
;  cp c
;  jp z, .updateScrollWithHalt ; SCY is also the same as the last copy, just scroll.

.mightNeedCopyRight
  ;; Now check if we're at a tile boundary. We can know this when scx ends in
  ;; 000
  ld a, b
  and %00000111
  jp nz, .updateScrollWithHalt ; not at a tile boundary, no need to copy.

  ;; Need to copy.

  ;; We're going to be copying another column so update vLastSCXCopy now while
  ;; we have the current scX in B.
  ld a, b
  ld [vLastSCXCopy], a

  ld a, [vWorldColumn + 1]
  ld [vBackupWorldColumn + 1], a
  add 21
  ld [vWorldColumn + 1], a
  ld a, [vWorldColumn]
  ld [vBackupWorldColumn], a
  adc a, 0
  ld [vWorldColumn], a
  
  ;; Row needs to be decremented by 1 because we need to get a buffer tile on
  ;; top.
  ld a, [vWorldRow + 1]
  ld [vBackupWorldRow + 1], a
  sub 1 ; using sub to get the carry flag
  ld [vWorldRow + 1], a
  ld a, [vWorldRow]
  ld [vBackupWorldRow], a
  sbc a, 0
  and %00000111 ; sub
  ld [vWorldRow], a

  call copyColumnWithHalt
  ld a, [vBackupWorldColumn + 1]
  add 1
  ld [vWorldColumn + 1], a
  ld a, [vBackupWorldColumn]
  adc a, 0
  ld [vWorldColumn], a
  ld a, [vBackupWorldRow]
  ld [vWorldRow], a
  ld a, [vBackupWorldRow + 1]
  ld [vWorldRow + 1], a

  jp .checkUp


.checkLeft
  bit 1, a
  jp nz, .checkUp
  ;; Left was pressed
  ld a, [hl]
  dec a
  ld [vNextSCX], a
  ld b, a
  ld a, [vLastSCXCopy]
  cp b
  jp z, .updateScrollWithHalt
;  jp nz, .mightNeedCopyLeft
;  ;; rSCX was the same as vLastSCXCopy, but maybe rSCY has changed.
;  ld a, [rSCY]
;  ld c, a
;  ld a, [vLastSCYCopy]
;  cp c
;  jp z, .updateScrollWithHalt ; SCY is also the same as the last copy, just scroll.

.mightNeedCopyLeft
  ;; Now check if we're at a tile boundary. We can know this when scx ends in
  ;; 000
  ld a, b
  and %00000111
  jp nz, .updateScrollWithHalt ; not at a tile boundary, no need to copy.

  ;; Need to do a copy.


  ;; We're going to be copying another column so update vLastSCXCopy now while
  ;; we have the current scX in B.
  ld a, b
  ld [vLastSCXCopy], a

  
  ld a, [vWorldColumn + 1]
  ld [vBackupWorldColumn + 1], a
  sub 2
  ld [vWorldColumn + 1], a
  ld a, [vWorldColumn]
  ld [vBackupWorldColumn], a
  sbc a, 0
  ld [vWorldColumn], a

  ;; Row needs to be decremented by 1 because we need to get a buffer tile on
  ;; top.
  ld a, [vWorldRow + 1]
  ld [vBackupWorldRow + 1], a
  sub 1 ; using sub to get the carry flag
  ld [vWorldRow + 1], a
  ld a, [vWorldRow]
  ld [vBackupWorldRow], a
  sbc a, 0
  ld [vWorldRow], a

  call copyColumnWithHalt
  ld a, [vBackupWorldColumn + 1]
  sub 1
  ld [vWorldColumn + 1], a
  ld a, [vBackupWorldColumn]
  sbc a, 0
  ld [vWorldColumn], a
  ld a, [vBackupWorldRow]
  ld [vWorldRow], a
  ld a, [vBackupWorldRow + 1]
  ld [vWorldRow + 1], a

  ;; checkUp next

.checkUp
  ld a, [rP1]
  bit 2, a
  jr nz, .checkDown
  ;; Up was pressed
  ld a, [rSCY]
  dec a
  ld [vNextSCY], a
  ld b, a
  ld a, [vLastSCYCopy]
  cp b
  jp z, .updateScrollWithHalt

  ;; Now check if we're at a tile boundary. We can know this when scx ends in
  ;; 000
  ld a, b
  and %00000111
  jp nz, .updateScrollWithHalt ; not at a tile boundary, no need to copy.

  ;; Need to copy.

  ;; We're going to be copying another row so update the vLastSCYCopy now while
  ;; we have the current scY in B.
  ld a, b
  ld [vLastSCYCopy], a

  ld a, [vWorldRow + 1]
  ld [vBackupWorldRow + 1], a
  sub 2
  ld [vWorldRow + 1], a
  ld a, [vWorldRow]
  ld [vBackupWorldRow], a
  sbc a, 0
  ld [vWorldRow], a

  ;; Column needs to be decremented by 1 because we need to get a buffer tile
  ;; on the left.
  ld a, [vWorldColumn + 1]
  ld [vBackupWorldColumn + 1], a
  sub 1 ; using sub to get the carry flag
  ld [vWorldColumn + 1], a
  ld a, [vWorldColumn]
  ld [vBackupWorldColumn], a
  sbc a, 0
  ld [vWorldColumn], a

  call copyRowWithHalt
  ld a, [vBackupWorldRow + 1]
  sub 1
  ld [vWorldRow + 1], a
  ld a, [vBackupWorldRow]
  sbc a, 0
  ld [vWorldRow], a
  ld a, [vBackupWorldColumn]
  ld [vWorldColumn], a
  ld a, [vBackupWorldColumn + 1]
  ld [vWorldColumn + 1], a

  jp .updateScrollNoHalt

.checkDown
  bit 3, a
  jr nz, .noButtons
  ;; Down was pressed
  ld a, [rSCY]
  inc a
  ld [vNextSCY], a
  ld b, a
  ld a, [vLastSCYCopy]
  cp b
  jp z, .updateScrollWithHalt

.mightNeedCopyDown
  ;; Now check if we're at a tile boundary. We can know this when scx ends in
  ;; 000
  ld a, b
  and %00000111
  jp nz, .updateScrollWithHalt ; not at a tile boundary, no need to copy.

  ;; Need to copy.

  ;; We're going to be copying another row so update the vLastSCYCopy now while
  ;; we have the current scY in B.
  ld a, b
  ld [vLastSCYCopy], a

  ld a, [vWorldRow + 1]
  ld [vBackupWorldRow + 1], a
  add 19
  ld [vWorldRow + 1], a
  ld a, [vWorldRow]
  ld [vBackupWorldRow], a
  adc a, 0
  ld [vWorldRow], a

  ;; Column needs to be decremented by 1 because we need to get a buffer tile
  ;; on the left.
  ld a, [vWorldColumn + 1]
  ld [vBackupWorldColumn + 1], a
  sub 1 ; using sub to get the carry flag
  ld [vWorldColumn + 1], a
  ld a, [vWorldColumn]
  ld [vBackupWorldColumn], a
  sbc a, 0
  ld [vWorldColumn], a

  call copyRowWithHalt
  ld a, [vBackupWorldRow + 1]
  add 1
  ld [vWorldRow + 1], a
  ld a, [vBackupWorldRow]
  adc a, 0
  ld [vWorldRow], a
  ld a, [vBackupWorldColumn]
  ld [vWorldColumn], a
  ld a, [vBackupWorldColumn + 1]
  ld [vWorldColumn + 1], a

  jp .updateScrollNoHalt


.noButtons
.waitForLYZeroThenLoop
  ;; enable STAT interupt to wait for OAM search.
  ld a, %00000010
  ld [rIE], a
  ;; Nothing to do, halt until next VBlank instead of doing a busy wait for a
  ;; button press.
  halt
  ;; clear rIF so that the next halt will wait for a new VBlank. Otherwise halt
  ;; doesn't do anything while rIF and rIE are set.
  xor a
  ld [rIF], a
  ;; switch back to vblank interupt
  ld a, %00000001
  ld [rIE], a
  jp .mainLoop

.updateScrollWithHalt
  ld a, [rSTAT]
  and %00000011
  cp 1
  jr z, .updateScrollNoHalt
  halt
  ;; clear rIF so that the next halt will wait for a new VBlank. Otherwise halt
  ;; doesn't do anything while rIF and rIE are set.
  xor a
  ld [rIF], a
.updateScrollNoHalt
  ld a, [vNextSCX]
  ld [rSCX], a
  ld a, [vNextSCY]
  ld [rSCY], a
  jp .waitForLYZeroThenLoop


copyRowWithHalt::
  call worldToDest
  call worldToSource
  ld d, 22
  ldh a, [vCopyDest]
  ld h, a
  ldh a, [vCopyDest + 1]
  ld l, a

  ldh a, [vSourceBank]
  ld [rROMB1], a
  ld b, a
  ldh a, [vSourceBank + 1]
  ld [rROMB0], a

  ;; check if we're in bank 0
  or b
  jr nz, .notBankZero
  call copyRowBankZeroTileWithHalt
  jp nz, .copyLoop
  ret 

.notBankZero
  ldh a, [vCopySource]
  ld b, a
  ldh a, [vCopySource + 1]
  ld c, a
  ld a, [bc]

  ld e, a
  xor a
  halt 
  ld [rIF], a
  ld a, e
.copyLoop
  ld [hl], a
  dec d
  ret z

  ldh a, [vWorldColumn + 1]
  inc a
  ldh [vWorldColumn + 1], a
  jp nz, .noOverflow
  ldh a, [vWorldColumn]
  inc a
  ldh [vWorldColumn], a
.noOverflow
  ld a, d
  ldh [vCopyCounter], a

  ld a, %11100000
  and l
  inc l
  ld e, a
  ld a, %00011111
  and l
  or e
  ld l, a

  inc c
  jr nz, .noSourceOverflow

  call worldToSource
  ldh a, [vSourceBank]
  ld [rROMB1], a
  ld e, a
  ldh a, [vSourceBank + 1]
  ld [rROMB0], a
  or e
  jr nz, .notBankZeroLoop
  ldh a, [vCopyCounter]
  ld d, a
  call copyRowBankZeroTileNoHalt
  jr nz, .noSourceOverflow
  ret 

.notBankZeroLoop
  ldh a, [vCopySource]
  ld b, a
  ldh a, [vCopySource + 1]
  ld c, a
.noSourceOverflow
  ldh a, [vCopyCounter]
  ld d, a
  ld a, [bc]
  jp .copyLoop


copyRowBankZeroTileWithHalt::
  ;; Args:
  ;;   d: copy count
  ;;   hl: start destination
  ;; Return flags:
  ;;   z: finished copying
  ;;   nz: reached the end of bank 0. HL has the next copy dest and d has
  ;;       the number of remaining tile map values to copy. It also calls
  ;;       worldToSource, sets the bank to $16 and sets BC to the next source
  ;;       address.
  ;; Modifies:
  ;;   a, b, c, e
  xor a
  halt 
  ldh [rIF], a
copyRowBankZeroTileNoHalt::
  ld e, 17
  ld a, e
.copyLoop
  ld [hl], a
  dec d
  ret z

  ldh a, [vWorldColumn + 1]
  inc a
  ldh [vWorldColumn + 1], a
  ld c, a

  ld a, %11100000
  and l
  inc l
  ld b, a
  ld a, %00011111
  and l
  or b
  ld l, a

  ;; Check to see if we're still in bank 0. We're out of bank 0 if
  ;; [vWorldColumn + 1] overflowed to 0.
  xor a
  cp c
  ld a, e
  jr nz, .copyLoop

  ld a, [vWorldColumn]
  inc a
  ld [vWorldColumn], a
  call worldToSource
  ;; Update the ROM bank.
  ;; Since we were in bank 0 and copying a row from left to right and now we're
  ;; past bank 0 we must be in bank 1.
  ld a, 1
  ld [rROMB0], a
  ldh a, [vCopySource]
  ld b, a
  ldh a, [vCopySource + 1]
  ld c, a
  ld a, [bc]
  ret
  

copyColumnWithHalt::
  ;; Modifies:
  ;;   everything
  call worldToDest
  call worldToSource
  ld d, 20
  ldh a, [vCopyDest]
  ld h, a
  ldh a, [vCopyDest + 1]
  ld l, a

  ldh a, [vSourceBank]
  ld [rROMB1], a
  ld b, a
  ldh a, [vSourceBank + 1]
  ld [rROMB0], a
  ;; check if we're in bank 0
  or b
  jr nz, .notBankZero
  call copyColumnBankZeroTileWithHalt
  jp nz, .copyLoop
  ret

.notBankZero
  ldh a, [vCopySource]
  ld b, a
  ldh a, [vCopySource + 1]
  ld c, a
  ld a, [bc]

  ld e, a
  xor a
  halt
  ld [rIF], a
  ld a, e
.copyLoop
  ld [hl], a
  dec d
  ret z

  ldh a, [vWorldRow + 1]
  inc a
  ldh [vWorldRow + 1], a
  jp nz, .noOverflow
  ldh a, [vWorldRow]
  inc a
  ldh [vWorldRow], a
.noOverflow
  ld a, d
  ldh [vCopyCounter], a
  ;call worldToDest
  ;ld a, d
  ;ld [vCopyCounter
  ld de, 32
  add hl, de
  ld a, %00000011
  and h
  or HIGH(_SCRN0)
  ld h, a

  inc b
  bit 7, b
  jr z, .noSourceOverflow

  call worldToSource
  ldh a, [vSourceBank]
  ld [rROMB1], a
  ld e, a
  ldh a, [vSourceBank + 1]
  ld [rROMB0], a
  or e
  jr nz, .notBankZeroLoop
  ldh a, [vCopyCounter]
  ld d, a
  call copyColumnBankZeroTileNoHalt
  jr nz, .noSourceOverflow
  ret 


.notBankZeroLoop
  ldh a, [vCopySource]
  ld b, a
  ldh a, [vCopySource + 1]
  ld c, a
  ;ld a, [vCopyDest]
  ;ld h, a
  ;ld a, [vCopyDest + 1]
  ;ld l, a
.noSourceOverflow
  ldh a, [vCopyCounter]
  ld d, a
  ld a, [bc]
  jp .copyLoop


copyColumnBankZeroTileWithHalt::
  ;; Args:
  ;;   d: copy count
  ;;   hl: start destination
  ;; Return flags:
  ;;   z: finished copying
  ;;   nz: reached the end of bank 0. HL has the next copy dest and d has
  ;;       the number of remaining tile map values to copy. It also calls
  ;;       worldToSource, sets the bank to $16 and sets BC to the next source
  ;;       address.
  ;; Modifies:
  ;;   a, b, c, e
  xor a
  halt 
  ldh [rIF], a
copyColumnBankZeroTileNoHalt::
  ld e, 17
  ld a, e
.copyLoop
  ld [hl], a
  dec d
  ret z

  ldh a, [vWorldRow + 1]
  inc a
  ldh [vWorldRow + 1], a

  ld bc, 32
  add hl, bc
  ld b, a ;; to check bank change
  ld a, %00000011
  and h
  or HIGH(_SCRN0)
  ld h, a

  ld a, e
  ;; Since we're in bank 0 we can't overflow when incrementing the row. We do
  ;; need to check if we're still in bank 0 though, we get out when bit 6 is
  ;; 1
  bit 6, b
  jr z, .copyLoop

  call worldToSource
  ;; Update the ROM bank.
  ;; Since we were in bank 0 and copying a column from top to bottom and now we
  ;; past bank 0 we must be in bank $16. The bank number is 9 bits with the high
  ;; 5 bits from the row. Bank was zero, we increased only the row, and now
  ;; it's past bank 0, so bit 5 of the bank is 1.
  ld a, $16
  ld [rROMB0], a
  ldh a, [vCopySource]
  ld b, a
  ldh a, [vCopySource + 1]
  ld c, a
  ld a, [bc]
  ret


worldToSource::
  ;; Converts vWorldColumn and vWorldRow into a copy source address and bank.
  ;; Stores the address in vCopySoure and the bank in vSourceBank.
  ;; Modifies:
  ;; A, B, C
  ;; vWorldRow has the form:
  ;;   xxxx xPQQ QQRR RRRR
  ;; vWorldColumn has the form:
  ;;   xxxx SSSS TTTT TTTT
  ;;
  ;; This function produces
  ;; vCopySource with the form:
  ;;   01RR RRRR TTTT TTTT
  ;; vSourceBank with the form:
  ;;   0000 000P QQQQ SSSS

  ;; First get the source address because it's simple.
  ldh a, [vWorldRow + 1]
  ld c, a ; need this later for the bank too
  and %00111111
  set 6, a
  ldh [vCopySource], a
  ldh a, [vWorldColumn + 1]
  ldh [vCopySource + 1], a

  ;; Next get the source bank.
  ldh a, [vWorldRow]
  and %00000111

  sla c ; Get the highest bit of the lower byte of vWorldRow in the carry flag.
  rla ; Rotate the carry flag into the higher byte of vWorldRow and get the
      ; high bit into the carry flag.
  sla c
  rla
  ;; A is now %000PQQQQ
  swap a
  ;; A is now %QQQQ000P
  ld b, a
  res 0, b ; B is now %QQQQ0000
  
  and %00000001 ; Clear all but P from A.
  ldh [vSourceBank], a
  
  ldh a, [vWorldColumn]
  and %00001111
  ;; A now has 0000 SSSS
  ;; and B has QQQQ 0000
  or b
  ;; Now A has QQQQ SSSS
  ldh [vSourceBank + 1], a
  ret


worldToDest::
  ;; Converts vWorldColumn and vWorldRow into a copy destination based on
  ;; _SCRN0. Stores the address in vCopyDest.
  ;; Modifies:
  ;;   A, B
  ;; vWorldRow has the form:
  ;;   xxxx xPQQ QQRr rrrr
  ;; vWorldColumn has the form:
  ;;   xxxx SSSS TTTt tttt
  ;;
  ;; This function produces
  ;; vCopyDest with the form:
  ;;   1001 10rr rrrt tttt
  ldh a, [vWorldRow + 1]
  and %00011111
  sla a
  ;; A now has 00rr rrr0.
  swap a
  ld b, a ; Will use B to calculate the lower byte later.
  ;; Use A to calculate the high byte.
  and %00000011
  or HIGH(_SCRN0)
  ;; A now contains 1001 10rr.
  ld [vCopyDest], a
  ld a, b
  and %11100000
  ld b, a
  ldh a, [vWorldColumn + 1]
  and %00011111
  or b
  ld [vCopyDest + 1], a
  ret


SECTION "Tile Set", ROM0
TileSet:
  incbin "numbers.gbgfx"
.end


;; LOOP include map data
FOR N, 0, $1FF
  SECTION "Tile Map {N}", ROMX,BANK[N + 1]

  IF N == 0
    TileMap:
  ENDC

  TileMap\@:
    incbin "long_map.tilemap", N * $4000, $4000
  .end
ENDR
;; END LOOP include map data
TileMapEnd:

SECTION "Work RAM", hram
vNextColumnAddress:
  ;; Where to read the next column of tile indexes from.
  ds 2
vLastSCXCopy:
  ;; The value of scX the last time a column was copied. This is to know if the
  ;; column for the current position has already been copied.
  ds 1
vLastSCYCopy:
  ds 1
vCurrentBank:
  ;; Current ROM bank. Split into High and Low variables
vCurrentBankHigh:
  ;; High byte of the current ROM bank. Using an MBC5 so this will be 0 or 1.
  ds 1
vCurrentBankLow:
  ;; Low byte of the current ROM bank.
  ds 1
vNextSCX:
  ;; Used to keep track of scrolling to sync with VBlank
  ds 1
vNextSCY:
  ;; Used to keep track of scrolling to sync with VBlank
  ds 1
vCopySource:
  ;; Where to get the next 18 tile map values from.
  ds 2
vSourceBank:
  ;; The ROM bank to get the next tile map values from
  ds 2
vCopyDest:
  ;; Where in the tile map to copy data to.
  ds 2
vDestinatonSCX:
  ;; The SCX of the column to copy to
  ds 1

vWorldColumn:
  ;; The tile column index in the full world tile map.
  ds 2

vWorldRow:
  ;; The tile row index in the full world tile map.
  ds 2
vBackupWorldColumn:
  ds 2
vBackupWorldRow:
  ds 2
vCopyCounter:
  ds 1