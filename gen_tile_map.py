#!/usr/bin/python3

import random

def pad(l, length):
  needed_padding = length - len(l)
  return ([0] * needed_padding) + l


def toHexCharacters(n):
  return [int(d, 16) for d in list(hex(n))[2:]]


def main():
  tile_values_sections = []

  col_start = 0x100
  for i in range(0x4000, 0x800000, 8):
    col_addr = i & 0xff
    row_addr = (i >> 8) & 0x3f
    col_bank = (i >> 14) & 0xf
    row_bank = (i >> 18) & 0x1f
    row = (row_bank << 6) | row_addr
    col = (col_bank << 8) | (col_addr >> 3)
    row_digits = pad(toHexCharacters(row), 3)
    col_digits = pad(toHexCharacters(col), 3)
    tile_values_sections.append(row_digits + [16] + col_digits + [17])
  map_values = [i for c in tile_values_sections for i in c]
  map_bytes = bytes(map_values)
  with open('long_map.tilemap', 'wb') as f:
    f.write(map_bytes)


def randomTiles():
  tiles = []
  for i in range(0x4000, 0x800000):
    tiles.append(random.randint(0, 17))
  map_bytes = bytes(tiles)

  with open('long_map.tilemap', 'wb') as f:
    f.write(map_bytes)


if __name__ == '__main__':
  main()
  #randomTiles()