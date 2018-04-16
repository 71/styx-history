"""
This file generates the X86-specific instruction parser for the Styx arch.rs module.
"""

from common import *

prefix = """"""

with output('x86') as out:
    write_head(prefix)

    define('add',   'r/m 8',      'r 8',        0x00)
    define('add',   'r/m 16/32',  'r 16/32',    0x01)
    define('add',   'r 8',        'r/m 8',      0x02)
    define('add',   'r 16/32',    'r/m 16/32',  0x03)
    define('or',    'r/m 8',      'r 8',        0x08)
    define('or',    'r/m 16/32',  'r 16/32',    0x09)
    define('or',    'r 8',        'r/m 8',      0x0A)
    define('or',    'r 16/32',    'r/m 16/32',  0x0B)
    define('and',   'r/m 8',      'r 8',        0x20)
    define('and',   'r/m 16/32',  'r 16/32',    0x21)
    define('and',   'r 8',        'r/m 8',      0x22)
    define('and',   'r 16/32',    'r/m 16/32',  0x23)
    define('sub',   'r/m 8',      'r 8',        0x28)
    define('sub',   'r/m 16/32',  'r 16/32',    0x29)
    define('sub',   'r 8',        'r/m 8',      0x2A)
    define('sub',   'r 16/32',    'r/m 16/32',  0x2B)
    define('xor',   'r/m 8',      'r 8',        0x30)
    define('xor',   'r/m 16/32',  'r 16/32',    0x31)
    define('xor',   'r 8',        'r/m 8',      0x32)
    define('xor',   'r 16/32',    'r/m 16/32',  0x33)
    define('cmp',   'r/m 8',      'r 8',        0x38)
    define('cmp',   'r/m 16/32',  'r 16/32',    0x39)
    define('cmp',   'r 8',        'r/m 8',      0x3A)
    define('cmp',   'r 16/32',    'r/m 16/32',  0x3B)
    define('je',    'rel 8',                    0x74)
    define('jz',    'rel 8',                    0x74)
    define('jne',   'rel 8',                    0x75)
    define('jnz',   'rel 8',                    0x75)
    define('xor',   'r/m 8',      'imm 8',      0x80, modify = '*op1.raw() += 8 * 6 + 0xC0;')
    define('xor',   'r/m 16/32',  'imm 16/32',  0x81, modify = '*op1.raw() += 8 * 6 + 0xC0;')
    define('xor',   'r/m 16/32',  'imm 8',      0x83, modify = '*op1.raw() += 8 * 6 + 0xC0;')
    define('jbe',   'rel 16/32',                0x86)
    define('mov',   'r/m 8',      'r 8',        0x88)
    define('mov',   'r/m 16/32',  'r 16/32',    0x89)
    define('mov',   'r 8',        'r/m 8',      0x8A)
    define('mov',   'r 16/32',    'r/m 16/32',  0x8B)
    define('nop',                               0x90)
    define('mov',   'r 8',        'imm 8',      0xB0)
    define('mov',   'r 16/32',    'imm 16/32',  0xB8)
    define('ret',                               0xC3)
    define('mov',   'r/m 8',      'imm 8',      0xC6)
    define('mov',   'r/m 16/32',  'imm 16/32',  0xC7)
    define('call',  'rel 16/32',                0xE8)
    define('jmp',   'rel 16/32',                0xE9)
    define('jmp',   'rel 8',                    0xEB)
    define('not',   'r/m 8',                    0xF6, modify = '*op1.raw() += 8 * 2 + 0xC0;')
    define('neg',   'r/m 8',                    0xF6, modify = '*op1.raw() += 8 * 3 + 0xC0;')
    define('not',   'r/m 16/32',                0xF7, modify = '*op1.raw() += 8 * 2 + 0xC0;')
    define('neg',   'r/m 16/32',                0xF7, modify = '*op1.raw() += 8 * 3 + 0xC0;')
    define('call',  'r/m 16/32',                0xFF)

    define('sete',  'r/m 8',                    0x0F, 0x94)

    write_tail()

