#!/usr/bin/env python
# -*- coding: utf-8 -*-

from typing import Optional, NamedTuple
import textwrap

class ElementType(NamedTuple):
    name: str
    rep_ty: str
    desc: str
    width: Optional[int]

MACH_WORD = None

element_types = [
    # (name, representation type, human description, width)
    #
    # width in bytes.
    # width == None denotes machine word

    ElementType("Char",      "Char#",        "8-bit character",                       1),
    ElementType("WideChar",  "Char#",        "32-bit character",                      4),
    ElementType("Int",       "Int#",         "word-sized integer",                    MACH_WORD),
    ElementType("Word",      "Word#",        "word-sized unsigned integer",           MACH_WORD),
    ElementType("Addr",      "Addr#",        "machine address",                       MACH_WORD),
    ElementType("Float",     "Float#",       "single-precision floating-point value", 4),
    ElementType("Double",    "Double#",      "double-precision floating-point value", 8),
    ElementType("StablePtr", "StablePtr# a", "{\\tt StablePtr#} value",               MACH_WORD),
]

# TODO: Eventually when the sized integer primops use proper unboxed types we
# should rather do:
#
#for n in [8,16,32,64]:
#    element_types += [ ElementType(f"Int{n}",  f"Int{n}#",  f"{n}-bit signed integer",   n // 8) ]
#
#for n in [8,16,32,64]:
#    element_types += [ ElementType(f"Word{n}", f"Word{n}#", f"{n}-bit unsigned integer", n // 8) ]

element_types += [
    ElementType("Int8",   "Int8#",  "8-bit signed integer",  1),
    ElementType("Int16",  "Int16#", "16-bit signed integer", 2),
    ElementType("Int32",  "Int32#", "32-bit signed integer", 4),
    ElementType("Int64",  "INT64",  "64-bit signed integer", 8),

    ElementType("Word8",  "Word8#",  "8-bit unsigned integer",  1),
    ElementType("Word16", "Word16#", "16-bit unsigned integer", 2),
    ElementType("Word32", "Word32#", "32-bit unsigned integer", 4),
    ElementType("Word64", "WORD64",  "64-bit unsigned integer", 8),
]

def pretty_offset(n: Optional[int]) -> str:
    if n == MACH_WORD:
        return 'machine words'
    elif n == 1:
        return 'bytes'
    else:
        return f'{n}-byte words'

def print_block(template: str, **kwargs) -> None:
    print(textwrap.dedent(template.format(**kwargs)).lstrip())

def header(s: str):
    print('')
    print_block('''
        ------------------------------------
        -- {s}
        ------------------------------------
    ''', s=s)


header("ByteArray# operations")

print('''
-- Do not edit. This file is generated by utils/genprimopcode/gen_bytearray_ops.py.
-- To regenerate run,
--
--      python3 utils/genprimops/gen_bytearray_ops.py > compiler/GHC/Builtin/bytearray-ops.txt.pp
''')

header('aligned index operations')
for t in element_types:
    offset = pretty_offset(t.width)
    print_block('''
        primop IndexByteArrayOp_{name} "index{name}Array#" GenPrimOp
           ByteArray# -> Int# -> {rep_ty}
           {{Read a {desc}; offset in {offset}.}}
           with can_fail = True
    ''', offset = offset, **t._asdict())

header('unaligned index operations')
for t in element_types:
    if t.name in ['Int8', 'Word8']: continue
    print_block('''
        primop IndexByteArrayOp_Word8As{name} "indexWord8ArrayAs{name}#" GenPrimOp
           ByteArray# -> Int# -> {rep_ty}
           {{Read a {desc}; offset in bytes.}}
           with can_fail = True
    ''', **t._asdict())

header('aligned read operations')
for t in element_types:
    offset = pretty_offset(t.width)
    print_block('''
        primop ReadByteArrayOp_{name} "read{name}Array#" GenPrimOp
           MutableByteArray# s -> Int# -> State# s -> (# State# s, {rep_ty} #)
           {{Read a {desc}; offset in {offset}.}}
           with has_side_effects = True
                can_fail = True
    ''', offset = offset, **t._asdict())

header('unaligned read operations')
for t in element_types:
    if t.name in ['Int8', 'Word8']: continue
    print_block('''
        primop ReadByteArrayOp_Word8As{name} "readWord8ArrayAs{name}#" GenPrimOp
           MutableByteArray# s -> Int# -> State# s -> (# State# s, {rep_ty} #)
           {{Read a {desc}; offset in bytes.}}
           with has_side_effects = True
                can_fail = True
    ''', **t._asdict())

header('aligned write operations')
for t in element_types:
    offset = pretty_offset(t.width)
    print_block('''
        primop WriteByteArrayOp_{name} "write{name}Array#" GenPrimOp
           MutableByteArray# s -> Int# -> {rep_ty} -> State# s -> State# s
           {{Write a {desc}; offset in {offset}.}}
           with has_side_effects = True
                can_fail = True
    ''', offset = offset, **t._asdict())

header('unaligned write operations')
for t in element_types:
    if t.name in ['Int8', 'Word8']: continue
    print_block('''
        primop WriteByteArrayOp_Word8As{name} "writeWord8ArrayAs{name}#" GenPrimOp
           MutableByteArray# s -> Int# -> {rep_ty} -> State# s -> State# s
           {{Write a {desc}; offset in bytes.}}
           with has_side_effects = True
                can_fail = True
    ''', **t._asdict())

