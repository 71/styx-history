"""
This file provides utilities used by all Python scripts in this directory
in order to define instructions on all supported architectures.
"""

from os import getcwd, path
from re import split

import inspect

outputvar = "out"

def output(arch):
    """Returns the output file in which we shall write for the specified architecture."""
    return open(path.join(getcwd(), arch + '.rs'), 'w')

def mktest(nth, arg):
    """Returns a string that, translated into Rust code, compares operands for kind and size equality."""
    tests = []
    sizes = []

    sizetest = 'op{}s == '.format(nth)

    for ch in split(' |/', arg):
        if ch == 'r':
            tests.append('D::Register')
        elif ch == 'm':
            tests.append('D::Memory')
        elif ch == 'imm':
            tests.append('D::Immediate')
        elif ch == 'rel':
            tests.append('D::Relative')
        elif ch == '8':
            sizes.append(sizetest + '1')
        elif ch == '16':
            sizes.append(sizetest + '2')
        elif ch == '32':
            sizes.append(sizetest + '4')
        elif ch == '64':
            sizes.append(sizetest + '8')
        elif ch == '128':
            sizes.append(sizetest + '16')
        else:
            raise Exception('Unknown memory specifier')

    return '(op{0}d & ({1})) != D::Null && ({2})'.format(nth, ' | '.join(tests), ' || '.join(sizes))

def define(name, *args, prefix = '', modify = ''):
    """Defines an op-code in the generated file."""
    condition = ''
    result = ''
    sh = 0
    nb = 1

    for arg in args:
        if isinstance(arg, str):
            test = mktest(nb, arg)

            if condition is '':
                condition = test
            else:
                condition += ' && {}'.format(test)

            nb += 1

        elif isinstance(arg, int):
            if sh == 0:
                result = '0x{:02x}'.format(arg)
            else:
                result += ' + (0x{:02x} << {})'.format(arg, sh)

            sh += 4

        else:
            raise Exception('Invalid argument')

    if prefix != '':
        raise Exception('Prefixes aren\'t supported yet')

    if condition != '':
        condition = 'if {} '.format(condition)

    inspect.currentframe().f_back.f_locals[outputvar].write('        "{}" {}=> {{ {} Some({}) }},\n'.format(name, condition, modify, result))

def write_head(prefix = ''):
    """Writes the head of the generated file."""
    inspect.currentframe().f_back.f_locals[outputvar].writelines([
            'use arch::{Operand, OpDef as D};\n\n',
            '#[allow(unused_variables)]\n',
            'pub fn get_opcode(name: &str, op1: &mut Operand, op2: &mut Operand, op3: &mut Operand) -> Option<u64> {\n',
            '    let (op1d, op1s) = (op1.definition(), op1.size());\n',
            '    let (op2d, op2s) = (op2.definition(), op2.size());\n',
            '    let (op3d, op3s) = (op3.definition(), op3.size());\n\n',
            '{}\n'.format(prefix),
            '    match name {\n'
    ])

def write_tail():
    """Writes the tail of the generated file."""
    inspect.currentframe().f_back.f_locals[outputvar].writelines([
        '        _ => None\n',
        '    }\n',
        '}\n'
    ])
