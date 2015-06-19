#!/usr/bin/env python

from __future__ import with_statement

import sys
import os
from distutils import core
from distutils.core import Extension
from distutils.command.build_ext import build_ext


def read_file(name):
    dir_name = os.path.dirname(os.path.abspath(__file__))
    file_name = os.path.join(dir_name, name)
    with open(file_name) as f_obj:
        return f_obj.read()

PYTHON_VERSION = sys.version[0] + '_' + sys.version[2]
C_NAME = '_pokereval_' + PYTHON_VERSION


core.setup(
    name = 'pokereval',
    py_modules = ['pokereval'],
    version = '1.39',
    description = '',
    author = '',
    author_email = '',
    url = '',
    license = read_file('COPYING'),
    package_data = {'pokereval': ["README"]},
    classifiers = [],
    ext_modules = [
        Extension(
            C_NAME,
            ['pypokereval.c'],
            include_dirs = ['include', '/usr/local/include/poker-eval', '/usr/include/poker-eval'],
            libraries = ['poker-eval'],
            define_macros = [
                ('PYTHON_VERSION', '"%s"' % PYTHON_VERSION),
                ('VERSION_NAME(w)', r'w ## ' + PYTHON_VERSION),
            ],
        ),
    ],
)

