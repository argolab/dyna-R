#!/usr/bin/env python

import os
from setuptools import setup
from Cython.Build import cythonize

# requirements = open(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'requirements.txt')).read().split('\n')
# requirements = [r.strip() for r in requirements if not r.strip().startswith('#') and r]

setup(
    name='dyna',
    version='0.0.1',
    description='Dyna language based on R-expression',
    packages=['dyna'],
    entry_points = {
        'console_scripts': ['dyna=dyna.repl:main'],
    },
    install_requires=[
    ],
    ext_modules = cythonize(['**/*.pyx'], compiler_directives={'language_level' : "3"}),
)
