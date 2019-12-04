#!/usr/bin/env python

from setuptools import setup



setup(
    name='dyna',
    version='0.0.1',
    description='Dyna language based on R-expression',
    packages=['dyna'],
    entry_points = {
        'console_scripts': ['dyna=dyna.repl:main'],
    },
    install_requires=[
        # should put stuff here instead in requirements.txt???
    ]
)
