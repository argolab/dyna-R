#!/usr/bin/env python

import os
from setuptools import setup


# this is only going to work in the csae that nothing is install directly from git etc
requirements = open(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'requirements.txt')).read().split('\n')
requirements = [r.strip() for r in requirements if not r.strip().startswith('#') and r]

setup(
    name='dyna',
    version='0.0.1',
    description='Dyna language based on R-expression',
    packages=['dyna', 'dyna_frontend', 'dyna_match_paper'],
    entry_points = {
        'console_scripts': ['dyna=dyna_frontend.repl:main'],
    },
    install_requires= requirements + [
    ]
)
