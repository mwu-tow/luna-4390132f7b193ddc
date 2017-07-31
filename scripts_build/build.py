#!/usr/bin/env python3

from . import atom_prepare as ap
import atom_apm
import copy_configs
import stack_build
import os
import subprocess

app_dir      = ap.prep_path('..')
backend_dir  = ap.prep_path('../build/backend')
frontend_dir = ap.prep_path('../luna-studio')

def main ():
    try:
        stack_build.run()
        ap.run()
        atom_apm.run() # RENAME: LunaStudioInstallInAtom ?
        copy_configs.run()

    except subprocess.CalledProcessError:
        print("Status : FAIL")


main()
