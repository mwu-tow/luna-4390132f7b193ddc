#!/usr/bin/env python3

import atom_prepare
import atom_apm
import copy_configs
import stack_build
import os
import subprocess

app_dir      = atom_prepare.prep_path('..')
backend_dir  = atom_prepare.prep_path('../build/backend')
frontend_dir = atom_prepare.prep_path('../luna-studio')

def main ():
    try:
        stack_build.run()
        atom_prepare.run()
        atom_apm.run() # RENAME: LunaStudioInstallInAtom ?
        copy_configs.run()

    except subprocess.CalledProcessError:
        print("Status : FAIL")


main()
