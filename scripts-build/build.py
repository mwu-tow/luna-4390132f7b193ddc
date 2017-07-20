#!/usr/bin/env python3


import os
import subprocess

def prep_path(path):
    script_abs_path = os.path.abspath(os.path.dirname(__file__))
    return os.path.normpath(os.path.join(script_abs_path, path))

app_dir = prep_path('..')
backend_dir = prep_path('./build/backend')
frontend_dir = prep_path('./luna-studio')
try:
    os.chdir(backend_dir)
    subprocess.check_output(['stack', 'build', '--copy-bins'])
    os.chdir(frontend_dir)
    subprocess.check_output(['stack', 'build'])

except subprocess.CalledProcessError:
    print("Status : FAIL")
