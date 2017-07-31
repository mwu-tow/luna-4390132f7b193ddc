#!/usr/bin/env python3

from . import atom_prepare as ap
import os
import subprocess
from . import system as system


app_dir      = ap.prep_path('..')
backend_dir  = ap.prep_path('../build/backend')
frontend_dir = ap.prep_path('../luna-studio')
runner_dir   = ap.prep_path('../runner')


def create_bin_dirs():
    for path in ('../dist/bin/private', '../dist/bin/public/luna-studio'):
        os.makedirs(ap.prep_path(path), exist_ok=True)

def build_ghcjs(frontend):
    os.chdir(frontend)
    if system.system == system.systems.WINDOWS:
        return ()
    elif system.system == system.systems.LINUX:
        subprocess.check_output(['stack', 'build'])
    elif system.system == system.systems.DARWIN:
        subprocess.check_output(['stack', 'build'])
    else: print("unknown system")

def build(backend,runner):
    os.chdir(backend)
    subprocess.check_output(['stack', 'build', '--copy-bins'])
    os.chdir(runner)
    subprocess.check_output(['stack', 'build', '--copy-bins'])

def link_main_bin ():
    os.chdir(ap.prep_path('../dist/bin'))
    os.symlink('./public/luna-studio', 'main', target_is_directory=True)

def run():
    create_bin_dirs()
    build_ghcjs(frontend_dir)
    build(backend_dir, runner_dir)
    link_main_bin ()

if __name__ == '__main__':
    run()
