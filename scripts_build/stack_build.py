#!/usr/bin/env python3

from . import atom_prepare as ap
from glob import glob
import os
import subprocess
from . import system as system


app_dir      = ap.prep_path('..')
backend_dir  = ap.prep_path('../build-config/backend')
frontend_dir = ap.prep_path('../luna-studio')
runner_dir   = ap.prep_path('../runner')


def create_bin_dirs():
    for path in ('../dist/bin/private', '../dist/bin/public/luna-studio'):
        os.makedirs(ap.prep_path(path), exist_ok=True)

def build_ghcjs(frontend_args):
    os.chdir(frontend_dir)
    if system.system == system.systems.WINDOWS:
        return ()
    elif system.system == system.systems.LINUX:
        subprocess.check_output(['stack', 'build'] + frontend_args)
    elif system.system == system.systems.DARWIN:
        subprocess.check_output(['stack', 'build'] + frontend_args)
    else: print("unknown system")

def build_runner(runner, runner_args):
    os.chdir(runner)
    print ("build runner")
    runnerPath = runner + '/src/StudioRunner.hs'
    hostPath = runner + '/src/System/Host.hs'
    resPath = runner + '/../resources/my.res'
    if system.system == system.systems.WINDOWS:

        subprocess.check_output(['stack', 'build'])
        os.system('stack exec ghc -- ' + runnerPath + ' ' + hostPath + ' ' + resPath)
    elif system.system == system.systems.LINUX:
        subprocess.check_output(['stack', 'build'] + runner_args)
    elif system.system == system.systems.DARWIN:
        subprocess.check_output(['stack', 'build'] + runner_args)
    else: print("unknown system")

def build_backend(backend_args):
    os.chdir(backend_dir)
    subprocess.check_output(['stack', 'build'] + backend_args)

def mv_runner(runner):
    if system.system == system.systems.WINDOWS:
        runner_src = runner + '/src/' + '/StudioRunner.exe'
        runner_dst = ap.prep_path('../dist/bin/public/luna-studio/luna-studio.exe')
        os.rename(runner_src, runner_dst)
    elif system.system == system.systems.LINUX:
        return ()
    elif system.system == system.systems.DARWIN:
        return ()
    else: print("unknown system")


def link_main_bin ():
    os.chdir(ap.prep_path('../dist/bin'))
    os.makedirs('main', exist_ok=True)
    for src_path in glob('public/luna-studio/*'):
        dst_path = os.path.join('main', os.path.basename(src_path))
        if os.path.isfile(dst_path):
            if os.path.isfile(src_path):
                os.remove(dst_path)
                os.symlink(os.path.relpath(src_path,'main/'),os.path.join('main', os.path.basename(src_path)))
            else: return ()
        else:
            if os.path.isfile(src_path):
                os.symlink(os.path.relpath(src_path,'main/'),os.path.join('main', os.path.basename(src_path)))
            else: return ()


    # os.symlink('./public/luna-studio', 'main', target_is_directory=True)
def copy_std_lib ():
    std_lib_path = ap.prep_path ( '../build-config/backend/.stack-work') + '/**/stdlib'
    std_lib_folder = glob(std_lib_path,recursive=True)
    print (std_lib_folder)


def run(backend_args, frontend_args, runner_args):
    create_bin_dirs()
    build_runner(runner_dir, runner_args)
    mv_runner(runner_dir)
    build_ghcjs(frontend_args)
    build_backend(backend_args)
    link_main_bin ()

# if __name__ == '__main__':
#     run()
