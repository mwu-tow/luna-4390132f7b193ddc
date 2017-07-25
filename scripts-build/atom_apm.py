#!/usr/bin/env python3

import atom_prepare

import os
import distutils.dir_util
import fileinput
import glob
import subprocess
import shutil
import platform
import sys

third_party_path = atom_prepare.prep_path('../dist/third-party/')
atom_home_path = atom_prepare.prep_path('../dist/user-config/atom')
studio_package_name = "luna-studio"
studio_atom_source_path = atom_prepare.prep_path("../luna-studio/atom")

def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)


systems = enum ('LINUX', 'WINDOWS', 'DARWIN')

osname = platform.system()
system = None
if osname == 'Windows' or osname.startswith('CYGWIN_NT'):
    system    = systems.WINDOWS
    osname = 'mingw32'
elif osname == 'Linux':
    system    = systems.LINUX
    osname = 'linux'
elif osname == 'Darwin':
    system    = systems.DARWIN
    osname = 'darwin'
else:
    print_error ("Unsupported system '%s'" % osname)
    sys.exit(1)

def apm_path(third_party_path):
    if   system == systems.WINDOWS: return ()
    elif system == systems.LINUX:   return third_party_path + '/atom/usr/share/atom/resources/app/apm/bin/apm'
    elif system == systems.DARWIN:  return third_party_path + "/Atom.app/Contents/Resources/app/apm/bin/apm"
    else: print("unknown system")

def oniguruma_path(third_party_path):
    if system == systems.WINDOWS:
        return ()
    elif system == systems.LINUX:
        return third_party_path + '/atom/usr/share/atom/resources/app/node_modules/oniguruma'
    elif system == systems.DARWIN:
        return third_party_path + '/Atom.app/Contents/Resources/app/node_modules/oniguruma'

def apm(third_party_path, atom_home_path, studio_package_name):
    package_path = atom_home_path + '/packages/' + studio_package_name
    oniguruma_package_path = package_path + '/node_modules/oniguruma'
    oniguruma = oniguruma_path(third_party_path)
    apm = apm_path(third_party_path)
    os.makedirs(package_path, exist_ok=True)
    distutils.dir_util.copy_tree(studio_atom_source_path, package_path)
    distutils.dir_util.copy_tree(oniguruma, oniguruma_package_path)
    os.chdir(package_path)
    popen = subprocess.Popen((apm, 'install', '.'), stdout=subprocess.PIPE)
    popen.wait()
    output = popen.stdout.read()
    print(output)


def run():
    apm(third_party_path, atom_home_path, studio_package_name)

if __name__ == '__main__':
    run()
