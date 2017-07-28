#!/usr/bin/env python3

import atom_prepare

import os
import distutils.dir_util
import fileinput
import glob
import subprocess
import shutil
import platform
import requests
import zipfile
import io
import sys
import system

third_party_path = atom_prepare.prep_path('../dist/third-party/')
atom_home_path = atom_prepare.prep_path('../dist/user-config/atom')
studio_package_name = "luna-studio"
studio_atom_source_path = atom_prepare.prep_path("../luna-studio/atom")

url = "http://10.62.1.34:8000/studio.zip"

def apm_path(third_party_path):
    if system.system == system.systems.WINDOWS:
        return third_party_path + '/Atom/resources/app/apm/bin/apm.cmd'
    elif system.system == system.systems.LINUX:
        return third_party_path + '/atom/usr/share/atom/resources/app/apm/bin/apm'
    elif system.system == system.systems.DARWIN:
        return third_party_path + "/Atom.app/Contents/Resources/app/apm/bin/apm"
    else: print("unknown system")

def oniguruma_path(third_party_path):
    if system.system == system.systems.WINDOWS:
        return third_party_path + '/Atom/resources/app/node_modules/oniguruma'
    elif system.system == system.systems.LINUX:
        return third_party_path + '/atom/usr/share/atom/resources/app/node_modules/oniguruma'
    elif system.system == system.systems.DARWIN:
        return third_party_path + '/Atom.app/Contents/Resources/app/node_modules/oniguruma'

def copy_studio (studio_atom_source_path, package_path):
    if system.system == system.systems.WINDOWS:
        print("ahbgkij")
        r = requests.get(url)
        print("reequests")
        z = zipfile.ZipFile(io.BytesIO(r.content))
        print("zipfile")
        z.extractall(package_path)
        print("extractall")
    elif system.system == system.systems.LINUX:
        distutils.dir_util.copy_tree(studio_atom_source_path, package_path)
    elif system.system == system.systems.DARWIN:
        distutils.dir_util.copy_tree(studio_atom_source_path, package_path)

def apm(third_party_path, atom_home_path, studio_package_name):
    package_path = atom_home_path + '/packages/' + studio_package_name
    print(package_path)
    oniguruma_package_path = package_path + '/node_modules/oniguruma'
    print(oniguruma_package_path)
    oniguruma = oniguruma_path(third_party_path)
    apm = apm_path(third_party_path)
    os.makedirs(package_path, exist_ok=True)
    copy_studio(studio_atom_source_path, package_path)
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
