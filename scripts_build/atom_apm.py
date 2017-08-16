#!/usr/bin/env python3

from . import atom_prepare as ap

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
from . import system as system

third_party_path = ap.prep_path('../dist/third-party/')
atom_home_path = ap.prep_path('../dist/user-config/atom')
studio_package_name = "luna-studio"
studio_atom_source_path = ap.prep_path("../luna-studio/atom")
package_config_path = ap.prep_path("../config/packages")
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
        r = requests.get(url)
        z = zipfile.ZipFile(io.BytesIO(r.content))
        z.extractall(package_path)
    elif system.system == system.systems.LINUX:
        distutils.dir_util.copy_tree(studio_atom_source_path, package_path)
    elif system.system == system.systems.DARWIN:
        distutils.dir_util.copy_tree(studio_atom_source_path, package_path)

def apm(third_party_path, atom_home_path, studio_package_name):
    package_path = atom_home_path + '/packages/' + studio_package_name
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

def list_packages(third_party_path, atom_home_path):
    apm = apm_path(third_party_path)
    os.environ['ATOM_HOME'] = atom_home_path
    popen = subprocess.Popen((apm, 'list', '--installed', '--bare'), stdout=subprocess.PIPE)
    popen.wait()
    output = popen.stdout.read()
    return output

def apm_package(third_party_path, atom_home_path, package_name, package_version):
    apm = apm_path(third_party_path)
    os.environ['ATOM_HOME'] = atom_home_path
    popen = subprocess.Popen((apm, 'install', package_name + '@' + package_version), stdout=subprocess.PIPE)
    popen.wait()
    output = popen.stdout.read()
    print(output)


def apm_packages(third_party_path, atom_home_path, package_config_path):
    installed_packages = list_packages(third_party_path, atom_home_path)
    with open(package_config_path) as f:
        packages_list = f.read().splitlines()
        for package in packages_list:
            if str.encode(package.split()[0]) not in installed_packages:
                apm_package(third_party_path, atom_home_path, package.split()[0], package.split()[1])
            else:
                if str.encode(package.split()[0] + '@' + package.split()[1]) in installed_packages:
                    continue
                else:
                    apm = apm_path(third_party_path)
                    os.environ['ATOM_HOME'] = atom_home_path
                    popen = subprocess.Popen((apm, 'uninstall', package.split()[0]), stdout=subprocess.PIPE)
                    popen.wait()
                    output = popen.stdout.read()
                    apm_package(third_party_path, atom_home_path, package.split()[0], package.split()[1])

def run():
    apm(third_party_path, atom_home_path, studio_package_name)
    apm_packages(third_party_path, atom_home_path,  package_config_path)

if __name__ == '__main__':
    run()
