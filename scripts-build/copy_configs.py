#!/usr/bin/env python3

import atom_prepare
import os
import distutils
import subprocess


resources_dir = atom_prepare.prep_path('../resources')
supervisor_dir = atom_prepare.prep_path('../supervisor')
env_dir = atom_prepare.prep_path('../env')


def copy_configs(supervisor,env):
    config_path = atom_prepare.prep_path('../dist/config/')
    supervisor_path = config_path + '/supervisor'
    env_path = config_path + '/env'
    distutils.dir_util.copy_tree(supervisor, supervisor_path)
    distutils.dir_util.copy_tree(env, env_path)

def copy_resources(resources):
    resources_path=atom_prepare.prep_path('../dist/bin/public/luna-studio/resources')
    distutils.dir_util.copy_tree(resources, resources_path)

def run():
    copy_configs(supervisor_dir,env_dir)
    copy_resources(resources_dir)

if __name__ == '__main__':
    run()
