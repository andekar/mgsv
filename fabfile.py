from __future__ import with_statement
from fabric.api import local, settings, abort, run, cd
from fabric.contrib.console import confirm

def deploy():
    code_dir = '/home/anders/mgsv/mgsv_deployed'
    with cd(code_dir):
        run("git pull")
        run("make release")
        run("/home/anders/mgsv/mgsv_deployed/rel/mgsv/bin/mgsv stop")
        run("/home/anders/mgsv/mgsv_deployed/rel/mgsv/bin/mgsv start")

def deploy_test():
    code_dir = '/home/anders/mgsv/mgsv_test'
    with cd(code_dir):
        run("git pull")
        run("make release")
        run("./make_test_server_config")
        run("make release")
        run("/home/anders/mgsv/mgsv_test/rel/mgsv/bin/mgsv stop")
        run("/home/anders/mgsv/mgsv_test/rel/mgsv/bin/mgsv start")

def deploy_full():
    code_dir = '/home/anders/mgsv/mgsv_test'
    with cd(code_dir):
        run("git pull")
        run("make release")
        run("/home/anders/mgsv/mgsv_test/rel/mgsv/bin/mgsv start")
    code_dir = '/home/anders/mgsv/mgsv_deployed'
    with cd(code_dir):
        run("git pull")
        run("make release")
        run("/home/anders/mgsv/mgsv_deployed/rel/mgsv/bin/mgsv start")
