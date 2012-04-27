#!/usr/bin/env python

import tempfile
import shutil
import subprocess
directory = tempfile.mkdtemp()
allegro = '%s/allegro' % directory
print "Created directory %s" % allegro

def copy_dir(what, to):
    import os
    # os.mkdir('%s/%s' % (to, what))
    print "Copying %s" % what
    shutil.copytree(what, '%s/%s' % (to, what))

subprocess.call(['svn', 'export', 'svn://crystalis.cs.utah.edu/code/allegro-scheme', allegro])

copy_dir('allegro-4.2.0', allegro)
copy_dir('libpng-1.2.5', allegro)

subprocess.call(['planet', 'create', allegro])

shutil.rmtree(directory, True)
print "Built allegro.plt"
