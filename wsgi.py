import os, sys

sys.path.append('/home/ubuntu/metpetdb/metpetdb_calculations')
os.environ['DJANGO_SETTINGS_MODULE'] = 'metpetdb_calculations.settings'

activate_this = '/home/ubuntu/.virtualenvs/metpetdb/bin/activate_this.py'
execfile(activate_this, dict(__file__=activate_this))

import django.core.handlers.wsgi
application = django.core.handlers.wsgi.WSGIHandler()
