"""
WSGI config for mpdb_calc project.

It exposes the WSGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/1.7/howto/deployment/wsgi/
"""

import os
import sys
import logging
logging.basicConfig(stream=sys.stderr)

# Add the app's directory to the PYTHONPATH
sys.path.insert(1,os.getcwd())
sys.path.insert(1,os.path.join(os.getcwd(),'metpetdb_calculations'))

os.environ['DJANGO_SETTINGS_MODULE'] = 'metpetdb_calculations.settings'

import django.core.handlers.wsgi
application = django.core.handlers.wsgi.WSGIHandler()
