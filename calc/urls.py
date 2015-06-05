from django.conf.urls import patterns, url
from calc import views

urlpatterns = patterns('',
        url(r'^(?P<scriptname>[\w\-]+)', views.calculate, name='calculate')
    )
