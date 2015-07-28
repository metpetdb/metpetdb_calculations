from django.conf.urls import patterns, url
from calc import views

urlpatterns = patterns('',
        url(r'^$', views.calculate, name='calculate')
    )
