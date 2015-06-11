<a name="logo"/>
<div align="center">
<a href="http://metpetdb.rpi.edu/" target="_blank">
<img src="http://metpetdb.rpi.edu/metpetweb/images/mpdb-logo.gif" alt="mpdb Logo" width="201" height="148"></img>
</a>
</div>

##MetPetDB-calculations

Fortran calculations for the Metpetdb system built in Django
(metpetdb.rpi.edu)

The API is built in Django and can be found here: https://github.com/metpetdb/metpetdb-py
The interface is built in Flask and can be found here: https://github.com/metpetdb/metpetdb_interface

We will now set up the system and virtual environment for a new/clean meachine, which means `/usr/lib/python2.7/site-packages` is not modified globally for any application. Let's start from installing global applications to virtual environment.

**Before setting up everything:**

	$ sudo apt-get update
	$ sudo apt-get upgrade -y

**Note: This setup instruction is tested on ubuntu 14.04. If you are using different release of Linux, pre-installed dependencies may vary according to your OS**
	
Apache Web Server Installation
------------------------------

####Install Apache
    $ sudo apt-get install apache2 -y
    $ sudo apt-get install apache2-threaded-dev python2.7-dev -y
	
####Install mod_wsgi

Now we install mod_wsgi by typing the following command

	$ sudo apt-get install libapache2-mod-wsgi python-dev -y

To enable mod_wsgi, run the following command:

	$ sudo a2enmod wsgi 
	
Restart Apache to get mod_wsgi to work.

	$ sudo service apache2 restart

## Python and Virtualenvwrapper setup

Virtualenv is probably what you want to use during development.
What problem does virtualenv solve? If you want to use Python for other projects besides Django-based web applications. it is very likely that you will be working with different versions of Python itself, or different versions of Python libraries. Quite often, libraries break backwards compatibility, and it’s unlikely that any serious application will have zero dependencies. So we create virtual environment to keep different project environments isolated if two or more of your projects have conflicting dependencies.

If Python 2.7 is not installed, install it

	$ sudo apt-get install python2.7

Install pip

	$ sudo apt-get install python-pip -y

Then use pip to install virtualenv:

	$ sudo pip install virtualenvwrapper
	
Check if virtualenvwrapper.sh and virtualenvwrapper_lazy.sh exist:

	$ which virtualenvwrapper.sh
	/usr/local/bin/virtualenvwrapper.sh
	
	$ which virtualenvwrapper_lazy.sh
	/usr/local/bin/virtualenvwrapper_lazy.sh
	
After this, we create a directory for the virtual environments:

	$ mkdir ~/.virtualenvs
	
Then, edit the .bashrc file:

	$ sudo nano ~/.bashrc
	
	# add these two lines at the end of the ~/.bashrc
	export WORKON_HOME=$HOME/.virtualenvs
	source /usr/local/bin/virtualenvwrapper_lazy.sh
	
	$ source ~/.bashrc
	
####Starting a New Virtual Environment

Virtualenvwrapper provides some nice commands we can use to play around with the environments.

To create a new virtual environment:

	$ mkvirtualenv metpetdb
	New python executable in metpetdb/bin/python
	Installing setuptools, pip...done.
	
As you might notice, the command prompt contains the name before you username
	
	**(metpetdb)**user@xxxx:~$ python --version
	Python 2.7.6

To deactivate and exit the virtual environment, just do

	$ deactivate
	
Tht next time you come back, start you environment by doing:

	$ workon environment_name

Other usage command:
- Remove your current environment: `rmvirtualenv environment_name`
- show a list of environments: `workon`

## Setting up MetpetDB calculations

If you have created a virtual environment for MetpetDB calculations and you have not yet fooled around with its settings and packages, start the virtual environment: `workon environment_name` 

If you are not sure, create a new clean virtual environment:
	
	$ mkvirtualenv metpetdb
	$ workon metpetdb

Create a directory for the project:

	$ mkdir metpetdb
	$ cd metpetdb
	
From here on out, we assume your environment name is metpetdb. Then we clone the calculations code from github:
	
	$ sudo apt-get install git -y && git clone https://github.com/metpetdb/metpetdb_calculations.git
	
We are close to finishing setting up MetpetDB locally. Next, install required dependentcies for the calculations.
		
	$ cd metpetdb_calculations
	$ pip install -r requirements.txt
	
Finally, run app.py to test if we have set up the calculations properly:

	$ python manage.py runserver

Visit `http://127.0.0.1:5000/` or `localhost:5000/` to view the project.

## Configuring Apache to Serve the Application
There should be a file named wsgi.py in your metpetdb_calculations directory. If not, here's what it should look like:

	import os, sys

	sys.path.append('/home/ubuntu/metpetdb/metpetdb_calculations')
	os.environ['DJANGO_SETTINGS_MODULE'] = 'metpetdb_calculations.settings'

	activate_this = '/home/ubuntu/.virtualenvs/metpetdb/bin/activate_this.py'
	execfile(activate_this, dict(__file__=activate_this))

	import django.core.handlers.wsgi
	application = django.core.handlers.wsgi.WSGIHandler()

Replace /home/ubuntu/metpetdb/metpetdb_calculations with the path to the project directory on your machine and replace /home/ubuntu with the path to your home directory. Now, we want to edit the default conf file to run our project:

	$ sudo nano /etc/apache2/sites-available/000-default.conf

Inside this file, you should replace everything with the following:

	<VirtualHost *:80>
        ServerName 127.0.1.1
        ServerAdmin admin@localhost
        DocumentRoot "/home/ubuntu/metpetdb/metpetdb_calculations"

        Alias /static/ /home/ubuntu/metpetdb/metpetdb_calculations/static/
        <Directory /home/ubuntu/metpetdb/metpetdb_calculations/static>
                Require all granted
        </Directory>
        <Location "/static/">
                Options -Indexes
        </Location>

        ErrorLog ${APACHE_LOG_DIR}/error.log
        CustomLog ${APACHE_LOG_DIR}/access.log combined
	</VirtualHost>

Remember to replace /home/ubuntu/metpetdb/metpetdb_calculations and if you're not running this locally, change 127.0.0.1 to your URL of choice. Lastly, we want to edit the apache.conf file:

	$ sudo nano /etc/apache2/apache2.conf

You should see a section that looks something like this:

	<Directory />
		Options FollowSymLinks
		AllowOverride None
		Require all granted
	</Directory>

	...

	#<Directory /srv/>
	#	Options Indexes FollowSymLinks
	#	AllowOverride None
	#	Require all granted
	#</Directory>

Add these lines right after that section:

	<Directory /home/ubuntu/metpetdb/metpetdb_calculations>
		<Files wsgi.py>
		Order deny,allow
		Require all granted
		</Files>
	</Directory>

	WSGIScriptAlias / home/ubuntu/metpetdb/metpetdb_calculations/wsgi.py
	WSGIPythonPath /home/ubuntu/metpetdb/metpetdb_calculations:/home/user/.virtualenvs/metpetdb/lib/python2.7/site-packages

	ServerName 127.0.0.1

Again, make sure you replace /home/ubuntu/metpetdb/metpetdb_calculations and /home/ubuntu with the correct paths and 127.0.0.1 with your URL.

Now run

	$ sudo service apache2 restart

to restart apache with these new configurations. Try going to 127.0.0.1 in your favorite browser to be sure it worked!
