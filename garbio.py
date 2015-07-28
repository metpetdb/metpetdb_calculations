import urllib2
import json

if __name__ == "__main__":
	# the default test values are garnet=0.8, biotite=0.5, p=5000
	garnet_val = float(raw_input("Input Fe/Fe+Mg for garnet ==> "))
	biotite_val = float(raw_input("Input Fe/Fe+Mg for biotite ==> "))
	lst = raw_input("List of results? y/N ==> ")

	pressures = []
	if lst.lower() == 'y' or lst.lower() == 'yes':
		# create a list of results for different pressures for graphing
		p = raw_input("Input pressure low,high,interval_length separated by commas ==> ").split(",")
		pressures = range(int(p[0]),int(p[1]),int(p[2]))
	else:
		# just display one result
		pressures = [raw_input("Input pressure in bars ==> ")]

	for p in pressures:
		q = 'http://54.152.139.14/calc/?scriptname=garnbiotTC&format=json&garnet=%f&biotite=%f&p='+str(p)
		f = urllib2.urlopen(q%(garnet_val, biotite_val))
		response = json.loads(f.read())
		print "TC at", p, "bars", response['TC']
