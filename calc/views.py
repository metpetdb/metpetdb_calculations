import sys, os, json
from django.http import HttpResponse

def calculate(request,scriptname):
	if request.GET.get('format','') != 'json':
        	return HttpResponse('Try appending ?format=json to the URL')
	data = {}
	with open(os.path.dirname(os.path.dirname(__file__))+'/static/calculations.json') as file:
		data = json.load(file)
	if scriptname not in data.keys() or len(request.GET) < 2:
		return HttpResponse(json.dumps(data))
	good = False
	vars = {}
	for r in data[scriptname]:
		vars[r] = float(request.GET.get(r,0))
		if vars[r] != 0:
			good = True
	if not good:
		return HttpResponse(json.dumps(data))
	sys.path.insert(1,os.path.dirname(os.path.dirname(__file__))+'/static/'+scriptname)
	mod = __import__('simple', globals(), locals(), [], -1)
	return HttpResponse(json.dumps(mod.calculate(vars)))
