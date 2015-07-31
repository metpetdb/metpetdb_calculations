import sys, os, json
from django.http import StreamingHttpResponse

sys.path.insert(1,os.path.dirname(os.path.dirname(__file__))+'/static/fortran')
from GTBforMetPetDB_subs import *

def calculate(request):
	if request.GET.get('format','') != 'json':
		return StreamingHttpResponse('Try appending ?format=json to the URL')

	reqargs = dict(request.GET)
	args = {}
	for r in reqargs:
		if r != 'scriptname' and r != 'format':
			args[r.lower()] = float(reqargs[r][0])
	scriptname = request.GET.get('scriptname','').lower()

	if scriptname == 'garnbiottc':
		sys.path.insert(1,os.path.dirname(os.path.dirname(__file__))+'/static/garnbiottc')
		from garnbiottc import garbio
		args['tc'] = garbio(args['garnet'], args['biotite'], args['p'], args['tc'])
	elif scriptname == 'garbio_fe_mg':
		args['tc'] = garbio_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['fe3garnet'], args['sibiotite'], args['albiotite'],
			args['tibiotite'], args['fe3biotite'], args['mgbiotite'], args['febiotite'],
			args['mnbiotite'], args['pbars'], 0, args['icalib'], 0)
	elif scriptname == 'garchl_fe_mg':
		args['tc'] = garchl_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['mgchl'], args['fechl'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'garhbl_fe_mg':
		args['tc'] = garhbl_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['mghbl'], args['fehbl'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'garphen_fe_mg':
		args['tc'] = garphen_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['mgmus'], args['femus'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'garilm_fe_mn':
		args['tc'] = garphen_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['feilm'], args['mnilm'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'garopx_fe_mg':
		args['tc'] = garphen_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['feopx'], args['mgopx'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'garolivine_fe_mg':
		args['tc'] = garphen_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['feolivine'], args['mgolivine'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'gartourmaline_fe_g':
		args['tc'] = garphen_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['fetour'], args['mgtour'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'biotitetourmaline_fe_mg':
		args['tc'] = biotitetourmaline_fe_mg(args['febiotite'], args['mgbiotite'], args['fetour'],
			args['mgtour'], args['pbars'], 0, args['icalib'], 0)
	elif scriptname == 'garcord_fe_mg':
		args['tc'] = garphen_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['fecord'], args['mgcord'], args['pbars'], 0,
			args['icalib'], 0)
	elif scriptname == 'garcpx_fe_mg':
		args['tc'] = garcpx_fe_mg(args['fegarnet'], args['mggarnet'], args['mngarnet'],
			args['cagarnet'], args['fe3garnet'], args['fecpx'], args['mgcpx'], args['pbars'],
			0, args['icalib'], 0)
	elif scriptname == 'hbldplag_na_ca':
		args['tc'] = hbldplag_na_ca(args['sihbl'], args['alhbl'], args['tihbl'],
			args['fe3hbl'], args['mghbl'], args['fehbl'], args['mnhbl'], args['cahbl'],
			args['nahbl'], args['khbl'], args['naplag'], args['caplag,'], args['kplag,'],
			args['pbars'], 0, args['icalib'], 0)
	elif scriptname == 'muscbiot_tschermak':
		args['tc'] = muscbiot_tschermak(args['sibiotite'], args['albiotite'], args['tibiotite'],
			args['fe3biotite'], args['mgbiotite'], args['febiotite'], args['mnbiotite'],
			args['kbiotite'], args['simuscovite'], args['almuscovite'], args['timuscovite'],
			args['fe3muscovite'], args['mgmuscovite'], args['femuscovite'], args['mnmuscovite'],
			args['kmuscovite'], args['pbars'], 0, args['icalib'], 0)
	else:
		return StreamingHttpResponse("Invalid scriptname")

	return StreamingHttpResponse(json.dumps(args))
