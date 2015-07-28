import sys, os, json
from django.http import StreamingHttpResponse

sys.path.insert(1,os.path.dirname(os.path.dirname(__file__))+'/static/fortran')
from GTBforMetPetDB_subs import *

def calculate(request):
	if request.GET.get('format','') != 'json':
		return HttpResponse('Try appending ?format=json to the URL')

	reqargs = dict(request.GET)
	args = {}
	for r in reqargs:
		if r != 'scriptname' and r != 'format':
			args[r] = float(reqargs[r][0])
	scriptname = request.GET.get('scriptname','').lower()

	if scriptname == 'garnbiottc':
		sys.path.insert(1,os.path.dirname(os.path.dirname(__file__))+'/static/garnbiotTC')
		from garnbiotTC import garbio
		args['TC'] = garbio(args['garnet'], args['biotite'], args['p'], args['TC'])
	elif scriptname == 'garbio_fe_mg':
		args['TC'] = garbio_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['Fe3Garnet'], args['SiBiotite'], args['AlBiotite'],
			args['TiBiotite'], args['Fe3Biotite'], args['MgBiotite'], args['FeBiotite'],
			args['MnBiotite'], args['Pbars'], args['TC'], args['iCalib'], args['iError'])
	elif scriptname == 'garchl_fe_mg':
		args['TC'] = garchl_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['MgChl'], args['FeChl'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'garhbl_fe_mg':
		args['TC'] = garhbl_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['MgHbl'], args['FeHbl'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'garphen_fe_mg':
		args['TC'] = garphen_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['MgMus'], args['FeMus'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'garilm_fe_mn':
		args['TC'] = garphen_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['FeIlm'], args['MnIlm'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'garopx_fe_mg':
		args['TC'] = garphen_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['FeOpx'], args['MgOpx'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'garolivine_fe_mg':
		args['TC'] = garphen_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['FeOlivine'], args['MgOlivine'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'gartourmaline_fe_g':
		args['TC'] = garphen_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['FeTour'], args['MgTour'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'biotitetourmaline_fe_mg':
		args['TC'] = biotitetourmaline_fe_mg(args['FeBiotite'], args['MgBiotite'], args['FeTour'],
			args['MgTour'], args['Pbars'], args['TC'], args['iCalib'], args['iError'])
	elif scriptname == 'garcord_fe_mg':
		args['TC'] = garphen_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['FeCord'], args['MgCord'], args['Pbars'], args['TC'],
			args['iCalib'], args['iError'])
	elif scriptname == 'garcpx_fe_mg':
		args['TC'] = garcpx_fe_mg(args['FeGarnet'], args['MgGarnet'], args['MnGarnet'],
			args['CaGarnet'], args['Fe3Garnet'], args['FeCpx'], args['MgCpx'], args['Pbars'],
			args['TC'], args['iCalib'], args['iError'])
	elif scriptname == 'hbldplag_na_ca':
		args['TC'] = hbldplag_na_ca(args['SiHbl'], args['AlHbl'], args['TiHbl'],
			args['Fe3Hbl'], args['MgHbl'], args['FeHbl'], args['MnHbl'], args['CaHbl'],
			args['NaHbl'], args['KHbl'], args['NaPlag'], args['CaPlag,'], args['KPlag,'],
			args['Pbars'], args['TC'], args['iCalib'], args['iError'])
	elif scriptname == 'muscbiot_tschermak':
		args['TC'] = muscbiot_tschermak(args['SiBiotite'], args['AlBiotite'], args['TiBiotite'],
			args['Fe3Biotite'], args['MgBiotite'], args['FeBiotite'], args['MnBiotite'],
			args['Kbiotite'], args['SiMuscovite'], args['AlMuscovite'], args['TiMuscovite'],
			args['Fe3Muscovite'], args['MgMuscovite'], args['FeMuscovite'], args['MnMuscovite'],
			args['KMuscovite'], args['Pbars'], args['TC'], args['iCalib'], args['iError'])
	elif scriptname == 'gs':
		args['Ggar'] = gs(args['TK'], args['P'], args['Xpy'],
			args['Xalm'], args['Xsp'], args['Xgr'], args['Ggar'],
			args['Imole'], args['imod'])
	else:
		return StreamingHttpResponse("Invalid scriptname")

	return StreamingHttpResponse(json.dumps(args))
