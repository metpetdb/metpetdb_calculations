from garnbiotTC import *

def variables():
	return ['garnet','biotite','p','TC']

def calculate(args):
	args['TC'] = garbio( args['garnet'], args['biotite'], args['p'], args['TC'] )
	return args
