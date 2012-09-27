#!/usr/bin/env python

import sys
import getopt

def main(argv):
	try:
		if argv[1]:
			writeKb(argv[1])
	except Exception, e:
		raise e

def writeKb(inputFile):
	islands = []
	try:
		f = open(inputFile, 'r')
		print 'Parsing file: {0}'.format(inputFile)

		islandsDimensions = f.readline().split(' ')
		nrRows = int(islandsDimensions[0])
		nrCols = int(islandsDimensions[1])
		for r in range(nrRows):
			line = f.readline()
			for c in range(nrCols):
				try:
					islands.append([[r+1,c+1], int(line[c])])
				except Exception, e:
					islands.append([[r+1,c+1], line[c]])				
	except Exception, e:
		print 'Could not open file: {0}\n{1}'.format(inputFile, e)
		return

	inputFile = inputFile.split('/')[1].split('.')[0]
	dbFile = inputFile + "_db.pl"
	try:
		f = open(dbFile, 'w')
		print 'Writing database file: {0}'.format(dbFile)
		prettyPrint = lambda x, comma: f.write('\t{0}{1}\n'.format(x, comma))
		f.write('rows({0}).\ncolumns({1}).\n'.format(nrRows, nrCols))
		f.write('grid([\n')
		i = 1
		for island in islands:
			if i < len(islands):
				prettyPrint(island, ',')
			else:
				prettyPrint(island, '')
			i += 1
		f.write(']).')
	except Exception, e:
		print 'Could not write file: {0}\n{1}'.format(dbFile, e)
	finally:
		pass
	
	engineFile = inputFile + '_engine.pl'
	try:
		f = open(engineFile, 'w')
		print 'Writing engine file: {0}'.format(engineFile)
		f.write(':-consult(\'{0}\').\n'.format(dbFile))
		f.write(':-consult(\'hashi.pl\').\n')
		f.write(':-grid(Grid),\n')
		f.write('islands(Grid,Islands),\n')
		f.write('transform(Grid,ListMatrix),\n')
		f.write('generate(ListMatrix, Solution),\n'),
		f.write('test(Solution,Islands),\n')
		f.write('writeSolution(Solution),\n')
		f.write('halt.')
	except Exception, e:
		raise e
if __name__ == "__main__":
	main(sys.argv[1:])