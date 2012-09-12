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
	# finally:	# finally:

	# 	f.close()

	outputFile = inputFile.split('/')[1].split('.')[0] + ".pl"
	try:
		f = open(outputFile, 'w')
		prettyPrint = lambda x, comma: f.write('\t{0}{1}\n'.format(x, comma))
		f.write('islands([\n')
		i = 1
		for island in islands:
			if i < len(islands):
				prettyPrint(island, ',')
			else:
				prettyPrint(island, '')
			i += 1
		f.write(']).')
	except Exception, e:
		print 'Could not write file: {0}\n{1}'.format(outputFile, e)
	finally:
		pass
	
if __name__ == "__main__":
	main(sys.argv[1:])