#!/usr/bin/env python

import sys
import getopt

V_BRIDGE = {
	1 : '|',
	2 : '"'
}

H_BRIDGE = {
	1 : '-',
	2 : '='
}

def main(argv):
	try:
		printsolutionFile(argv[1], argv[2])
	except Exception, e:
		print 'Usage: ./output.py -c <test.txt> <solution.txt>'

def printsolutionFile(mapFile, solutionFile):
	grid = []
	print 'Attempting to read input file: {0}'.format(mapFile)
	try:
		f = open(mapFile, 'r')

		gridDimensions = f.readline().split(' ')
		nrRows = int(gridDimensions[0])
		nrCols = int(gridDimensions[1])
		grid = ['.' for r in range(nrRows)]
		for c in range(nrRows):
			grid[c] = ['.' for a in range(nrCols)]
		
		for r in range(nrRows):
			line = f.readline()
			for c in range(nrCols):
				grid[r][c] = line[c]
	except Exception, e:
		print 'Error reading input file: {0}\n{1}'.format(mapFile, e)

	print 'Attempting to read solution file: {0}'.format(solutionFile)	
	try:
		f = open(solutionFile, 'r')
		for l in f.readlines():
			sRow,sCol,eRow,eCol,bridges = map( lambda x: int(x)-1, l.split(' '))
			bridges += 1;
			if sRow == eRow: # horizontal
				for i in interval(sCol, eCol):
					#print 'H: Start: {0},{1} End: {2},{3} Interval: {4}'.format(sRow, sCol, eRow, eCol, interval(sCol, eCol))
					grid[sRow][i] = H_BRIDGE[bridges]
			else:	# vertical
				for i in interval(sRow, eRow):
					#print 'V: Start: {0},{1} End: {2},{3} Interval: {4}'.format(sRow, sCol, eRow, eCol, interval(sCol, eCol))
					grid[i][sCol] = V_BRIDGE[bridges]
	except Exception, e:
		print 'Error reading solution file: {0}\n{1}'.format(solutionFile, e)	
	
	
	path = mapFile.split('/')
	if(len(path) == 0):
		solutionFile = mapFile.split('.')[0]
	else:
		solutionFile = path[len(path)-1].split('.')[0]
	solutionFile += '.out'
	print 'Writing solution file: {0}'.format(solutionFile)
	try:
		f = open(solutionFile, 'w')
		for r in range(nrRows):
			for c in range(nrCols):
				f.write(grid[r][c])
			f.write('\n')
	except Exception, e:
		print 'Could not write solution file{0}\n{1}'.format(solutionFileFile, e)

def interval(start, end):
	return filter(lambda x: x > min(start,end), range(max(start, end)))

if __name__ == "__main__":
	main(sys.argv[1:])