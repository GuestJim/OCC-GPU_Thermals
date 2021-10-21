import sys, os, shutil

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"

os.chdir(droppedPath)

Z	=	1

for file in os.listdir(droppedPath):
	if file.endswith("Stats.txt"):
		droppedSTAT	=	file

TYPE	=	"Data GPU\\"
if TYPE in droppedPath:
	droppedCOOL	=	droppedPath.split(TYPE)[1].rsplit("\\", 2)[0]
	droppedList	=	droppedPath.split(TYPE)[0]	+	TYPE
elif "Data\\" in droppedPath:
	droppedCOOL	=	droppedPath.split("Data\\")[1].rsplit("\\", 2)[0]
	droppedList	=	droppedPath.split("Data\\")[0]	+	"Data\\"

if	"GPUs.txt"	in	os.listdir(droppedList):
	GPUs	=	open(droppedList + "GPUs.txt", 'r').readlines()
	GPUs	=	[line.rstrip('\n') for line in GPUs]
else:
	GPUs	=	[]
	for DIR in os.listdir(droppedList):
		if os.path.isdir(droppedList + DIR) and "@" not in DIR:
			GPUs.append(DIR)
#	ternary operator form:
#	[GPUs.append(x)	if os.path.isdir(droppedList + x) and "@" not in x	else None	for x in os.listdir(droppedList)]

if	"Coolers.txt"	in	os.listdir(droppedList):
	COOL	=	open(droppedList + "Coolers.txt", 'r').readlines()
	COOL	=	[line.rstrip('\n') for line in COOL]
else:
	COOL	=	""

BENCHs	=	[\
["3DMark",	1]
]

TESTgpu	=	[\
["Fire Strike",	1],\
["Time Spy",	2],\
["Port Royal",	3],\
[" (RT Off)",	4]
]

RESO	=	[\
["Extreme",		1],\
["Ultra",		2]
]

PACE	=	[\
["Loop",	1]
]

TYPEs	=	[\
['Frequency',			0],\
['Course',				0],\
['FPS',					1],\
['Hist - Temperature',	2],\
['Hist - Power', 		3],\
['Hist - Frequency',	4],\
['Hist - Frame',		5],\
['TS - Frame',			6],\
['TS - Frequency',		7],\
['TS - Power',			8],\
['TS - Temperature',	9]
]

def	codFind	(statname, list):
	for i in reversed(range(len(list))):
		if list[i][0] in statname:
			return(list[i][1])

def numFind	(filename, list):
	if list == [""]:
		return(0)
	for i in reversed(range(len(list))):
		if list[i] in filename:
			return(i+1)
	return(0)

def codGraph	(filename, list):
	for i in range(len(list)):
		if list[i][0] + ".png" == filename:
			return(list[i][1])

def numGen (filename):
	CODE	=	[	[],	[],	[],	[],	[],	[],	[]	]
	#	GPU, Cooler, Bench, Test, Reso,	Pace, Graph
	CODE[0]	=	numFind(droppedCOOL.split("\\")[0],	GPUs)
	CODE[1]	=	numFind(droppedCOOL.split("\\")[1],	COOL)	if	"\\" in droppedCOOL	else 0
	CODE[2]	=	codFind(droppedSTAT,	BENCHs)
	CODE[3]	=	codFind(droppedSTAT,	TESTgpu)
	CODE[4]	=	codFind(droppedSTAT,	RESO) or 0
	CODE[5]	=	codFind(droppedSTAT,	PACE) or 2
	CODE[6]	=	codGraph(filename,		TYPEs)

	code	=	""
	for x in CODE:
		if x != "":
			code	=	code + str(x).zfill(Z)

	return(code)

if not os.path.exists("@Graphs"):
	os.mkdir("@Graphs")

for file in os.listdir(droppedPath):
	if file.endswith(".png"):
		print(numGen(file))
		shutil.copyfile(file, "@Graphs\\" + numGen(file) + ".png")

# os.system("pause")