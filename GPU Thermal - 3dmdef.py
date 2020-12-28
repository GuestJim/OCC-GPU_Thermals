import os, sys, subprocess, time, psutil, shutil, datetime
#	for Intel CPUs, uses Power Gadget
#		https://software.intel.com/en-us/articles/intel-power-gadget
#	for AMD CPUs, uses uProf and GPUz (to get the CPU temperature)
#		https://developer.amd.com/amd-uprof/
#		https://www.techpowerup.com/gpuz/

print("\tUses GPU-z")
print("\thttps://www.techpowerup.com/gpuz/")
print("\tGPU-z needs to place its Sensor Log on the Desktop")
print("")

#	shortcut name lists
lnkGPUz			=	"GPU-z.lnk"
lnk3DMark		=	"3DMarkCmd.lnk"

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"

def lnkCheck(LNK):
	if not LNK.endswith(".lnk"):
		LNK	=	LNK + ".lnk"
	if not LNK.startswith("\"") and not LNK.endswith("\""):
		if " " in LNK:
			return("\"" + LNK + "\"")
	return(LNK)

lnkGPUz			=	lnkCheck(lnkGPUz)
lnk3DMark		=	lnkCheck(lnk3DMark)

def INPUT(DEFAULT, TEXT = "Default: !DEF!"):
	return(input(TEXT.replace("!DEF!", str(DEFAULT))) or DEFAULT)
#	to make changing the default value easier

def	_3DMARK(defin):
	if dataPath in defin:
		return(lnk3DMark + " --definition=\"" + defin + ".3dmdef\" --loop=0 --audio=off --online=off")
	elif os.path.exists(scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef"):
		return(lnk3DMark + " --definition=\"" + scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef\" --loop=0 --audio=off --online=off")
	else:
		return(lnk3DMark + " --definition=" + defin + ".3dmdef --loop=0 --audio=off --online=off")

def _3DMARK_DEF(test, time):
	if test[0]	==	"1" or test[0] == "2":
		out	=	"3DMark - "
		if test[1]	==	"1":
			out		=	out + "Fire Strike"
			defin	=	"firestrike"
			TEST	=	"FireStrikeGt1"
			LOOP	=	int(time / 30) + 5
		if test[1]	==	"2":
			out		=	out + "Time Spy"
			defin	=	"timespy"
			TEST	=	"TimeSpyGt1"
			LOOP	=	int(time / 60) + 5
		if test[1]	==	"3" or test[1]	==	"4":
			out		=	out + "Port Royal"
			defin	=	"portroyal"
			TEST	=	"PortRoyalGt1"
			LOOP	=	int(time / 108) + 5
		if test[1]	==	"4":
			out		=	out + " (RT Off)"
			
		if test[2]	==	"0":
			out		=	out + ""
			defin	=	defin + ""
			TEST	=	TEST + "P"
		if test[2]	==	"1":
			out		=	out + " - Extreme"
			defin	=	defin + "_extreme"
			TEST	=	TEST + "X"
		if test[2]	==	"2":
			out		=	out + " - Ultra"
			defin	=	defin + "_ultra"
			TEST	=	TEST + "R"
		
		if test[0] == "1":
			return([out, defin])
		elif test[0]	==	"2":
			out	=	out + " - Loop"
			TEST	=	TEST + "ST"
			defin =	defin + "_loop"

	XML	=	str("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
<benchmark>\n\
\n\
  <application_info>\n\
    <selected_workloads>\n\
      <selected_workload name=\"" + TEST + "\"/>\n\
    </selected_workloads>\n\
  </application_info>\n\
\n\
<!-- Fire Strike loop is approximately 30 seconds -->\n\
<!-- Time Spy loop is approximately 60 seconds -->\n\
<!-- Port Royal loop is approximately 110 seconds -->\n\
\n\
	<settings>\n\
		<setting>\n\
			<name>loop_count</name>\n\
			<value>" + str(LOOP) + "</value>\n\
		</setting>\n\
		!OPTS!\
	</settings>\n\
\n\
</benchmark>")

	if test[1] == 4:
		XML	=	XML.replace("!OPTS!", "\
	<setting>\
		<name>reflection_mode</name>\
		<value>traditional</value>\
	</setting>\
	<setting>\
		<name>disable_rt_shadows</name>\
		<value>1</value>\
	</setting>"
		)
	else:
		XML	=	XML.replace("!OPTS!", "")

	with open(dataPath + defin + ".3dmdef", 'w') as fout:
		fout.write(XML)
		fout.close()
	return([out, dataPath + defin])

def	kill(proc_pid):
    process	=	psutil.Process(proc_pid)
    for proc in process.children(recursive=True):
        proc.kill()
    process.kill()


GPUname		=	input("GPU Name: ")

COOLERname	=	INPUT("",		"GPU Cooler Name (default empty): ")

zeroRPM		=	INPUT("NULL",	"GPU Zero RPM Threshold (default empty): ")
maxPOWER	=	INPUT("NULL",	"GPU Maximum Power (default empty): ")

# print("Available Tests:")
print("Available Tests (3DMark requires Professional Edition) :")
options	=	[
	"It is necessary for this console to be focused for 3DMark to launch correctly",
	"110 \t-\t 3DMark Fire Strike (1080p)",
	"111 \t-\t 3DMark Fire Strike Extreme (1440p)",
	"112 \t-\t 3DMark Fire Strike Ultra (2160p)",
	"",
	"120 \t-\t 3DMark Time Spy (1440p)",
	"121 \t-\t 3DMark Time Spy Extreme (2160p)",
	"",
	"130 \t-\t 3DMark Port Royal (1440p)",
	# "140 \t-\t 3DMark Port Royal RT Off (1440p)",
	"",
	"Hybrid-Looping versions below (repeated seamless-loop runs)",
	"210 \t-\t 3DMark Fire Strike (1080p)",
	"211 \t-\t 3DMark Fire Strike Extreme (1440p)",
	"212 \t-\t 3DMark Fire Strike Ultra (2160p)",
	"",
	"220 \t-\t 3DMark Time Spy (1440p)",
	"221 \t-\t 3DMark Time Spy Extreme (2160p)",
	"",
	"230 \t-\t 3DMark Port Royal (1440p)"#,
	# "240 \t-\t 3DMark Port Royal RT Off (1440p)"
	]

for OPT in options:
	print(OPT)
# test	=	input("Test ID Number (default 100 [Cinebench R20 - Multi-thread - Constant]): ") or str(100)
test	=	INPUT("110",	"Test ID Number (default !DEF!): ")

duration	=	int(INPUT(3600,	"Duration (default !DEF! s) : "))
warm		=	int(INPUT(300,	"Warmup Duration (default !DEF! s) : "))
coolCOEF	=	1
length		=	int((1 + coolCOEF)*duration + warm + 1)


TIME	=	datetime.datetime.now().strftime("%Y-%m-%d %H.%M")

if COOLERname == "":
	COOLERfold	=	""
else:
	COOLERfold	= 	COOLERname + "\\"

if os.path.exists(scriptPath + "CPU Thermal.py"):
	dataPath	=	scriptPath + "Data GPU\\" + 	GPUname + "\\" + COOLERfold + TIME + "\\"
else:
	dataPath	=	scriptPath + "Data\\" + 		GPUname + "\\" + COOLERfold + TIME + "\\"

if not os.path.exists(dataPath):
	os.makedirs(dataPath)

if os.path.exists(scriptPath + "GPU-Z Sensor Log.txt"):
	print("")
	DEL	=	INPUT("Y", "Old GPU-Z Sensor Log.txt found. Should it be deleted?\nY/n (Y): ")
	if DEL	==	"Y" or DEL == "y":
		os.remove(scriptPath + "GPU-Z Sensor Log.txt")

GPUz	=	subprocess.Popen(lnkGPUz + " -minimized", shell=True)

print("\nWarm-up")
time.sleep(warm)

print("\nGPU Load")
if test[0]	==	"1" or test[0] == "2":
	TESTname	=	_3DMARK_DEF(test, duration)[0]
	TEST3dmdef	=	_3DMARK_DEF(test, duration)[1]
	Bench		=	subprocess.Popen(_3DMARK(TEST3dmdef), shell = True)
	time.sleep(duration)

kill(Bench.pid)
Bench.kill()

print("\nCooldown")
time.sleep(duration * coolCOEF)

kill(GPUz.pid)
#	GPUz.kill() doesn't work because subprocess with shell=True makes it a separate process that cannot be controlled by Python
time.sleep(1)
#	give enough time for GPUz to be killed before trying to move the file

if os.path.exists(scriptPath + "GPU-Z Sensor Log.txt"):
	shutil.move(scriptPath + "GPU-Z Sensor Log.txt", dataPath + "GPU-Z Sensor Log.txt")
else:
	DESKTOP	=	"\\".join(scriptPath.split("\\", 3)[:3]) + "\\Desktop\\"
	if os.path.exists(DESKTOP + "GPU-Z Sensor Log.txt"):
		shutil.move(DESKTOP + "GPU-Z Sensor Log.txt", dataPath + "GPU-Z Sensor Log.txt")

if not os.path.exists(dataPath + "@GPU Thermal - Input.r"):
	with open(scriptPath + "GPU Thermal - Input.r", 'r') as fref, open(dataPath + "@GPU Thermal - Input.r", 'w') as fout:
		for line in fref:
			fout.write(line	\
				.replace("!TEST!",		TESTname)		\
				.replace("!GPU!",		GPUname)		\
				.replace("!COOLER!",	COOLERname)		\
				.replace("!zRPM!",		str(zeroRPM))		\
				.replace("!maxPWR!",	str(maxPOWER))		\
				.replace("!DUR!",		str(duration))	\
				.replace("!WARM!",		str(warm))		\
				.replace("!PATH!",		dataPath.replace("\\", "/"))	\
			)
		fout.close()

if not os.path.exists(dataPath + "~GPU Thermal - Data.r"):
	shutil.copyfile(scriptPath + "GPU Thermal - Data.r", dataPath + "~GPU Thermal - Data.r")
#	with ~ these scripts will be after the @ scripts
if not os.path.exists(dataPath + "@GPU Thermal - Output.r"):
	shutil.copyfile(scriptPath + "GPU Thermal - Output.r", dataPath + "@GPU Thermal - Output.r")