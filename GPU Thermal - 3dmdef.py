import os, sys, subprocess, time, psutil, shutil

os.system("title GPU Thermal Testing")

print("Uses GPU-z\
\n\thttps://www.techpowerup.com/gpuz/\
\n\tGPU-z needs to place its Sensor Log in the folder with the Scripts\
\n")
print("3DMark tests require Professional Edition")

#	defaults
duration	=	3600
#	length of Load period
warm		=	300
#	length of Warm-up Period
coolCOEF	=	1
#	ratio between Load and Cooldown periods
pulse	=	"NULL"
#	length of pause between loads, if test is pulsing

#	shortcut name lists
lnkGPUz			=	"GPU-z.lnk"
lnk3DMark		=	"3DMarkCmd.lnk"
lnkPresentMon	=	"PresentMon.lnk"

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
lnkPresentMon	=	lnkCheck(lnkPresentMon)

def INPUT(DEFAULT, TEXT = "Default: !DEF!"):
	return(input(TEXT.replace("!DEF!", str(DEFAULT))) or DEFAULT)
#	to make changing the default value easier

def	_3DMARK(defin):
	#	the best practice is likely going to be to run PresentMon to collect globally or OCAT to get the proper EXE name from the CSV
	if dataPath in defin:
		return(lnk3DMark + " --definition=\"" + defin + ".3dmdef\" --loop=0 --audio=off --online=off")
	elif os.path.exists(scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef"):
		return(lnk3DMark + " --definition=\"" + scriptPath + "Thermal_Definitions\\" + defin + ".3dmdef\" --loop=0 --audio=off --online=off")
	else:
		return(lnk3DMark + " --definition=" + defin + ".3dmdef --loop=0 --audio=off --online=off")

def _3DMARK_DEF(CODE, time):
	global	TESTname
	global	TESTexe
	global	TESTlen

	TESTname	=	"3DMark - "
	if CODE[1]	==	"1":
		TESTname	=	TESTname + "Fire Strike"
		TESTexe		=	"3DMarkICFWorkload.exe"
		defin	=	"firestrike"
		TEST	=	"FireStrikeGt1"
		TESTlen	=	30
	if CODE[1]	==	"2":
		TESTname	=	TESTname + "Time Spy"
		TESTexe		=	"3DMarkTimeSpy.exe"
		defin	=	"timespy"
		TEST	=	"TimeSpyGt1"
		TESTlen	=	60
	if CODE[1]	==	"3" or CODE[1]	==	"4":
		TESTname		=	TESTname + "Port Royal"
		TESTexe			=	"3DMarkPortRoyal.exe"
		defin	=	"portroyal"
		TEST	=	"PortRoyalGt1"
		TESTlen	=	108
	if CODE[1]	==	"4":
		TESTname		=	TESTname + " (RT Off)"
		defin	=	defin + "_rtoff"
	
	LOOP	=	int(time / TESTlen) + 5
	
	if CODE[2]	==	"0":
		TESTname		=	TESTname + ""
		defin	=	defin + ""
		TEST	=	TEST + "P"
	if CODE[2]	==	"1":
		TESTname		=	TESTname + " - Extreme"
		defin	=	defin + "_extreme"
		TEST	=	TEST + "X"
	if CODE[2]	==	"2":
		TESTname		=	TESTname + " - Ultra"
		defin	=	defin + "_ultra"
		TEST	=	TEST + "R"
	
	if CODE[0] == "2":
		TESTname	=	TESTname + " - Pulse"
		return(defin)
	elif CODE[0]	==	"1":
		TESTname	=	TESTname + " - Loop"
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

	if CODE[1] == 4:
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
	return(dataPath + defin)

opt3DM	=	[
	"10 \t-\t 3DMark Fire Strike (1080p)",
	"11 \t-\t 3DMark Fire Strike Extreme (1440p)",
	"12 \t-\t 3DMark Fire Strike Ultra (2160p)",
	"",
	"20 \t-\t 3DMark Time Spy (1440p)",
	"21 \t-\t 3DMark Time Spy Extreme (2160p)",
	"",
	"30 \t-\t 3DMark Port Royal (1440p)",
	# "40 \t-\t 3DMark Port Royal RT Off (1440p)",
	]
opt3DMseam	=	["1" + x	if x != ""	else ""	for x in opt3DM]
opt3DMrepe	=	["2" + x	if x != ""	else ""	for x in opt3DM]

opt3DMark	=	[
	"It is necessary for this console to be focused for 3DMark to launch correctly",
	"Seamless Looping"] + opt3DMseam + [
	"",
	"Pulsing Runs with Loading and Pulse Pause"] + opt3DMrepe


def	kill(proc_pid):
    process	=	psutil.Process(proc_pid)
    for proc in process.children(recursive=True):
        proc.kill()
    process.kill()

def timeFUT(END):
	return(time.strftime("%I:%M %p", time.localtime(time.time() + END)))

#	may need to overhaul this and more if additional benchmarks than 3DMark are added
def PULSEx(TEST):
	t_end	=	time.time() + duration

	while time.time() < t_end:
		Bench	=	subprocess.Popen(TEST, shell = True)

		#	Bench.wait() with checking
		while Bench.poll() is None:
			if	time.time() >= t_end:
				kill(Bench.pid)
				Bench.kill()
				return
			time.sleep(1)

		#	time.sleep() with checking
		t_pulse	=	time.time() + pulse
		while time.time() < t_pulse:
			if	time.time() >= t_end:
				return
			time.sleep(1)
	return

OPTIONS	=	[]

if	os.path.exists(scriptPath + lnk3DMark.replace("\"", "")):
	OPTIONS	=	OPTIONS + opt3DMark + [""]
if	OPTIONS	==	[]:
	sys.exit()

GPUname		=	input("GPU Name: ")

COOLERname	=	INPUT("",		"GPU Cooler Name (default empty): ")

zeroRPM		=	INPUT("NULL",	"GPU Zero RPM Temperature Threshold (°C: default empty): ")
maxPOWER	=	INPUT("NULL",	"GPU Maximum Power (W: default empty): ")


print("Available Tests :")
print("")
for OPT in OPTIONS:
	print(OPT)
TESTcode	=	INPUT("110",	"Test ID Number (default !DEF!): ")

duration	=	int(INPUT(duration,	"Duration (default !DEF! s) : "))
warm		=	int(INPUT(warm,	"Warmup Duration (default !DEF! s) : "))
coolCOEF	=	coolCOEF
length		=	int((1 + coolCOEF)*duration + warm + 1)
if TESTcode[0]	==	"2":
	pulse	=	int(INPUT(0, "Pulse Pause in addition to Loading Time (default !DEF! s) : "))


TIME	=	time.strftime("%Y-%m-%d %H.%M", time.localtime())

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
	if DEL.lower()	==	"y" or DEL.lower() == "yes":
		os.remove(scriptPath + "GPU-Z Sensor Log.txt")

GPUz	=	subprocess.Popen(lnkGPUz + " -minimized", shell=True)


if TESTcode[0]	==	"1" or TESTcode[0] == "2":
	TEST3dmdef	=	_3DMARK_DEF(TESTcode, duration)
	TEST		=	_3DMARK(TEST3dmdef)
else:
	sys.exit()

print("\nWarm-up\tEnds at " + timeFUT(warm))

if os.path.exists(scriptPath + lnkPresentMon):
	# os.system("start " + lnkPresentMon + " -output_file \"" + dataPath + "PresentMon.csv\" -terminate_on_proc_exit")		#	records all processes
	# os.system("start " + lnkPresentMon + " -process_name " + TESTexe + " -output_file \"" + dataPath + "PresentMon.csv\" -terminate_on_proc_exit")		#	records until selected process ends
	os.system("start " + lnkPresentMon + " -process_name " + TESTexe + " -output_file \"" + dataPath + "PresentMon.csv\" -timed " + str(duration + warm + 5) + " -terminate_after_timed")		#	records for length of time

time.sleep(warm)

print("\nGPU Load\tEnds at " + timeFUT(duration))

if TESTcode[0]	==	"1":
	Bench		=	subprocess.Popen(TEST, shell = True)
	time.sleep(duration)
	
	kill(Bench.pid)
	Bench.kill()
if TESTcode[0]	==	"2":
	PULSEx(TEST)


print("\nCooldown\tEnds at " + timeFUT(duration*coolCOEF))
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
				.replace("!TEST!",		TESTname)	\
				.replace("!GPU!",		GPUname)	\
				.replace("!COOLER!",	COOLERname)	\
				.replace("!zRPM!",		str(zeroRPM))	\
				.replace("!maxPWR!",	str(maxPOWER))	\
				.replace("!DUR!",		str(duration))	\
				.replace("!WARM!",		str(warm))	\
				.replace("!PATH!",		dataPath.replace("\\", "/"))	\
				.replace("!PULSE!",		str(pulse))	\
				.replace("!LEN!",		str(TESTlen))	\
			)
		fout.close()

if not os.path.exists(dataPath + "~GPU Thermal - Data.r"):
	shutil.copyfile(scriptPath + "GPU Thermal - Data.r", dataPath + "~GPU Thermal - Data.r")
#	with ~ these scripts will be after the @ scripts
if not os.path.exists(dataPath + "@GPU Thermal - Output.r"):
	shutil.copyfile(scriptPath + "GPU Thermal - Output.r", dataPath + "@GPU Thermal - Output.r")