library(readr)
library(tidyr)
library(ggplot2)

DATA	=	new.env()

DATA$duration	=	!DUR!
DATA$warm		=	!WARM!
DATA$TESTname	=	"!TEST!"
DATA$GPUname		=	"!GPU!"
DATA$COOLERname	=	"!COOLER!"
DATA$PULSE		=	!PULSE!	#only relevant if test was pulsed
DATA$testLEN		=	!LEN!	#approximate run length

DATA$levsPER		=	c("Warm-up", DATA$TESTname, "Cooldown")

DATA$zRPM		=	!zRPM!
DATA$maxPWR		=	!maxPWR!

for (obj in ls(DATA, all.names = TRUE))	assign(obj, get(obj, DATA))

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

gWIDTH	=	16
gHEIGH	=	9
app.BREAK	=	TRUE
graphTS		=	TRUE
FREQ.COEF	=	NULL
#	this will be set automatically in Output.r based on maximum power and clock values
#	it might be desirable to manually tweak this value, and if you do here it will not be altered
fH.UPPER	=	0.999
#	frame time histogram arguments as sometimes they need to be altered, mainly UPPER, especially for pulsed data

FREQspec	=	NULL	#	for frequency specs, can take vector
pulseTSoff	=	0		#	offset for incorporating load time for GPU-z based time series graphs. Requires manual tweaking but 9 for Fire Strike and -15 for Time Spy work for me

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}

if (file.exists("Data.csv.bz2"))	{
	dataALL		=	read_csv("Data.csv.bz2", guess_max = 10, lazy = TRUE, show_col_types = FALSE)
	write_csv(dataALL, "Data.csv.bz2")
}	else	{
	hold	=	new.env()
	source("~GPU Thermal - Data.r",	local = hold)
	rm(hold)
}

if (file.exists("PresentMon.csv.bz2"))	{
	PresentMon	=	read_csv("PresentMon.csv.bz2", guess_max = 10, lazy = TRUE, show_col_types = FALSE)
}	else	{
	PresentMon	=	NULL
}

dataALL$Period	=	ordered(dataALL$Period, levels = levsPER)
dataALL$GPU		=	ordered(GPUname)
dataALL$Cooler	=	ordered(COOLERname)
dataALL$Test	=	ordered(TESTname)
dataALL$Timestamp	=	NULL	#unnecessary column and breaks viewing dataALL due to POSIXct class, but keeping in Data.csv.bz2 is best

if (min(PresentMon$TimeInSeconds) > warm)	PresentMon$TimeInSeconds	=	PresentMon$TimeInSeconds - warm

DATA$dataALL	=	dataALL
DATA$PresentMon	=	PresentMon
# saveRDS(DATA, "DATA.env", compress="bzip2")

source("@GPU Thermal - Output.r")


sinkTXT()
sinkHTML()

message("Course")
customSave("Course",	plot = graphMEAN())
message("Temperature by Period")
customSave("Hist - Temperature",	plot = HIST.Temp,		width	=	gHEIGH * 1.25)
message("Frequency by Period")
customSave("Hist - Frequency",		plot = HIST.Frequency,	width	=	gHEIGH * 1.25)
message("GPU Power by Period")
customSave("Hist - Power",			plot = HIST.Socket,		width	=	gHEIGH * 1.25)
if (!is.null(PresentMon))	{
	message("FPS")
	customSave("FPS",	plot = graphFPS())
	message("Frame Time Histogram")
	customSave("Hist - Frame",			plot = HIST.Frame,		width	=	gHEIGH * 1.25,	height	=	gHEIGH * 0.666)
}
# message("GPU Frequency vs Voltage")
# customSave("Freq-Volt",			plot = graphFrVo(),		width	=	gHEIGH * 1.25)

#	Time Series graph creation
if (!graphTS)	stop()
if (!exists("pulseTSoff"))	pulseTSoff	=	0	#in case the offset has not been set in Input.r
if (!is.null(PresentMon))	{
	message("Time Series - Frame Time")
	customSave("TS - Frame",		plot = frameTS(),												width	=	gHEIGH * 1.25)
}
message("Time Series - GPU Clock")
customSave("TS - Frequency",		plot = frameTS(duration/(testLEN + pulseTSoff), "GPU_Clock"),	width	=	gHEIGH * 1.25)
message("Time Series - GPU Power")
customSave("TS - Power",			plot = frameTS(duration/(testLEN + pulseTSoff), "GPU_Power"),	width	=	gHEIGH * 1.25)
message("Time Series - Temperature")
customSave("TS - Temperature",		plot = frameTS(duration/(testLEN + pulseTSoff), "GPU_Temp"),	width	=	gHEIGH * 1.25)
