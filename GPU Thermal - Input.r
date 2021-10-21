library(readr)
library(tidyr)
library(ggplot2)

duration	=	!DUR!
warm		=	!WARM!
TESTname	=	"!TEST!"
GPUname		=	"!GPU!"
COOLERname	=	"!COOLER!"
PULSE		=	!PULSE!	#only relevant if test was pulsed
testLEN		=	!LEN!	#approximate run length

levsPER		=	c("Warm-up", TESTname, "Cooldown")

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

zRPM		=	!zRPM!
maxPWR		=	!maxPWR!
FREQspec	=	NULL	#	for frequency specs, can take vector
pulseTSoff	=	0		#	offset for incorporating load time for GPU-z based time series graphs. Requires manual tweaking but 9 for Fire Strike and -15 for Time Spy work for me

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}

if (file.exists("Data.csv.bz2"))	{
	dataALL		=	read_csv("Data.csv.bz2")
}	else	{
	source("~GPU Thermal - Data.r")
}

if (file.exists("PresentMon.csv.bz2"))	{
	PresentMon	=	read_csv("PresentMon.csv.bz2")
}	else	{
	PresentMon	=	NULL
}

dataALL$Period	=	ordered(dataALL$Period, levels = levsPER)
dataALL$GPU		=	ordered(GPUname)
dataALL$Cooler	=	ordered(COOLERname)
dataALL$Test	=	ordered(TESTname)
dataALL$Timestamp	=	NULL	#unnecessary column and breaks viewing dataALL due to POSIXct class, but keeping in Data.csv.bz2 is best

if (min(PresentMon$TimeInSeconds) > warm)	PresentMon$TimeInSeconds	=	PresentMon$TimeInSeconds - warm

source("@GPU Thermal - Output.r")
