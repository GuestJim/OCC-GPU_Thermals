library(readr)
library(tidyr)
library(ggplot2)

duration	=	!DUR!
warm		=	!WARM!
TESTname	=	"!TEST!"
GPUname		=	"!GPU!"
COOLERname	=	"!COOLER!"

levsPER		=	c("Warm-up", TESTname, "Cooldown")

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

gWIDTH	=	16
gHEIGH	=	9
app.BREAK	=	TRUE
FREQ.COEF	=	1/1000*50

zRPM		=	!zRPM!
maxPWR		=	!maxPWR!

if (interactive())	{
	setwd("!PATH!")
}	else	{
	pdf(NULL)
}

if (!file.exists("Data.csv.bz2"))	{
	source("~GPU Thermal - Data.r")
}	else	{
	dataALL	=	read_csv("Data.csv.bz2")
}

dataALL$Period	=	ordered(dataALL$Period, levels = levsPER)
dataALL$GPU		=	ordered(GPUname)
dataALL$Test	=	ordered(TESTname)

source("@GPU Thermal - Output.r")
