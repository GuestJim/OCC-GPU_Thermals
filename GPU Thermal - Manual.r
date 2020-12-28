library(readr)
library(tidyr)
library(ggplot2)

INPUT	=	function(TEXT)	 readline(prompt = TEXT)

duration	=	as.numeric(INPUT("Test Duration: "))
warm		=	as.numeric(INPUT("Warm-up: "))
TESTname	=	INPUT("Test Name: ")
GPUname		=	INPUT("GPU Name: ")
COOLERname	=	INPUT("GPU Cooler: ")

levsPER		=	c("Warm-up", TESTname, "Cooldown")

theme_set(theme_grey(base_size = 16))
DPI			=	120
ggdevice	=	"png"

gWIDTH	=	16
gHEIGH	=	9
app.BREAK	=	TRUE
FREQ.COEF	=	1/1000*50

# setwd(choose.dir())
setwd("E:/Users/Jim/My Documents/OCC/@Reviews/GPU Thermal Testing Scripts/@Scripts/Data/RTX 2060/2020-09-03 10.59 - Copy")

if (!file.exists("Data.csv.bz2"))	{
	source("GPU Thermal - Data.r")
}	else	{
	dataALL	=	read_csv("Data.csv.bz2")
}

dataALL$Period	=	ordered(dataALL$Period, levels = levsPER)
dataALL$GPU		=	ordered(GPUname)
dataALL$Test	=	ordered(TESTname)

source("GPU Thermal - Output.r")
