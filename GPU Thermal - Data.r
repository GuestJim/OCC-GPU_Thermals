if (file.exists("PresentMon.csv"))	{
	PresentMon	=	read_csv("PresentMon.csv")
	PresentMon.small	=	PresentMon[, c(
		"Application",
		# "ProcessID",
		# "SwapChainAddress",
		# "Runtime",
		# "SyncInterval",
		# "PresentFlags",
		# "AllowsTearing",
		# "PresentMode",
		"Dropped",
		"TimeInSeconds",
		"MsBetweenPresents",
		"MsBetweenDisplayChange"#,
		# "MsInPresentAPI",
		# "MsUntilRenderComplete",
		# "MsUntilDisplayed"
	)	]
	PresentMon.small$GPU	=	GPUname
	PresentMon.small$Cooler	=	COOLERname
	PresentMon.small$Test	=	TESTname
	write_csv(PresentMon.small, "PresentMon.csv.bz2")
}

GPUz	=	read_csv("GPU-Z Sensor Log.txt")
POWER	=	NULL
if (grepl("Vega", GPUname, ignore.case = TRUE))	POWER	=	"GPU Chip Power Draw [W]"
#	RX Vega GPUs do not give a separate Board Power value, making it necessary to selectively search for it
desCOLS	=	c(
	"Date",
	"GPU Clock",
	"Memory Clock",
	"GPU Temperature [",
	"Power Consumption (W)",
	"GPU only Power Draw",
	"Board Power Draw [W]",
	POWER,
	"VDDC",
	"GPU Voltage [V]"
)

desIND	=	pmatch(desCOLS, colnames(GPUz))
desIND	=	desIND[!is.na(desIND)]

dataALL	=	GPUz[, desIND]
colnames(dataALL)	=	c("Timestamp", "GPU_Clock", "VRAM_Clock", "GPU_Temp", "GPU_Power", "GPU_Voltage")
# colnames(dataALL)	=	c("GPU_Clock", "VRAM_Clock", "GPU_Temp", "GPU_Power", "GPU_Voltage")

dataALL$Time	=	as.numeric(dataALL$Timestamp)
dataALL$Time	=	dataALL$Time - min(dataALL$Time)
#	converts Timestamp to number of seconds and then removes the minimum to make measurements relative

#	there is an issue with GPU-z not keeping time properly, resulting in two recordings at the same Timestamp, and mis-times where there is a double recording and then a skipped second
DOUB	=	which(diff(dataALL$Time) == 0)	;	MISS	=	which(diff(dataALL$Time) == 2)
#	when GPU-z doubles a second				;	#	when GPU-z misses a second

MIUB	=	intersect(MISS + 1, DOUB)						#	when a miss precedes a double
dataALL[MIUB, "Time"]	=	dataALL[MIUB, "Time"] - 1		#	pulling double to fill miss

DOUB	=	which(diff(dataALL$Time) == 0)	;	MISS	=	which(diff(dataALL$Time) == 2)
DOSS	=	intersect(MISS, DOUB + 1)						#	when a double precedes a miss
dataALL[DOSS, "Time"]	=	dataALL[DOSS, "Time"] + 1		#	pushing double to fill miss

#	removes misses that cannot be corrected with doubles
if (any(diff(dataALL$Time) == 0))	dataALL	=	dataALL[-(which(diff(dataALL$Time) == 0)), ]
#	it is necessary to check if any doubles exist as trying to remove numeric(0) columns breaks things

# dataALL$Time	=	1:nrow(dataALL)
#	forces it to 1 Hz sampling rate


PERIODS	=	function(DATA,	BREAKS = c(warm, duration),	LABELS = levsPER){
	if	(DATA <= BREAKS[1])									return(ordered(LABELS[1], LABELS))
	if	(BREAKS[1] < DATA & DATA <= BREAKS[1] + BREAKS[2])	return(ordered(LABELS[2], LABELS))
	if	(BREAKS[1] + BREAKS[2] < DATA)						return(ordered(LABELS[3], LABELS))
}

dataALL$Period	=	sapply(dataALL$Time, PERIODS)
dataALL$Time	=	dataALL$Time - warm
dataALL$GPU		=	ordered(GPUname)
dataALL$Cooler	=	ordered(COOLERname)
dataALL$Test	=	ordered(TESTname)

diff.CONS	=	function(DATA, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(DATA, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(DATA, lag = lag)))
}

dataALL$GPU_Temp_Diff	=	diff.CONS(dataALL$GPU_Temp)

dataALL	=	dataALL[order(dataALL$Time), ]

write_csv(dataALL, "Data.csv.bz2")