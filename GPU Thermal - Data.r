GPUz	=	read_csv("GPU-Z Sensor Log.txt")
desCOLS	=	c(
	# "Date",
	"GPU Clock",
	"Memory Clock",
	"GPU Temperature",
	"Power Consumption (W)",
	"VDDC"
)

dataALL	=	GPUz[, pmatch(desCOLS, colnames(GPUz))]
# colnames(GPUz)	=	c("Timestamp", "GPU_Clock", "VRAM_Clock", "GPU_Temp", "GPU_Power", "GPU_Voltage")
colnames(dataALL)	=	c("GPU_Clock", "VRAM_Clock", "GPU_Temp", "GPU_Power", "GPU_Voltage")

dataALL$Time	=	1:nrow(dataALL)
#	it seems GPUz is not perfect at keeping to 1 second intervals, causing multiple measurements at the same reported timestamp
#	forcing it to 1 Hz sampling rate

PERIODS	=	function(DATA,	BREAKS = c(warm, duration),	LABELS = levsPER){
	out	=	ifelse(DATA$Time <= BREAKS[1], LABELS[1],
			ifelse(BREAKS[1] < DATA$Time & DATA$Time <= BREAKS[2] + BREAKS[1], LABELS[2],
			ifelse(BREAKS[2] + BREAKS[1] < DATA$Time, LABELS[3], NA
				)))
	out	=	ordered(out, levels = LABELS)
	return(out)
}

dataALL$Period	=	PERIODS(dataALL)
dataALL$Time	=	dataALL$Time - warm
dataALL$GPU		=	GPUname
dataALL$Test	=	TESTname

diff.CONS	=	function(DATA, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(DATA, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(DATA, lag = lag)))
}

dataALL$GPU_Temp_Diff	=	diff.CONS(dataALL$GPU_Temp)

dataALL	=	dataALL[order(dataALL$Time), ]

write_csv(dataALL, "Data.csv.bz2")