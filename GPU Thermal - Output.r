labelBreak	=	function(breaks, SEC = FALSE, r = 2)	{
	if (!app.BREAK)	return(breaks)
	if (is.numeric(breaks))	breaks	=	round(breaks, r)
	BREAK	=	c("", "\n")
	if	(is.numeric(breaks)	&	0 %in% breaks)	if	((which(breaks %in% 0) %% 2) == 0)	BREAK	=	rev(BREAK)
	if	(!SEC)	return(	paste0(rep(BREAK, length.out = length(breaks)),	breaks)	)
	if	(SEC)	return(	paste0(breaks, rep(BREAK, length.out = length(breaks)))	)
}
#	can be disabled by setting app.BREAK to FALSE

sci2norm	=	function(DATA)	format(DATA, scientific = FALSE)
rem_		=	function(INPUT)	gsub("_", " ", INPUT)
round2	=	function(DATA, r = 2)	{
	numCOL		=	sapply(DATA, is.numeric)
	DATA[numCOL]	=	round(DATA[numCOL], r)
	return(DATA)
}

nearCEIL	=	function(DATA, VAL)	ceiling(max(DATA, na.rm = TRUE) / VAL) * VAL
nearFLOOR	=	function(DATA, VAL)	floor(max(DATA, na.rm = TRUE) / VAL) * VAL

if	(!is.numeric(maxPWR))	maxPWR	=	nearCEIL(dataALL$GPU_Power, 75)
if	(!is.numeric(FREQ.COEF))	{
	maxCLK		=	nearCEIL(dataALL$GPU_Clock, 500)
	# FREQ.COEF	=	nearFLOOR(maxPWR/maxCLK, 0.1)
	FREQ.COEF	=	signif(exp(round(log(maxPWR/maxCLK), 0)), 1)
}
if (FREQ.COEF < 1)	FREQ.COEF	<-	1 / FREQ.COEF

stats		=	function(DATA)	{
	return(c(
		Min		=	min(DATA,		na.rm	=	TRUE),
		Median	=	median(DATA,	na.rm	=	TRUE),
		Mean	=	mean(DATA,		na.rm	=	TRUE),
		Max		=	max(DATA,		na.rm	=	TRUE)
	)	)
}

sepCOL	=	function(aggOUT)	{
	matCOL	=	sapply(aggOUT, is.matrix)
	out		=	aggOUT[, !matCOL]
	for (FUN in which(matCOL))	{
		DATA			=	as.data.frame(aggOUT[, FUN])
		colnames(DATA)	=	paste(colnames(aggOUT)[FUN], colnames(DATA), sep = " - ")
		
		out	=	cbind(out, DATA)
	}
	return(out)
}
remUNI	=	function(IN)	IN[, -intersect(
	which(!sapply(IN, is.numeric)),
	which(lapply(lapply(IN, unique), length) == 1))
	]
#	identifies the columns identifying the groups first so it will not alter the data

unitCOL	=	function(DATA)	{
	levs	=	levels(DATA)
	if	(is.character(DATA))	levs	=	DATA
	levs[grep("GPU_Temp", levs)]	=	paste0(levs[grep("GPU_Temp", levs)],	" (°C)")
	levs[grep("Clock", levs)]		=	paste0(levs[grep("Clock", levs)],		" (MHz)")
	levs[grep("Power", levs)]		=	paste0(levs[grep("Power", levs)],		" (W)")
	
	return(rem_(levs))
}

GROUPS	=	list(
		Period		=	dataALL$Period,
		GPU			=	dataALL$GPU,
		Cooler		=	dataALL$Cooler,
		Test		=	dataALL$Test
		)
DATAS	=	list(
		GPU_Temp		=	dataALL$GPU_Temp,
		GPU_Clock		=	dataALL$GPU_Clock,
		VRAM_Clock		=	dataALL$VRAM_Clock,
		GPU_Power		=	dataALL$GPU_Power
		)

dataSUM	=	sepCOL(aggregate(DATAS, GROUPS, stats))
dataSUM	=	remUNI(dataSUM)

longSUM	=	pivot_longer(dataSUM,
	cols			=	which(sapply(dataSUM, is.numeric)),
	names_to		=	c("Measurement", ".value"),
	names_sep		=	' - ',
	names_ptypes	=	list(Measurement = factor(ordered = TRUE))
)

levels(longSUM$Measurement)	=	unitCOL(levels(longSUM$Measurement))
longSUM	=	round2(longSUM)


tempCROSS	=	function(DATA, PERIOD, QUAN, OP = NULL, LIST = 10)	{
	COLS	=	c("Time", "GPU_Temp", "GPU_Temp_Diff")
	out		=	DATA[DATA$Period == PERIOD, COLS]
	if (PERIOD == "Cooldown")	out$dTime	=	out$Time - duration
	
	if (QUAN < 1)	LIM	=	quantile(out$GPU_Temp, QUAN)
	if (QUAN > 1)	LIM	=	QUAN
	
	if (is.null(OP))	{
		if (PERIOD == TESTname)		OP	=	">="
		if (PERIOD == "Cooldown")	OP	=	"<="
	}
	
	if (OP == "<=")		return(out[out$GPU_Temp <= LIM, ][1:LIST, ])
	if (OP == ">=")		return(out[out$GPU_Temp >= LIM, ][1:LIST, ])
}

FPSsummary	=	function(UNIT = "ms", DATA = PresentMon)	{
	# DATA$FPS	=	1000/DATA$MsBetweenPresents
	SECTS	=	function(QUAN)	quantile(DATA$TimeInSeconds, QUAN)

	# slope	=	function(IN)	lm(FPS ~ TimeInSeconds,	IN)$coefficients[2]
	slope	=	function(IN)	lm(MsBetweenPresents ~ TimeInSeconds,	IN)$coefficients[2]

	STATS	=	function(IN)	{
		UNITdata	=	switch(UNIT,	"ms" = IN$MsBetweenPresents,	"FPS" = 1000/IN$MsBetweenPresents)
		c(median(UNITdata),	mean(UNITdata),	slope(IN),	quantile(UNITdata, c(0.01, 0.99)))
		}

	out	=	data.frame(matrix(ncol = 5, nrow = 0))
	out	=	rbind(out,
		STATS(DATA),
		STATS(DATA[SECTS(0.01) <= DATA$TimeInSeconds	&	DATA$TimeInSeconds < SECTS(0.11), ]),
		STATS(DATA[SECTS(0.9) <= DATA$TimeInSeconds, ])
	)

	colnames(out)	=	c("Median",	"Mean",	"Slope_(ms)",	"0.1%",	"99%")
	rownames(out)	=	c("Test Period",	"1% to 11%",	"Last 10%")
	out$Unit	=	UNIT
	return(out)
}

#	returns the linear regression slopes for certain variables as a data frame
GPUslopes	=	function(DATA = dataALL, PERIOD = TESTname, WID = 0.1, OFF = 0.01)	{
	dataTEST	=	dataALL[dataALL$Period == PERIOD, ]
	PERCS	=	c(OFF,	WID + OFF,	1 - WID - OFF,	1 - OFF)
	SECTS	=	quantile(dataTEST$Time, PERCS)
	
	slope	=	function(DATA = dataTEST)	{
		c(
		coef(lm(GPU_Temp ~ Time,	data = DATA))[2],
		coef(lm(GPU_Clock ~ Time,	data = DATA))[2],
		coef(lm(GPU_Power ~ Time,	data = DATA))[2]	)
	}
	
	out	=	rbind(
		slope(),
		slope(dataTEST[SECTS[1] < dataTEST$Time & dataTEST$Time <= SECTS[2], ]),
		slope(dataTEST[SECTS[3] < dataTEST$Time & dataTEST$Time <= SECTS[4], ])
		)
	colnames(out)	=	c("GPU_Temp",	"GPU_Clock",	"GPU_Power")
	rownames(out)	=	c("Test Period",	paste0(PERCS[1]*100, "% to ", PERCS[2]*100, "%"),	paste0(PERCS[3] * 100, "% to ", PERCS[4]*100, "%"))
	return(out)
}

ecdfFREQ	=	function(DATA = dataALL, PERIOD = TESTname, FREQ = FREQspec)	{
	ECDF	=	ecdf(ceiling(DATA[DATA$Period == PERIOD, ]$GPU_Clock))
	BASE	=	min(FREQ)

	LESS	=	ECDF(FREQ - 0.01)					;	names(LESS)	=	paste0("<",	FREQ,	" MHz")
	EQUA	=	diff(ECDF(c(BASE - 0.01, BASE)))	;	names(EQUA)	=	paste0("=",	BASE,	" MHz")

	return(c(LESS, EQUA)[order(substring(names(c(LESS, EQUA)), 2))])
}

sinkTXT	=	function()	{
	options(width = 1000)
	printFrame	=	function(FRAME, ...)	print.data.frame(FRAME, row.names = FALSE, ...)
	
	sink(paste0(TESTname, " - Stats.txt"), split = TRUE)
		writeLines(TESTname)
		writeLines(GPUname)
		writeLines(COOLERname)
		writeLines(ifelse(is.numeric(zRPM),		paste0("Zero RPM Threshold:\t", zRPM, " °C"),							"")	)
		writeLines(ifelse(is.numeric(PULSE),	paste0("Pulse pause length, in addition to loading:\t", PULSE, " s"),	"")	)
		
		writeLines("\nWarm-up Period")
		printFrame(longSUM[longSUM$Period == "Warm-up", ])		
		
		writeLines(paste0("\n", TESTname, " Period"))
		printFrame(longSUM[longSUM$Period == TESTname, ])
		
		if (!is.null(FREQspec))	{
			writeLines("\nFrequency Percentages")
			print(round(ecdfFREQ(dataALL, TESTname, FREQspec) * 100, 2))
		}
		
		writeLines("\nLinear Model Slopes:")
		print(GPUslopes())
		writeLines("\nLinear Model Slopes (minute):")
		print(GPUslopes() * 60)
		
		if (!is.null(PresentMon)){
			writeLines("\nFrame Rate Summary")
			print(FPSsummary())
		}
		
		writeLines("\nFirst Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == TESTname, ]$GPU_Temp, 0.25), " °C\n"))
		printFrame(tempCROSS(dataALL, TESTname, 0.25, ">="))
		
		
		writeLines("\nCooldown Period")
		printFrame(longSUM[longSUM$Period == "Cooldown", ])
		
		writeLines("\nThird Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == "Cooldown", ]$GPU_Temp, 0.75), " °C\n"))
		printFrame(tempCROSS(dataALL, "Cooldown", 0.75, "<="))
		
		if	(is.numeric(zRPM))	{
			writeLines(paste0("\nZero RPM Threshold:\t", zRPM, " °C"))
			printFrame(tempCROSS(dataALL, "Cooldown", zRPM, "<="))
		}
	sink()
}

writeOCC	=	function(DATA, dataNAME, name=TESTname, fold = "")	{
	if (!require(tableHTML)) return(NULL)
	OCCHTML	=	function(DATA)	{
		tableHTML(DATA, rownames = FALSE, class="OCC") %>%
		replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
		replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
		replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
	}

	if	(fold != "")	{
		write_tableHTML(OCCHTML(DATA), file = paste0(fold, "\\", name, " - ", dataNAME,".html"))
	}	else	{
		write_tableHTML(OCCHTML(DATA), file = paste0(name, " - ", dataNAME,".html"))
	}
}

sinkHTML = function()	{
	writeOCC(longSUM,									dataNAME	=	"All",			name	=	TESTname)
	writeOCC(longSUM[longSUM$Period == "Warm-up", ],	dataNAME	=	"Warm-up",		name	=	TESTname)
	writeOCC(longSUM[longSUM$Period == TESTname, ],		dataNAME	=	TESTname,		name	=	TESTname)
	writeOCC(longSUM[longSUM$Period == "Cooldown", ],	dataNAME	=	"Cooldown",		name	=	TESTname)
}


customSave	=	function(type="", device=ggdevice, plot = last_plot(), width=gWIDTH, height=gHEIGH, dpi=DPI)	{
	device	=	tolower(device)
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(type, ".png"), plot = plot, device="png", width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(type, ".pdf"), plot = plot, device="pdf", width=width, height=height)
	}
}

CAPTION	=	c(GPUname,	COOLERname,	ifelse(is.null(PULSE), TESTname, paste0(TESTname, " - Pulse: ", PULSE, " s"))	)
CAPTION	=	labs(caption = paste(CAPTION, collapse = "\n"))

TEMP_point	=	function(DATA = dataALL, COEF = 1){
	geom_point(
		data	=	DATA,
		aes(y	=	GPU_Temp*COEF, 		color	=	"Temperature"),
		stat 	=	"unique",
		# color	=	"red",
		shape 	=	3,
		show.legend	=	TRUE
	)
}
POWR_point	=	function(DATA = dataALL, COEF = 1)	{
	geom_point(
		data	=	DATA,
		aes(y	=	GPU_Power*COEF,		color	=	"GPU Power"),
		stat	=	"unique",
		# color	=	"green",
		shape 	=	3,
		show.legend	=	TRUE
	)
}
FREQ_point	=	function(DATA = dataALL, COEF = 1/1000)	{
	geom_point(
		data	=	DATA,
		aes(y	=	GPU_Clock*COEF,		color	=	"Frequency"),
		# color	=	"blue",
		show.legend	=	TRUE
	)
}
VRAM_point	=	function(DATA = dataALL, COEF = 1/1000)	{
	geom_point(
		data	=	DATA,
		aes(y	=	VRAM_Clock*COEF,	color	=	"VRAM Frequency"),
		# color	=	"orange",
		show.legend	=	TRUE
	)
}

FPS_point	=	function(COEF = 1)	{
	if (is.null(PresentMon))	return(NULL)
	geom_point(
		data	=	PresentMon,
		aes(y	=	1000/MsBetweenPresents * COEF, x=TimeInSeconds, color	=	"FPS"),
		# color	=	"magenta",
		shape	=	18,
		show.legend	=	TRUE
	)
}

zRPM_line	=	function(DIR,	zeroRPM	=	zRPM)	{
	if	(!is.numeric(zeroRPM))	return(NULL)
	
	if	(DIR	==	"hline")	{
		out	=	geom_hline(
			aes(	yintercept	=	zeroRPM,
					color		=	"Zero RPM"),
			linetype	=	"dashed"
		)
	}
	if	(DIR	==	"vline")	{
		out	=	geom_vline(
			xintercept	=	zeroRPM,
			color		=	"black",
			linetype	=	"dashed"
		)
	}
	return(out)
}

COLORS	=	scale_color_manual(
		name	=	NULL,
		values	=	c(
			Temperature			=	"red",
			Frequency			=	"blue",
			"GPU Power"			=	"green",
			"VRAM Frequency"	=	"orange",
			"Zero RPM"			=	"black",
			"FPS"				=	"magenta")
	)

themeSCALES	=	function(COEF = FREQ.COEF){
	list(
		theme(
			plot.title.position		=	"plot",
			legend.position			=	"top",
			legend.justification	=	"left",
			legend.margin			=	margin(t = 0, unit = "cm")
			),
		scale_x_continuous(
			name	=	"Time (seconds)",
			# breaks	=	unique(c(seq(0, warm, by = warm/3), seq(warm, 2 * duration + warm, by = duration/6))),
			breaks	=	unique(c(seq(-warm, 0, by = warm/3), seq(0, 2 * duration, by = duration/6))),
			# labels	=	function(x)	labelBreak(x - warm),
			labels	=	labelBreak,
			minor_breaks	=	NULL,
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	NULL,
				# breaks	=	c(warm, duration + warm),
				breaks	=	c(0, duration),
				labels	=	c("Load Start", "Load Stop/End")
			)
		),
		scale_y_continuous(
			breaks		=	seq(0, 1800, by = 10),
			limits		=	c(0, maxPWR),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frequency (MHz)",
				labels	=	function(IN)	IN * COEF
				)
			),
		COLORS
	)
}

Ylab	=	function()	{
	if (!is.null(PresentMon))	return(ylab("Temperature (°C), Power (W), and Frame Rate (FPS)"))
	return(ylab("Temperature (°C) and Power (W)"))
}

#	adds the FPS slope value to the graph as text
#		currently disabled with a line comment
SLOPE.label	=	function()	{
	if (is.null(PresentMon)) return(NULL)
	# geom_label(aes(x = duration/24, y = 10, label = paste0("FPS Trend: ", round(FPSsummary()["Slope"], 3))), hjust="left", vjust="bottom", fill = "grey90", color = "magenta")
	geom_text(aes(x = duration/24, y = 10, label = paste0("FPS Trend: ", round(FPSsummary()["Slope_(ms)"], 3))), hjust="left", vjust="bottom", color = "magenta")
}


insertFPS	=	ggplot(data = PresentMon, aes(x = TimeInSeconds, y = 1000/MsBetweenPresents)) + 
	ggtitle("Frame Rate Behavior") +
	theme(plot.title.position = "plot", 	legend.position = "none") + 
	FPS_point() + COLORS + 
	scale_y_continuous(
		name	=	NULL,
		breaks	=	seq(0, 1800, by = 10),
		limits	=	quantile(1000/PresentMon$MsBetweenPresents, c(0.005, 0.995)),
		expand	=	c(0.02, 0)
		) + 
	scale_x_continuous(
		name	=	NULL,
		breaks	=	sort(c(round(min(PresentMon$TimeInSeconds)), seq(0, 400, by = 15))),
		expand		=	c(0.02, 0)
		) + 
	coord_cartesian(xlim = c(120, min(PresentMon$TimeInSeconds) + 240)) +
	theme(panel.grid.minor = element_blank()) + 
	theme(plot.margin = unit(c(0.15, 0.15, 0.15, 0), "point"), plot.background = element_rect(fill = "#fcfcfc")) + 
	theme(text = element_text(size = 10))


graphMEAN	=	function(COEF = FREQ.COEF)	{
	lowX	=	duration + 60		;	lowY 	=	maxPWR/2
	highX	=	max(dataALL$Time)	;	highY	=	maxPWR
	#	the expand padding is enough to separate the insert from the plot edge
	
	INSERT	=	NULL
	if (!is.null(PresentMon))	INSERT	=	annotation_custom(grob = ggplotGrob(insertFPS), xmin = lowX, xmax = highX, ymin = lowY, ymax = highY)
	
	ggplot(data = dataALL, aes(x=Time)) + 
	ggtitle("Frequency with Temperature and Power") + CAPTION +
	zRPM_line("hline") +
	FPS_point() +
	# geom_smooth(method = "lm", data = PresentMon, aes(x = TimeInSeconds, y = 1000/MsBetweenPresents)) + 
	TEMP_point() + 
	POWR_point() + 
	VRAM_point(COEF = COEF) + 
	FREQ_point(COEF = COEF) + 
	# SLOPE.label() + 
	themeSCALES(COEF) + Ylab() +
	INSERT
}

#	Provides a closer look at the FPS behavior for the test
graphFPS	=	function()	{
	insertFPS %+%	#	this way I am adding or replacing the characteristics of the insert
	ggtitle("Frame Rate Behavior") %+% CAPTION %+%
	scale_y_continuous(
		name	=	"Frame Rate (FPS)",
		breaks	=	seq(0, 1800, by = 10),
		limits	=	c(0, quantile(1000/PresentMon$MsBetweenPresents, 0.999)),
		expand	=	c(0.02, 0)
		) %+% 
	scale_x_continuous(
		name	=	"Time (Seconds)",
		breaks	=	sort(c(round(min(PresentMon$TimeInSeconds)), seq(0, 400, by = 15))),
		labels	=	labelBreak,
		expand		=	c(0.02, 0)
		) %+% 
	theme(text = element_text(size = 16)) %+%
	coord_cartesian(xlim = c(0, min(PresentMon$TimeInSeconds) + 300)) %+%
	theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0), "point"))
	#	by using coord_cartesian like this, it appears the data is continuing off the graph, which is the desired effect
}

#	this graph plots frequency versus voltage, which might be interesting, but I am unsure
#	it might be better to create a variant that only looks at the test period and zooms in on the bulk of the data
graphFrVo	=	function(X.by = 0.1, Y.by = 250, BIN = 8) {
	ggplot(data = dataALL, aes(x = GPU_Voltage, y = GPU_Clock)) + 
	ggtitle("GPU Frequency vs Voltage") + CAPTION +
	# stat_bin_hex(binwidth = c(X.by, Y.by)/BIN,	show.legend = FALSE, aes(fill = after_stat(ndensity)))	+ #scale_fill_viridis_c() +
	stat_bin_2d(binwidth = c(X.by, Y.by)/BIN, aes(fill = after_stat(ndensity)))	+ #scale_fill_viridis_c() +
	scale_fill_gradient2(name = "Density", low = "blue", mid = "green", high = "red") +
	facet_grid(rows = vars(Period), switch = "y",
		labeller = labeller(Period = function(IN) gsub(" - ", "\n", IN))
		) +
	theme(
		plot.title.position			=	"plot",
		legend.position				=	"bottom",
		legend.justification		=	"left",
		legend.margin				=	margin(t = -2, b = -2, l = -2, unit = "lines"),
		legend.key.width			=	unit(0.055, "npc")
		) + 
	scale_x_continuous(name	=	"GPU Voltage (V)",
		breaks	=	seq(0, 1.5, by = X.by),
		limits	=	c(0, NA),
		expand	=	c(0.02, 0),
		labels	=	paste0	#paste0 so integers are writen as that
		) + 
	scale_y_continuous(name	=	"GPU Frequency (MHz)",
		breaks	=	seq(0, 3000, by = Y.by),
		limits	=	c(0, NA),
		expand	=	c(0.02, 0)
		)
}

frameHIST	=	function(UPPER = 0.999, binWID	=	0.05, X.break = 1)	{
	if (is.null(PresentMon))	return(NULL)
	ggplot(data = PresentMon, aes(x = MsBetweenPresents)) +
	ggtitle(			"Frame Time",
		subtitle	=	"Histogram & Box Plot with Red Mean Line"	) + CAPTION + 
	scale_fill_gradient2("ms", low="blue", mid = "green", midpoint = quantile(PresentMon$MsBetweenPresents, 1/3),  high="red", limits = c(0, quantile(PresentMon$MsBetweenPresents, UPPER))) + 
	theme(
		plot.title.position			=	"plot",
		legend.position				=	"bottom",
		legend.justification		=	"left",
		legend.margin				=	margin(t = -2, b = -2, l = 0, unit = "lines"),
		legend.key.width			=	unit(0.055, "npc")
		) + 
	geom_boxplot(outlier.alpha = 0, 				coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) + 
	geom_histogram(aes(y = stat(ndensity),	fill = after_stat(x)),	binwidth = binWID) + 
	geom_boxplot(outlier.alpha = 0, alpha = 0.15,	coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) + 
	geom_vline(aes(xintercept = mean(MsBetweenPresents)), 	color = "red") + 
	scale_x_continuous(
		name	=	"Frame Time",
		breaks	=	seq(0, 10000, by = X.break),
		limits	=	c(0, quantile(PresentMon$MsBetweenPresents, UPPER)),
		guide 	=	guide_axis(n.dodge = 2),
		expand	=	c(0.02, 0)
		) + 
	scale_y_continuous(name = "", breaks = NULL)
}

#	Time Series graph
frameTS	=	function(DELT = duration, DATA = "MsBetweenPresents", RANDcon = FALSE)	{
	# require(ggfortify)	#	for using time series with ggplot2
	#	by using the time function to get the sample times/indices, ggfortify is no longer necessary as I can construct the data frame myself
	
	DATAsets	=	list(
		"MsBetweenPresents"	=	PresentMon$MsBetweenPresents,
		"GPU_Power"			=	dataALL[dataALL$Period == TESTname & dataALL$Time <= duration * 0.99, ]$GPU_Power,
		"GPU_Clock"			=	dataALL[dataALL$Period == TESTname & dataALL$Time <= duration * 0.99, ]$GPU_Clock,
		"GPU_Temp"			=	dataALL[dataALL$Period == TESTname & dataALL$Time <= duration * 0.99, ]$GPU_Temp
		)
	COLORsets	=	list(
		"MsBetweenPresents"	=	"FPS",
		"GPU_Power"			=	"GPU Power",
		"GPU_Clock"			=	"Frequency",
		"GPU_Temp"			=	"Temperature"
	)
	NAMEsets	=	list(
		"MsBetweenPresents"	=	"Frame Time (ms)",
		"GPU_Power"			=	"Power (W)",
		"GPU_Clock"			=	"Frequency (MHz)",
		"GPU_Temp"			=	"Temperature (°C)"
	)
	BREAKsets	=	list(
		"MsBetweenPresents"	=	seq(0,	100,	by = 0.5),
		"GPU_Power"			=	seq(0,	600,	by = 5),
		"GPU_Clock"			=	seq(0,	3000,	by = 50),
		"GPU_Temp"			=	seq(0,	200,	by = 1)
	)
	
	TS	=	decompose(ts(DATAsets[[DATA]], deltat = 1/DELT, start = 0), type = "additive")	#	start = 0 to align with Time labels
	# TS.df	=	fortify(TS)	#	from ggfortify
	RAND	=	0
	if (RANDcon)	RAND	=	TS$random
	TS.df	=	data.frame(as.vector(time(TS$trend)), as.vector(TS$trend + RAND))
	colnames(TS.df)	=	c("Index", "Trend")
	TEXT	=	NULL
	if (DATA == "MsBetweenPresents")	{
		smoothDATA	=	layer_data(ggplot() + stat_smooth(data = TS.df, aes(x = Index, y = Trend), na.rm = TRUE))
		MIN	=	which.min(smoothDATA$ymin)	;	MAX	=	which.max(smoothDATA$ymax)
		TEXT		=	list(
			annotate("label",	label	=	round2(smoothDATA[MIN, "ymin"]),
				x	=	smoothDATA[MIN, "x"],	y	=	smoothDATA[MIN, "ymin"],
				hjust	=	"inward",	vjust	=	"outward"
				),
			annotate("label",	label	=	round2(smoothDATA[MAX, "ymax"]),
				x	=	smoothDATA[MAX, "x"],	y	=	smoothDATA[MAX, "ymax"],
				hjust	=	"inward",	vjust	=	"inward"
				)
		)
	}
	if (DATA == "GPU_Clock")	{
		smoothDATA	=	layer_data(ggplot() + stat_smooth(data = TS.df, aes(x = Index, y = Trend), na.rm = TRUE))
		MIN	=	which.min(smoothDATA$ymin)	;	MAX	=	which.max(smoothDATA$ymax)
		TEXT		=	list(
			annotate("label",	label	=	round2(smoothDATA[MIN, "ymin"]),
				x	=	smoothDATA[MIN, "x"],	y	=	smoothDATA[MIN, "ymin"],
				hjust	=	"inward",	vjust	=	"inward"
				),
			annotate("label",	label	=	round2(smoothDATA[MAX, "ymax"]),
				x	=	smoothDATA[MAX, "x"],	y	=	smoothDATA[MAX, "ymax"],
				hjust	=	"inward",	vjust	=	"outward"
				)
		)
	}
	
	# autoplot(TS$trend, ts.colour = "magenta")	#	from ggfortify
	
	ggplot(data = TS.df, aes(x = Index, y = Trend)) + 
	ggtitle(paste0(NAMEsets[[DATA]], " - Time Series Trend")) + CAPTION +
	geom_line(aes(color = COLORsets[[DATA]]), show.legend = FALSE) + 
	stat_smooth(na.rm = TRUE) + TEXT + 
	# scale_x_continuous(
		# name	=	"Index",
		# expand	=	c(0.02, 0)
		# ) + 
	scale_x_continuous(
		name	=	"Approximate Time (seconds)",
		expand	=	c(0.02, 0),
		breaks	=	seq(0,	max(TS.df$Index),	by = max(TS.df$Index)/6),
		labels	=	seq(0,	duration,			by = duration/6),
		limits	=	c(0, NA)
		) + 
	scale_y_continuous(name = NAMEsets[[DATA]],	breaks = BREAKsets[[DATA]]) + 
	theme(plot.title.position = "plot")	+ COLORS
}
# frameTS()
# frameTS(duration/(testLEN + pulseTSoff), "GPU_Clock")


# library(scales)
#	would be necessary to use the oob_squish function, for handling out of bounds values

graphHIST	=	function(TYPE, TITLE, X.name, X.break, X.limits, FILL.unit, FILL.mid, FILL.limits, FILL.breaks, binWID = NULL, COEF = 1)	{
	if	(is.null(binWID))	{
		binWID	=	abs(diff(dataALL[, TYPE][[1]]))
		binWID	=	min(ceiling(binWID[binWID != 0]), na.rm = TRUE)
	}
	if	(binWID != 1)	X.name		=	paste0(X.name, "\nBin Width: ", binWID)

	ggplot(data = dataALL, aes(x = get(TYPE) / COEF)) +
	ggtitle(			TITLE,
		subtitle	=	"Histograms & Box Plots with Red Mean Line") + CAPTION + 
	# scale_fill_gradient2(FILL.unit, low="blue", mid = "green", midpoint = FILL.mid,  high="red", limits = FILL.limits, breaks = FILL.breaks, oob = oob_squish) + 
	scale_fill_gradient2(FILL.unit, low="blue", mid = "green", midpoint = FILL.mid,  high="red", limits = FILL.limits, breaks = FILL.breaks) + 
	theme(
		plot.title.position			=	"plot",
		legend.position				=	"bottom",
		legend.justification		=	"left",
		legend.margin				=	margin(t = -2, b = -2, l = -2, unit = "lines"),
		legend.key.width			=	unit(0.055, "npc")
		) + 	
	geom_boxplot(outlier.alpha = 0, 				coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) + 
	geom_histogram(aes(y = stat(ndensity),	fill = after_stat(x)),	binwidth = binWID) + 
	geom_boxplot(outlier.alpha = 0, alpha = 0.15,	coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) + 
	geom_vline(data = aggregate(dataALL[, paste0(TYPE)], GROUPS, mean, na.rm = TRUE),	aes(xintercept = x / COEF), 	color = "red") +
	# facet_grid(rows = vars(Period), switch = "y", labeller = labeller(Period = label_wrap_gen(20))) +
	facet_grid(rows = vars(Period), switch = "y",
		labeller = labeller(Period = function(IN) gsub(" - ", "\n", IN))
		) +
	scale_x_continuous(
		name = X.name,
		breaks	=	seq(0, 10000, by = X.break),
		limits	=	X.limits,
		guide 	=	guide_axis(n.dodge = 2),
		expand	=	c(0.02, 0)
		) + 
	scale_y_continuous(name = "", breaks = NULL)
}

FREQspec_line	=	function(FREQ	=	FREQspec)	{
	if	(!is.numeric(FREQ))	return(NULL)
	
	FREQdata	=	list(
		Period	=	ordered(levsPER[1], levsPER),	x	=	FREQ,	y	=	Inf,
		TEXT	=	FREQ,
		ECDF	=	round2(ecdf(dataALL[dataALL$Period == TESTname, ]$GPU_Clock)(FREQ))
		)
		
	list(geom_vline(
			xintercept	=	FREQ,
			color		=	"black",
			linetype	=	"dashed"
		), 
		geom_text(data	=	data.frame(FREQdata),
			aes(x = x,	y = y,	label = TEXT),
			vjust	=	-0.5
		),
		coord_cartesian(clip = "off")
	)
}

#Temperature
HIST.Temp		=	graphHIST(
	TYPE		=	"GPU_Temp",
	TITLE		=	"GPU Temperature Normalized Distribution by Period",
	X.name		=	"Temperature (°C)",
	X.break		=	5,
	X.limits	=	c(0, NA),
	FILL.unit	=	"°C",
	FILL.mid	=	60,
	FILL.limits	=	c(25, 95),
	FILL.breaks	=	seq(30, 90, by = 10)
	)	+	zRPM_line("vline")

#Frequency
HIST.Frequency	=	graphHIST(
	TYPE		=	"GPU_Clock",
	TITLE		=	"Frequency Normalized Distribution by Period",
	X.name		=	"Frequency (MHz)",
	X.break		=	200,
	X.limits	=	c(0, NA),
	FILL.unit	=	"MHz",
	FILL.mid	=	1000,
	FILL.limits	=	c(300, 2500),
	FILL.breaks	=	seq(0, 10000, by = 500)#,
	# binWID		=	1
	)	+	FREQspec_line(FREQspec)

#GPU Power
HIST.Socket		=	graphHIST(
	TYPE		=	"GPU_Power",
	TITLE		=	"GPU Power Normalized Distribution by Period",
	X.name		=	"Power (W)",
	X.break		=	10,
	X.limits	=	c(0, NA),
	FILL.unit	=	"W",
	FILL.mid	=	maxPWR/2,
	FILL.limits	=	c(0, maxPWR),
	FILL.breaks	=	seq(0, maxPWR, length.out = 6),
	COEF		=	1
	)

#Frame Time
HIST.Frame		=	frameHIST(
	UPPER	=	fH.UPPER
	# binWID	=	0.05,
	# X.break	=	1
	)

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
