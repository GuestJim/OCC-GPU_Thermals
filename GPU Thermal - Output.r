labelBreak	=	function(breaks, SEC = FALSE)	{
	if (!app.BREAK)	return(breaks)
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

if	(!is.numeric(maxPWR))	maxPWR	=	ceiling(max(dataALL$GPU_Power) / 75) * 75

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

unitCOL	=	function(DATA)	{
	levs	=	levels(DATA)
	if	(is.character(DATA))	levs	=	DATA
	levs[grep("GPU_Temp", levs)]	=	paste0(levs[grep("GPU_Temp", levs)],		" (°C)")
	levs[grep("Clock", levs)]		=	paste0(levs[grep("Clock", levs)],	" (MHz)")
	levs[grep("Power", levs)]		=	paste0(levs[grep("Power", levs)],		" (W)")
	
	return(rem_(levs))
}

GROUPS	=	list(
		Period		=	dataALL$Period,
		GPU			=	dataALL$GPU,
		Test		=	dataALL$Test
		)
DATAS	=	list(
		GPU_Temp		=	dataALL$GPU_Temp,
		GPU_Clock		=	dataALL$GPU_Clock,
		VRAM_Clock		=	dataALL$VRAM_Clock,
		GPU_Power		=	dataALL$GPU_Power
		)

dataSUM	=	sepCOL(aggregate(DATAS, GROUPS, stats))

longSUM	=	pivot_longer(dataSUM,
	cols			=	which(sapply(dataSUM, is.numeric)),
	names_to		=	c("Measurement", ".value"),
	names_sep		=	' - ',
	names_ptypes	=	list(Measurement = factor(ordered = TRUE))
)

levels(longSUM$Measurement)	=	unitCOL(levels(longSUM$Measurement))
longSUM	=	round2(longSUM)

tempQUART	=	function(DATA, PERIOD, QUAN, OP, LIST = 10)	{
	if (OP == "<=")	{
		out	=	DATA[
			DATA$Period == PERIOD & 
			DATA$GPU_Temp <= quantile(DATA[DATA$Period == PERIOD, ]$GPU_Temp, QUAN),
			c("Time", "GPU_Temp", "GPU_Temp_Diff")][1:LIST, ]
	}

	if (OP == ">=")	{
		out	=	DATA[
			DATA$Period == PERIOD & 
			DATA$GPU_Temp >= quantile(DATA[DATA$Period == PERIOD, ]$GPU_Temp, QUAN),
			c("Time", "GPU_Temp", "GPU_Temp_Diff")][1:LIST, ]
	}
	return(out)
}

tempZERO	=	function(DATA, PERIOD, OP, LIST = 10)	{
	if (OP == "<=")	{
		out	=	DATA[
			DATA$Period == PERIOD & 
			DATA$GPU_Temp <= zRPM,
			c("Time", "GPU_Temp", "GPU_Temp_Diff")][1:LIST, ]
	}

	if (OP == ">=")	{
		out	=	DATA[
			DATA$Period == PERIOD & 
			DATA$GPU_Temp >= zRPM,
			c("Time", "GPU_Temp", "GPU_Temp_Diff")][1:LIST, ]
	}
	return(out)
}

sinkTXT	=	function()	{
	options(width = 1000)
	printFrame	=	function(FRAME, ...)	print.data.frame(FRAME, row.names = FALSE, ...)

	sink(paste0(TESTname, " - Stats.txt"), split = TRUE)
		writeLines(TESTname)
		writeLines(GPUname)
		writeLines(COOLERname)
		if (is.numeric(zRPM))	writeLines(paste0("Zero RPM Threshold:\t", zRPM, " °C"))
		
		writeLines("\nWarm-up Period")
		printFrame(longSUM[longSUM$Period == "Warm-up", ])		
		
		writeLines(paste0("\n", TESTname, " Period"))
		printFrame(longSUM[longSUM$Period == TESTname, ])
		
		writeLines("\nFirst Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == TESTname, ]$GPU_Temp, 0.25), " °C\n"))
		printFrame(tempQUART(dataALL, TESTname, 0.25, ">="))
		
		
		writeLines("\nCooldown Period")
		printFrame(longSUM[longSUM$Period == "Cooldown", ])
		
		writeLines("\nThird Quartile Temperature Reached")
		writeLines(paste0(quantile(dataALL[dataALL$Period == "Cooldown", ]$GPU_Temp, 0.75), " °C\n"))
		printFrame(tempQUART(dataALL, "Cooldown", 0.75, "<="))
		
		if	(is.numeric(zRPM))	{
			writeLines(paste0("\nZero RPM Threshold:\t", zRPM, " °C"))
			printFrame(tempZERO(dataALL, "Cooldown", "<="))
		}
	sink()
}

library(tableHTML)
OCCHTML	=	function(DATA)	{
	tableHTML(DATA, rownames = FALSE, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE)
}

writeOCC	=	function(DATA, dataNAME, name=gameGAQF, fold = "")	{
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
	if	(device	==	"png"	|	device == "both")	{
		ggsave(filename=paste0(type, ".png"), plot = plot, device="png", width=width, height=height, dpi=dpi)
	}
	if	(device	==	"pdf"	|	device == "both")	{
		ggsave(filename=paste0(type, ".pdf"), plot = plot, device="pdf", width=width, height=height)
	}
}

CAPTION	=	c(GPUname, TESTname)
if (COOLERname != "")	CAPTION	=	c(GPUname, COOLERname, TESTname)
CAPTION	=	labs(caption = paste(CAPTION, collapse = "\n"))

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

COLORS	=	scale_color_manual(
		name	=	NULL,
		values	=	c(Temperature = "red",	Frequency = "blue",	"GPU Power" = "green",	"VRAM Frequency" = "orange",	"Zero RPM" = "black")
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
			limits		=	c(0, NA),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frequency (MHz)",
				labels	=	function(IN)	IN / COEF
				)
			),
		COLORS
	)
}


graphMEAN	=	function(COEF = FREQ.COEF)	{
	ggplot(data = dataALL, aes(x=Time)) + 
	ggtitle("Frequency with Temperature and Power") + CAPTION +
	zRPM_line("hline") +
	TEMP_point() + 
	POWR_point() + 
	VRAM_point(COEF = COEF) + 
	FREQ_point(COEF = COEF) + 
	themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

#	this graph plots frequency versus voltage, which might be interesting, but I am unsure
#	it might be better to create a variant that only looks at the test period and zooms in on the bulk of the data
graphFrVo	=	function(X.by = 0.1, Y.by = 250, BIN = 8) {
	ggplot(data = dataALL, aes(x = GPU_Voltage, y = GPU_Clock)) + 
	ggtitle("GPU Frequency vs Voltage") + CAPTION +
	# geom_hex(binwidth = c(X.by, Y.by)/BIN,	show.legend = FALSE, aes(fill = after_stat(ndensity)))	+ scale_fill_viridis_c() +
	stat_bin2d(binwidth = c(X.by, Y.by)/BIN, aes(fill = after_stat(ndensity)))	+ #scale_fill_viridis_c() +
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
		labels	=	paste0
		) + 
	scale_y_continuous(name	=	"GPU Frequency (MHz)",
		breaks	=	seq(0, 3000, by = Y.by),
		limits	=	c(0, NA),
		expand	=	c(0.02, 0)
		)
}

# library(scales)
#	would be necessary to use the oob_squish function, for handling out of bounds values

graphHIST	=	function(TYPE, TITLE, X.name, X.break, X.limits, FILL.unit, FILL.mid, FILL.limits, FILL.breaks, binWID = 1, COEF = 1)	{
	ggplot(data = dataALL, aes(x = get(TYPE)*COEF)) +
	ggtitle(			TITLE,
		subtitle	=	"Histograms & Box Plots with Red Mean Line"	) + CAPTION + 
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
	geom_vline(data = dataALL[dataALL$Period == "Warm-up", ],	aes(xintercept = mean(get(TYPE)*COEF)), 	color = "red") + 
	geom_vline(data = dataALL[dataALL$Period == TESTname, ],	aes(xintercept = mean(get(TYPE)*COEF)), 	color = "red") + 
	geom_vline(data = dataALL[dataALL$Period == "Cooldown", ],	aes(xintercept = mean(get(TYPE)*COEF)), 	color = "red") + 
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

#Temperature
HIST.Temp		=	graphHIST(
	TYPE		=	"GPU_Temp",
	TITLE		=	"GPU Temperature Normalized Distribution by Period",
	X.name		=	"Temperature (°C)",
	X.break		=	5,
	X.limits	=	NULL,
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
	FILL.breaks	=	seq(0, 10000, by = 500),
	binWID	=	1
	)

#GPU Power
HIST.Socket		=	graphHIST(
	TYPE		=	"GPU_Power",
	TITLE		=	"GPU Power Normalized Distribution by Period",
	X.name		=	"Power (W)",
	X.break		=	10,
	X.limits	=	NULL,
	FILL.unit	=	"W",
	FILL.mid	=	maxPWR/2,
	FILL.limits	=	c(0, maxPWR),
	FILL.breaks	=	seq(0, maxPWR, length.out = 6),
	COEF		=	1
	)


sinkTXT()
sinkHTML()

message("Frequency")
customSave("Frequency",	plot = graphMEAN())
message("Temperature by Period")
customSave("Hist - Temperature",	plot = HIST.Temp,		width	=	gHEIGH * 1.25)
message("Frequency by Period")
customSave("Hist - Frequency",		plot = HIST.Frequency,	width	=	gHEIGH * 1.25)
message("GPU Power by Period")
customSave("Hist - Power",			plot = HIST.Socket,		width	=	gHEIGH * 1.25)
# message("GPU Frequency vs Voltage")
# customSave("Freq-Volt",			plot = graphFrVo(),		width	=	gHEIGH * 1.25)