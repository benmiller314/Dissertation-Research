## job search aids

# make sure the data exists
if (!exists("noexcludes")) {source('/Users/benmiller314/Dropbox/coursework, etc/dissertation/data, code, and figures/Dissertation Research/start here.R')}
if(!exists("sumbytags", mode="function")) { source(file="collocation heatmap.R") }

# narrow to a particular school 		TO DO: make this a parameter of a wraparound function... and loopable
s <- "University of Pittsburgh-Pittsburgh Campus"
s.short <- "University of Pittsburgh"
cuny <- "CUNY Graduate School and University Center"
cuny.short <- "CUNY Grad Center"
# get the data from just that school
s.data <- noexcludes[which(noexcludes$School == s),]
cuny.data <- noexcludes[which(noexcludes$School == cuny),]


# zoom in on tags to aid in plotting
df <- data.frame(s.data[, tagnames], row.names=s.data$Pub.number)


if(remake_figs) { pdf(filename=paste0(imageloc, "figures for ", s, ".pdf")) }
	sumbytags("s.data")
	mtext(paste("Method correlations for ", s.short))
if(remake_figs) { dev.off() }

# Get method count data for various analyses
a <- sumbytags("s.data")

# Raw count of methods ("methodological output")
if(remake_figs) { pdf(file=paste0(imageloc, "method barplot for ", s.short, ".pdf")) }
	barplot(a$total.counts[c("Phil", "Crit", "Rhet", "Hist", "Disc", "Poet", "Meta", "Othr", "Modl", "Surv", "Intv", "Expt", "Prac", "Ethn", "Clin")], main=paste("Method tag counts for", s.short), horiz=F, las=2)
	
if(remake_figs) { dev.off() }

# Percentages of methods ("methodological focus")

	# build color scale
	require(RColorBrewer)
	myCol <- brewer.pal(9, "Greys")
	
	# and make a legend for it	
	if(remake_figs)	{
		filename <- paste0(imageloc, "color legend for ", a$dataset, " methodological focus.pdf")
		pdf(filename)
	}
		xleft <- seq(0, 1, length.out=length(myCol))
		xdiff <- xleft[2]-xleft[1]
		
		plot(0, 0, xlim=c(0,1+xdiff), ylim=c(0,1), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
		rect(xleft=xleft, xright=xleft+xdiff, ybottom=1-xdiff/3, ytop=1, col=myCol)
		text(x=xleft, y=1-xdiff/1.8, labels=100*round(seq(0, 1, length.out=length(myCol)), 2))
	if(remake_figs) { dev.off() }	
	

	# prepare to calculate values and map them to that color scale
	# colorme <- function (val, min.val=0, max.val=1, cols=myCol) {
		# colIndex <- round(val*10); colIndex
		# if(colIndex == 0) { colIndex <- 1 }
		# return(cols[colIndex])
	# }
	colorme <- function (val, min.val, max.val) {
		colIndex <- round(length(myCol)-min.val * (val)
		colIndex <- max(1,colIndex)
		return(myCol[colIndex])
	}
	
	colorme <- colorRamp(myCol, interpolate="linear", alpha=T)			
	
	# then use it in a plot
	if(remake_figs) { pdf(filename=paste0(imageloc, "method barplot for ", s.short, ", normed.pdf")) }

	b <- a$total.counts[c("Phil", "Crit", "Rhet", "Hist", "Disc", "Poet", "Meta", "Othr", "Modl", "Surv", "Intv", "Expt", "Prac", "Ethn", "Clin")] / sum(a$total.counts)
		# set up a blank canvas of the right size
		plot(0, 0, xlim=c(0.5,0.5+length(b)), ylim=c(0.5,0.5+1), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
		for(i in 1:length(b)) {
			symbols(i,1+length(b)-i,squares=1, add=TRUE, inches=FALSE, bg=colorme(b[i]))
			text(i,1+length(b)-i, round(b[i], 2), cex=0.65)
		}
		barplot(b, main=paste("Method tag percentages for", s.short), col=sapply(b, colorme), horiz=F, las=2)
	
	
	if(remake_figs) { dev.off() }	



# Compare method rankings at target school and home school
if(!exists("compare_method_ranks", mode="function")) { source(file="compare method ranks.R") }
compare_method_ranks("cuny.data", "s.data", colorful=T, pcts=T, betterlabels=c(cuny.short, s.short))

pitt.english <- s.data[grep("English", s.data$Department), ]
pitt.comm <- s.data[grep("Communication", s.data$Department), ]

compare_method_ranks("pitt.english", "pitt.comm", colorful=T, betterlabels=c("Pitt English", "Pitt Communication"), pcts=F)



colSums(s.data[, tagnames])
nrow(s.data)


s.data.simple <- noexcludes[which(noexcludes$School == s), c("Year", tagnames.simple)]
df.s <- data.frame(s.data.simple[, tagnames.simple], row.names=s.data.simple$Pub.number)


## Find out the topic distribution at this school
  if(!exists("get.doctopic.grid", mode="function")) { source(file="get doctopic grid.R") }
grid <- as.data.table(get.doctopic.grid()$outputfile)
head(grid)

s.index <- noexcludes.dt[School %in% s, Pub.number]
s.grid <- grid[grid$Pub.number %in% s.index, ]
