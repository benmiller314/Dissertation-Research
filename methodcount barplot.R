# Save a bar plot of method counts per dissertation
# NB: assumes you've already run dataprep.R

methods.barplot <- function(data, dataname="noexcludes") {
	data.t <- table(data)
	rows <- length(data)
	
	filename <- paste0(imageloc, "method count barplot, ", dataname, " (N ",rows,").pdf")	
	
	if (remake_figs) {pdf(file=filename)}
	
	barplot(data.t, 
			main="Most Dissertations use Multiple Methods",
			sub=dataname,
			xlab="Method Tags Assigned",
			ylab="Dissertations",
			las=1		# labels always horizontal
		)
	
		for(i in 1:length(data.t)) {
			if(data.t[[i]] > 20) {
				text(1.2*i-0.5, data.t[[i]] - 20, data.t[[i]])		
			} else {
				text(1.2*i-0.5, data.t[[i]] + 20, data.t[[i]])			
			}
		}
		
		data.mean <- mean(data)
		data.sd <- sd(data)

		mtext(side=4, las=1, adj=1, 
			text=paste("mean =",round(data.mean,2),"\n","sd =",round(data.sd,2),"\n","N =",rows)
		)

	if (remake_figs) {dev.off()}
}

methods.barplot(noexcludes$Method.Count, "All Schools")
methods.barplot(consorts$Method.Count, "Consortium Schools Only")
methods.barplot(nonconsorts$Method.Count, "Non-Consortium Schools Only")
