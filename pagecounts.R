# find out the average, min, and max number of pages for dissertations by tag

require(doBy)

summaryBy(Pages~Case+Crit+Cult+Disc+Ethn+Expt+Hist+Intv+Meta+Modl+Phil+Poet+Pract+Rhet+Surv,data=monomethodics) -> monotag.pages
summary(monotag.pages$Pages.mean)