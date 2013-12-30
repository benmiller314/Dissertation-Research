# read in and parse the Carnegie Classification data
carnegie.all <- read.csv(file="../cc2010_classification_data_datasheet_06.03.2013.csv")
attach(carnegie.all)

# identify and subset out schools in my dataset
# all schools with at least one doctoral program
disses.all.fields <- carnegie.all$PROF_D + carnegie.all$SOC_D + carnegie.all$STEM_D + carnegie.all$HUM_D


cdoc2010 <- carnegie.all[which(IPGRAD2010 > 11),]
cdoc2005 <- carnegie.all[which(CCIPGRAD > 11),]
cdoc2000 <- carnegie.all[which(CC2000 %in% c(15,16)),]

rows <- with(carnegie.all, !which(NAME %in% noexcludes$School))

cdoc2010.geo <- merge(cdoc2010,all_schools[,1:3], by.x="NAME", by.y="School")


s <- schools.geo[,1:3]
c <- carnegie.all[,2:4]

cc <- merge(s,c,all.x=T,all.y=F,by.x="School",by.y="NAME")
cc			# hmm. a lot of missed matches. may need to get creative. or just use Refine.
o <- paste("Still missing", length(which(is.na(cc$CITY)))," of ",nrow(schools.geo),"schools.")
o
rm(s,c,o)


# determine whether these schools changed classification between 2000 and 2010


# analyses to try in further documents:
# 1. all comp/rhet dissertations by school classification (try different levels of drill-down; see 2010classifications_logic.pdf / http://classifications.carnegiefoundation.org/methodology/grad_program.php)
# 2. map of all schools vs. map of schools with comp/rhet 
# 3. correlation table of school classification vs. method tag
