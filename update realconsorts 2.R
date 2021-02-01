####################
# 
# GOAL: a clean refactor of consortium/rhetmap program confirmation
# 
# PLAN: 
#       1. from dataset, let to_match = name, pub.number, school, department, title, year
#       2. filter to_match by school %in% (union(consortium$University, rhetmaplist$Carnegie2015_name, rhetmaplist$Carnegie2010_name)
#       3. build master alumni_list from files (also with name, school, program, year)
#       4. for name in to_match, 
#           a. search for last name in alumni_list$found_name, or (if not there) in alumni_list$name
#           b. do manual matching if needed, then update alumni_list$found_name
#           c. update to_match$realconsort
#       5. save updated alumni_list
#       6. merge to_match back into dataset
# 
####################


require(magrittr)
require(tidyr)

dataset <- noexcludes2011_2015
to_match <- dataset %>% 
    select(Author, Year, School, Department, Title, Pub.number) %>%
    arrange(School, Year, Author)
