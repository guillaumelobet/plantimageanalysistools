

library(readxl)


rs <- read_excel("pia_data.xlsx")


## IS THERE A LINK TO THE TOOL ?
rs$link_alive[rs$link_alive == -1] <- "no link"
rs$link_alive[rs$link_alive == 1] <- "working link"
rs$link_alive[rs$link_alive == 0] <- "broken link"
table(rs$link_alive)



## HOW WAS THE TOOL VALIDATED ?
table(rs$validation_type)


## HOW MANY IMAGES WERE USED FOR THE VALIDATION ?
rs$image_class <- "-1"
rs$image_class[rs$image_number == 0 ] <- "0"
rs$image_class[rs$image_number > 0 & rs$image_number < 50] <- "50"
rs$image_class[rs$image_number > 50 & rs$image_number < 100] <- "100"
rs$image_class[rs$image_number > 100 & rs$image_number < 1000] <- "1000"
rs$image_class[rs$image_number > 1000] <- "1000+"
rs$image_class[is.na(rs$image_number)] <- NA

table(rs$image_class)



## WHERE THESE IMAGE RELEASED?
table(rs$image_available)


## WHAT WAS THE LICENCE FOR THE SOFTWARE?
rs$licence[grepl("open", rs$licence)] <- "open-source"
rs$licence[grepl("free", rs$licence)] <- "freeware"
rs$licence[grepl("demand", rs$licence)] <- "on-demand"
table(rs$licence)

## WAS THE PAPER PUBLISHED IN OPEN ACCESS?
table(rs$open_access)

## WHAT IS THE CITATION RATE FOR THE PAPERS
rs$citation_rate <- rs$cites / (2017-rs$year)
summary(rs$citation_rate)
hist(rs$citation_rate)

boxplot(rs$citation_rate ~ rs$method)
fit <- aov(rs$citation_rate ~ rs$method)
summary(fit)


############################################################
######    RESULTS FROM THE SURVEY
############################################################


survey <- read_excel("pia_responses.xlsx")


message(paste0(nrow(rs)-nrow(survey), " / ", nrow(survey)))

# HOW WAS THE TOOL DEVELOPPED ?
table(survey$developped)


# WHO WAS THE TOOL DEVELOPPER ?
table(survey$permanent)

# IS THE TOOL STILL MAINTAINED?
table(survey$maintained)


