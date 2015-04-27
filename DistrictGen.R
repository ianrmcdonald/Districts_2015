##########################################################################
##  DistrictGen.R  Routines to generate data and plots for Districts 2015 Paper
##  I. McDonald
##########################################################################


rm(list=ls())
library(foreign)
library(plyr)
library(reshape2)
library(dplyr)
library(stringr)

work_dir <- "/Users/irm16/Dropbox/Work Projects/Districts 2015/Source/"

set.district2015.dir <- function(dir.name) {
     s.district2015_dir <- dir.name
     setwd(s.district2015_dir)
}

set.district2015.dir(work_dir)

v.mult.mem.states = c("AZ","ID","NJ","SD","WA")   ## States with multi member districts
v.exclude.states = c("VT","NH","MD","MA")         ## States to exclude for the time being

f.TW.lower <- "TW_lower.csv"
f.TW.upper <- "TW_upper.csv"
f.house.gazette <- "2013_Gaz_sldl_national.csv"
f.senate.gazette <- "2013_Gaz_sldu_national.csv"
MM <- v.mult.mem.states 
EX <- v.exclude.states

## Section 2:  Initialize Values and Read Warshaw and Tausnovitch files -------------------------------------------
## NB:  The downloaded csv files were modified to include Washington State 2008 president %'s
## The original names are shd_mrp_export.csv and ssd_export.csv.
## Reference information is contained in TW.xlsx

df.tw.lower <- read.csv(paste(work_dir, f.TW.lower, sep=""), stringsAsFactors=FALSE, header=TRUE)
df.tw.upper <- read.csv(paste(work_dir, f.TW.upper, sep=""), stringsAsFactors=FALSE, header=TRUE)

v.mult_memb_states <- as.factor(MM)  ## multi member district states
v.all.states <- unique(df.tw.lower$abb)
v.non_mult_memb_states <- v.all.states[!(v.all.states %in% v.mult_memb_states)]
v.exclude <- EX

df.tw.lower <- df.tw.lower[df.tw.lower$abb %in% v.all.states,]
df.tw.upper <- df.tw.upper[df.tw.upper$abb %in% v.all.states,]

df.house.gazette <- read.csv(f.house.gazette, stringsAsFactors=FALSE, header=TRUE)
df.house.gazette$sm_name_2 <- paste(df.house.gazette$st, df.house.gazette$sm_name_1, sep=":")
df.senate.gazette <- read.csv(f.senate.gazette, stringsAsFactors=FALSE, header=TRUE)
df.senate.gazette$sm_name_2 <- paste(df.senate.gazette$st, df.senate.gazette$sm_name_1, sep=":")


####################################################################################################
## 2:  READ CENSUS POPULATION DATA
## Reference information is contained in State Leg Population 2010.xls
####################################################################################################

df.fips <- read.csv("fips.csv")
df.pop2000.lower <- read.csv("pop2000.csv",stringsAsFactors=FALSE)      ## total population by state for 2000
df.pop2000.upper <- df.pop2000.lower                ## clone the pop.2000 file and use for upper houses
df.sld2010 <- read.csv("lower2010.csv",stringsAsFactors=FALSE)    ## population by lower house district 2010 ACS
df.sld2010$sld_fips <- paste(df.sld2010$fips,df.sld2010$district,sep="")
df.ssd2010 <- read.csv("upper2010.csv",stringsAsFactors=FALSE) #  # population by upper house district 2010 ACS
df.ssd2010$ssd_fips <- paste(df.ssd2010$fips,df.ssd2010$district,sep="")


## determine number of districts per state and use as baseline in population growth percentage calculation
## house

## lower houses
df.sld2010$index <- 1
v.st_sum <- tapply(df.sld2010$pop2010,df.sld2010$stcd,sum)
df.distnums.lower <- as.data.frame(tapply(df.sld2010$index,df.sld2010$stcd,sum)); names(df.distnums.lower) <- "distnums"
df.distnums.lower$stcd <- rownames(df.distnums.lower)

df.pop2000.lower <- merge(df.distnums.lower,df.pop2000.lower,by="stcd",all.x=TRUE)
df.pop2000.lower$pd <- round(df.pop2000.lower$pop2000 / df.pop2000.lower$distnums,0)  ## 2000 population per lower house district

df.sld2010 <- merge(df.sld2010,df.pop2000.lower,by="stcd",all.x=TRUE)
df.sld2010$gpct <- (df.sld2010$pop2010 - df.sld2010$pd) / df.sld2010$pd
df.sld2010$st_hd <- paste(df.sld2010$stcd,df.sld2010$district,sep="")
df.sld2010$st_hd <- paste(df.sld2010$stcd,df.sld2010$district,sep=":")
# ## these states have issues with district mapping.  Eliminate for now.

df.sld2010EX <- df.sld2010[df.sld2010$stcd %in% v.exclude,]
df.sld2010 <- df.sld2010[!df.sld2010$stcd %in% v.exclude,]
df.sld2010 <- df.sld2010[df.sld2010$stcd %in% v.all.states,c("st_hd","gpct")]

## upper houses
df.ssd2010$index <- 1
v.st_sum <- tapply(df.ssd2010$pop2010,df.ssd2010$stcd,sum)
df.distnums.upper <- as.data.frame(tapply(df.ssd2010$index,df.ssd2010$stcd,sum)); names(df.distnums.upper) <- "distnums"
df.distnums.upper$stcd <- rownames(df.distnums.upper)

df.pop2000.upper <- merge(df.distnums.upper,df.pop2000.upper,by="stcd",all.x=TRUE)
df.pop2000.upper$pd <- round(df.pop2000.upper$pop2000 / df.pop2000.upper$distnums,0)

df.ssd2010 <- merge(df.ssd2010,df.pop2000.upper,by="stcd",all.x=TRUE)
df.ssd2010$gpct <- (df.ssd2010$pop2010 - df.ssd2010$pd) / df.ssd2010$pd
df.ssd2010$st_sd <- paste(df.ssd2010$stcd,df.ssd2010$district,sep=":")


# ## these states have issues with district mapping.  Eliminate for now.
df.ssd2010EX <- df.ssd2010[df.ssd2010$stcd %in% v.exclude,]
df.ssd2010 <- df.ssd2010[!df.ssd2010$stcd %in% v.exclude,]
df.ssd2010 <- df.ssd2010[df.ssd2010$stcd %in% v.all.states,c("st_sd","gpct")]


####################################################################################################
## 3:  CREATE LEGISLATOR SCORES FROM SHOR & MCCARTY
## Downloaded from Harvard Dataverse:
## Shor, Boris; McCarty, Nolan, 2014, "Individual State Legislator Shor-McCarty Ideology Data, July 2014 update"
## http://goo.gl/qPGnqn
####################################################################################################

load("state legislator scores july 2014.RData") ## data appears in df "x"
df.leg.scores <- x
rm(x)
df.leg.scores.EX <- df.leg.scores[df.leg.scores$st %in% v.exclude,]
df.leg.scores <- df.leg.scores[!df.leg.scores$st %in% v.exclude,]
v.leg.score.names <- names(df.leg.scores)
v.leg.score.year <- str_sub(v.leg.score.names, start= -4)
suppressWarnings(v.leg.score.year <- as.numeric(v.leg.score.year))

##### Melt experiment
suppressWarnings(y <- melt(df.leg.scores,id=c("name","party","st","st_id","np_score")))
##### Choosing not to use melt at this step.  


generate.consolidated.extracts <- function(n.year = 2003, TRIM.MULTIMEMBERS = TRUE) {
     

        ##  set columns and column names in Shor McCarty extract.
        ##  This logic currently assume house precedes senate
        
        # Testing df.house --------------------------------------------------------
        
        
        v.col.names <- v.leg.score.names[v.leg.score.year==n.year & !is.na(v.leg.score.year)]
        
        ## Some error checking:  make sure v.col.names has exactly 4 items
        if(length(v.col.names) != 4) stop("Number of column names is not 4")
        
        v.col.numbers <- which(v.leg.score.names %in% v.col.names)
        n.hc1 <- v.col.numbers[2]
        n.hc2 <- v.col.numbers[4]
        n.sc1 <- v.col.numbers[1]
        n.sc2 <- v.col.numbers[3]
        
        ### Some more error checking on aligning column names
        if(!grepl("hdistrict",v.leg.score.names[n.hc1])) stop("Column names are incorrrect")
        if(!grepl("house",v.leg.score.names[n.hc2])) stop("Column names are incorrrect")
        if(!grepl("sdistrict",v.leg.score.names[n.sc1])) stop("Column names are incorrrect")
        if(!grepl("senate",v.leg.score.names[n.sc2])) stop("Column names are incorrrect")
        
        p.hc1 <- n.hc1 - 2  ## For previous year for counting incumbency
        p.hc2 <- n.hc2 - 2 ## Assuming 2 year house and 4 year senate terms which...
        p.sc1 <- n.sc1 - 4  ## ...is a bad assumption and needs to be validated.
        p.sc2 <- n.sc2 - 4 
        
        txt.houseYEAR.col <- v.leg.score.names[n.hc2]  ## name of corresponding column "houseYEAR"
        txt.houseYEAR.col.previous <- v.leg.score.names[p.hc2]  ## name of corresponding column "hdistrictYEAR
        txt.hdistrictYEAR.col <- v.leg.score.names[n.hc1]  
        txt.hdistrictYEAR.col.previous <- v.leg.score.names[p.hc1]
        
        txt.senateYEAR.col <- v.leg.score.names[n.sc2] 
        txt.senateYEAR.col.previous <- v.leg.score.names[p.sc2]
        txt.sdistrctYEAR.col <- v.leg.score.names[n.sc1]
        txt.sdistrctYEAR.col.previous <- v.leg.score.names[p.sc1] 
        
        df.house.20xx <- df.leg.scores[!is.na(df.leg.scores[[txt.houseYEAR.col]]), c(1:5,n.hc1,n.hc2,p.hc1,p.hc2)]
        df.senate.20xx <- df.leg.scores[!is.na(df.leg.scores[[txt.senateYEAR.col]]), c(1:5,n.sc1,n.sc2,p.sc1,p.sc2)]
        
        ### Edit various district id's  good candidate for MUTATE function
        
        ## CASE 1:  Washington State
        ## Data intermittently includes a "-1" or "-2" at the end of district id, but not always
        df.house.20xx[df.house.20xx$st=="WA",txt.hdistrictYEAR.col] <- substr(df.house.20xx[df.house.20xx$st=="WA",txt.hdistrictYEAR.col],1,3)
        df.senate.20xx[df.senate.20xx$st=="WA",txt.sdistrctYEAR.col] <- substr(df.senate.20xx[df.senate.20xx$st=="WA",txt.sdistrctYEAR.col],1,3)
        
        ## CASE 2:  South Dakota
        ## Data includes a "-1" or "-2" for districts 26 and 28
        ## We will consolidate them
        df.house.20xx[df.house.20xx$st=="SD",txt.hdistrictYEAR.col] <- substr(df.house.20xx[df.house.20xx$st=="SD",txt.hdistrictYEAR.col],1,3)
        df.senate.20xx[df.senate.20xx$st=="SD",txt.sdistrctYEAR.col] <- substr(df.senate.20xx[df.senate.20xx$st=="SD",txt.sdistrctYEAR.col],1,3)
        df.sld2010[df.sld2010$st_hd =="SD26A","st_hd"] <- "SD026"
        df.sld2010[df.sld2010$st_hd =="SD28A","st_hd"] <- "SD028"
        
        ## CASE 3:  Idaho
        ## Data intermittently includes a "-1" or "-2" at the end of district id, but not always
        df.house.20xx[df.house.20xx$st=="ID",txt.hdistrictYEAR.col] <- substr(df.house.20xx[df.house.20xx$st=="ID",txt.hdistrictYEAR.col],1,3)
        df.senate.20xx[df.senate.20xx$st=="ID",txt.sdistrctYEAR.col] <- substr(df.senate.20xx[df.senate.20xx$st=="ID",txt.sdistrctYEAR.col],1,3)
        
        
        ###  Determine if incumbent
        df.house.20xx$first_term <- is.na(df.house.20xx[[txt.houseYEAR.col.previous]])
        df.senate.20xx$first_term <-is.na(df.senate.20xx[[txt.senateYEAR.col.previous]])
        
        
        ## validate states against v.all.states vector, to eliminate any records w/o valid state code
        df.house.20xx <- df.house.20xx[df.house.20xx$st %in% v.all.states,]
        df.senate.20xx <- df.senate.20xx[df.senate.20xx$st %in% v.all.states,]
        
        
        ## rename district column to generic "LD" name
        df.house.20xx$LD <- df.house.20xx[[txt.hdistrictYEAR.col]]
        df.senate.20xx$LD <- df.senate.20xx[[txt.sdistrctYEAR.col]]
        ###########################################################
        
        
        
        ## validate number of reps from each district for the year good candidate for GROUPBY operation
        df.house.20xx[,txt.houseYEAR.col] <- as.numeric(df.house.20xx[,txt.houseYEAR.col])
        ## create a generic format of LD field to "000" with a length of 3
        df.house.20xx$LD[nchar(df.house.20xx$LD)==1] <- paste("00",df.house.20xx$LD[nchar(df.house.20xx$LD)==1],sep="")
        df.house.20xx$LD[nchar(df.house.20xx$LD)==2] <- paste("0",df.house.20xx$LD[nchar(df.house.20xx$LD)==2],sep="")
        df.house.20xx$st_hd <- paste(df.house.20xx$st,df.house.20xx[,"LD"],sep=":")
        df.house.20xx.count <- group_by(df.house.20xx, st_hd) %>% summarise(dnum = n())
        df.house.20xx <- left_join(df.house.20xx,df.house.20xx.count,by="st_hd")
        rm(df.house.20xx.count)
        
        
        ## validate number of senators from each district for the year good candidate for GROUPBY operation
        df.senate.20xx[,txt.senateYEAR.col] <- as.numeric(df.senate.20xx[,txt.senateYEAR.col])
        ## create a generic format of LD field to "000" with a length of 3
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==1] <- paste("00",df.senate.20xx$LD[nchar(df.senate.20xx$LD)==1],sep="")
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==2] <- paste("0",df.senate.20xx$LD[nchar(df.senate.20xx$LD)==2],sep="")
        df.senate.20xx$st_sd <- paste(df.senate.20xx$st,df.senate.20xx[,"LD"],sep=":")
        df.senate.20xx.count <- group_by(df.senate.20xx, st_sd) %>% summarise(dnum = n())
        df.senate.20xx <- left_join(df.senate.20xx,df.senate.20xx.count,by="st_sd")
        rm(df.senate.20xx.count)
        
        
        ## Testing conditions for legislatures that have 3 members or more.  
        ##  handle records for more than two reps per district or one senator per district
        ##  routine can do one of two things:
        ##  1) keeps only the reps with the two highest st_id values, presumably the last two
        ##  to serve.  should work for any number N reps.
        ##  2) keep that np_score or compute the mean.
        
        ## keep in mind the problem is that members can be from multimember districts, or just more than one over a year.
        ## create a flag for treated records
        ## should we flag members from districts that we know are multimember?
        ## for now, we are going to ignore the v.mult_memb_states, and if we get more than two, we will live with it
        ## assuming no multimember senate districts
        ## the routine below will compute the mean np_score and eliminate all but one record for districts with the flag set to TRUE
        
        
        df.house.20xx$multmem.flag <- FALSE
        df.senate.20xx$multmem.flag <- FALSE
        df.house.20xx$multmem.flag[df.house.20xx$dnum > 1 &!df.house.20xx$st %in% v.mult_memb_states] <- TRUE
        df.senate.20xx$multmem.flag[df.senate.20xx$dnum > 1] <- TRUE
        
        df.house.20xx.mm <- df.house.20xx[df.house.20xx$multmem.flag,]
        df.house.20xx.aa <- df.house.20xx[!df.house.20xx$multmem.flag,]
        df.senate.20xx.mm<- df.senate.20xx[df.senate.20xx$multmem.flag,]
        df.senate.20xx.aa<- df.senate.20xx[!df.senate.20xx$multmem.flag,]
        
        if(TRIM.MULTIMEMBERS) {
                
                if(nrow(df.house.20xx.mm) > 0) {
                        
                        df.house.mean_np <- group_by(df.house.20xx.mm, st_hd) %>% 
                                summarise(np_mean = mean(np_score))
                        
                        df.house.20xx.mm <- left_join(df.house.20xx.mm,df.house.mean_np,by="st_hd")
                        df.house.20xx.mm <- df.house.20xx.mm %>% group_by(st_hd) %>% filter(min_rank(desc(st_id)) == 1)
                        if (nrow(df.house.20xx.mm) != nrow(df.house.mean_np)) stop("Check for duplicates after reducing mm districts to one.")
                        
                        df.house.20xx.mm$np_mean <- NULL
                        df.house.20xx <- rbind(df.house.20xx.aa, df.house.20xx.mm)
                        
                }
                
                if(nrow(df.senate.20xx.mm) > 0) {
                        
                        df.senate.mean_np <- group_by(df.senate.20xx.mm, st_sd) %>% 
                                summarise(np_mean = mean(np_score))
                        
                        df.senate.20xx.mm <- left_join(df.senate.20xx.mm,df.senate.mean_np,by="st_sd")
                        df.senate.20xx.mm <- df.senate.20xx.mm %>% group_by(st_sd) %>% filter(min_rank(desc(st_id)) == 1)
                        if (nrow(df.senate.20xx.mm) != nrow(df.senate.mean_np)) stop("Check for duplicates after reducing mm districts to one.")
                        
                        df.senate.20xx.mm$np_mean <- NULL
                        df.senate.20xx <- rbind(df.senate.20xx.aa, df.senate.20xx.mm)
                        
                }
                
                rm("df.house.20xx.mm","df.senate.20xx.mm","df.house.20xx.aa","df.senate.20xx.aa")
        }
        
        
        ###################################################
        
        
        df.house.20xx$year <- n.year
        df.senate.20xx$year <- n.year
        names(df.house.20xx)[names(df.house.20xx)==txt.houseYEAR.col] <- "hdistrict"
        names(df.senate.20xx)[names(df.senate.20xx)==txt.senateYEAR.col] <- "sdistrict"
        ## dplyr's rename and select functions did not work well here because of the literal string behavior
        
        v.drop.Columns <- c(txt.houseYEAR.col, txt.senateYEAR.col, txt.hdistrictYEAR.col, txt.sdistrctYEAR.col, txt.houseYEAR.col.previous, txt.senateYEAR.col.previous, txt.hdistrictYEAR.col.previous, txt.sdistrctYEAR.col.previous)
        
        df.house.20xx <- df.house.20xx[,!(names(df.house.20xx) %in% v.drop.Columns)]
        df.senate.20xx <- df.senate.20xx[,!(names(df.senate.20xx) %in% v.drop.Columns)]
        
        
        ### Merge with growth data. 
        ### If party isn't D or R, assign D or R based on np_score, and create a "third party" flag.  
        
        df.house.20xx$party <- as.character(df.house.20xx$party)
        df.house.20xx$third_party <- df.house.20xx$party != "D" & df.house.20xx$party != "R"
        df.house.20xx$party[df.house.20xx$third_party==TRUE & df.house.20xx$np_score >= 0] <- "R"
        df.house.20xx$party[df.house.20xx$third_party==TRUE & df.house.20xx$np_score < 0] <- "D"
        
        df.senate.20xx$party <- as.character(df.senate.20xx$party)
        df.senate.20xx$third_party <- df.senate.20xx$party != "D" & df.senate.20xx$party != "R"
        df.senate.20xx$party[df.senate.20xx$third_party==TRUE & df.senate.20xx$np_score >= 0] <- "R"
        df.senate.20xx$party[df.senate.20xx$third_party==TRUE & df.senate.20xx$np_score < 0] <- "D"
        
        ########################### Bookmark 4/26/15 6:23pm
        
        
        ## Fix some problems with the Minnesota data.  I'm not sure why this section is here; should be in a 
        ## separate data fix section
        
        if(n.year >= 2009) {
                v.mn1 <- df.house.gazette[df.house.gazette$st=="MN",c("sm_name_1")]
                v.mn1 <- substr(v.mn1,2,nchar(v.mn1))
                df.house.gazette[df.house.gazette$st=="MN",c("sm_name_1")] <- v.mn1
                df.house.20xx$st_hd[df.house.20xx$st.x=="MN"] <-
                        paste("MN:0",df.house.20xx$LD[df.house.20xx$st.x=="MN"],sep="")
        }
        
        
        ## joins 
        df.house.20xx <- left_join(df.house.20xx,df.house.gazette,by=c("st_hd"="sm_name_2"))
        df.house.20xx <- left_join(df.house.20xx,df.sld2010,by="st_hd")
        
        df.senate.20xx <- left_join(df.senate.20xx,df.senate.gazette,by=c("st_sd"="sm_name_2"))
        df.senate.20xx <- left_join(df.senate.20xx,df.ssd2010,by="st_sd")
        
        
        ## identify house districts with more than one member, and at least one from each party.  These are from the non-mm flag districts
        ## ...which will generally come from states identified as multi-member
        ## ignore for senate for now
        ## the table is making a factor
        
        m.house.party <- table(df.house.20xx$st_hd,df.house.20xx$party)
        m.house.party <- as.data.frame(cbind(st_hd = rownames(m.house.party),m.house.party))
        m.house.party$D <- as.numeric(m.house.party$D); m.house.party$R <- as.numeric(m.house.party$R)
        m.house.party$blend <- as.numeric(m.house.party$D) > 0 & as.numeric(m.house.party$R) > 0
        df.house.20xx <- merge(df.house.20xx,m.house.party,by="st_hd"); df.house.20xx$D <- NULL; df.house.20xx$R <- NULL
        df.senate.20xx$blend <- FALSE
        
        
        
        
        
        
        ######################################################################
        
        
        
        df.tw.lower$st_hd <- paste(df.tw.lower$abb,sprintf("%03d",df.tw.lower$ssd_fips_num %% 1000),sep=":")
                
        df.mn <- df.tw.lower[df.tw.lower$abb=="MN",c("ssd_fips","st_hd")]
        
        ### This is more correction to Minnesota
        if(n.year >= 2009) {
                df.mn$st_hd <- paste("MN:",substr(df.mn$ssd_fips,nchar(df.mn$ssd_fips)-2,nchar(df.mn$ssd_fips)),sep="")
        } else {
                df.mn$st_hd <- paste("MN:0",substr(df.mn$ssd_fips,nchar(df.mn$ssd_fips)-2,nchar(df.mn$ssd_fips)),sep="")
        }
        df.tw.lower[df.tw.lower$abb=="MN","st_hd"] <- df.mn$st_hd
        
        ## Create a lookup field for the gazette
        
        df.house.20xx <- left_join(df.house.20xx, df.tw.lower, by="st_hd")
        df.house.20xx$pres_2008 <- as.numeric(df.house.20xx$pres_2008)
        df.house.20xx$mrp_estimate <- as.numeric(df.house.20xx$mrp_estimate)
        df.house.20xx$mrp_se <- as.numeric(df.house.20xx$mrp_se)
        
        df.tw.upper$st_sd <- paste(df.tw.upper$abb,sprintf("%03d",df.tw.upper$ssd_fips_num %% 1000),sep=":")
        
        ### Fix on Alaska Senate coding
        df.tw.upper[df.tw.upper$abb=="AK",]$st_sd <-
                        paste("AK:",substr(df.tw.upper$ssd_fips[df.tw.upper$abb=="AK"],3,6),sep="")
        
        df.senate.20xx <- left_join(df.senate.20xx,df.tw.upper,by="st_sd")  
        df.house.20xx$pres_2008 <- as.numeric(df.house.20xx$pres_2008)
        df.house.20xx$mrp_estimate <- as.numeric(df.house.20xx$mrp_estimate)
        df.house.20xx$mrp_se <- as.numeric(df.house.20xx$mrp_se)
        
        df.house.20xx <<- df.house.20xx
        df.senate.20xx <<- df.senate.20xx

}

for (year in 2003:2012) {
        
        generate.consolidated.extracts(year)
        if (year==2004) {
                df.house.2000s <- df.house.20xx
        } else {
                df.house.2000s <- rbind(df.house.2000s,df.house.20xx)
        }
        
}

####  Notes:
####  A significant number of records are getting dropped for 2009 and 2010
#### Data missing for some states
#### Need to extapolote??
#### 2004 2005 2006 2007 2008 2009 2010 2011 2012 
#### 4420 4448 4457 4470 4464 2417 2518 4046 3973 

#### Count number of records by year/state


