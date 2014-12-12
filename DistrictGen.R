##########################################################################
##  DistrictGen.R  Routines to generate data and plots for Districts 2015 Paper
##  I. McDonald
##########################################################################

##  Iniitialize.  Note that workspace is cleared.
library(foreign)
library(Hmisc)
library(data.table)
library(dplyr)
library(ggplot2)
library(zoo)
library(dplyr)
library(reshape)
library(stringr)
library(lme4)


rm(list=ls())
s.district2015_dir <- "/Users/irm16/Dropbox/Work Projects/SPSA 2015/Source/"
setwd(s.district2015_dir)

####################################################################################################
## 1:  READ AND CLEAN THE TAUSONOVITCH AND WARSHAW DATA.  
## NB:  The downloaded csv files were modified to include Washington State 2008 president %'s
## The original names are shd_mrp_export.csv and ssd_export.csv.
## Reference information is contained in TW.xlsx
####################################################################################################

step1 <- function() {
        df.tw.lower <<- read.csv(paste(s.district2015_dir,"TW_lower.csv",sep=""),header=TRUE)
        df.tw.upper <<- read.csv(paste(s.district2015_dir,"TW_upper.csv",sep=""),header=TRUE)
        
        v.mult_memb_states <<- as.factor(c("AZ","NJ","SD","WA"))  ## multi member district states
        v.all.states <<- unique(df.tw.lower$abb)
        v.non_mult_memb_states <<- v.all.states[!(v.all.states %in% v.mult_memb_states)]
}
step1()

####################################################################################################




####################################################################################################
## 2:  READ CENSUS POPULATION DATA 
## Reference information is contained in State Leg Population 2010.xls
####################################################################################################

df.fips <- read.csv("fips.csv")
df.pop2000.lower <- read.csv("pop2000.csv")      ## total population by state for 2000
df.pop2000.upper <- df.pop2000.lower                ## clone the pop.2000 file and use for upper houses
df.sld2010 <- read.csv("lower2010.csv")    ## population by lower house district 2010 ACS
df.ssd2010 <- read.csv("upper2010.csv") #  # population by upper house district 2010 ACS


## determine number of districts per state and use as baseline in population growth percentage calculation
## house

## lower houses
df.sld2010$index <- 1
st_sum <- tapply(df.sld2010$pop2010,df.sld2010$stcd,sum)
distnums <- as.data.frame(tapply(df.sld2010$index,df.sld2010$stcd,sum)); names(distnums) <- "distnums"
distnums$stcd <- rownames(distnums)

df.pop2000.lower <- merge(distnums,df.pop2000.lower,by="stcd")
df.pop2000.lower$pd <- round(df.pop2000.lower$pop2000 / df.pop2000.lower$distnums,0)  ## 2000 population per lower house district

df.sld2010 <- merge(df.sld2010,df.pop2000.lower,by="stcd")
df.sld2010$gpct <- (df.sld2010$pop2010 - df.sld2010$pd) / df.sld2010$pd
df.sld2010$st_hd <- paste(df.sld2010$stcd,df.sld2010$district,sep="")

## something is wrong with New Hampshire; calculation way out of range.  Eliminate for now.
df.sld2010NH <- df.sld2010[df.sld2010$stcd=="NH",]
df.sld2010 <- df.sld2010[df.sld2010$stcd!="NH",]
df.sld2010 <- df.sld2010[df.sld2010$stcd %in% v.all.states,c("st_hd","gpct")]

## upper houses
df.ssd2010$index <- 1
distnums.upper <- as.data.frame(tapply(df.ssd2010$index,df.ssd2010$stcd,sum)); names(distnums.upper) <- "distnums"
distnums.upper$stcd <- rownames(distnums.upper)

df.pop2000.upper <- merge(distnums.upper,df.pop2000.upper,by="stcd")
df.pop2000.upper$pd <- round(df.pop2000.upper$pop2000 / df.pop2000.upper$distnums,0)

df.ssd2010 <- merge(df.ssd2010,df.pop2000.upper,by.x="stcd")
df.ssd2010$gpct <- (df.ssd2010$pop2010 - df.ssd2010$pd) / df.ssd2010$pd
df.ssd2010$st_sd <- paste(df.ssd2010$stcd,df.ssd2010$district,sep="")

## something is wrong with New Hampshire; calculation way out of range.  Eliminate for now.
df.ssd2010NH <- df.ssd2010[df.ssd2010$stcd=="NH",]
df.ssd2010 <- df.ssd2010[df.ssd2010$stcd!="NH",]

df.ssd2010 <- df.ssd2010[df.ssd2010$stcd %in% v.all.states,c("st_sd","gpct")]

####################################################################################################


####################################################################################################
## 3:  CREATE LEGISLATOR SCORES FROM SHOR & MCCARTY
## Downloaded from Harvard Dataverse:  
## Shor, Boris; McCarty, Nolan, 2014, "Individual State Legislator Shor-McCarty Ideology Data, July 2014 update"
## http://goo.gl/qPGnqn
####################################################################################################

## create legislator scores
load("state legislator scores july 2014.RData") ## data appears in df "x"
df.leg.scores <- x
rm(x)


df.tw.lower <- df.tw.lower[df.tw.lower$abb %in% v.all.states,]
df.tw.upper <- df.tw.upper[df.tw.upper$abb %in% v.all.states,]
FILTER.MM <- FALSE   ## A semaphore that indicates whether to use only the four multimember states. 


####################################################################################################
## 4:  FUNCTION TO GENERATE CONSOLIDATED DATA FRAME AND ADDITIONAL VALUES
####################################################################################################

## Column specifications for the Shor McCarty table
n.hc1.init <- 37
n.hc2.init <- 79
n.sc1.init <- 16
n.sc2.init <- 58


for (v.ought in 1:10)  {
        
        ##  set columns in Shor McCarty extract.
        n.hc1 <- n.hc1.init + v.ought - 1
        n.hc2 <- n.hc2.init + v.ought - 1 
        n.sc1 <- n.sc1.init + v.ought - 1 
        n.sc2 <- n.sc2.init + v.ought - 1
        n.oyear <- v.ought + 2002
        
        s.currhcol <- paste("house",n.oyear,sep="")
        s.prevhcol <- paste("house",n.oyear-2,sep="")
        s.currhdcol <- paste("hdistrict",n.oyear,sep="")
        s.prevhdcol <- paste("hdistrict",n.oyear-2,sep="")

        s.currscol <- paste("senate",n.oyear,sep="")
        s.prevscol <- paste("senate",n.oyear-4,sep="")
        s.currsdcol <- paste("sdistrict",n.oyear,sep="")
        s.prevsdcol <- paste("sdistrict",n.oyear-4,sep="")
        
        ## select relevant columns  filter out any potential NA's
        df.house.20xx <- df.leg.scores[df.leg.scores[[s.currhdcol]] != "NA", c(1:5,n.hc1,n.hc2,n.hc2-2)]
        df.senate.20xx <- df.leg.scores[df.leg.scores[[s.currsdcol]] != "NA", c(1:5,n.sc1,n.sc2,n.sc2-4)]
       
        ## clean up spaces in s.currhcol and s.currscol
        df.house.20xx[,s.currhcol] <- substr(df.house.20xx[,s.currhcol],1,3)
        df.senate.20xx[,s.currscol] <- substr(df.senate.20xx[,s.currscol],1,3)
        
        df.house.20xx$first_term <- is.na(df.house.20xx[[s.prevhcol]])
        df.senate.20xx$first_term <- is.na(df.senate.20xx[[s.prevscol]])
              
        ## validate states against v.all.states vector, to eliminate any records w/o valid state code
        df.house.20xx <- df.house.20xx[df.house.20xx$st %in% v.all.states,]
        df.senate.20xx <- df.senate.20xx[df.senate.20xx$st %in% v.all.states,]
        
        df.house.20xx$LD <- df.house.20xx[[s.currhdcol]]
        df.senate.20xx$LD <- df.senate.20xx[[s.currsdcol]]
        ##  set columns in Shor McCarty extract.
        
        ## validate number of reps from each district
        df.house.20xx[,s.currhcol] <- as.numeric(df.house.20xx[,s.currhcol])
        df.house.20xx$LD[nchar(df.house.20xx$LD)==1] <- paste("00",df.house.20xx$LD[nchar(df.house.20xx$LD)==1],sep="")
        df.house.20xx$LD[nchar(df.house.20xx$LD)==2] <- paste("0",df.house.20xx$LD[nchar(df.house.20xx$LD)==2],sep="")
        df.house.20xx$st_hd <- paste(df.house.20xx$st,df.house.20xx[,"LD"],sep="")
        v.house.20xx.tmp  <- tapply(df.house.20xx[,s.currhcol],df.house.20xx$st_hd,sum)
        df.house.20xx.tmp <- cbind.data.frame("st_hd"=names(v.house.20xx.tmp),"dnum" = as.numeric(v.house.20xx.tmp))
        df.house.20xx <- merge(df.house.20xx,df.house.20xx.tmp,by="st_hd")
        rm(v.house.20xx.tmp,df.house.20xx.tmp)    
        
        ## validate number of senators from each district
        df.senate.20xx[,s.currscol] <- as.numeric(df.senate.20xx[,s.currscol])
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==1] <- paste("00",df.senate.20xx$LD[nchar(df.senate.20xx$LD)==1],sep="")
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==2] <- paste("0",df.senate.20xx$LD[nchar(df.senate.20xx$LD)==2],sep="")
        df.senate.20xx$st_sd <- paste(df.senate.20xx$st,df.senate.20xx[,"LD"],sep="")
        v.senate.20xx.tmp  <- tapply(df.senate.20xx[,s.currscol],df.senate.20xx$st_sd,sum)
        df.senate.20xx.tmp <- cbind.data.frame("st_sd"=names(v.senate.20xx.tmp),"dnum" = as.numeric(v.senate.20xx.tmp))
        df.senate.20xx <- merge(df.senate.20xx,df.senate.20xx.tmp,by="st_sd")
        rm(v.senate.20xx.tmp,df.senate.20xx.tmp)      
        
        ## Testing conditions for legislatures that have 3 members or more.  GA, MD, NH, and WV are the relevant ones here
        ##  handle records for more than two reps per district or one senator per district
        ##  routine can do one of two things:
        ##  1) keeps only the reps with the two highest st_id values, presumably the last two
        ##  to serve.  should work for any number N reps.
        ##  2) keep that np_score or compute the mean.
        
        ## keep in mind the problem is that members can be from multimember districts, or just more than one over a year.
        ## create a flag for treated records
        df.house.20xx$multmem.flag <- FALSE
        df.senate.20xx$multmem.flag <- FALSE
        df.house.20xx$multmem.flag[df.house.20xx$dnum > 1 &!df.house.20xx$st %in% v.mult_memb_states] <- TRUE
        df.house.20xx$multmem.flag[df.house.20xx$dnum > 2 & df.house.20xx$st %in% v.mult_memb_states] <- TRUE
        df.senate.20xx$multmem.flag[df.senate.20xx$dnum > 1] <- TRUE
        
        df.house.20xx.mm <- df.house.20xx[df.house.20xx$multmem.flag,]
        df.house.20xx.aa <- df.house.20xx[!df.house.20xx$multmem.flag,]
        df.senate.20xx.mm<- df.senate.20xx[df.senate.20xx$multmem.flag,]
        df.senate.20xx.aa<- df.senate.20xx[!df.senate.20xx$multmem.flag,]
                
        if(nrow(df.house.20xx.mm) > 0) {
                
                v.n2dists <- unique(df.house.20xx.mm$st_hd)
                v.house.20xx.mean_np <- aggregate(np_score ~ st_hd, data=df.house.20xx.mm, mean)
                df.house.20xx.mm <- merge(df.house.20xx.mm,v.house.20xx.mean_np,by="st_hd")
                df.house.20xx.mm <- rename(df.house.20xx.mm, c("np_score.x"="np_score", "np_score.y"="np_mean"))
                
                #eliminate the lowest st_id's
                ########### 12/10 10:32 working on this section.
                df.house.20xx.mm <- df.house.20xx.mm[order(df.house.20xx.mm$st_hd,df.house.20xx.mm$st_id,decreasing=TRUE),]
                ## just take one member.  Need two separate routines (mm and !mm)
                
                
                for (i in 1:length(v.n2dists)) {
                        df.iter <- df.house.20xx.mm[df.house.20xx.mm$st_hd==v.n2dists[i],]
                        v.iter.st_id <- df.iter$st_id[c(1:1)]
                        df.house.20xx.mm <- df.house.20xx.mm[df.house.20xx.mm$st_hd != v.n2dists[i] | 
                                                                     df.house.20xx.mm$st_id %in% v.iter.st_id,]
                        df.house.20xx.mm$np_score <- df.house.20xx.mm$np_mean
                }
                
                df.house.20xx.mm$np_mean <- NULL
                
                df.house.20xx <- rbind(df.house.20xx.aa, df.house.20xx.mm)  
        }
        
        
        
        if(nrow(df.senate.20xx.mm) > 0) {
                
                v.n2dists <- unique(df.senate.20xx.mm$st_sd)
                v.senate.20xx.mean_np <- aggregate(np_score ~ st_sd, data=df.senate.20xx.mm, mean)
                df.senate.20xx.mm <- merge(df.senate.20xx.mm,v.senate.20xx.mean_np,by="st_sd")
                df.senate.20xx.mm <- rename(df.senate.20xx.mm, c("np_score.x"="np_score", "np_score.y"="np_mean"))
                
                #eliminate the lowest st_id's
                ########### 12/10 10:32 working on this section.
                df.senate.20xx.mm <- df.senate.20xx.mm[order(df.senate.20xx.mm$st_sd,df.senate.20xx.mm$st_id,decreasing=TRUE),]
                ## just take one member.  Need two separate routines (mm and !mm)
                
                
                for (i in 1:length(v.n2dists)) {
                        df.iter <- df.senate.20xx.mm[df.senate.20xx.mm$st_sd==v.n2dists[i],]
                        v.iter.st_id <- df.iter$st_id[c(1:1)]
                        df.senate.20xx.mm <- df.senate.20xx.mm[df.senate.20xx.mm$st_sd != v.n2dists[i] | 
                                                                     df.senate.20xx.mm$st_id %in% v.iter.st_id,]
                        df.senate.20xx.mm$np_score <- df.senate.20xx.mm$np_mean
                }
                
                df.senate.20xx.mm$np_mean <- NULL
                
                df.senate.20xx <- rbind(df.senate.20xx.aa, df.senate.20xx.mm)        
        }
        
  
        
        
        
        
        
        
         
        rm(df.house.20xx.mm,df.senate.20xx.mm,df.house.20xx.aa,df.senate.20xx.aa,df.iter)
        df.house.20xx$year <- n.oyear
        df.senate.20xx$year <- n.oyear

        ### NB:  Rename in plyr does not seem to work with reference variables!!
        names(df.house.20xx)[names(df.house.20xx)==s.currhcol] <- "hdistrict"
        names(df.senate.20xx)[names(df.senate.20xx)==s.currscol] <- "sdistrict"       
        v.drops <- c(s.currhcol, s.currscol, s.currhdcol, s.currsdcol, s.prevhcol, s.prevscol, s.prevhdcol, s.prevsdcol, "house2010","senate2010", "np_mean")
        
        df.house.20xx <- df.house.20xx[,!(names(df.house.20xx) %in% v.drops)]
        df.senate.20xx <- df.senate.20xx[,!(names(df.senate.20xx) %in% v.drops)]
               
        ### Merge with growth data.  We are losing a lot of records at this step.
       
        df.house.20xx$ind <- df.house.20xx$party != "D" & df.house.20xx$party != "R"       
        df.house.20xx$party[df.house.20xx$ind==TRUE & df.house.20xx$np_score >= 0] <- "R"
        df.house.20xx$party[df.house.20xx$ind & df.house.20xx$np_score < 0] <- "D"
        df.house.20xx <- merge(df.house.20xx,df.sld2010,by="st_hd")
               
        df.senate.20xx$ind <- df.senate.20xx$party != "D" & df.senate.20xx$party != "R"       
        df.senate.20xx$party[df.senate.20xx$ind==TRUE & df.senate.20xx$np_score >= 0] <- "R"
        df.senate.20xx$party[df.senate.20xx$ind & df.senate.20xx$np_score < 0] <- "D"        
        df.senate.20xx <- merge(df.senate.20xx,df.ssd2010,by="st_sd")
       
        m.house.party <- table(df.house.20xx$st_hd,df.house.20xx$party)
        m.senate.party <- table(df.senate.20xx$st_sd,df.senate.20xx$party)
        
        m.house.party <- as.data.frame(cbind(st_hd = rownames(m.house.party),m.house.party))
        m.house.party$blend <- m.house.party$D==1 & m.house.party$R==1       
        df.house.20xx <- merge(df.house.20xx,m.house.party,by="st_hd"); df.house.20xx$D <- NULL; df.house.20xx$R <- NULL
        
        ################# ASSUME SENATE DISTRICTS NEVER HAS BLEND ######################
        df.senate.20xx$blend <- FALSE
        ######################################################################
             
        df.tw.lower$st_hd <- paste(df.tw.lower$abb,sprintf("%03d",df.tw.lower$ssd_fips_num %% 1000),sep="")
        df.house.20xx <- merge(df.house.20xx,df.tw.lower,by.x="st_hd",by.y="st_hd")
        df.house.20xx$pres_2008 <- as.numeric(df.house.20xx$pres_2008)
        df.house.20xx$mrp_estimate <- as.numeric(df.house.20xx$mrp_estimate)
        df.house.20xx$mrp_se <- as.numeric(df.house.20xx$mrp_se)
        
        df.tw.upper$st_sd <- paste(df.tw.upper$abb,sprintf("%03d",df.tw.upper$ssd_fips_num %% 1000),sep="")
        df.tw.upper[df.tw.upper$abb=="AK",]$st_sd <- paste("AK00",str_sub(df.tw.upper[df.tw.upper$abb=="AK",]$ssd_df.fips,-1),sep="")
        df.senate.20xx <- merge(df.senate.20xx,df.tw.upper,by.x="st_sd",by.y="st_sd")
        df.senate.20xx$pres_2008 <- as.numeric(df.senate.20xx$pres_2008)
        df.senate.20xx$mrp_estimate <- as.numeric(df.senate.20xx$mrp_estimate)
        df.senate.20xx$mrp_se <- as.numeric(df.senate.20xx$mrp_se)
        
        
        if (v.ought==1) df.house.2000s <- df.house.20xx else df.house.2000s <- rbind(df.house.2000s, df.house.20xx)
        if (v.ought==1) df.senate.2000s <- df.senate.20xx else df.senate.2000s <- rbind(df.senate.2000s, df.senate.20xx)
        
}       


save(df.senate.2000s, df.house.2000s,v.mult_memb_states, v.all.states, v.non_mult_memb_states, file= "alldata.RData")

#######################################################################


