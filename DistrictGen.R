##########################################################################
##  DistrictGen.R  Routines to generate data and plots for Districts 2015 Paper
##  I. McDonald
##########################################################################

##  Iniitialize.  Note that workspace is cleared.
library(foreign)
library(dplyr)
library(reshape)


rm(list=ls())

set.district2015.dir <- function(dir.name) {
        s.district2015_dir <<- dir.name
        setwd(s.district2015_dir)
}
set.district2015.dir("/Users/irm16/Dropbox/Work Projects/SPSA 2015/Source/")

read.and.initialize.1 <- function(f.TW.lower, f.TW.upper, f.gazette, 
                                  MM=c("AZ","NJ","SD","WA"), EX=c("VT","NH","MD","MA")) {       
        ####################################################################################################
        ## 1:  READ AND CLEAN THE TAUSONOVITCH AND WARSHAW DATA.  
        ## NB:  The downloaded csv files were modified to include Washington State 2008 president %'s
        ## The original names are shd_mrp_export.csv and ssd_export.csv.
        ## Reference information is contained in TW.xlsx
        ####################################################################################################
        
        df.tw.lower <- read.csv(paste(s.district2015_dir, f.TW.lower, sep=""), stringsAsFactors=FALSE, header=TRUE)
        df.tw.upper <- read.csv(paste(s.district2015_dir, f.TW.upper, sep=""), stringsAsFactors=FALSE, header=TRUE)
       
        v.mult_memb_states <<- as.factor(MM)  ## multi member district states
        v.all.states <<- unique(df.tw.lower$abb)
        v.non_mult_memb_states <<- v.all.states[!(v.all.states %in% v.mult_memb_states)]
        v.exclude <<- EX
        
        df.tw.lower <<- df.tw.lower[df.tw.lower$abb %in% v.all.states,]
        df.tw.upper <<- df.tw.upper[df.tw.upper$abb %in% v.all.states,]
        
        df.gazette <<- read.csv(f.gazette, stringsAsFactors=FALSE, header=TRUE)
        df.gazette$sm_name_2 <<- paste(df.gazette$st, df.gazette$sm_name_1, sep=":")
        
        ## Column specifications for the Shor McCarty table
        n.hc1.init <<- 37
        n.hc2.init <<- 79
        n.sc1.init <<- 16
        n.sc2.init <<- 58
}
read.and.initialize.2 <- function() {
        
        ####################################################################################################
        ## 2:  READ CENSUS POPULATION DATA 
        ## Reference information is contained in State Leg Population 2010.xls
        ####################################################################################################
        
        df.fips <<- read.csv("fips.csv")
        df.pop2000.lower <<- read.csv("pop2000.csv",stringsAsFactors=FALSE)      ## total population by state for 2000
        df.pop2000.upper <<- df.pop2000.lower                ## clone the pop.2000 file and use for upper houses
        df.sld2010 <<- read.csv("lower2010.csv",stringsAsFactors=FALSE)    ## population by lower house district 2010 ACS
        df.sld2010$sld_fips <<- paste(df.sld2010$fips,df.sld2010$district,sep="")
        df.ssd2010 <<- read.csv("upper2010.csv",stringsAsFactors=FALSE) #  # population by upper house district 2010 ACS
        df.ssd2010$ssd_fips <<- paste(df.ssd2010$fips,df.ssd2010$district,sep="")
        
        
        ## determine number of districts per state and use as baseline in population growth percentage calculation
        ## house
        
        ## lower houses
        df.sld2010$index <<- 1
        v.st_sum <<- tapply(df.sld2010$pop2010,df.sld2010$stcd,sum)
        df.distnums.lower <<- as.data.frame(tapply(df.sld2010$index,df.sld2010$stcd,sum)); names(df.distnums.lower) <<- "distnums"
        df.distnums.lower$stcd <<- rownames(df.distnums.lower)
        
        df.pop2000.lower <<- merge(df.distnums.lower,df.pop2000.lower,by="stcd",all.x=TRUE)
        df.pop2000.lower$pd <<- round(df.pop2000.lower$pop2000 / df.pop2000.lower$distnums,0)  ## 2000 population per lower house district
        
        df.sld2010 <<- merge(df.sld2010,df.pop2000.lower,by="stcd",all.x=TRUE)
        df.sld2010$gpct <<- (df.sld2010$pop2010 - df.sld2010$pd) / df.sld2010$pd
        df.sld2010$st_hd <<- paste(df.sld2010$stcd,df.sld2010$district,sep="")
        df.sld2010$st_hd <<- paste(df.sld2010$stcd,df.sld2010$district,sep=":")
        # ## these states have issues with district mapping.  Eliminate for now.
        
        df.sld2010EX <<- df.sld2010[df.sld2010$stcd %in% v.exclude,]
        df.sld2010 <<- df.sld2010[!df.sld2010$stcd %in% v.exclude,]
        df.sld2010 <<- df.sld2010[df.sld2010$stcd %in% v.all.states,c("st_hd","gpct")]
        
        ## upper houses
        df.ssd2010$index <<- 1
        v.st_sum <<- tapply(df.ssd2010$pop2010,df.ssd2010$stcd,sum)
        df.distnums.upper <<- as.data.frame(tapply(df.ssd2010$index,df.ssd2010$stcd,sum)); names(df.distnums.upper) <<- "distnums"
        df.distnums.upper$stcd <<- rownames(df.distnums.upper)
        
        df.pop2000.upper <<- merge(df.distnums.upper,df.pop2000.upper,by="stcd",all.x=TRUE)
        df.pop2000.upper$pd <<- round(df.pop2000.upper$pop2000 / df.pop2000.upper$distnums,0)
        
        df.ssd2010 <<- merge(df.ssd2010,df.pop2000.upper,by="stcd",all.x=TRUE)
        df.ssd2010$gpct <<- (df.ssd2010$pop2010 - df.ssd2010$pd) / df.ssd2010$pd
        df.ssd2010$st_sd <<- paste(df.ssd2010$stcd,df.ssd2010$district,sep=":")
        
        
        # ## these states have issues with district mapping.  Eliminate for now.
        df.ssd2010EX <<- df.ssd2010[df.ssd2010$stcd %in% v.exclude,]
        df.ssd2010 <<- df.ssd2010[!df.ssd2010$stcd %in% v.exclude,]
        df.ssd2010 <<- df.ssd2010[df.ssd2010$stcd %in% v.all.states,c("st_sd","gpct")]

}
load.legislator.scores <- function() {
        
        ####################################################################################################
        ## 3:  CREATE LEGISLATOR SCORES FROM SHOR & MCCARTY
        ## Downloaded from Harvard Dataverse:  
        ## Shor, Boris; McCarty, Nolan, 2014, "Individual State Legislator Shor-McCarty Ideology Data, July 2014 update"
        ## http://goo.gl/qPGnqn
        ####################################################################################################
        
        load("state legislator scores july 2014.RData") ## data appears in df "x"
        df.leg.scores <- x
        rm(x)
        df.leg.scores.EX <<- df.leg.scores[df.leg.scores$st %in% v.exclude,]
        df.leg.scores <<- df.leg.scores[!df.leg.scores$st %in% v.exclude,]
}
generate.consolidated.extracts <- function(v.ought) {
        
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
       
        ## CASE 1:  Washington State
        ## Data intermittently includes a "-1" or "-2" at the end of district id, but not always
        df.house.20xx[df.house.20xx$st=="WA",s.currhdcol] <- substr(df.house.20xx[df.house.20xx$st=="WA",s.currhdcol],1,3)
        df.senate.20xx[df.senate.20xx$st=="WA",s.currsdcol] <- substr(df.senate.20xx[df.senate.20xx$st=="WA",s.currsdcol],1,3)
        
        ## CASE 2:  South Dakota
        ## Data includes a "-1" or "-2" for districts 26 and 28
        ## We will consolidate them
        df.house.20xx[df.house.20xx$st=="SD",s.currhdcol] <- substr(df.house.20xx[df.house.20xx$st=="SD",s.currhdcol],1,3)
        df.senate.20xx[df.senate.20xx$st=="SD",s.currsdcol] <- substr(df.senate.20xx[df.senate.20xx$st=="SD",s.currsdcol],1,3)
        df.sld2010[df.sld2010$st_hd =="SD26A","st_hd"] <- "SD026"
        df.sld2010[df.sld2010$st_hd =="SD28A","st_hd"] <- "SD028"
        
        ## CASE 3:  Idaho
        ## Data intermittently includes a "-1" or "-2" at the end of district id, but not always
        df.house.20xx[df.house.20xx$st=="ID",s.currhdcol] <- substr(df.house.20xx[df.house.20xx$st=="ID",s.currhdcol],1,3)
        df.senate.20xx[df.senate.20xx$st=="ID",s.currsdcol] <- substr(df.senate.20xx[df.senate.20xx$st=="ID",s.currsdcol],1,3)
        
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
        df.house.20xx$st_hd <- paste(df.house.20xx$st,df.house.20xx[,"LD"],sep=":")
        v.house.20xx.tmp  <- tapply(df.house.20xx[,s.currhcol],df.house.20xx$st_hd,sum)
        df.house.20xx.tmp <- cbind.data.frame("st_hd"=names(v.house.20xx.tmp),"dnum" = as.numeric(v.house.20xx.tmp))
        df.house.20xx <- merge(df.house.20xx,df.house.20xx.tmp,by="st_hd",all.x=TRUE)
        rm(v.house.20xx.tmp,df.house.20xx.tmp)    
        
        ## validate number of senators from each district
        df.senate.20xx[,s.currscol] <- as.numeric(df.senate.20xx[,s.currscol])
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==1] <- paste("00",df.senate.20xx$LD[nchar(df.senate.20xx$LD)==1],sep="")
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==2] <- paste("0",df.senate.20xx$LD[nchar(df.senate.20xx$LD)==2],sep="")
        df.senate.20xx$st_sd <- paste(df.senate.20xx$st,df.senate.20xx[,"LD"],sep="")
        v.senate.20xx.tmp  <- tapply(df.senate.20xx[,s.currscol],df.senate.20xx$st_sd,sum)
        df.senate.20xx.tmp <- cbind.data.frame("st_sd"=names(v.senate.20xx.tmp),"dnum" = as.numeric(v.senate.20xx.tmp))
        df.senate.20xx <- merge(df.senate.20xx,df.senate.20xx.tmp,by="st_sd",all.x=TRUE)
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
                df.house.20xx.mm <- merge(df.house.20xx.mm,v.house.20xx.mean_np,by="st_hd",all.x=TRUE)
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
                df.senate.20xx.mm <- merge(df.senate.20xx.mm,v.senate.20xx.mean_np,by="st_sd",all.x=TRUE)
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
                 
        rm("df.house.20xx.mm","df.senate.20xx.mm","df.house.20xx.aa","df.senate.20xx.aa")
        if(exists("df.iter")) rm(df.iter)
        
        df.house.20xx$year <- n.oyear
        df.senate.20xx$year <- n.oyear

        names(df.house.20xx)[names(df.house.20xx)==s.currhcol] <- "hdistrict"
        names(df.senate.20xx)[names(df.senate.20xx)==s.currscol] <- "sdistrict"       
        v.drops <- c(s.currhcol, s.currscol, s.currhdcol, s.currsdcol, s.prevhcol, s.prevscol, s.prevhdcol, s.prevsdcol, 
                     "house2010","senate2010","np_mean")
        
        df.house.20xx <- df.house.20xx[,!(names(df.house.20xx) %in% v.drops)]
        df.senate.20xx <- df.senate.20xx[,!(names(df.senate.20xx) %in% v.drops)]
               
        ### Merge with growth data.  We are losing a lot of records at this step.
        
       
        df.house.20xx$ind <- df.house.20xx$party != "D" & df.house.20xx$party != "R"       
        df.house.20xx$party[df.house.20xx$ind==TRUE & df.house.20xx$np_score >= 0] <- "R"
        df.house.20xx$party[df.house.20xx$ind & df.house.20xx$np_score < 0] <- "D" 
       
        if(n.oyear >= 2009) {
                v.mn1 <- df.gazette[df.gazette$st=="MN",c("sm_name_1")]
                v.mn1 <- substr(v.mn1,2,nchar(v.mn1))
                df.gazette[df.gazette$st=="MN",c("sm_name_1")] <- v.mn1
        }
        
        df.house.20xx <- merge(df.house.20xx,df.gazette,by.x="st_hd",by.y="sm_name_2",all.x=TRUE)
        df.house.20xx <- merge(df.house.20xx,df.sld2010,by="st_hd",all.x=TRUE)
        
               
        df.senate.20xx$ind <- df.senate.20xx$party != "D" & df.senate.20xx$party != "R"       
        df.senate.20xx$party[df.senate.20xx$ind==TRUE & df.senate.20xx$np_score >= 0] <- "R"
        df.senate.20xx$party[df.senate.20xx$ind & df.senate.20xx$np_score < 0] <- "D"        
        df.senate.20xx <- merge(df.senate.20xx,df.gazette,by.x="st_sd",by.y="sm_name_2",all.x=TRUE)
        df.senate.20xx <- merge(df.senate.20xx,df.ssd2010,by.x="st_sd",by.y="st_sd",all.x=TRUE)
       
        m.house.party <- table(df.house.20xx$st_hd,df.house.20xx$party)
        m.senate.party <- table(df.senate.20xx$st_sd,df.senate.20xx$party)
        
        m.house.party <- as.data.frame(cbind(st_hd = rownames(m.house.party),m.house.party))
        m.house.party$blend <- as.numeric(m.house.party$D)>=0 & as.numeric(m.house.party$R)>=0       
        df.house.20xx <- merge(df.house.20xx,m.house.party,by="st_hd"); df.house.20xx$D <- NULL; df.house.20xx$R <- NULL
        
        ################# ASSUME SENATE DISTRICTS NEVER BLEND ######################
        df.senate.20xx$blend <- FALSE
        ######################################################################
        
        
        
        df.tw.lower$st_hd <- paste(df.tw.lower$abb,sprintf("%03d",df.tw.lower$ssd_fips_num %% 1000),sep=":")
        df.mn <- df.tw.lower[df.tw.lower$abb=="MN",c("ssd_fips","st_hd")]
        if(n.oyear >= 2009) {
                df.mn$st_hd <- paste("MN:",substr(df.mn$ssd_fips,nchar(df.mn$ssd_fips)-2,nchar(df.mn$ssd_fips)),sep="")
        } else {
                df.mn$st_hd <- paste("MN:0",substr(df.mn$ssd_fips,nchar(df.mn$ssd_fips)-2,nchar(df.mn$ssd_fips)),sep="") 
        }
        
        df.tw.lower[df.tw.lower$abb=="MN","st_hd"] <- df.mn$st_hd
        
        #df.tw.lower[df.tw.lower$abb=="MN"]$st_hd <- paste(df.tw.lower[df.tw.lower$abb=="MN"]$st_hd,substr(, nchar(x)-n+1, nchar(x))
        #df.tw.lower <- merge(df.tw.lower, df.gazette, by.x="st_hd",by.y="sm_name_2",all.x=TRUE)
        
        ## Create a lookup field for the gazette
        df.house.20xx <-merge(df.house.20xx,df.tw.lower,by.x="st_hd",by.y="st_hd",all.x=TRUE)  
        df.house.20xx$pres_2008 <- as.numeric(df.house.20xx$pres_2008)
        df.house.20xx$mrp_estimate <- as.numeric(df.house.20xx$mrp_estimate)
        df.house.20xx$mrp_se <- as.numeric(df.house.20xx$mrp_se)
        
        df.tw.upper$st_sd <- paste(df.tw.upper$abb,sprintf("%03d",df.tw.upper$ssd_fips_num %% 1000),sep="")
        df.tw.upper <- df.tw.upper  ## Need to replicate gazette process for house
        df.tw.upper[df.tw.upper$abb=="AK",]$st_sd <- paste("AK00",str_sub(df.tw.upper[df.tw.upper$abb=="AK",]$ssd_df.fips,-1),sep="")
        df.senate.20xx <- merge(df.senate.20xx,df.tw.upper,by.x="st_sd",by.y="st_sd",all.x=TRUE)  ## NOT MERGING WITH GAZETTE!!!! (12/16)
             
        if (v.ought==1) df.house.2000s <- df.house.20xx else df.house.2000s <- rbind(df.house.2000s, df.house.20xx)
        if (v.ought==1) df.senate.2000s <- df.senate.20xx else df.senate.2000s <- rbind(df.senate.2000s, df.senate.20xx)
        df.house.2000s <<- df.house.2000s
        df.senate.2000s <<- df.senate.2000s
}       
read.and.initialize.1("TW_lower.csv","TW_upper.csv","2013_Gaz_sldl_national.csv")
read.and.initialize.2()

load.legislator.scores()
for (year.inp in 1:10) generate.consolidated.extracts(year.inp)

save(df.senate.2000s, df.house.2000s,v.mult_memb_states, v.all.states, v.non_mult_memb_states, file= "alldata.RData")

#######################################################################
del <- df.house.2000s[is.na(df.house.2000s$mrp_estimate),]
View(df.house.2000s[df.house.2000s$st.x=="MN",])

