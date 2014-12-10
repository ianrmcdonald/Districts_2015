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

df.tw.lower <- read.csv(paste(district2015_dir,"TW_lower.csv",sep=""),header=TRUE)
df.tw.upper <- read.csv(paste(district2015_dir,"TW_upper.csv",sep=""),header=TRUE)

v.mult_memb_states<- as.factor(c("AZ","NJ","SD","WA"))  ## multi member district states
v.all.states <- unique(df.tw.lower$abb)
v.non_mult_memb_states <- v.all.states[!(v.all.states %in% v.mult_memb_states)]

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

n.oughts <- as.character(c(2003:2012))


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
        n.prevyear <- n.oyear - 2
        
        s.currhcol <- paste("house",n.oyear,sep="")
        s.prevhcol <- paste("house",n.prevyear,sep="")
        s.currhdcol <- paste("hdistrict",n.oyear,sep="")
        s.prevhdcol <- paste("hdistrict",n.prevyear,sep="")

        s.currscol <- paste("senate",n.oyear,sep="")
        s.prevscol <- paste("senate",n.prevyear-2,sep="")
        s.currsdcol <- paste("sdistrict",n.oyear,sep="")
        s.prevsdcol <- paste("sdistrict",n.prevyear,sep="")
        
        ## select relevant columns  filter out any potential NA's
        df.house.20xx <- df.leg.scores[df.leg.scores[[s.currhdcol]] != "NA", c(1:5,n.hc1,n.hc2,n.hc2-2)]
        df.senate.20xx <- df.leg.scores[df.leg.scores[[s.currsdcol]] != "NA", c(1:5,n.sc1,n.sc2,n.sc2-4)]
       
        ## clean up spaces in s.currhcol and s.currscol
        df.house.20xx[,s.currhcol] <- substr(df.house.20xx[,s.currhcol],1,3)
        df.senate.20xx[,s.currscol] <- substr(df.senate.20xx[,s.currscol],1,3)
        
        df.house.20xx$first_term <- is.na(df.house.20xx[[s.prevhcol]])
        df.senate.20xx$first_term <- is.na(df.senate.20xx[[s.prevscol]])
              
        ## validate states against v.all.states vector, to eliminate DC, Guam, etc.
        df.house.20xx <- df.house.20xx[df.house.20xx$st %in% v.all.states,]
        df.senate.20xx <- df.senate.20xx[df.senate.20xx$st %in% v.all.states,]
        
        df.house.20xx$LD <- df.house.20xx[[s.currhdcol]]
        df.senate.20xx$LD <- df.senate.20xx[[s.currsdcol]]
        
        ## validate number of reps from each district
        df.house.20xx[,s.currhcol] <- as.numeric(df.house.20xx[,s.currhcol])
        df.house.20xx$LD[nchar(df.house.20xx$LD)==1] <- paste("00",df.house.20xx$LD,sep="")
        df.house.20xx$LD[nchar(df.house.20xx$LD)==2] <- paste("0",df.house.20xx$LD,sep="")
        df.house.20xx$st_hd <- paste(df.house.20xx$st,df.house.20xx[,"LD"],sep="")
        v.house.20xx.tmp <- as.data.frame(tapply(df.house.20xx[,s.currhcol],df.house.20xx$st_hd,sum))
        
        ## validate number of senators from each district
        df.senate.20xx[,s.currscol] <- as.numeric(df.senate.20xx[,s.currscol])
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==1] <- paste("00",df.senate.20xx$LD,sep="")
        df.senate.20xx$LD[nchar(df.senate.20xx$LD)==2] <- paste("0",df.senate.20xx$LD,sep="")
        df.senate.20xx$st_hd <- paste(df.senate.20xx$st,df.senate.20xx[,"LD"],sep="")
        v.senate.20xx.tmp <- as.data.frame(tapply(df.senate.20xx[,s.currscol],df.senate.20xx$st_hd,sum))
        
        
        
        
        ############# Stop here 5:33 12/9
        names(rep) <- "LD"; rep$st_hd <- row.names(rep)
        house.20xx.mm <- merge(house.20xx.mm,rep,by.x="st_hd",by.y="st_hd",all.x=TRUE)
        names(sen) <- "LD"; sen$st_sd <- row.names(sen)
        senate.20xx.mm <- merge(senate.20xx.mm,sen,by.x="st_sd",by.y="st_sd",all.x=TRUE)
        
      
        ## Eliminate house districts that aren't mm.
        if (FILTER.MM) house.20xx.mm <- house.20xx.mm[house.20xx.mm$LD.y > 1,]
        
        ##  handle records for more than two reps per district or one senator per district
        ##  routine can do one of two things:
        ##  1) keeps only the reps with the two highest st_id values, presumably the last two
        ##  to serve.  should work for any number N reps.
        ##  2) keep that np_score or compute the mean.
        
        ######  TESTING to keep all recs
        house.20xx.n2 <- house.20xx.mm[house.20xx.mm$LD.y > 2,]
        senate.20xx.n2 <- senate.20xx.mm[senate.20xx.mm$LD.y > 1,]
        
        if(nrow(house.20xx.n2 > 0)) {
                n2dists <- unique(house.20xx.n2$st_hd)
                house.20xx.mean_np <- aggregate(np_score ~ st_hd, house.20xx.n2, mean)
                house.20xx.n2 <- merge(house.20xx.n2,house.20xx.mean_np,by="st_hd")
                house.20xx.n2 <- rename(house.20xx.n2, c("np_score.x"="np_score", "np_score.y"="np_mean"))

                house.20xx.n2 <- house.20xx.n2[order(house.20xx.n2$st_hd,house.20xx.n2$st_id,decreasing=TRUE),]
                for (i in 1:length(n2dists)) {
                        iter <- house.20xx.n2[house.20xx.n2$st_hd==n2dists[i],]
                        iter.st_id <- iter$st_id[c(1:2)]
                        house.20xx.n2 <- house.20xx.n2[house.20xx.n2$st_hd != n2dists[i] | 
                                house.20xx.n2$st_id %in% iter.st_id,]
                }
                house.20xx.mm <- house.20xx.mm[!house.20xx.mm$st_hd %in% n2dists,]
                house.20xx.mm$np_mean <- house.20xx.mm$np_score
                house.20xx.mm <- rbind(house.20xx.mm,house.20xx.n2)
        }
        
        if(nrow(senate.20xx.n2 > 0)) {
                n2dists <- unique(senate.20xx.n2$st_sd)
                senate.20xx.mean_np <- aggregate(np_score ~ st_sd, senate.20xx.n2, mean)
                senate.20xx.n2 <- merge(senate.20xx.n2,senate.20xx.mean_np,by="st_sd")
                senate.20xx.n2 <- rename(senate.20xx.n2, c("np_score.x"="np_score", "np_score.y"="np_mean"))
                
                senate.20xx.n2 <- senate.20xx.n2[order(senate.20xx.n2$st_sd,senate.20xx.n2$st_id,decreasing=TRUE),]
                for (i in 1:length(n2dists)) {
                        iter <- senate.20xx.n2[senate.20xx.n2$st_sd==n2dists[i],]
                        iter.st_id <- iter$st_id[c(1:1)]
                        senate.20xx.n2 <- senate.20xx.n2[senate.20xx.n2$st_sd != n2dists[i] | 
                                                               senate.20xx.n2$st_id %in% iter.st_id,]
                }
                senate.20xx.mm <- senate.20xx.mm[!senate.20xx.mm$st_sd %in% n2dists,]
                senate.20xx.mm$np_mean <- senate.20xx.mm$np_score
                senate.20xx.mm <- rbind(senate.20xx.mm,senate.20xx.n2)
        }
        
        rm(house.20xx.n2)
        rm(senate.20xx.n2)
        house.20xx.mm$year <- oyear
        senate.20xx.mm$year <- oyear

        house.20xx.mm <- rename(house.20xx.mm, c(coldist="hdistrict"))
        senate.20xx.mm <- rename(senate.20xx.mm, c(scoldist="sdistrict"))
        
        drops <- c("LD.x","LD.y",col,scol,pcol,pscol)
        house.20xx.mm <- house.20xx.mm[,!(names(house.20xx.mm) %in% drops)]
        senate.20xx.mm <- senate.20xx.mm[,!(names(senate.20xx.mm) %in% drops)]
        
        ## Find and mark blended house districts.
        house.20xx.mm$ind <- house.20xx.mm$party != "D" & house.20xx.mm$party != "R"
        
        house.20xx.mm$party[house.20xx.mm$ind==TRUE & house.20xx.mm$np_score >= 0] <- "R"
        house.20xx.mm$party[house.20xx.mm$ind & house.20xx.mm$np_score < 0] <- "D"
        
        house.20xx.mm <- merge(house.20xx.mm,df.sld2010,by="st_hd")
        
        senate.20xx.mm$ind <- senate.20xx.mm$party != "D" & senate.20xx.mm$party != "R"
       
        senate.20xx.mm$party[senate.20xx.mm$ind==TRUE & senate.20xx.mm$np_score >= 0] <- "R"
        senate.20xx.mm$party[senate.20xx.mm$ind & senate.20xx.mm$np_score < 0] <- "D"
        
        senate.20xx.mm <- merge(senate.20xx.mm,df.sld2010,by.x="st_sd",by.y="st_hd")
        
        
        house.20xx.mm$ind <- house.20xx.mm$party != "D" & house.20xx.mm$party != "R"
        house.20xx.mm$party[house.20xx.mm$ind==TRUE & house.20xx.mm$np_score >= 0] <- "R"
        house.20xx.mm$party[house.20xx.mm$ind & house.20xx.mm$np_score < 0] <- "D"
        senate.20xx.mm$ind <- senate.20xx.mm$party != "D" & senate.20xx.mm$party != "R"
        senate.20xx.mm$party[senate.20xx.mm$ind==TRUE & senate.20xx.mm$np_score >= 0] <- "R"
        senate.20xx.mm$party[senate.20xx.mm$ind & senate.20xx.mm$np_score < 0] <- "D"
        
       
        house.party <- table(house.20xx.mm$st_hd,house.20xx.mm$party)
        senate.party <- table(senate.20xx.mm$st_sd,senate.20xx.mm$party)
        
        hh <- as.data.frame(cbind(st_hd = rownames(house.party),house.party))
        hh$blend <- hh$D==1 & hh$R==1       
        house.20xx.mm <- merge(house.20xx.mm,hh,by.x="st_hd",by.y="st_hd")
        
        
        
        ss <- as.data.frame(cbind(st_sd = rownames(senate.party),senate.party))
        ss$blend <- ss$D==1 & ss$R==1       
        senate.20xx.mm <- merge(senate.20xx.mm,ss,by.x="st_sd",by.y="st_sd")
      
        tw.lower$st_hd <- paste(tw.lower$abb,sprintf("%03d",tw.lower$ssd_df.fips_num %% 1000),sep="")
        house.20xx.mm <- merge(house.20xx.mm,tw.lower,by.x="st_hd",by.y="st_hd")
        house.20xx.mm$pres_2008 <- as.numeric(house.20xx.mm$pres_2008)
        house.20xx.mm$mrp_estimate <- as.numeric(house.20xx.mm$mrp_estimate)
        house.20xx.mm$mrp_se <- as.numeric(house.20xx.mm$mrp_se)
        house.20xx.mm[,coldist] <- NULL
        
        tw.upper$st_sd <- paste(tw.upper$abb,sprintf("%03d",tw.upper$ssd_df.fips_num %% 1000),sep="")
        tw.upper[tw.upper$abb=="AK",]$st_sd <- paste("AK00",str_sub(tw.upper[tw.upper$abb=="AK",]$ssd_df.fips,-1),sep="")
        senate.20xx.mm <- merge(senate.20xx.mm,tw.upper,by.x="st_sd",by.y="st_sd")
        senate.20xx.mm$pres_2008 <- as.numeric(senate.20xx.mm$pres_2008)
        senate.20xx.mm$mrp_estimate <- as.numeric(senate.20xx.mm$mrp_estimate)
        senate.20xx.mm$mrp_se <- as.numeric(senate.20xx.mm$mrp_se)
        senate.20xx.mm[,scoldist] <- NULL
        
        if (ought==1) house.2000s.mm <- house.20xx.mm else house.2000s.mm <- rbind(house.2000s.mm, house.20xx.mm)
        if (ought==1) senate.2000s.mm <- senate.20xx.mm else senate.2000s.mm <- rbind(senate.2000s.mm, senate.20xx.mm)
        
}       


save(senate.2000s.mm, house.2000s.mm,v.mult_memb_states, v.all.states, v.non_mult_memb_states, file= "alldata.RData")

#######################################################################


#######################################################################
rm(list=ls())

load("alldata.RData")
house <- house.2000s.mm; rm(house.2000s.mm); senate <- senate.2000s.mm; rm(senate.2000s.mm)
house$party <- as.factor(house$party)
house$party <- factor(house$party, levels = c("R","D"))
cutoff <- .1
house$g2[house$gpct >= cutoff] <- 1
house$g2[house$gpct < cutoff] <- 0
house$pindex <- c()
house$pindex[house$party=="R"] <- 1
house$pindex[house$party=="D"] <- -1

MMONLY <- FALSE
if (MMONLY) {
        house <- house[house$st %in% v.mult_memb_states,]
        senate <- senate[senate$st %in% v.mult_memb_states,]
}

house.yr <- split(house,house$year)    ## creates list with individual years    
senate.yr <- split(senate,senate$year)       
      
## create fields used for graphic displays
size <- 1; varexp <- 1
house$datacolor <- rep("light blue",length(house$party))
house$datacolor[house$party == "R"] <- "pink"
house$datacolor[house$party == "R" & house$blend==TRUE] <- "red"
house$datacolor[house$party == "D" & house$blend==TRUE] <- "blue"
house$d.size <- size*house$gpct^varexp*10
        
house.party <- split(house,house$party)
house.R.year <- split(house.party[["R"]],house.party[["R"]]$year)
house.D.year <- split(house.party[["D"]],house.party[["D"]]$year)
senate.party <- split(senate,senate$party)
senate.R.year <- split(senate.party[["R"]],senate.party[["R"]]$year)
senate.D.year <- split(senate.party[["D"]],senate.party[["D"]]$year)



##  1.  Descriptive statistics on intradecade population growth
##  a.  Histogram and density of growth for all house and senate districts



hy <- house.yr[["2012"]]
hy$party <- as.factor(hy$party)
hy$party <- factor(hy$party, levels = c("R","D"))

h3 <- house.yr[["2003"]]
h3$party <- as.factor(h3$party)
h3$party <- factor(h3$party, levels = c("R","D"))

m <- ggplot(hy, aes(x = gpct, colour=party)) + geom_density(aes(fill = party), position = "fill") +
        xlim(quantile(hy$gpct,.1),quantile(hy$gpct,.9))

m + scale_fill_manual(values = c(D = "blue", R = "red")) + geom_rug(col="black",alpha=.1,sides="b") + 
        xlab("District Growth 2000-2010") + ylab("Percentage of Districts") + ggtitle("Distribution of State Legislative Disrict Population Growth")


sy <- senate.yr[["2012"]]
sy$party <- as.factor(sy$party)
sy$party <- factor(sy$party, levels = c("R","D"))

#####################
## Graphs
#####################

m <- ggplot(sy, aes(x = gpct, colour=party)) + geom_density(aes(fill = party), position = "fill") +
        xlim(quantile(sy$gpct,.05),quantile(sy$gpct,.95))

m + scale_fill_manual(values = c(D = "blue", R = "red")) + geom_rug(col="black",alpha=.1,sides="b") + 
        xlab("District Growth 2000-2010") + ylab("Percentage of Districts") + ggtitle("Distribution of State Senate Disrict Population Growth")


(n <- ggplot(hy, aes(party,gpct)) + geom_boxplot() + ylim(quantile(hy$gpct,.1),quantile(hy$gpct,.9)) + ggtitle("Distribution of Growth by Party"))
(n <- ggplot(sy, aes(party,gpct)) + geom_boxplot() + ylim(quantile(sy$gpct,.1),quantile(sy$gpct,.9)) + ggtitle("Distribution of Growth by Party"))

## Basic descriptive scatterplot
o <- ggplot(hy, aes(mrp_estimate, np_score, colour=party)) + geom_point() + geom_smooth(method=lm)
o 


STINP <- "NJ"
hx <- house[house$st==STINP & house$year == "2004",]
hx$psize <- c()
hx$psize[hx$blend == 1] <- 7
hx$psize[hx$blend == 0] <- 3

hx$dnum <- substr(hx$st_hd,4,5)
hx$dnum[hx$blend == 0] <- ""

o <- ggplot(hx, aes(pres_2008, np_score, colour=party,size=psize)) + geom_point() + geom_smooth(method=lm)
o #+ ylim(-3,3) + xlim(-1.5,1.5)
o + annotate("text", x = hx$pres_2008, y = hx$np_score, label = hx$dnum, size=5)
#o + ylim(-3,3) + xlim(-1.5,1.5) + facet_grid(st ~ .)

o <- ggplot(house[house$st=="MN",], aes(gpct, np_score, colour=party)) + geom_point() + geom_smooth(method=lm)
o + ylim(-2,2) + xlim(-.3,.6)



o <- ggplot(sy, aes(mrp_estimate, np_score, colour=party)) + geom_point() + geom_smooth(method=lm)
o
















lines(density(house.R.year[["2012"]]$gpct))
# boxplot(np_score ~ party*st,data=house,col=c(rep("white",4),rep("lightgray",4)))
# boxplot(np_score ~ party*st,data=house.yr[["2012"]],col=c(rep("white",2),rep("lightgray",2)))
hist(house.R.year[["2012"]]$gpct,breaks=25)
## create a two-way proportional graph.



q <- glm(house$np_score ~ house$mrp_estimate + house$gpct * house$pindex); summary(q)
q <- lmer(np_score ~ pindex * gpct + mrp_estimate + (1|st) + (1|year),data=house,REML=FALSE); summary(q)
q <- lmer(np_score ~ pindex * gpct + mrp_estimate + (1|st) ,data=house[house$year=="2012",],REML=FALSE); summary(q)

##  by year increase in pindex:gpct


q <- lmer(np_score ~ gpct + mrp_estimate + (1|st) + (1|year),data=house.party[["R"]],REML=FALSE); summary(q)
q <- lmer(np_score ~ g2 + mrp_estimate + (1|st) + (1|year),data=house.party[["R"]],REML=FALSE); summary(q)


q <- lmer(np_score ~ gpct + mrp_estimate + (1|st),data=house[house$party=="D",]); summary(q)

hall$pindex <- c()
hall$pindex[hall$party=="R"] <- 1
hall$pindex[hall$party=="D"] <- -1
hmm$pindex <- c()
hmm$pindex[hmm$party=="R"] <- 1
hmm$pindex[hmm$party=="D"] <- -1
s <- lmer(np_score ~ pindex * gpct + mrp_estimate + (1|st) + (1|year) + (1|first_term),data=house); summary(s)
s <- lmer(np_score ~ party + gpct + mrp_estimate + (1|st) + (1|first_term),data=hmm[hmm$year==2010,]); summary(s)
s <- lmer(np_score ~ gpct + mrp_estimate + (1|st),data=hall[hall$party=="D" & hall$year==2012,]); summary(s)
s <- lmer(np_score ~ gpct + mrp_estimate + (1|st),data=hmm[hmm$party=="D",]); summary(s)
s <- lmer(np_score ~ party + mrp_estimate + mrp_se + (1|st) + (1|first_term),data=hall); summary(s)
s <- lmer(np_score ~ party + g2 + mrp_estimate + mrp_se + (1|st) + (1|year) + (1|first_term),data=hall); summary(s)
s <- lmer(np_score ~ pindex + gpct + mrp_estimate + mrp_se + (1|st) + (1|year) + first_term,data=hall); summary(s)
s <- lmer(np_score ~ gpct + blend + party + mrp_estimate + (1|year),data=hmm[hmm$st=="WA",]); summary(s)
s <- lmer(np_score ~ gpct + blend + mrp_estimate + (1|year),data=hmm[hmm$party=="R" & hmm$st=="WA",]); summary(s)
s <- lmer(np_score ~ gpct + blend + mrp_estimate + (1|year),data=hmm[hmm$party=="D" & hmm$st=="WA",]); summary(s)
### tweak this 12/2/14
s <- lmer(np_score ~ mrp_estimate + pindex * gpct + pindex * mrp_se + (1|st),data=hall,REML=FALSE); summary(s)
s <- lmer(np_score ~ mrp_estimate + gpct + mrp_se + (1|st),data=hall[hmm$party=="R",],REML=FALSE); summary(s)
s <- lmer(np_score ~ mrp_estimate + gpct + mrp_se + (1|st),data=hall[hmm$party=="D",],REML=FALSE); summary(s)


q <- lmer(np_score ~ party + pres_2008 + (1|st), data=house,REML=FALSE); summary(q)
q <- lmer(np_score ~ party*pres_2008 + (1+ pres_2008*party|st), data=house,REML=FALSE); summary(q)
q.null <-lmer(np_score ~ gpct + (1+ pres_2008*party|st), data=house,REML=FALSE); summary(q.null)
anova(q.null,q)
hall.old <- hall
hall12 <- hall[hall$year==2012,]
summary(hall12$gpct[hall12$party=="R"])
summary(hall12$gpct[hall12$party=="D"])
hall$color <- c(); hall$color[hall$party=="R"]<-"red"; hall$color[hall$party=="D"]<-"blue"
hmm$color <- c(); hmm$color[hmm$party=="R"]<-"red"; hmm$color[hmm$party=="D"]<-"blue"

hmm12 <- hmm[hmm$year ==2012,]
hall12R <- hall12[hall12$party=="R",]
plot(hall12R$gpct, hall12R$np_score,xlim=c(-1,1),ylim=c(-1,2),col=hall12R$color)
plot(hmm12$gpct, hmm12$np_score,xlim=c(-.25,1),ylim=c(-2,2),col=hmm12$color)
abline(lsfit(hmm12$gpct[hmm12$party=="R"],hmm12$np_score[hmm12$party=="R"]))
abline(lsfit(hmm12$gpct[hmm12$party=="D"],hmm12$np_score[hmm12$party=="D"]))

plot(hall12$gpc, hall12$np_score,xlim=c(-1,2),col=hall12$color)

s <- lmer(np_score ~ mrp_estimate + pindex * gpct + (1|st),data=hall12,REML=FALSE); summary(s)
s <- lmer(np_score ~ mrp_estimate + gpct +(1|st),data=hall12R,REML=FALSE); summary(s)
hall12R$gpct2 <- hall12R$gpct^2
s <- lmer(np_score ~ mrp_estimate + gpct + gpct2 +(1|st),data=hall12R,REML=FALSE); summary(s)
s <- lmer(np_score ~ mrp_estimate + pindex * gpct + (1|st),data=hmm,REML=FALSE); summary(s)
s <- lmer(np_score ~ mrp_estimate + pindex * gpct + (1|st),data=hmm,REML=FALSE); summary(s)



plot(hall$mrp_se, hall$np_score,xlim=c(-1,2),col=hall$color)
boxplot(gpct ~ party,data=hall12)
     
# focus on blend

library(reshape2)
blendy <- houseperm[houseperm$blend,]
noblendy <- houseperm[!houseperm$blend,]
bbx <- dcast(noblendy, st_hd ~ party, mean, value.var = "np_score")
bb <- dcast(blendy, st_hd ~ party, mean, value.var = "np_score")
bbx$delta <- bbx$R - bbx$D
bb$delta <- bb$R - bb$D
boxplot(bb$delta,bbx$delta)
summary(bb)
summary(bbx)

mean(noblendy[noblendy$party=="R",]$np_score) - mean(noblendy[noblendy$party=="D",]$np_score)


### BAYES MODEL

library(R2jags)



np_score <- house$np_score
pres_2008 <- house$pres_2008
mrp_estimate <- house$mrp_estimate
gpct <- house$gpct #[i]
pindex <- house$pindex
pin.gpct <- gpct*pindex
st <- house$st
N <- length(house$np_score)


## figuring out bugs
set.seed(1280)
x<-runif(50)
y<-3+2*x+rnorm(50, mean=0, sd=2)
N<-length(x)
M<-length(unique(unit))

setwd("~/Dropbox/Work Projects/SPSA 2015/Source")

house <- house[house$year=="2012",]
#dat<- list("N" = N, "y" = np_score, "unit" = st, "x1" = mrp_estimate, "x2" = gpct, "x3" = pindex, "x4" = pin.gpct)
dat<- list("N" = N, "y" = np_score, "unit" = st, "x" = mrp_estimate)
dat<- list("N" = N, "y" = y, "unit" = y, "x" = x)
jags.inits <- function() {list (alpha = 0, beta = 0, tau.y = 1, tau.re = 0.5, ranef.v = rep(0, 20))}
#jags.inits <- function() {list (alpha = 0, beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, tau.y = 1, tau.re = 0.5, ranef.v = rep(0, 20))}
#parameters<- c("alpha", "beta1", "beta2","beta3","beta4","tau.y","tau.re")
parameters<- c("alpha", "beta","tau.y","tau.re")
# reg.jags<-jags.model(file="luck.bug", data=dat, inits=jags.inits, n.chains=1, n.adapt=1000)
# update(reg.jags, n.iter=1000) # burn in
# regression.sim<-coda.samples(reg.jags, variable.names=parameters, n.iter=15000)

re.mod.jags<-jags.model(file="luck.bug", data=dat, inits=jags.inits, n.chains=4, n.adapt=1000)
update(re.mod.jags, n.iter=1000) # burn in
re.sim<-coda.samples(re.mod.jags, variable.names=parameters, n.iter=20000, n.chain=4)

st <- c(rep(1,2000),rep(2,2008))
N<-length(x)
M<-length(unique(unit))
dat<- list("N" = N, "M" = M, "unit" = unit, "y" = y, "x" = x)
jags.inits <- function() {list (alpha = 0, beta = 0, tau.y = 1, tau.re = 0.5, ranef.v = rep(0, 20))}
parameters<- c("alpha", "beta", "tau.y", "tau.re")
re.mod.jags<-jags.model(file="luck.bug", data=dat, inits=jags.inits, n.chains=4, n.adapt=1000)
############################################
####  Hierarchical This worked.
df.fips <- read.csv("df.fips.csv")
df.fipsy <- match(house$st, df.fips$stcd)
st <- df.fipsy

(N<-length(np_score))
M <- 51
dat<- list("N" = N, "M" = M, "unit" = st, "y" = np_score, "x" = mrp_estimate)
jags.inits <- function() {list (alpha = 0, beta = 0, tau.y = 1, tau.re = 0.5, ranef.v = rep(0, 51))}
parameters<- c("alpha", "beta", "tau.y", "tau.re")
re.mod.jags<-jags.model(file="luck.bug", data=dat, inits=jags.inits, n.chains=4, n.adapt=1000)
geweke.diag(re.sim)
heidel.diag(re.sim)
raftery.diag(re.sim)

library(mcmcplots)
mcmcplot(re.sim, dir=getwd())

denplot(re.sim)
denplot(re.sim, collapse=T)
caterplot(re.sim, c("alpha", "beta"), val.lim=c(-1,6))
abline(v=0, lty=2)
############################################


geweke.diag(regression.sim)
heidel.diag(as.mcmc(regression.sim))
raftery.diag(as.mcmc(regression.sim))

summary(regression.sim)

library(mcmcplots)
mcmcplot(regression.sim, dir=getwd())

denplot(regression.sim)
caterplot(regression.sim, c("alpha", "beta"), val.lim=c(-1,6))
abline(v=0., lty=2)

