
#######################################################################
## DistrictOutput.R
## routines to generate models and graphs from district data
##
#######################################################################


library(ggplot2)
rm(list=ls())
dir_working <- "/Users/irm16/Dropbox/Work Projects/Districts 2015/Source/"
setwd(dir_working)
load("alldata.RData")
library(lme4)

# 1.  Read Consolidated District Data -------------------------------------

f_load_house <- function(df.input, i.cutoff=.1, MMONLY=FALSE, MMVEC=v.mult_memb_states) {

        df.input$party <- as.factor(df.input$party)
        df.input$party <- factor(df.input$party, levels = c("R","D"))

        df.input$growth_level[df.input$gpct >= i.cutoff] <- 1
        df.input$growth.level[df.input$gpct < i.cutoff] <- 0

        df.input$pindex <- c()
        df.input$pindex[df.input$party=="R"] <- 1
        df.input$pindex[df.input$party=="D"] <- -1

        if (MMONLY) {
                df.input <- df.input[df.input$st.x %in% MMVEC,]
        }
        return(df.input)
}

df.house <- f_load_house(df.house.2000s, MMONLY=FALSE) #; rm(df.house.2000s)
df.senate <- f_load_house(df.senate.2000s, MMONLY=TRUE) #; rm(df.senate.2000s)


# 2.  Create fields used for graphic displays -----------------------------
size <- 1; varexp <- 1
df.house$datacolor <- rep("light blue",length(df.house$party))
df.house$datacolor[df.house$party == "R"] <- "pink"
df.house$datacolor[df.house$party == "R" & df.house$blend==TRUE] <- "red"
df.house$datacolor[df.house$party == "D" & df.house$blend==TRUE] <- "blue"
df.house$d.size <- size*df.house$gpct^varexp*10

# 3.  Generate lists of df’s for each year, party, and party/year combination  ----

df.house.yr <- split(df.house,df.house$year)
df.senate.yr <- split(df.senate,df.senate$year)
df.house.party <- split(df.house,df.house$party)
df.house.R.year <- split(df.house.party[["R"]],df.house.party[["R"]]$year)
df.house.D.year <- split(df.house.party[["D"]],df.house.party[["D"]]$year)
df.senate.party <- split(df.senate,df.senate$party)
df.senate.R.year <- split(df.senate.party[["R"]],df.senate.party[["R"]]$year)
df.senate.D.year <- split(df.senate.party[["D"]],df.senate.party[["D"]]$year)



#  4.  Descriptive statistics on intradecade population growth ---------------------
##  a.  Histogram and density of growth for all house and senate districts

f.pick.year <- function(df.input, year) {
        df.output <- df.input[[year]]
        return(df.output)
}

hy <- f.pick.year(df.house.yr,"2012")
sy <- f.pick.year(df.senate.yr,"2012")


##  Conditional Distribution of growth by party (House) ----------------

m <- ggplot(hy, aes(x = gpct*100, colour=party)) +
        geom_density(aes(fill = party), position = "fill") +
        xlim(quantile(hy$gpct*100,.1,na.rm=TRUE),quantile(hy$gpct*100,.9,na.rm=TRUE))
        ## Note:  generates a warning message for removed rows

m + scale_fill_manual(values = c(D = "blue", R = "red")) +
        geom_rug(col="black",alpha=.1,sides="b") +
        xlab("District Growth % 2000-2010") +
        ylab("Percentage of Districts") +
        ggtitle("Distribution of State Legislative Disrict Population Growth")


##  Conditional Distribution of growth by party (Senate) ----------------

m <- ggplot(sy, aes(x = gpct*100, colour=party)) +
        geom_density(aes(fill = party), position = "fill") +
        xlim(quantile(sy$gpct*100,.1),quantile(sy$gpct*100,.9))

m + scale_fill_manual(values = c(D = "blue", R = "red")) +
        geom_rug(col="black",alpha=.1,sides="b") +
        xlab("District Growth % 2000-2010") +
        ylab("Percentage of Districts") +
        ggtitle("Distribution of State Senate Disrict Population Growth")

##  boxplot growth by party (House) ----------------

n <- ggplot(hy, aes(party,gpct)) +
        geom_boxplot() +
        ylim(quantile(hy$gpct,.1,na.rm=TRUE),quantile(hy$gpct,.9,na.rm=TRUE)) +
        ggtitle("Distribution of Growth by Party")

n

##  boxplot growth by party (Senate) ----------------

n <- ggplot(sy, aes(party,gpct)) +
        geom_boxplot() +
        ylim(quantile(hy$gpct,.1,na.rm=TRUE),quantile(hy$gpct,.9,na.rm=TRUE)) +
        ggtitle("Distribution of Growth by Party")

n

###############################################################################################

## Fig 4  Basic descriptive scatterplot
o <- ggplot(hy, aes(mrp_estimate, np_score, colour=party)) +
        geom_point() +
        geom_smooth(method=lm)

o

o <- ggplot(sy, aes(mrp_estimate, np_score, colour=party)) +
        geom_point() +
        geom_smooth(method=lm)

o

####################### Break work on 2/26/15 at 2:44 ####################################



STINP <- c("FL","NJ","NC")
hx <- df.house[df.house$st.x %in% STINP,]
hx$psize <- c()
hx$psize[hx$blend == 1] <- 7
hx$psize[hx$blend == 0] <- 3
 
hx$dnum <- substr(hx$st_hd,4,5)
hx$dnum[hx$blend == 0] <- ""

o <- ggplot(hy, aes(gpct, np_score, st.x, colour=party)) + 
        geom_point()  +
        geom_smooth(method=lm)

o <- ggplot(df.house[df.house$year==2003,], aes(gpct, np_score, colour=party)) + 
        geom_point() + 
        geom_smooth(method=lm)

o + facet_wrap( ~ st.x) + ylim(-2,2) + 
        xlim(-.3,.6)


o <- ggplot(sy, aes(mrp_estimate, np_score, colour=party)) + geom_point() + geom_smooth(method=lm)
o 





## Note:  Remember to do a null model
library(nlme)
model0 <- lme(fixed=np_score~1, random=~1|st.x, data=hy, method="ML")
q <- glm(np_score ~ mrp_estimate + gpct + pindex,data=hy); summary(q)
q <- glm(np_score ~ mrp_estimate + gpct * pindex,data=hy); summary(q)
q <- lmer(np_score ~ pindex * gpct + mrp_estimate + (1|st.x) + (1|year),data=house,REML=FALSE); summary(q)
q1 <- lmer(np_score ~ pindex * gpct + mrp_estimate + (1 + gpct|st.x),data=house,REML=FALSE); summary(q1)

nl <- lmer(np_score ~ pindex + mrp_estimate + (1|st.x) + (1|year),data=house,REML=FALSE); summary(nl)
q <- lmer(np_score ~ pindex * gpct + mrp_estimate + (1|st.x) ,data=house[house$year=="2012",],REML=FALSE); summary(q)

##  by year increase in pindex:gpct


q2 <- lmer(np_score ~ gpct + mrp_estimate + (1|st.x) + (1|year),data=house.party[["R"]],REML=FALSE); summary(q2)
q <- lmer(np_score ~ g2 + mrp_estimate + (1|st.x) + (1|year),data=house.party[["R"]],REML=FALSE); summary(q)


q <- lmer(np_score ~ gpct + mrp_estimate + (1|st.x),data=house[house$party=="D",]); summary(q)

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

