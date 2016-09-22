###################upshot replication
###This replicates the published survey weights. It is substantively identical 
###I can't rule out the possibility of very tiny differences. The actual survey was weighted by Siena; there could be a different algorithm or substantively irrelevant (<.1 pct) diferences in population parameters. 


needs(dplyr, survey)

# setwd("~/dropbox/siena-poll/github")
ncdata <- read.csv("nc-nyt-siena-0922.csv", stringsAsFactors=F)

svydata <- svydesign(id=~1, data=ncdata)

#population parameters from L2 active voters
age.dist <- data.frame(age4=c("1","2","3","4"), Freq=c(.175,.208,.371,.246)*nrow(ncdata))
region.dist <- data.frame(regionw=c("Western","Southwest","Piedmont-Triad","North Central","East/ South Central"), Freq=c(.138,.218,.166,.246,.232)*nrow(ncdata))
race.dist <- data.frame(race.st=c("Black","Hispanic","Other","White"), Freq=c(.218,.021,.057,.704)*nrow(ncdata))
sex.dist <- data.frame(gender=c("F","M"), Freq=c(.545,.455)*nrow(ncdata))
party.dist <- data.frame(partyw=c("Democratic","Republican","Other"), Freq=c(.4,.316,.284)*nrow(ncdata))
turnout.dist <- data.frame(turnout3=c("Low","Middle","High"), Freq=c(.107,.303,.59)*nrow(ncdata))

raked <- rake(svydata, sample.margins=list(~age4, ~regionw, ~partyw, ~race.st, ~gender, ~turnout3), 
              population.margins=list(age.dist, region.dist, party.dist, race.dist, sex.dist, turnout.dist))

replicated <- ncdata %>% 
  mutate(weight.rv.replicated = weights(raked),
         lv.screen.replicated = (likelyyn + turnout.score)/2,
         weight.lv.replicated = weight.rv.replicated * lv.screen.replicated)

###replication v. actual
plot(weight.rv.replicated~rv.weight, replicated)
plot(weight.lv.replicated~lv.weight, replicated)

