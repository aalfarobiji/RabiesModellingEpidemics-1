#
# This script analyses all measures reports (extracted from Ubelix as all_measuresReport.txt),
# saves the summaries per region and strategy and plots and saves boxplots
#

setwd("C:/Work/UniSydney/OzRab/analyse_results/measuresRep_results")
dat <- read.table("C:/Work/UniSydney/OzRab/Ubelix/Results_20140829/all_measuresReport_20140829.txt", h=T)

# add a column with the strategy name only
for (i in 1:nrow(dat)) {
 dat[i,"strategy"] <- strsplit(as.character(dat$scenario[i]),"/")[[1]][2] }

# define vectors of occurred started in NPA or Galiwinku
Gali_runs <- which(dat$R0_Galiwinku != 0)
NPA_runs <- which(dat$R0_Galiwinku == 0)

# define vectors of runs for the different control strategies and name them accordingly
sapply(unique(dat$strategy), function(strat) {
 cur_strat <- which(dat$strategy == strat)
 assign(strat, cur_strat, envir=.GlobalEnv)
})

# define the empty summary tables used for the following sapply loop
sums_Gali_size <- c()
sums_NPA_size <- c()
sums_Gali_dur <- c()
sums_NPA_dur <- c()
sums_Gali_R0 <- c()
sums_NPA_R0 <- c()
sums_Gali_mov <- c()
sums_NPA_mov <- c()
sums_Gali_vacc <- c()
sums_NPA_vacc <- c()
sums_Gali_cull <- c()
sums_NPA_cull <- c()

# loop through all strategies and extract the size, duration, overall R0 and nb of
# dogs moved, vaccinated and culled of all outbreak of the given strategy and summaries
# them for runs in NPA and Galiwinku seperately.
sapply(unique(dat$strategy), function(strat) {

 cur_strat <- eval(parse(text=strat))

 # define the lines with the current control strategy and either occured in Galiwinku
 # or NPA
 cur_strat_Gali_runs <- Gali_runs[which(Gali_runs %in% cur_strat)]
 cur_strat_NPA_runs <- NPA_runs[which(NPA_runs %in% cur_strat)]

 # calculate the summaries of the sizes and add them all in a table
 cur_sum_Gali_size <- summary(dat[cur_strat_Gali_runs,"size"])
 cur_sum_NPA_size <- summary(dat[cur_strat_NPA_runs,"size"])
 sums_Gali_size <<- rbind(sums_Gali_size, cur_sum_Gali_size)
 sums_NPA_size <<- rbind(sums_NPA_size, cur_sum_NPA_size)
 
 # calculate the summaries of the durations and add them all in a table
 cur_sum_Gali_dur <- summary(dat[cur_strat_Gali_runs,"duration"])
 cur_sum_NPA_dur <- summary(dat[cur_strat_NPA_runs,"duration"])
 sums_Gali_dur <<- rbind(sums_Gali_dur, cur_sum_Gali_dur)
 sums_NPA_dur <<- rbind(sums_NPA_dur, cur_sum_NPA_dur)
 
 # calculate the summaries of the R0 and add them all in a table
 cur_sum_Gali_R0 <- summary(dat[cur_strat_Gali_runs,"R0_overall"])
 cur_sum_NPA_R0 <- summary(dat[cur_strat_NPA_runs,"R0_overall"])
 sums_Gali_R0 <<- rbind(sums_Gali_R0, cur_sum_Gali_R0)
 sums_NPA_R0 <<- rbind(sums_NPA_R0, cur_sum_NPA_R0)
 
 # calculate the summaries of the movements and add them all in a table
 cur_sum_Gali_mov <- summary(dat[cur_strat_Gali_runs,"moved"])
 cur_sum_NPA_mov <- summary(dat[cur_strat_NPA_runs,"moved"])
 sums_Gali_mov <<- rbind(sums_Gali_mov, cur_sum_Gali_mov)
 sums_NPA_mov <<- rbind(sums_NPA_mov, cur_sum_NPA_mov)
 
 # calculate the summaries of the vaccinated dogs and add them all in a table
 cur_sum_Gali_vacc <- summary(dat[cur_strat_Gali_runs,"n_vacc"])
 cur_sum_NPA_vacc <- summary(dat[cur_strat_NPA_runs,"n_vacc"])
 sums_Gali_vacc <<- rbind(sums_Gali_vacc, cur_sum_Gali_vacc)
 sums_NPA_vacc <<- rbind(sums_NPA_vacc, cur_sum_NPA_vacc)

 # calculate the summaries of the culled dogs and add them all in a table
 cur_sum_Gali_cull <- summary(dat[cur_strat_Gali_runs,"reactCull"])
 cur_sum_NPA_cull <- summary(dat[cur_strat_NPA_runs,"reactCull"])
 sums_Gali_cull <<- rbind(sums_Gali_cull, cur_sum_Gali_cull)
 sums_NPA_cull <<- rbind(sums_NPA_cull, cur_sum_NPA_cull)
})

# add the strategy names as row names of the summary tables
row.names(sums_Gali_size) <- unique(dat$strategy)
row.names(sums_NPA_size) <- unique(dat$strategy)
row.names(sums_Gali_dur) <- unique(dat$strategy)
row.names(sums_NPA_dur) <- unique(dat$strategy)
row.names(sums_Gali_R0) <- unique(dat$strategy)
row.names(sums_NPA_R0) <- unique(dat$strategy)
row.names(sums_Gali_mov) <- unique(dat$strategy)
row.names(sums_NPA_mov) <- unique(dat$strategy)
row.names(sums_Gali_vacc) <- unique(dat$strategy)
row.names(sums_NPA_vacc) <- unique(dat$strategy)
row.names(sums_Gali_cull) <- unique(dat$strategy)
row.names(sums_NPA_cull) <- unique(dat$strategy)

# save the summaries as tables
write.csv(sums_Gali_size, file="sums_Gali_size.csv")
write.csv(sums_Gali_dur, file="sums_Gali_dur.csv")
write.csv(sums_Gali_R0, file="sums_Gali_R0.csv")
write.csv(sums_Gali_mov, file="sums_Gali_mov.csv")
write.csv(sums_Gali_vacc, file="sums_Gali_vacc.csv")
write.csv(sums_Gali_cull, file="sums_Gali_cull.csv")
write.csv(sums_NPA_size, file="sums_NPA_size.csv")
write.csv(sums_NPA_dur, file="sums_NPA_dur.csv")
write.csv(sums_NPA_R0, file="sums_NPA_R0.csv")
write.csv(sums_NPA_mov, file="sums_NPA_mov.csv")
write.csv(sums_NPA_vacc, file="sums_NPA_vacc.csv")
write.csv(sums_NPA_cull, file="sums_NPA_cull.csv")


# create and save boxplots of the measures per strategy and region (Galiwinku and NPA)
tiff(file="boxplot_size.tiff")
par(mar=c(6.5,4,3.5,2))
par(mfrow=c(2,1))
boxplot(size ~ strategy, data=dat[Gali_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="outbreak size in Galiwinku \n number of dead dogs (without outliers)",
                         cex.main=1)
boxplot(size ~ strategy, data=dat[NPA_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="outbreak size in NPA \n number of dead dogs (without outliers)",
                         cex.main=1)
dev.off()

tiff(file="boxplot_rabidDogs.tiff")
par(mar=c(6.5,4,3.5,2))
par(mfrow=c(2,1))
boxplot(size-reactCull ~ strategy, data=dat[Gali_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="outbreak size in Galiwinku \n number of rabid dogs (without outliers)",
                         cex.main=1)
boxplot(size-reactCull ~ strategy, data=dat[NPA_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="outbreak size in NPA \n number of rabid dogs (without outliers)",
                         cex.main=1)
dev.off()

tiff(file="boxplot_duration.tiff")
par(mar=c(6.5,4,3.5,2))
par(mfrow=c(2,1))
boxplot(duration ~ strategy, data=dat[Gali_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="outbreak duration in Galiwinku \n in days (without outliers)",
                         cex.main=1)
boxplot(duration ~ strategy, data=dat[NPA_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="outbreak duration in NPA \n in days (without outliers)",
                         cex.main=1)
dev.off()

tiff(file="boxplot_R0.tiff")
par(mar=c(6.5,4,3.5,2))
par(mfrow=c(2,1))
boxplot(R0_overall ~ strategy, data=dat[Gali_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="overall R0 of outbreaks in Galiwinku \n (without outliers)",
                         cex.main=1)
boxplot(R0_overall ~ strategy, data=dat[NPA_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="overall R0 of outbreaks in NPA \n (without outliers)",
                         cex.main=1)
dev.off()

tiff(file="boxplot_moved.tiff")
par(mar=c(6.5,4,3.5,2))
par(mfrow=c(2,1))
boxplot(moved ~ strategy, data=dat[Gali_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="nb of dogs moved (without outliers) \n  between communities in outbreaks in Galiwinku",
                         cex.main=1)
boxplot(moved ~ strategy, data=dat[NPA_runs,],outline=F, cex.axis=0.8, las=2, lwd=0.6,
                         main="nb of dogs moved (without outliers) \n  between communities in outbreaks in NPA",
                         cex.main=1)
dev.off()

tiff(file="boxplot_vaccinated.tiff")
par(mar=c(6.5,4,3.5,2))
par(mfrow=c(2,1))
vacc_Gali <- Gali_runs[which(Gali_runs %in% c(vacc_com_50,vacc_com_70,vacc_reg_50,vacc_reg_70))]
vacc_NPA <- NPA_runs[which(NPA_runs %in% c(vacc_com_50,vacc_com_70,vacc_reg_50,vacc_reg_70))]

boxplot(n_vacc ~ strategy, data=dat[vacc_Gali,],outline=T, cex.axis=0.8, las=2, lwd=0.6,
                         main="nb of dogs vaccinated during outbreaks in Galiwinku",
                         cex.main=1)
boxplot(n_vacc ~ strategy, data=dat[vacc_NPA,],outline=T, cex.axis=0.8, las=2, lwd=0.6,
                         main="nb of dogs vaccinated during outbreaks in NPA",
                         cex.main=1)
dev.off()

tiff(file="boxplot_culled.tiff")
par(mar=c(6.5,4,3.5,2))
par(mfrow=c(2,1))
cull_Gali <- Gali_runs[which(Gali_runs %in% c(cull_com_30,cull_com_50,cull_com_70,cull_reg_30,cull_reg_50,cull_reg_70))]
cull_NPA <- NPA_runs[which(NPA_runs %in% c(cull_com_30,cull_com_50,cull_com_70,cull_reg_30,cull_reg_50,cull_reg_70))]

boxplot(reactCull ~ strategy, data=dat[cull_Gali,],outline=T, cex.axis=0.8, las=2, lwd=0.6,
                         main="nb of dogs reactively culled during outbreaks in Galiwinku",
                         cex.main=1)
boxplot(reactCull ~ strategy, data=dat[cull_NPA,],outline=T, cex.axis=0.8, las=2, lwd=0.6,
                         main="nb of dogs reactively culled during outbreaks in NPA",
                         cex.main=1)
dev.off()

# Are all unvaccinated dogs dead in vaccination strategies?
vacc_runs<- which(dat$n_vacc!=0)
cbind(dat[vacc_runs,c("size","n_vacc","R0_Galiwinku")],sum=dat$size[vacc_runs]+dat$n_vacc[vacc_runs])
hist(dat$size[vacc_runs]+dat$n_vacc[vacc_runs], breaks=100)
length(which(dat$size[vacc_runs]+dat$n_vacc[vacc_runs] < 360)) / length(vacc_runs)
table(dat$strategy[vacc_runs[which(dat$size[vacc_runs]+dat$n_vacc[vacc_runs] < 360)]])
# vacc_com_50 vacc_com_70 vacc_reg_50 vacc_reg_70
#           6           9           9          12



