#This script performs the statistical analyses in Stausland Johnsen 2012 "From perception to phonology: The emergence of perceptually motivated constraint rankings" and prints the results to a text file

#This analysis was performed using R-2.13.1 and lme4_0.999375-40
#There is no guarantee that the results will be 100% identical with other software/package versions

#####################################################
#                  Begin preamble                   #
#####################################################

#Create folders for packages and data
dir.create(path = "C:/R-2.13.1/mylibrary")
dir.create(path = "C:/R-2.13.1/mydata")

#Download data
download.file(url = "http://folk.uio.no/sverrej/publication_files/2012/lingua/stausland.johnsen_lingua.2012.data.txt", destfile = "C:/R-2.13.1/mydata/Data.txt")

#Set local library for my packages
my.library <- "C:/R-2.13.1/mylibrary"

#Set working directory
setwd(dir = "C:/R-2.13.1/mydata")

#Install necessary packages
install.packages(pkgs = "C:/lme4_0.999375-40.zip", lib = my.library, repos = NULL, contriburl = NULL)

#Load necessary packages
library(package = "lme4", lib.loc = my.library)

#####################################################
#                   End preamble                    #
#####################################################

#####################################################
#            Begin analysis Experiment A            #
#####################################################

#Load data from experiment A
read.table(file = "Data.txt", header = TRUE, sep = "\t", nrows = 2688) -> Exp_A

#Compare category 'sV' - 'st' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + X:Category, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "st",], family = binomial(link = "logit")) -> Without.interaction.sVst.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + X:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "st",], family = binomial(link = "logit")) -> With.interaction.sVst.Exp_A.glmer
anova(Without.interaction.sVst.Exp_A.glmer, With.interaction.sVst.Exp_A.glmer, test = "Chisq") -> sVst.Exp_A

#Compare category 'sV' - 'sk' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + Category + Stimulus, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "sk",], family = binomial(link = "logit")) -> Without.interaction.sVsk.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + Category + Stimulus + Category:Stimulus, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "sk",], family = binomial(link = "logit")) -> With.interaction.sVsk.Exp_A.glmer
anova(Without.interaction.sVsk.Exp_A.glmer, With.interaction.sVsk.Exp_A.glmer, test = "Chisq") -> sVsk.Exp_A

#Compare category 'sV' - 'd' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + Category + Stimulus + Trial:Category + A:Category, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "d",], family = binomial(link = "logit")) -> Without.interaction.sVd.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + Category + Stimulus + Trial:Category + A:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "d",], family = binomial(link = "logit")) -> With.interaction.sVd.Exp_A.glmer
anova(Without.interaction.sVd.Exp_A.glmer, With.interaction.sVd.Exp_A.glmer, test = "Chisq") -> sVd.Exp_A

#Compare category 'sV' - 't' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + Category + Stimulus + Trial:Category, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "t",], family = binomial(link = "logit")) -> Without.interaction.sVt.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + Category + Stimulus + Trial:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "t",], family = binomial(link = "logit")) -> With.interaction.sVt.Exp_A.glmer
anova(Without.interaction.sVt.Exp_A.glmer, With.interaction.sVt.Exp_A.glmer, test = "Chisq") -> sVt.Exp_A

#Compare category 'sV' - 'n' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + Category + Stimulus + Trial:Category, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "n",], family = binomial(link = "logit")) -> Without.interaction.sVn.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + Category + Stimulus + Trial:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "sV" | Exp_A$Category == "n",], family = binomial(link = "logit")) -> With.interaction.sVn.Exp_A.glmer
anova(Without.interaction.sVn.Exp_A.glmer, With.interaction.sVn.Exp_A.glmer, test = "Chisq") -> sVn.Exp_A

#Compare category 'st' - 'sk' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "sk",], family = binomial(link = "logit")) -> Without.interaction.stsk.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Category:Stimulus, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "sk",], family = binomial(link = "logit")) -> With.interaction.stsk.Exp_A.glmer
anova(Without.interaction.stsk.Exp_A.glmer, With.interaction.stsk.Exp_A.glmer, test = "Chisq") -> stsk.Exp_A

#Compare category 'st' - 'd' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "d",], family = binomial(link = "logit")) -> Without.interaction.std.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "d",], family = binomial(link = "logit")) -> With.interaction.std.Exp_A.glmer
anova(Without.interaction.std.Exp_A.glmer, With.interaction.std.Exp_A.glmer, test = "Chisq") -> std.Exp_A

#Compare category 'st' - 't' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + X + Category + Stimulus + Trial:Category + X:Category, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "t",], family = binomial(link = "logit")) -> Without.interaction.stt.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + X + Category + Stimulus + Trial:Category + X:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "t",], family = binomial(link = "logit")) -> With.interaction.stt.Exp_A.glmer
anova(Without.interaction.stt.Exp_A.glmer, With.interaction.stt.Exp_A.glmer, test = "Chisq") -> stt.Exp_A

#Compare category 'st' - 'n' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "n",], family = binomial(link = "logit")) -> Without.interaction.stn.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "st" | Exp_A$Category == "n",], family = binomial(link = "logit")) -> With.interaction.stn.Exp_A.glmer
anova(Without.interaction.stn.Exp_A.glmer, With.interaction.stn.Exp_A.glmer, test = "Chisq") -> stn.Exp_A

#Compare category 'sk' - 'd' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category + Trial:Stimulus, data = Exp_A[Exp_A$Category == "sk" | Exp_A$Category == "d",], family = binomial(link = "logit")) -> Without.interaction.skd.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category + Trial:Stimulus + Category:Stimulus, data = Exp_A[Exp_A$Category == "sk" | Exp_A$Category == "d",], family = binomial(link = "logit")) -> With.interaction.skd.Exp_A.glmer
anova(Without.interaction.skd.Exp_A.glmer, With.interaction.skd.Exp_A.glmer, test = "Chisq") -> skd.Exp_A

#Compare category 'sk' - 't' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + X + Category + Stimulus + Trial:Category + X:Category, data = Exp_A[Exp_A$Category == "sk" | Exp_A$Category == "t",], family = binomial(link = "logit")) -> Without.interaction.skt.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + A + X + Category + Stimulus + Trial:Category + X:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "sk" | Exp_A$Category == "t",], family = binomial(link = "logit")) -> With.interaction.skt.Exp_A.glmer
anova(Without.interaction.skt.Exp_A.glmer, With.interaction.skt.Exp_A.glmer, test = "Chisq") -> skt.Exp_A

#Compare category 'sk' - 'n' in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category, data = Exp_A[Exp_A$Category == "sk" | Exp_A$Category == "n",], family = binomial(link = "logit")) -> Without.interaction.skn.Exp_A.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + Trial:Category + Category:Stimulus, data = Exp_A[Exp_A$Category == "sk" | Exp_A$Category == "n",], family = binomial(link = "logit")) -> With.interaction.skn.Exp_A.glmer
anova(Without.interaction.skn.Exp_A.glmer, With.interaction.skn.Exp_A.glmer, test = "Chisq") -> skn.Exp_A

#####################################################
#             End analysis Experiment A             #
#####################################################

#####################################################
#       Begin post-hoc analysis Experiment A        #
#####################################################

#Extract hits for 'sV' and 'st'
Exp_A.Hits.sVst <- Exp_A[Exp_A$Stimulus == "different" & Exp_A$Response == "different" & (Exp_A$Category == "sV" | Exp_A$Category == "st"),]

#Extract false alarms for 'sV' and 'st'
Exp_A.False.sVst <- Exp_A[Exp_A$Stimulus == "same" & Exp_A$Response == "different" & (Exp_A$Category == "sV" | Exp_A$Category == "st"),]

#Number of responses necessary for a rate of 1 for one category
Exp_A.max.resp <- (nrow(x = Exp_A[Exp_A$Stimulus == "different",]) / nlevels(x = Exp_A$Category)) / nlevels(x = Exp_A$Subject)

#Number of hits for each subject for 'sV'
Exp_A.Hits.sV <- xtabs(formula = ~ Subject + (Category == "sV"), data = Exp_A.Hits.sVst[Exp_A.Hits.sVst$Category=="sV",])
Exp_A.Hits.sV <- data.frame(Exp_A.Hits.sV)[,c(1, 3)]

#Number of false alarms for each subject for 'sV'
Exp_A.False.sV <- xtabs(formula = ~ Subject + (Category == "sV"), data = Exp_A.False.sVst[Exp_A.False.sVst$Category=="sV",]) 
Exp_A.False.sV <- data.frame(Exp_A.False.sV)[,c(1, 3)]

#Merge hits and false alarm data for 'sV'
cbind(Exp_A.Hits.sV, Exp_A.False.sV$Freq) -> Exp_A.sV
colnames(x = Exp_A.sV)[2:3] <- c("FreqHits", "FreqFalse")

#Extract subjects that have a hit rate = 1 and false alarm rate != 1 for 'sV'
Exp_A.sV.max.hit <- Exp_A.sV[Exp_A.sV$FreqHits == (Exp_A.max.resp) & Exp_A.sV$FreqFalse != (Exp_A.max.resp),]

#Extract subjects that have a false alarm rate = 0 and hit rate != 0 for 'sV'
Exp_A.sV.min.false <- Exp_A.sV[Exp_A.sV$FreqHits != 0 & Exp_A.sV$FreqFalse == 0,]

#Extract names of subjects with infinite perceived distance for 'sV'
Exp_A.Inf.sV <- unique(x = rbind(Exp_A.sV.max.hit, Exp_A.sV.min.false)["Subject"])

#Number of hits for each subject for 'st'
Exp_A.Hits.st <- xtabs(formula = ~ Subject + (Category == "st"), data = Exp_A.Hits.sVst[Exp_A.Hits.sVst$Category=="st",]) 
Exp_A.Hits.st <- data.frame(Exp_A.Hits.st)[,c(1, 3)]

#Number of false alarms for each subject for 'st'
Exp_A.False.st <- xtabs(formula = ~ Subject + (Category == "st"), data = Exp_A.False.sVst[Exp_A.False.sVst$Category=="st",]) 
Exp_A.False.st <- data.frame(Exp_A.False.st)[,c(1, 3)]

#Merge hits and false alarm data for 'st'
cbind(Exp_A.Hits.st, Exp_A.False.st$Freq) -> Exp_A.st
colnames(x = Exp_A.st)[2:3] <- c("FreqHits", "FreqFalse")

#Extract subjects that have a hit rate = 1 and false alarm rate != 1 for 'st'
Exp_A.st.max.hit <- Exp_A.st[Exp_A.st$FreqHits == (Exp_A.max.resp) & Exp_A.st$FreqFalse != (Exp_A.max.resp),]

#Extract subjects that have a false alarm rate = 0 and hit rate != 0 for 'st'
Exp_A.st.min.false <- Exp_A.st[Exp_A.st$FreqHits != 0 & Exp_A.st$FreqFalse == 0,]

#Extract names of subjects with infinite perceived distance for 'st'
Exp_A.Inf.st <- unique(x = rbind(Exp_A.st.max.hit, Exp_A.st.min.false)["Subject"])

#Names of subjects with infinitive perceived distance for both 'sV' and 'st'
rbind(Exp_A.Inf.sV, Exp_A.Inf.st) -> Exp_A.Inf.sV.or.st
duplicated(x = Exp_A.Inf.sV.or.st) -> Exp_A.Inf.sV.or.st$duplicate
Exp_A.Inf.sV.and.st <- Exp_A.Inf.sV.or.st[Exp_A.Inf.sV.or.st$duplicate == TRUE,]$Subject
Exp_A.Inf.sV.and.st[drop = TRUE] -> Exp_A.Inf.sV.and.st
levels(x = Exp_A.Inf.sV.and.st) -> Exp_A.Inf.sV.and.st

#Remove subjects with infinitive perceived distance for both 'sV' and 'st' from Experiment A data
Exp_A$Subject %in% Exp_A.Inf.sV.and.st -> Exp_A$Remove
Exp_A[Exp_A$Remove == FALSE,] -> APH
Exp_A <- Exp_A[,c(1:7)]
APH <- APH[,c(1:7)]

#Compare category 'sV' - 'st' for remaining data in experiment A
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + X:Category, data = APH[APH$Category == "sV" | APH$Category == "st",], family = binomial(link = "logit")) -> Without.interaction.sVst.Exp_A.Posthoc.glmer
glmer(formula = Response ~ 1 + (1|Subject) + Trial + X + Category + Stimulus + X:Category + Category:Stimulus, data = APH[APH$Category == "sV" | APH$Category == "st",], family = binomial(link = "logit")) -> With.interaction.sVst.Exp_A.Posthoc.glmer
anova(Without.interaction.sVst.Exp_A.Posthoc.glmer, With.interaction.sVst.Exp_A.Posthoc.glmer, test = "Chisq") -> sVst.Exp_A.Posthoc

#Remove data sets for experiment A from memory
remove(list = c("Exp_A", "APH"))

#####################################################
#        End post-hoc analysis Experiment A         #
#####################################################

#####################################################
#            Begin analysis Experiment B            #
#####################################################

#Load data from experiment B
read.table(file = "Data.txt", header = TRUE, sep = "\t", nrows = 2857, skip = 2689) -> Exp_B

#Compare category 'sV' - 'st' in experiment B
glmer(formula = Response ~ 1 + (1|Subject) + log(x = Trial) + log(x = ReactionTime) + X + Category + Stimulus +	log(x = Trial):log(x = ReactionTime) + log(x = Trial):X + log(x = Trial):Stimulus + log(x = ReactionTime):Stimulus + log(x = Trial):log(x = ReactionTime):Stimulus, data = Exp_B[Exp_B$Category == "sV" | Exp_B$Category == "st",],	family = binomial(link = "logit")) -> Without.interaction.sVst.Exp_B.glmer
glmer(formula = Response ~ 1 + (1|Subject) + log(x = Trial) + log(x = ReactionTime) + X + Category + Stimulus +	log(x = Trial):log(x = ReactionTime) + log(x = Trial):X + log(x = Trial):Stimulus + log(x = ReactionTime):Stimulus + Category:Stimulus + log(x = Trial):log(x = ReactionTime):Stimulus, data = Exp_B[Exp_B$Category == "sV" | Exp_B$Category == "st",],	family = binomial(link = "logit")) -> With.interaction.sVst.Exp_B.glmer
anova(Without.interaction.sVst.Exp_B.glmer, With.interaction.sVst.Exp_B.glmer, test = "Chisq") -> sVst.Exp_B

#####################################################
#             End analysis Experiment B             #
#####################################################

#####################################################
#       Begin post-hoc analysis Experiment B        #
#####################################################

#Load data for stimulus length
read.table(file = "Data.txt", header = TRUE, sep = "\t", skip = 5549) -> Exp_B_Stimlength

#Student's paired t-test of stimulus length means
t.test(x = Exp_B_Stimlength$Length[Exp_B_Stimlength$Category == "sV"], y = Exp_B_Stimlength$Length[Exp_B_Stimlength$Category == "st"], alternative = "two.sided", paired = TRUE, var.equal = TRUE) -> Exp_B_Stimlength.t.test

#Wilcoxon rank sum test of reaction time means
wilcox.test(x = Exp_B$ReactionTime[Exp_B$Category == "sV"], y = Exp_B$ReactionTime[Exp_B$Category == "st"], alternative = "two.sided", paired = FALSE) -> Exp_B_RT.wilcox

#Recode responses as 1 (correct) and 0 (wrong)
Exp_B$Response == Exp_B$Stimulus -> Exp_B$Response
Exp_B[Exp_B$Response == TRUE,]$Response = 1

glmer(formula = Response ~ 1 + (1|Subject) + log(x = Trial) + poly(x = log(x = ReactionTime), degree = 2, raw = FALSE) + A + Category + Stimulus + log(x = Trial):poly(x = log(x = ReactionTime), degree = 2, raw = FALSE) + log(x = Trial):A + poly(x = log(x = ReactionTime), degree = 2, raw = FALSE):A + log(x = Trial):Stimulus + poly(x = log(x = ReactionTime), degree = 2, raw = FALSE):Stimulus, data = Exp_B[Exp_B$Category == "sV" | Exp_B$Category == "st",], family = binomial(link = "logit")) -> Without.interaction.RTCat.sVst.Exp_B.Posthoc.glmer
glmer(formula = Response ~ 1 + (1|Subject) + log(x = Trial) + poly(x = log(x = ReactionTime), degree = 2, raw = FALSE) + A + Category + Stimulus + log(x = Trial):poly(x = log(x = ReactionTime), degree = 2, raw = FALSE) + log(x = Trial):A + poly(x = log(x = ReactionTime), degree = 2, raw = FALSE):A + log(x = Trial):Stimulus + poly(x = log(x = ReactionTime), degree = 2, raw = FALSE):Stimulus +poly(x = log(x = ReactionTime), degree = 2, raw = FALSE):Category, data = Exp_B[Exp_B$Category == "sV" | Exp_B$Category == "st",], family = binomial(link = "logit")) -> With.interaction.RTCat.sVst.Exp_B.Posthoc.glmer
anova(Without.interaction.RTCat.sVst.Exp_B.Posthoc.glmer, With.interaction.RTCat.sVst.Exp_B.Posthoc.glmer, test = "Chisq") -> RTCat.sVst.Exp_B.Posthoc

#Remove data sets for experiment B from memory
remove(Exp_B)

#####################################################
#        End post-hoc analysis Experiment B         #
#####################################################

#####################################################
#                Begin print results                #
#####################################################

sVst.Exp_A <- sVst.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
sVsk.Exp_A <- sVsk.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
sVd.Exp_A <- sVd.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
sVt.Exp_A <- sVt.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
sVn.Exp_A <- sVn.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
stsk.Exp_A <- stsk.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
std.Exp_A <- std.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
stt.Exp_A <- stt.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
stn.Exp_A <- stn.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
skd.Exp_A <- skd.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
skt.Exp_A <- skt.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
skn.Exp_A <- skn.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
sVsk.Exp_A <- capture.output(sVsk.Exp_A, file = NULL)
sVd.Exp_A <- capture.output(sVd.Exp_A, file = NULL)
sVt.Exp_A <- capture.output(sVt.Exp_A, file = NULL)
sVn.Exp_A <- capture.output(sVn.Exp_A, file = NULL)
stsk.Exp_A <- capture.output(stsk.Exp_A, file = NULL)
std.Exp_A <- capture.output(std.Exp_A, file = NULL)
stt.Exp_A <- capture.output(stt.Exp_A, file = NULL)
stn.Exp_A <- capture.output(stn.Exp_A, file = NULL)
skd.Exp_A <- capture.output(skd.Exp_A, file = NULL)
skt.Exp_A <- capture.output(skt.Exp_A, file = NULL)
skn.Exp_A <- capture.output(skn.Exp_A, file = NULL)
cat("                            Experiment A\n", file = "Results.txt", append = FALSE)
cat("-------------------------------------------------------------------\n\n", file = "Results.txt", append = TRUE)
capture.output(sVst.Exp_A, file = "Results.txt", append = TRUE)
cat(sVsk.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(sVd.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(sVt.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(sVn.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)
cat(stsk.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(std.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(stt.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(stn.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)
cat(skd.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(skt.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(skn.Exp_A[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(skn.Exp_A[3], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(skn.Exp_A[4], file = "Results.txt", sep = " ", append = TRUE)
cat("\n\n\n", file = "Results.txt", append = TRUE)

sVst.Exp_A.Posthoc <- sVst.Exp_A.Posthoc[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
cat("                        Experiment A post-hoc\n", file = "Results.txt", append = TRUE)
cat("-------------------------------------------------------------------\n\n", file = "Results.txt", append = TRUE)
capture.output(sVst.Exp_A.Posthoc, file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)

sVst.Exp_B <- sVst.Exp_B[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
cat("                            Experiment B\n", file = "Results.txt", append = TRUE)
cat("-------------------------------------------------------------------\n\n", file = "Results.txt", append = TRUE)
capture.output(sVst.Exp_B, file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)

Exp_B_Stimlength.t.test <- capture.output(Exp_B_Stimlength.t.test, file = NULL)
cat("                        Experiment B post-hoc\n", file = "Results.txt", append = TRUE)
cat("-------------------------------------------------------------------\n\n", file = "Results.txt", append = TRUE)
cat(Exp_B_Stimlength.t.test[2], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(Exp_B_Stimlength.t.test[3], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(Exp_B_Stimlength.t.test[5], file = "Results.txt", append = TRUE)
cat("\n\n\n", file = "Results.txt", append = TRUE)

Exp_B_RT.wilcox <- capture.output(Exp_B_RT.wilcox, file = NULL)
cat(Exp_B_RT.wilcox[2], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(Exp_B_RT.wilcox[3], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(Exp_B_RT.wilcox[5], file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)

RTCat.sVst.Exp_B.Posthoc <- RTCat.sVst.Exp_B.Posthoc[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
capture.output(RTCat.sVst.Exp_B.Posthoc, file = "Results.txt", append = TRUE)

#####################################################
#                 End print results                 #
#####################################################

#####################################################
#                  Begin postamble                  #
#####################################################

#Remove all objects from memory
remove(list = ls(all.names = TRUE))

#Detach loaded packages
detach(package:lme4)

#####################################################
#                   End postamble                   #
#####################################################
