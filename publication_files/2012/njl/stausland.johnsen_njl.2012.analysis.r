#This script performs the statistical analyses in Stausland Johnsen 2012 "Variation in Norwegian retroflexion" and prints the results to a text file

#This analysis was performed using R-2.15.1 and lme4_0.999999-0
#There is no guarantee that the results will be 100% identical with other software/package versions

#####################################################
#                  Begin preamble                   #
#####################################################

#Create folders for packages and data
dir.create(path = "C:/R-2.15.1/mylibrary")
dir.create(path = "C:/R-2.15.1/mydata")

#Download data
download.file(url = "http://folk.uio.no/sverrej/publication_files/2012/njl/stausland.johnsen_njl.2012.data.txt", destfile = "C:/R-2.15.1/mydata/Data.txt")

#Set local library for my packages
my.library <- "C:/R-2.15.1/mylibrary"

#Set working directory
setwd(dir = "C:/R-2.15.1/mydata")

#Install necessary packages
install.packages(pkgs = "C:/lme4_0.999999-0.zip", lib = my.library, repos = NULL, contriburl = NULL)

#Load necessary packages
library(package = "lme4", lib.loc = my.library)

#####################################################
#                   End preamble                    #
#####################################################

#####################################################
#            Begin analysis Experiment A            #
#####################################################

#Load data from experiment A
read.table(file = "Data.txt", header = TRUE, sep = "\t", nrows = 2866) -> Exp_A

#Recode vowel height as an interval variable
factor(x = Exp_A$vheight, levels = c("low", "mid", "high")) -> Exp_A$vheight.int
as.integer(x = Exp_A$vheight.int) -> Exp_A$vheight.int

#Compare category 'sV' - 'st' in experiment A
glmer(formula = retro ~ 1 + (1|subject) +
	log(x = block) +
	log(x = block) : onset +
	log(x = block) : vheight.int +
	log(x = block) : vround +
	log(x = block) : vfront +
	log(x = block) : vlength +
		log(x = pos) +
		log(x = pos) : onset +
			onset +
			onset : vheight.int +
			onset : vround +
			onset : vfront +
				vheight.int + 
					vround +
						vfront +
							vlength +
								log(x = frequency),
	family = binomial(link = "logit"), data = Exp_A[Exp_A$onset == "sV" | Exp_A$onset == "st",]) -> With.Onset.sVst.Exp_A.glmer
glmer(formula = retro ~ 1 + (1|subject) +
	log(x = block) +
	log(x = block) : vheight.int +
	log(x = block) : vround +
	log(x = block) : vfront +
	log(x = block) : vlength +
		log(x = pos) +
				vheight.int + 
					vround +
						vfront +
							vlength +
								log(x = frequency),
	family = binomial(link = "logit"), data = Exp_A[Exp_A$onset == "sV" | Exp_A$onset == "st",]) -> Without.Onset.sVst.Exp_A.glmer
anova(Without.Onset.sVst.Exp_A.glmer, With.Onset.sVst.Exp_A.glmer, test = "Chisq") -> sVst.Exp_A

#Remove data set for experiment A from memory
remove(Exp_A)

#####################################################
#             End analysis Experiment A             #
#####################################################

#####################################################
#            Begin analysis Experiment B            #
#####################################################

#Load data from experiment B
read.table(file = "Data.txt", header = TRUE, sep = "\t", skip = 2868) -> Exp_B

#Recode position as a categorical variable
as.factor(x = Exp_B$pos) -> Exp_B$pos.cat

#Compare category 'sV' - 'st' in experiment B
glmer(formula = retro ~ 1 + (1|subject) + block + (block : onset) + pos.cat + (pos.cat : vfront) + onset + (onset : vfront) + vfront, family = binomial(link = "logit"), data = Exp_B[Exp_B$onset == "sV" | Exp_B$onset == "st",]) -> With.Onset.sVst.Exp_B.glmer
glmer(formula = retro ~ 1 + (1|subject) + block + pos.cat + (pos.cat : vfront) + vfront, family = binomial(link = "logit"), data = Exp_B[Exp_B$onset == "sV" | Exp_B$onset == "st",]) -> Without.Onset.sVst.Exp_B.glmer
anova(Without.Onset.sVst.Exp_B.glmer, With.Onset.sVst.Exp_B.glmer, test = "Chisq") -> sVst.Exp_B

#Compare category 'sV' - 'sk' in experiment B
glmer(formula = retro ~ 1 + (1|subject) + block + (block : vheight) + pos.cat + (pos.cat : vfront) + onset + vheight + vfront, family = binomial(link = "logit"), data = Exp_B[Exp_B$onset == "sV" | Exp_B$onset == "sk",]) -> With.Onset.sVsk.Exp_B.glmer
glmer(formula = retro ~ 1 + (1|subject) + block + (block : vheight) + pos.cat + (pos.cat : vfront) + vheight + vfront, family = binomial(link = "logit"), data = Exp_B[Exp_B$onset == "sV" | Exp_B$onset == "sk",]) -> Without.Onset.sVsk.Exp_B.glmer
anova(Without.Onset.sVsk.Exp_B.glmer, With.Onset.sVsk.Exp_B.glmer, test = "Chisq") -> sVsk.Exp_B

#Compare category 'st' - 'sk' in experiment B
glmer(formula = retro ~ 1 + (1|subject) + block + pos.cat + onset + vfront, family = binomial(link = "logit"), data = Exp_B[Exp_B$onset == "st" | Exp_B$onset == "sk",]) -> With.Onset.stsk.Exp_B.glmer
glmer(formula = retro ~ 1 + (1|subject) + block + pos.cat + vfront, family = binomial(link = "logit"), data = Exp_B[Exp_B$onset == "st" | Exp_B$onset == "sk",]) -> Without.Onset.stsk.Exp_B.glmer
anova(Without.Onset.stsk.Exp_B.glmer, With.Onset.stsk.Exp_B.glmer, test = "Chisq") -> stsk.Exp_B

#Remove data set for experiment B from memory
remove(Exp_B)

#####################################################
#             End analysis Experiment B             #
#####################################################

#####################################################
#                Begin print results                #
#####################################################

sVst.Exp_A <- sVst.Exp_A[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
cat("                            Experiment A\n", file = "Results.txt", append = FALSE)
cat("-------------------------------------------------------------------\n\n", file = "Results.txt", append = TRUE)
capture.output(sVst.Exp_A, file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)

sVst.Exp_B <- sVst.Exp_B[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
sVsk.Exp_B <- sVsk.Exp_B[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
stsk.Exp_B <- stsk.Exp_B[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,]
sVst.Exp_B <- capture.output(sVst.Exp_B, file = NULL)
sVsk.Exp_B <- capture.output(sVsk.Exp_B, file = NULL)
stsk.Exp_B <- capture.output(stsk.Exp_B, file = NULL)

cat("                            Experiment B\n", file = "Results.txt", append = TRUE)
cat("-------------------------------------------------------------------\n\n", file = "Results.txt", append = TRUE)
cat(sVst.Exp_B[1], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(sVst.Exp_B[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(sVsk.Exp_B[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(stsk.Exp_B[2], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(stsk.Exp_B[3], file = "Results.txt", sep = " ", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
cat(stsk.Exp_B[4], file = "Results.txt", sep = " ", append = TRUE)

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
