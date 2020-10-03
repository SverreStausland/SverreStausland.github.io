#This script performs the statistical analyses in Stausland Johnsen 2015 "Vowel reduction in verbs in King Alfred’s Pastoral Care" and prints the results to a text file

#This analysis was performed using R-3.1.1 and the packages lme4_1.1-7, minqa_1.2.3, Rcpp_0.11.2, and nloptr_1.0.4
#There is no guarantee that the results will be 100% identical with other software/package versions

#####################################################
#                  Begin preamble                   #
#####################################################

#Install necessary packages
install.packages(pkgs = c("C:/lme4_1.1-7.zip", "C:/Rcpp_0.11.2.zip", "C:/minqa_1.2.3.zip", "C:/nloptr_1.0.4.zip"), lib = .libPaths(), repos = NULL, contriburl = NULL)

#Load necessary packages
library(package = "lme4")

#Create folder for data
dir.create(path = "C:/R-3.1.1/mydata")

#Download data
download.file(url = "http://folk.uio.no/sverrej/publication_files/2015/ichl/stausland.johnsen_ichl.2015.data.txt", destfile = "C:/R-3.1.1/mydata/Data.txt")

#Set working directory
setwd(dir = "C:/R-3.1.1/mydata")

#####################################################
#                   End preamble                    #
#####################################################

#####################################################
#          Begin analysis van Helten's rule         #
#####################################################

#Load Old English data
read.table(file = "Data.txt", header = TRUE, sep = "\t", row.names = NULL, encoding = "UTF-8") -> Corpus

#Collapse 'u' and 'o' as one vowel
Corpus[Corpus$Vowel == "o",]$Vowel = "u"

#Remove rows where the vowel is not 'a' or 'u'
Corpus[Corpus$Vowel == "a" | Corpus$Vowel == "u",] -> Corpus

#Create new column for forms where an original */u/ followed
Corpus$Orig.u = 0

#The forms that had an original */u/ following are:
Corpus[Corpus$Grammar == "pret.1.pl.ind." | Corpus$Grammar == "pret.2.pl.ind." | Corpus$Grammar == "pret.3.pl.ind." | Corpus$Grammar == "perf.m.dat.sg." | Corpus$Grammar == "perf.m.dat.pl." | Corpus$Grammar == "perf.f.nom.sg." | Corpus$Grammar == "perf.n.dat.sg." | Corpus$Grammar == "perf.n.nom.pl." | Corpus$Grammar == "perf.n.acc.pl." | Corpus$Grammar == "perf.n.dat.pl.",]$Orig.u = 1

#Create new column where vowel 'u' is 1 and 'a' is 0
Corpus$Vowel.Num = 1
Corpus[Corpus$Vowel == "a",]$Vowel.Num = 0

#Logistic regression model with van Helten's rule
glmer(formula = Vowel.Num ~ 1 + (1 + Orig.u | Word) + Orig.u, data = Corpus, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa")) -> With.vanHelten.glmer

#Logistic regression model without van Helten's rule
glmer(formula = Vowel.Num ~ 1 + (1 + Orig.u | Word), data = Corpus, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa")) -> Without.vanHelten.glmer

#Compare the two models
round(x = anova(With.vanHelten.glmer, Without.vanHelten.glmer, test = "Chisq"), digits = 3) -> vanHelten.anova

#####################################################
#          End analysis van Helten's rule           #
#####################################################

#####################################################
#           Begin analysis new hypothesis           #
#####################################################

#Create new column where syllable 'mid' is 1 and 'final' is 0
Corpus$Syllable.Num = 1
Corpus[Corpus$Syllable == "final",]$Syllable.Num = 0

#Logistic regression model with syllable position
glmer(formula = Vowel.Num ~ 1 + (1 | Word) + (0 + Syllable.Num | Word) + Syllable.Num, data = Corpus, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa")) -> With.Syllable.glmer

#Logistic regression model without syllable position
glmer(formula = Vowel.Num ~ 1 + (1 | Word) + (0 + Syllable.Num | Word), data = Corpus, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa")) -> Without.Syllable.glmer

#Compare the two models
anova(With.Syllable.glmer, Without.Syllable.glmer, test = "Chisq") -> Syllable.anova

#Remove data set from memory
remove(Corpus)

#####################################################
#            End analysis new hypothesis            #
#####################################################

#####################################################
#                Begin print results                #
#####################################################

vanHelten.summary <- summary(With.vanHelten.glmer)
vanHelten.summary.output <- capture.output(vanHelten.summary)
cat("            Van Helten's rule\n", file = "Results.txt", append = FALSE)
cat("--------------------------------------------\n", file = "Results.txt", append = TRUE)
cat(vanHelten.summary.output[14], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
capture.output(print(VarCorr(With.vanHelten.glmer), comp = c("Variance", "Std.Dev."), digits = 4), file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)
cat(vanHelten.summary.output[20], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
round(x = vanHelten.summary$coefficients, digits = 3)[,-4] -> vanHelten.fixedeffects
capture.output(vanHelten.fixedeffects, file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)
capture.output(vanHelten.anova[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,], file = "Results.txt", append = TRUE)
cat("--------------------------------------------\n\n\n", file = "Results.txt", append = TRUE)

newhypothesis.summary <- summary(With.Syllable.glmer)
newhypothesis.summary.output <- capture.output(newhypothesis.summary)
cat("            New hypothesis\n", file = "Results.txt", append = TRUE)
cat("--------------------------------------------\n", file = "Results.txt", append = TRUE)
cat(newhypothesis.summary.output[14], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
capture.output(print(VarCorr(With.Syllable.glmer), comp = c("Variance", "Std.Dev."), digits = 4), file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)
cat(newhypothesis.summary.output[20], file = "Results.txt", append = TRUE)
cat("\n", file = "Results.txt", append = TRUE)
round(x = newhypothesis.summary$coefficients, digits = 3)[,-4] -> newhypothesis.fixedeffects
capture.output(newhypothesis.fixedeffects, file = "Results.txt", append = TRUE)
cat("\n\n", file = "Results.txt", append = TRUE)
capture.output(print(Syllable.anova[c("Chi Df", "Chisq", "Pr(>Chisq)")][2,], signif.stars = FALSE), file = "Results.txt", append = TRUE)
cat("--------------------------------------------", file = "Results.txt", append = TRUE)

#####################################################
#                 End print results                 #
#####################################################

#####################################################
#                  Begin postamble                  #
#####################################################

#Remove all objects from memory
remove(list = ls(all.names = TRUE))

#Prompt user to quit R
quit(save = "ask")

#####################################################
#                   End postamble                   #
#####################################################