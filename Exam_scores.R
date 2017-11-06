##ANALYSIS FOR TEST TYPE##########

setwd("~/GitHub/Exams")

#import dataset
scores <- read_csv("scores.csv")


scores_low <- scores[which(scores$PreCat=='Low'), ]
scores_med <- scores[which(scores$PreCat=='Medium'), ]
scores_high <- scores[which(scores$PreCat=='High'), ]

scores_ml <- scores[which(scores$PreCat=='Low' | scores$PreCat=='Medium'), ]

scores$Test_diff2 <- (scores$FinalExam - (2*scores$Pretest))

#look for correlations
source("~/GitHub/corrPlot_v2/corrPlot_v2.R")

corrPlot2(scores[,c(3,5:9, 19:21, 23:24, 25:27)])

corrPlot2(scores[,c(3,5:6, 20, 23:24)])

#load ggplot
library(ggplot2)
library(wesanderson)



###regression
#is there a significant treatment effect on final exam score?
modfin <- lm(FinalExam ~ Treatment, scores)
anova(modfin)
  #no treatment
modfinfull <- lm(FinalExam ~ Treatment + Pretest + PreLTSR + PreMATE + Homework_Total + Participation_Total 
                 + Writing_Total + Examtotal, scores)
anova(modfinfull)
  #no treatment, no writing, no participation, no pre-mate
modfinfullre <- lm(FinalExam ~ Treatment + Pretest + PreLTSR + Homework_Total + Examtotal, scores)
anova(modfinfullre)
  #no treatment

#now look at if they were low pretest students
modfinlow <- lm(FinalExam ~ Treatment, scores_low)
anova(modfinlow)
  #no treatment

#med pretest students
modfinmed <- lm(FinalExam ~ Treatment, scores_med)
anova(modfinmed)
###ALMOST####
#no treatment

#med and low pretest studetns
modfinml <- lm(FinalExam ~ Treatment, scores_ml)
anova(modfinml)
#no treatment

#high pretest students
modfinh <- lm(FinalExam ~ Treatment, scores_high)
anova(modfinh)
#no treatment

#look more at med pretest students
modfinmed_full <- lm(FinalExam ~ Treatment + PreLTSR + PreMATE + Homework_Total + Participation_Total 
                        + Writing_Total + Examtotal, scores)
anova(modfinmed_full)
#no treatment

modfinmed_fullre <- lm(FinalExam ~ Treatment + PreLTSR + Homework_Total + Examtotal, scores)
anova(modfinmed_fullre)
#no treatment

####PLOT 
#med pretest students
library(ggplot2)
ggplot(data = scores_med, aes(Treatment, FinalExam)) + geom_boxplot()
ggplot(data = scores, aes(Treatment, FinalExam)) + geom_boxplot()
ggplot(data = scores, aes(Treatment, Test_diff)) + geom_boxplot()
ggplot(data = scores, aes(Treatment, Test_diff2)) + geom_boxplot()
ggplot(data = scores, aes(Treatment, FinalExam)) + geom_boxplot() + facet_grid(.~Grade)



ggplot(data = scores, aes(2*Pretest, FinalExam)) + 
  geom_point(aes(color = Treatment, shape = Treatment)) + 
  #geom_smooth(aes(group = Treatment, color = Treatment), method = "lm") +
  facet_grid (.~Treatment) +
  labs(x = "Pre-Test", y = "Post-Test") +
  theme_bw() + 
  scale_color_manual(values = wes_palette("Darjeeling"))  #colors


####Look at test difference
modtestdiff <- lm(Test_diff2 ~ Treatment, scores)
anova(modtestdiff)
#no treatment

modtestdiff_full <- lm(Test_diff2 ~ Treatment + PreLTSR + PreMATE + Homework_Total + Participation_Total 
                       + Writing_Total + Examtotal, scores)
anova(modtestdiff_full)

modtestdiff_fullre <- lm(Test_diff2 ~ Treatment + PreLTSR + Homework_Total + Examtotal + FINALGRADE, scores)
anova(modtestdiff_fullre)


ggplot(data = scores, aes(Treatment, Test_diff2)) + 
  geom_boxplot(aes(color = Treatment)) + 
  facet_grid(.~ Grade) +
  labs(y = "Pre/Post-Test Difference") +
  theme_bw() +
  scale_color_manual(values = wes_palette("Darjeeling"))  #colors


ggplot(data = scores, aes(Treatment, LTSR_diff)) + 
  geom_boxplot(aes(color = Treatment)) + 
  #facet_grid(.~ Grade) +
  theme_bw() +
  scale_color_manual(values = wes_palette("Darjeeling")) 


ggplot(data = scores, aes(Treatment, Examtotal)) + 
  geom_boxplot(aes(color = Treatment)) + 
  facet_grid(.~ Grade) +
  theme_bw() +
  scale_color_manual(values = wes_palette("Darjeeling")) 

ggplot(data = scores, aes(Treatment, Examtotal)) + 
  geom_boxplot(aes(color = Treatment)) + 
  facet_grid(.~ PreCat) +
  theme_bw() +
  labs(x = "") +
  scale_color_manual(values = wes_palette("Darjeeling")) 



###Look at Exam Totals
modexamtot <- lm(Examtotal ~ Treatment, scores)
anova(modexamtot)


modexamtotfull <- lm(Examtotal ~ Treatment + PreLTSR + PreMATE + Homework_Total + Participation_Total 
                     + Writing_Total + Examtotal, scores)
anova(modexamtotfull)

modexamtotfullre <- lm(Examtotal ~ Treatment + PreLTSR  + Homework_Total + Participation_Total 
                     + Writing_Total + Examtotal, scores)
anova(modexamtotfullre)

mod3 <- lm(Examtotal ~ Treatment + Pretest + PreLTSR + PreMATE + Homework_Total + Participation_Total 
           + Writing_Total + FinalExam, scores)
anova(mod3)

modx <- lm(Examtotal ~ Treatment + Pretest + PreLTSR + Homework_Total + Participation_Total 
           + Writing_Total + FinalExam, scores)
summary(modx)
anova(modx)
###SIGNIFICANT!

modxre <- lm(Examtotal ~ Treatment + Pretest + PreLTSR, scores)
summary(modxre)
anova(modxre)


#look at before and afters by treatment
library(reshape2)
library(tidyr)
library(dplyr)
scores %>% group_by(Treatment) %>% summarise(avgPreTest = 2*mean(Pretest, na.rm = TRUE), 
                                             avgPreLTSR = mean(PreLTSR, na.rm = TRUE), 
                                             avgPreMate = mean(PreMATE, na.rm = TRUE),
                                             avgFinal = mean(FinalExam, na.rm = TRUE),
                                             avgPostLTSR = mean(PostLTSR, na.rm = TRUE),
                                             avgPostMate = mean(PostMATE, na.rm = TRUE))

medscores <- scores %>% group_by(Treatment) %>% summarise(medPreTest = 2*median(Pretest, na.rm = TRUE), 
                                             medPreLTSR = median(PreLTSR, na.rm = TRUE), 
                                             medPreMate = median(PreMATE, na.rm = TRUE),
                                             medFinal = median(FinalExam, na.rm = TRUE),
                                             medPostLTSR = median(PostLTSR, na.rm = TRUE),
                                             medPostMate = median(PostMATE, na.rm = TRUE))



scores %>% group_by(Treatment) %>% summarise(minPreTest = 2*min(Pretest, na.rm = TRUE), 
                                             minPreLTSR = min(PreLTSR, na.rm = TRUE), 
                                             minPreMate = min(PreMATE, na.rm = TRUE),
                                             minFinal = min(FinalExam, na.rm = TRUE),
                                             minPostLTSR = min(PostLTSR, na.rm = TRUE),
                                             minPostMate = min(PostMATE, na.rm = TRUE))

scores %>% group_by(Treatment) %>% summarise(maxPreTest = 2*max(Pretest, na.rm = TRUE), 
                                             maxPreLTSR = max(PreLTSR, na.rm = TRUE), 
                                             maxPreMate = max(PreMATE, na.rm = TRUE),
                                             maxFinal = max(FinalExam, na.rm = TRUE),
                                             maxPostLTSR = max(PostLTSR, na.rm = TRUE),
                                             maxPostMate = max(PostMATE, na.rm = TRUE))

p1 <- ggplot(data = scores, aes(Treatment, Pretest)) + geom_violin(aes(color=Treatment)) + geom_boxplot(width=.1, aes(color=Treatment)) + theme_bw() + scale_color_manual(values = wes_palette("Darjeeling")) + theme(legend.position="none") + scale_y_continuous(limits=c(35,215)) + ylab("PreTest")
p2 <- ggplot(data = scores, aes(Treatment, PreLTSR)) + geom_violin(aes(color=Treatment)) + geom_boxplot(width=.1, aes(color=Treatment)) + theme_bw() + scale_color_manual(values = wes_palette("Darjeeling")) + theme(legend.position="none") + scale_y_continuous(limits=c(2,24))
p3 <- ggplot(data = scores, aes(Treatment, PreMATE))  + geom_violin(aes(color=Treatment)) + geom_boxplot(width=.1, aes(color=Treatment)) + theme_bw() + scale_color_manual(values = wes_palette("Darjeeling")) + theme(legend.position="none") + scale_y_continuous(limits=c(39,100))
p4 <- ggplot(data = scores, aes(Treatment, FinalExam))  + geom_violin(aes(color=Treatment)) + geom_boxplot(width=.1, aes(color=Treatment)) + theme_bw() + scale_color_manual(values = wes_palette("Darjeeling"))+ theme(legend.position="none") + scale_y_continuous(limits=c(35,215))+ ylab("PostTest")
p5 <- ggplot(data = scores, aes(Treatment, PostLTSR))  + geom_violin(aes(color=Treatment)) + geom_boxplot(width=.1, aes(color=Treatment)) + theme_bw() + scale_color_manual(values = wes_palette("Darjeeling"))  + theme(legend.position="none") + scale_y_continuous(limits=c(2,24))
p6 <- ggplot(data = scores, aes(Treatment, PostMATE))  + geom_violin(aes(color=Treatment)) + geom_boxplot(width=.1, aes(color=Treatment)) + theme_bw() + scale_color_manual(values = wes_palette("Darjeeling"))+ theme(legend.position="none") + scale_y_continuous(limits=c(39,100))

source("~/Documents/GitHub/Exams/multiplot.R")

multiplot(p1, p4, p2, p5, p3, p6, cols=3)
multiplot(p1, p2, p3, p4, p5, p6, cols=2)

ggplot(data = scores, aes(Pretest, PreLTSR)) + geom_point(aes(color = Treatment))

ggplot(data = scores, aes(PreMATE, PreLTSR)) + geom_raster(aes(fill=Pretest))



modsr <- lm(PostLTSR ~ Treatment, scores)
summary(modsr)
anova(modsr)
###SIGNIFICANT!


anova(lm(Pretest ~ Treatment, scores))
anova(lm(FinalExam ~ Treatment, scores))
anova(lm(PreLTSR ~ Treatment, scores))
anova(lm(PostLTSR ~ Treatment, scores)) #YAHOO!
anova(lm(PreMATE ~ Treatment, scores))
anova(lm(PostMATE ~ Treatment, scores))



mod <- lm(FinalExam ~ Treatment + Pretest + PreLTSR + PreMATE + Homework_Total + Participation_Total 
            + Writing_Total + Examtotal + Grade, scores)
anova(mod)

mod2 <- lm(FinalExam ~ Treatment + Pretest + PreLTSR + Homework_Total + Examtotal + Grade, scores)
anova(mod2)

mod3 <- lm(Examtotal ~ Treatment + Pretest + PreLTSR + PreMATE + Homework_Total + Participation_Total 
          + Writing_Total + FinalExam, scores)
anova(mod3)

mod4 <- lm(Examtotal ~ Treatment + Pretest + PreLTSR + Homework_Total + Participation_Total 
           + Writing_Total + FinalExam, scores)
anova(mod4)

mod5 <- lm(Examtotal ~ Treatment + FinalExam + PostLTSR, scores)
anova(mod5)


mod6 <- lm(Examtotal ~ Treatment + Pretest + FinalExam + PostLTSR, scores)
anova(mod6)
#sig tratment

mod6 <- lm(Examtotal ~ Treatment + Test_diff + PostLTSR, scores)
anova(mod6)
#sig

mod7 <- lm(Examtotal ~ Treatment, scores)
summary(mod7)
anova(mod7)

mod8 <- lm(Examtotal ~ Treatment + PreCat, scores)
anova (mod8)

mod9 <- lm(Examtotal ~ Treatment, scores_high)
summary(mod9)
anova (mod9)
#sig

mod10 <- lm(Test_diff ~ Treatment, scores_ml)
summary(mod10)
anova (mod10)


scores$Treatment <- as.factor(scores$Treatment)
summary(scores$Treatment)