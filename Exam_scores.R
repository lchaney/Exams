##ANALYSIS FOR TEST TYPE##########

setwd("~/GitHub/Exams")

#import dataset
scores <- read_csv("scores.csv")

scores_low <- scores[which(scores$PreCat=='Low'), ]
scores_med <- scores[which(scores$PreCat=='Medium'), ]
scores_high <- scores[which(scores$PreCat=='High'), ]

scores_ml <- scores[which(scores$PreCat=='Low' | scores$PreCat=='Medium'), ]

#look for correlations
source("~/GitHub/corrPlot_v2/corrPlot_v2.R")

corrPlot2(scores[,c(3, 21, 25:27)])

#load ggplot
library(ggplot2)

ggplot(data = scores, aes(Pretest, FinalExam)) + 
         geom_point(aes(color = Treatment))

ggplot(data = scores, aes(Treatment, Test_diff)) + geom_boxplot() + facet_grid(.~ Grade)

ggplot(data = scores, aes(Treatment, FinalExam)) + geom_boxplot() + facet_grid(.~ Grade)
#good one
ggplot(data = scores, aes(Treatment, Examtotal)) + geom_boxplot() + facet_grid(.~ Grade)
ggplot(data = scores, aes(Treatment, Examtotal)) + geom_boxplot() + facet_grid(PreCat~ Grade)

ggplot(data = scores, aes(Pretest)) + geom_dotplot() + facet_grid(.~ Treatment)




ggplot(data = scores, aes(Treatment, FinalExam)) + geom_boxplot() + facet_grid(.~ PreCat)

ggplot(data = scores, aes(Treatment, Pretest)) + geom_boxplot()

ggplot(data = scores, aes(FinalExam, Test_diff) + geom_point(aes(color = Treatment)))

#regression
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

mod6 <- lm(Examtotal ~ Treatment + Test_diff + PostLTSR, scores)
anova(mod6)


mod7 <- lm(Examtotal ~ Treatment, scores)
summary(mod7)
anova(mod7)

mod8 <- lm(Examtotal ~ Treatment + PreCat, scores)
anova (mod8)

mod9 <- lm(Examtotal ~ Treatment, scores_high)
summary(mod9)
anova (mod9)


mod10 <- lm(Test_diff ~ Treatment, scores_ml)
summary(mod10)
anova (mod10)
