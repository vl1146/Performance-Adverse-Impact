## MAIN QUESTION - IS THERE ADVERSE IMPACT ON PERFORMANCE TEST RATINGS AND DOES THE TEST SUCCESSFULLY PREDICT PERFORMANCE? 


data1 <- read.csv("D:/desktop/Programming/R/Perfadvimpact/data.csv")

str(data1)

library(dplyr)


Male <- data1 %>% filter(Sex =="M")
Female <- data1 %>% filter(Sex =="F")
White <- data1 %>% filter(Race =="W")
Minority <- data1 %>% filter(Race =="M")

## Differences in test score based on race and gender

aov1 <- aov(Score~Race*Sex, data = data1)
summary(aov1)

# No main effect found for race, p = .094, or sex, p = .712
# No interaction could be analyzed since all minorities were female

library(ggpubr)
ggboxplot(data1, x = "Race", y = "Score", color = "Sex",
          legend="right", palette = c("#0045a5", "#00c698"))


## Differences in test score based on race

t.test(White$Score, Minority$Score, alternative = "two.sided", var.equal = FALSE)

# p value .066

library(ggpubr)
ggboxplot(data1, x = "Race", y = "Score")


## Differences in test score based on gender

t.test(Male$Score, Female$Score, alternative = "two.sided", var.equal = FALSE)

# p value .738

library(ggpubr)
ggboxplot(data1, x = "Sex", y = "Score")

## Differences in performance based on race and gender

aov2 <- aov(Performance~Race*Sex, data = data1)
summary(aov2)

# No main effect found for race, p = .339, or sex, p = .056
# No interaction could be analyzed since all minorities were female

library(ggpubr)
ggboxplot(data1, x = "Race", y = "Performance", color = "Sex",
          legend="right", palette = c("#0045a5", "#00c698"))


## Differences in performance based on race

t.test(White$Performance, Minority$Performance, alternative = "two.sided", var.equal = FALSE)

# p value .343

library(ggpubr)
ggboxplot(data1, x = "Race", y = "Performance")


## Differences in performance based on gender

t.test(Male$Performance, Female$Performance, alternative = "two.sided", var.equal = FALSE)

# p value .048

library(ggpubr)
ggboxplot(data1, x = "Sex", y = "Performance")

## Does Test Score Predict Performance

corr1 <- cor.test(data1$Performance, data1$Score, method = "pearson")
summary(corr1)

lm1 <- lm(Performance~Score, data = data1)
summary(lm1)

# The test significantly predicts performance, p = .0262, r^2 .0988

ggplot(data1, aes(Performance, Score)) + 
  geom_point() +
  labs(y = "Job Performance", x = "Test Score") +
  theme_classic() +
  theme(axis.title.y = element_text(family = "Verdana",
                                    size = 20, vjust = 2.5),
        axis.title.x = element_text(family = "Verdana",
                                    size = 20, vjust = -.5),
        axis.text.x = element_text(family = "Georgia",
                                   color = "black", size = 18),
        axis.text.y = element_text(family = "Georgia",
                                   color = "black", size = 18),
        plot.title = element_text(size = 20, family = "Verdana", 
                                  hjust = 10, face ="bold"))+
  geom_smooth(method='lm', formula= y~x, color ="purple")
