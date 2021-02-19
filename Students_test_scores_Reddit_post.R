### 3D Printed Heart Model Simulated Data and Analysis ###

# Feel free to use any aspect and/or this entire script! 

###########################################################

#Consider the problem

## There are two primary variables: 1) presence/absence of the heart model and 2) test
## The heart model variable is a nominal variable with values of yes or no (ie + or -)
## The test variable is ordinal having values of test 1 or test 2. To note, more tests could be added at later time points.

#Assumptions

## I'll assume that we're comparing 2 different classes of equal size for this problem. I'll also assume that any other
## potentially impactful variables of each student (e.g. sex, aptitude, temperament, etc.) are equally distributed between
## each class. I'll say that you've got 25 students per class.

## To note, the suggestion by user standard_error to use block randomization is a very good suggestion. Ie this prevents
## all students in class A having the model while all students in class B lack the model. This introduces method bias into
## your set-up that could only be alleviated by repeating this experiment over multiple semesters/years and randomizing which class
## is A or B. For the purposes of this demonstration, I'm going to assume that block randomization is not feasible 
## (i.e. students would notice if half of their classmates have a model that they don't get to use). If this is feasible,
## feel free to reach back out to me, and I can rewrite this script to account for that

#Library
library(dplyr)
library(tidyr)
library(ggplot2)
library(ez)

#Simulate the data

set.seed(123)

df <- data.frame(student_id = c(1:50), #An arbitrary id variable that uniquely identifies each student
           heart_model = c(rep("yes", 25), rep("no", 25)), #This variable designates the heart model and essentially designates class A vs. class B
           test1_scores = c(rnorm(25, 85, 15), #I'm assuming that heart+ students will have a mean test score of 85 with standard deviation (sd) of 15
                            rnorm(25, 70, 15)) #I'm assuming that hear- students will have a mean test score of 70 with the same sd
           )

df_cor <- df %>% 
  mutate(test1_scores = sapply(df$test1_scores, function(x) ifelse(x > 100, 100, x))) %>% #this will cap scores at 100 if the rnorm function pushes them above 100
  mutate(test2_scores = ifelse(heart_model == "yes", 
                               yes = test1_scores + rnorm(1, -10, 5), #Test scores will only drop by 10 points with the heart model
                               no = test1_scores + rnorm(1, -15, 5))) #Test scores will drop by 15 points for no heart model

#Let's plot the test 1 scores as a function of the heart model variable

ggplot(df_cor, aes(heart_model, test1_scores))+
  geom_boxplot()+
  geom_jitter()+
  ggtitle("Boxplot (Median and IQRs) of Test Scores")+
  theme_classic() #This just makes it look a bit better

## Alternatively, we could look at the mean and standard deviation

ggplot(df_cor, aes(heart_model, test1_scores))+
  stat_summary(geom = "crossbar", fun.data = "mean_sdl",
               fun.args = list(mult = 1))+
  geom_jitter()+
  ggtitle("Mean and SD of Test Scores")+
  theme_classic() #This just makes it look a bit better

#Plot the change in scores over time

df_cor %>% 
  pivot_longer(cols = c(test1_scores, test2_scores),
               names_to = "test",
               values_to = "scores") %>% 
  ggplot(., aes(test, scores, color = heart_model, group = student_id))+
  geom_line(alpha = 0.5)+
  geom_point(size = 3, alpha = 0.5)+
  ggtitle("Difference in Test Scores between Heart Model Groups")+
  theme_classic()

#Consider your question

## Now, your question is really: "Does the heart model improve student's retention of the material?" You're assessing whether
## there is an interaction between the use of the heart model and students' abilities to perform on tests reviewing material
## they learned previously. We can test this using a two-way ANOVA mixed model (completely random on variable heart_model 
## and repeated measures on variable test)

#ANOVA
df_cor %>% 
  pivot_longer(cols = c(test1_scores, test2_scores),
               names_to = "test",
               values_to = "scores") %>% 
  mutate(student_id = as.factor(student_id),
         test = as.factor(test)) %>% 
  ezANOVA(dv = scores, 
          wid = student_id,
          between = heart_model,
          within = test)

## Here, from this ANOVA test, we see that there is a significant interaction between the use of the heart model and students'
## performances on the two tests. The hypothesis test portion of this is done. It is not appropriate to test the main effects of each variable
## given that there is a significant interaction at play. Instead, we can just collect the descriptive summary statistics (ie the difference in means, sd, etc.)
## in a summary table.

#Summary table of the descriptive statistics to compare them between the group

df_cor %>% 
  pivot_longer(cols = c(test1_scores, test2_scores),
               names_to = "test",
               values_to = "scores") %>% 
  group_by(heart_model, test) %>% 
  summarise(mean = mean(scores), sd = sd(scores), range = max(scores) - min(scores),
            best = max(scores))
