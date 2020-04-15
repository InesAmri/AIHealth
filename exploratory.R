  
        ###################### Running PCA ###################### 

#Load the csv file 
data=read.csv(file.choose())
#Let's see if there are missing values 
data=na.omit(data) #No missing values
#Let's make a copy of our data to help manipulate the data
dt=data
#We have two categorical variables in the begining and then two discrete variables.
#All the other variables are ordinal with 28 likert-scaled ordinal questions.
#let's create a new data frame with just the ordinal
df_lsc=data[,6:33]

#First we need to evaluate the sampling adequacy with KMO
library(psych)
KMO(df_lsc)

#We can now run PCA on the data using prcomp().
#We use screeplot() as a qualitative assessment to check how many components we should retain 
fit=prcomp(df_lsc)
screeplot(fit,type="lines")

#According to the scree plot, we should retain 2 PCs
#Let's try parallel analysis
library(MASS)
library(paran)
paran(df_lsc)

#Let's run the analysis again, this time using the pca() method
#The number of components is set to the amount we got with the Horn's analysis.
fit=pca(df_lsc,nfactors=2)
print(fit)

#We can add the component scores back into the original Data sheet for further analysis
data$RC1 <- fit$scores[,1]
data$RC2 <- fit$scores[,2]

dt$Score_Instructor <- fit$scores[,1]
dt$Score_Course <- fit$scores[,2]

      #### END PCA #######

#let's change these to factors so R doesn't treat them like numeric
data$instr=as.factor(data$instr)
data$class=as.factor(data$class)
data$attendance=as.factor(data$attendance)
data$difficulty=as.ordered(data$difficulty)
data$nb.repeat=as.ordered(data$nb.repeat)


#Let's see if there are some correlations 
#The values in the data frame 'dt' are numeric
cor.test(x=dt$difficulty,y=dt$attendance,method = "kendall")
cor.test(x=dt$difficulty,y=dt$nb.repeat,method = "kendall")

cor.test(x=dt$difficulty,y=data$RC1,method = "kendall") #Instructor 
cor.test(x=dt$difficulty,y=data$RC2,method = "kendall") #Course 

cor.test(x=dt$attendance,y=data$RC1,method = "kendall")
cor.test(x=dt$attendance,y=data$RC2,method = "kendall")


       ################ Running MANOVA on RC1/RC2 with nb.repeat ###################### 


#Let's check the parametric assumptions 
#Normality Assumption of the scores as our dependent variable 
#Let's run a Shapiro-wilk Test on RC1
shapiro.test(data$RC1) 
#Our test failed because of the large sample size, let's try a qualitative assessment

#RC1
qqnorm(data$RC1)
qqline(data$RC1, col = "red", lwd = 2)
#RC2
qqnorm(data$RC2)
qqline(data$RC2, col = "red", lwd = 2)
#fairly normal

qqnorm(data[data$instr=="1",]$RC1)
qqline(data[data$instr=="1",]$RC1, col = "red", lwd = 2)
qqnorm(data[data$instr=="2",]$RC1)
qqline(data[data$instr=="2",]$RC1, col = "red", lwd = 2)
qqnorm(data[data$instr=="3",]$RC1)
qqline(data[data$instr=="3",]$RC1, col = "red", lwd = 2)
#Somewhat normal

qqnorm(data[data$attendance=="0",]$RC1)
qqnorm(data[data$attendance=="1",]$RC1)
qqnorm(data[data$attendance=="2",]$RC1)
qqnorm(data[data$attendance=="3",]$RC1)
qqnorm(data[data$attendance=="4",]$RC1)
#Somewhat normal

qqnorm(data[data$attendance=="0",]$RC2)
qqnorm(data[data$attendance=="1",]$RC2)
qqnorm(data[data$attendance=="2",]$RC2)
qqnorm(data[data$attendance=="3",]$RC2)
qqnorm(data[data$attendance=="4",]$RC2)
qqline(data[data$attendance=="4",]$RC1, col = "red", lwd = 2)
#a little bit normal

qqnorm(data[data$difficulty=="1",]$RC2)
qqnorm(data[data$difficulty=="2",]$RC2)
qqnorm(data[data$difficulty=="3",]$RC2)
qqnorm(data[data$difficulty=="4",]$RC2)
qqnorm(data[data$difficulty=="5",]$RC2)
#okay
qqnorm(data[data$difficulty=="1",]$RC1)
qqline(data[data$difficulty=="1",]$RC1, col = "red", lwd = 2) #problem with end tail
qqnorm(data[data$difficulty=="2",]$RC1)
qqnorm(data[data$difficulty=="3",]$RC1)
qqnorm(data[data$difficulty=="4",]$RC1)
qqnorm(data[data$difficulty=="5",]$RC1) #ok

qqnorm(data[data$nb.repeat=="1",]$RC1)
qqnorm(data[data$nb.repeat=="2",]$RC1)
qqnorm(data[data$nb.repeat=="3",]$RC1)#ok

qqnorm(data[data$nb.repeat=="1",]$RC2)
qqnorm(data[data$nb.repeat=="2",]$RC2)
qqnorm(data[data$nb.repeat=="3",]$RC2)
qqline(data[data$nb.repeat=="3",]$RC2, col = "red", lwd = 2) #ok


#Homogeneity of covariance matrix 
library(heplots)
boxM(cbind(RC1,RC2) ~ instr, data = data) #Violated
boxM(cbind(RC1,RC2) ~ difficulty , data = data) #Violated
boxM(cbind(RC1,RC2) ~class, data = data) #Violated
boxM(cbind(RC1,RC2) ~attendance , data = data) #Violated

boxM(cbind(RC1,RC2) ~nb.repeat , data = data) #Not violated

#Lomogeneity of variance 
library(MASS)
library(car)  

summary(data)
#The groups are not equal 

#Homogeneity of Variance 

leveneTest(RC1~attendance,data=data)
leveneTest(RC1~class,data=data)
leveneTest(RC1~difficulty,data=data)
leveneTest(RC1~instr,data=data) 
#Violated

leveneTest(RC1~nb.repeat,data=data) #Not violated

leveneTest(RC2~attendance,data=data)
leveneTest(RC2~class,data=data)
leveneTest(RC2~difficulty,data=data)
leveneTest(RC2~instr,data=data) 
#Violated

leveneTest(RC2~nb.repeat,data=data) #Not violated

#DV's fairly normal
#Homogeneity of variance with the variable nb.repeat
#Equal group size violated
#homogeneity of co variance 
#The groups are independant 
#The dependant variable RC1 / RC2 is continuous 

#Omnibus test
fit=manova(cbind(data$RC1,data$RC2)~data$nb.repeat)
summary(fit)
summary.aov(fit)
#Independant ANOVA
fit=aov(data$RC1~data$nb.repeat)
#post hoc test
TukeyHSD(fit)

#plot
library(sjPlot)

dt$nb.repeat=as.ordered(dt$nb.repeat) #Changing type just to plot with the name "Score_Instuctor"

fit=lm(Score_Instructor~nb.repeat,data = dt)
plot_model(fit,type = "pred",ci.lvl = 0.95)

dt$nb.repeat=as.numeric(dt$nb.repeat) #Putting back as numeric cancel any changes


           ################# Running Non-parametric alternative n-way ANOVA #################
#RC1
#H0:class and difficulty does not have an effect on the perceived instructor performance 
#H1:class and difficulty does have an effect on the perceived instructor performance 
#RC2
#H0:class and difficulty does not have an effect on the perceived quality of the course
#H1:class and difficulty does have an effect on the perceived quality of the course

#Data transformation
# I = Instructor,  C=Course
library(ARTool)
I=art(RC1~difficulty*class,data=data)
C=art(RC2~difficulty*class,data=data)
#verify the ART procedure is appropriate to the dataset
summary(I)
summary(C)
#Run the Anova
anova(I)
anova(C)
#post hoc test 
library(emmeans)
emmeans(artlm(I,"class"),pairwise~"class")
emmeans(artlm(C,"class"),pairwise~"class")

#plot
dt$difficulty=as.factor(dt$difficulty)
dt$class=as.factor(dt$class)

fit=lm(Score_Instructor~difficulty,data = dt)
plot_model(fit,type = "pred",ci.lvl = 0.95)

fit=lm(Score_Course~difficulty,data = dt)
plot_model(fit,type = "pred",ci.lvl = 0.95)


#Let's Check the Assumptions

#DVs continuous 
#Groups are independant 
#Variance Violated 
#unequal Sample Size  
#Non Parametric Alternative


        
        ######## non parametric alternative to t-tests ########

#Unequal group size and Unequal Variance 

#Are the mean scores for intructor satisfaction of the people who 
#have a high level of attendance different or equal to those who have low level of attendance 
#H0: the level of attendance does not have an effect on the perceived instructor performance
#H1:the level of attendance does have an effect on the perceived instructor performance

data$attendance=as.numeric(data$attendance)

wilcox.test(data[data$attendance<"3",]$RC1,data[data$attendance>"2",]$RC1,alternative = "less")

#Are the mean scores for course satisfaction of the people who 
#have a high level of attendance different or equal to those who have low level of attendance 
#H0: the level of attendance does not have an effect on the perceived course quality
#H1:the level of attendance does have an effect on the perceived course quality

wilcox.test(data[data$attendance<"3",]$RC2,data[data$attendance>"2",]$RC2,alternative = "less")

#effect size
library(psych)
library(effsize)
cohen.d(data[data$attendance<"3",]$RC1,data[data$attendance>"2",]$RC1)
cohen.d(data[data$attendance<"3",]$RC2,data[data$attendance>"2",]$RC2)

#Let's plot this
library(gmodels)
dt$attendance=as.ordered(dt$attendance)
dt$instr=as.factor(dt$instr)

fit=lm(Score_Course~attendance,data=dt)
plot_model(fit,type="pred")

fit=lm(Score_Instructor~attendance,data=dt)
plot_model(fit,type="pred")

fit=lm(Score_Instructor~instr,data=dt)
plot_model(fit,type="pred")

data$attendance=as.ordered(data$attendance)

#### was not in the paper ####

#Are the mean scores for instructor perfomance of the people who had the instructor 1,2,3 different or equal
#H0: the type of instructor does not have an effect on the perceived instructor performance 
#H1:the type of instructor does not have an effect on the perceived instructor performance 
kruskal.test(RC1~instr,data=data)

#Are the mean scores for course quality of the people who 
#had the instructor 1,2,3 different or equal
#H0: the type of instructor does not have an effect on the perceived quality of the course
#H1:the type of instructor does not have an effect on the perceived quality of the course
kruskal.test(RC2~instr,data=data)
#get η2 by dividing the chi-squared value by N-1
122.35/(5820-1)
44.44/(5820-1)
#Let's see where the true differences in the groups are
library("car")
library(FSA)
dunnTest(RC1~instr, data = data)
dunnTest(RC2~instr, data = data)
#Significant difference between the instructor 3 and the two others for both RC1 and RC2


### END ##
