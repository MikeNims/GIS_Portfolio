#Library packages
library(desc)
library(DescTools)
library(dplyr)
library(ggplot2)
library(pastecs)
library(psych)

#_______________________________________________________________________

#Initial Plots
aggregate(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$tot_cases, by=list(State=United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state), FUN=sum)
aggregate(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$tot_death, by=list(State=United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state), FUN=sum)
#TOTAL cases by state
barplot(Covid_deaths_by_April_12_2021$tot_cases)
pie(Covid_deaths_by_April_12_2021$tot_cases)
#total deaths by state
barplot(Covid_deaths_by_April_12_2021$tot_death)
pie(Covid_deaths_by_April_12_2021$tot_death)
#____________________________________________________________________________________________________

#Organizing States for study
#Graphs presented for each state
# Central tendency, confidence intervals, st dev


#Louisiana 
Louisiana <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="LA"),]
barplot(Louisiana$tot_cases~Louisiana$submission_date,ylim = c(0,440142),col = "Red")
barplot(Louisiana$tot_death~Louisiana$submission_date,ylim = c(0,10000),col = "Red")
boxplot(Louisiana$tot_cases,Louisiana$tot_death,col = "Red")
#Louisiana central tendency
summary(Louisiana$tot_cases)
summary(Louisiana$tot_death)
stat.desc(Louisiana$tot_cases,basic = F)
stat.desc(Louisiana$tot_death,basic = F)
describe(Louisiana$tot_cases)
describe(Louisiana$tot_death)
hist(Louisiana$new_case,main = "LA New Cases Per Day",col = "Red")
describe(Louisiana$new_case)
hist(Louisiana$new_death,main = "LA New Deaths Per Day",col = "Red")
describe(Louisiana$new_death)
plot(Louisiana$tot_cases,Louisiana$tot_death,col = "Red")


#Kentucky
Kentucky <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="KY"),]
barplot(Kentucky$tot_cases~Kentucky$submission_date,ylim = c(0,440142),col = "Orange")
barplot(Kentucky$tot_death~Kentucky$submission_date,ylim = c(0,10000),col = "Orange")
boxplot(Kentucky$tot_cases,Kentucky$tot_death,col = "Orange")
#Kentucky central tendency
summary(Kentucky$tot_cases)
summary(Kentucky$tot_death)
stat.desc(Kentucky$tot_cases,basic = F)
stat.desc(Kentucky$tot_death,basic = F)
describe(Kentucky$tot_cases)
describe(Kentucky$tot_death)
hist(Kentucky$new_case,main = "Ky New Cases Per Day",col = "Orange")
describe(Kentucky$new_case)
hist(Kentucky$new_death,main = "KY New Deaths Per Day",col = "Orange")
describe(Kentucky$new_death)
plot(Kentucky$tot_cases,Kentucky$tot_death,col = "Orange")


#Oregon
Oregon <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="OR"),]
barplot(Oregon$tot_cases~Oregon$submission_date,ylim = c(0,440142),col = "Yellow")
barplot(Oregon$tot_death~Oregon$submission_date,ylim = c(0,10000),col = "Yellow")
boxplot(Oregon$tot_cases,Oregon$tot_death,col = "Yellow")
#Oregon central tendency
summary(Oregon$tot_cases)
summary(Oregon$tot_death)
stat.desc(Oregon$tot_cases,basic = F)
stat.desc(Oregon$tot_death,basic = F)
describe(Oregon$tot_cases)
describe(Oregon$tot_death)
hist(Oregon$new_case,main = "OR New Cases Per Day",col = "Yellow")
describe(Oregon$new_case)
hist(Oregon$new_death,main = "OR New Deaths Per Day",col = "Yellow")
describe(Oregon$new_death)
plot(Oregon$tot_cases,Oregon$tot_death,col = "Yellow")



#Oklahoma <- Data[which(Data$state=='OK'),]
Oklahoma <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="OK"),]
barplot(Oklahoma$tot_cases~Oklahoma$submission_date,ylim = c(0,440142),col = "Green")
barplot(Oklahoma$tot_death~Oklahoma$submission_date,ylim = c(0,10000),col = "Green")
boxplot(Oklahoma$tot_cases,Oklahoma$tot_death,col = "Green")
#Oklahoma central tendency
summary(Oklahoma$tot_cases)
summary(Oklahoma$tot_death)
stat.desc(Oklahoma$tot_cases,basic = F)
stat.desc(Oklahoma$tot_death,basic = F)
describe(Oklahoma$tot_cases)
describe(Oklahoma$tot_death)
hist(Oklahoma$new_case,main = "OK New Cases Per Day",col = "Green")
describe(Oklahoma$new_case)
hist(Oklahoma$new_death,main = "OK New Deaths Per Day",col = "Green")
describe(Oklahoma$new_death)
plot(Oklahoma$tot_cases,Oklahoma$tot_death,col = "Green")



#Connecticut
Connecticut <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="CT"),]
barplot(Connecticut$tot_cases~Connecticut$submission_date,ylim = c(0,440142),col = "Blue")
barplot(Connecticut$tot_death~Connecticut$submission_date,ylim = c(0,10000),col = "Blue")
boxplot(Connecticut$tot_cases,Connecticut$tot_death,col = "Blue")
#Connecticut central tendency
summary(Connecticut$tot_cases)
summary(Connecticut$tot_death)
stat.desc(Connecticut$tot_cases,basic = F)
stat.desc(Connecticut$tot_death,basic = F)
describe(Connecticut$tot_cases)
describe(Connecticut$tot_death)
hist(Connecticut$new_case,main = "CT New Cases Per Day",col = "Blue")
describe(Connecticut$new_case)
hist(Connecticut$new_death,main = "CT New Deaths Per Day",col = "Blue")
describe(Connecticut$new_death)
plot(Connecticut$tot_cases,Connecticut$tot_death,col = "Blue")



#Utah
Utah <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="UT"),]
barplot(Utah$tot_cases~Utah$submission_date,ylim = c(0,440142),col = "Turquoise")
barplot(Utah$tot_death~Utah$submission_date,ylim = c(0,10000),col = "Turquoise")
boxplot(Utah$tot_cases,Utah$tot_death,col = "Turquoise")
#Utah central tendency
summary(Utah$tot_cases)
summary(Utah$tot_death)
stat.desc(Utah$tot_cases,basic = F)
stat.desc(Utah$tot_death,basic = F)
describe(Utah$tot_cases)
describe(Utah$tot_death)
hist(Utah$new_case,main = "UT New Cases Per Day",col = "Turquoise")
describe(Utah$new_case)
hist(Utah$new_death,main = "UT New Deaths Per Day",col = "Turquoise")
describe(Utah$new_death)
plot(Utah$tot_cases,Utah$tot_death,col = "Turquoise")


#Iowa
Iowa <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="IA"),]
barplot(Iowa$tot_cases~Iowa$submission_date,ylim = c(0,440142),col = "Purple")
barplot(Iowa$tot_death~Iowa$submission_date,ylim = c(0,10000),col = "Purple")
boxplot(Iowa$tot_cases,Iowa$tot_death,col = "Purple") 
#Iowa central tendency
summary(Iowa$tot_cases)
summary(Iowa$tot_death)
stat.desc(Iowa$tot_cases,basic = F)
stat.desc(Iowa$tot_death,basic = F)
describe(Iowa$tot_cases)
describe(Iowa$tot_death)
hist(Iowa$new_case,main = "IA New Cases Per Day",col = "Purple")
describe(Iowa$new_case)
hist(Iowa$new_death,main = "IA New Deaths Per Day",col = "Purple")
describe(Iowa$new_death)
plot(Iowa$tot_cases,Iowa$tot_death,col = "Purple")


# Extra States, not part of study___________________________________________________________
#Puerto_Rico
Puerto_Rico <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="PR"),]
barplot(Puerto_Rico$tot_cases~Puerto_Rico$submission_date,ylim = c(0,440142),col = "Purple")
barplot(Puerto_Rico$tot_death~Puerto_Rico$submission_date,ylim = c(0,10000),col = "Purple")
boxplot(Puerto_Rico$tot_cases,Puerto_Rico$tot_death,col = "Purple") 


#Kansas
Kansas <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="KS"),]
barplot(Kansas$tot_cases~Kansas$submission_date,ylim = c(0,440142),col = "Orange")
barplot(Kansas$tot_death~Kansas$submission_date,ylim = c(0,10000),col = "Orange")
boxplot(Kansas$tot_cases,Kansas$tot_death,col = "Orange")
Texas <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="TX"),]
Arkansas <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time[which(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time$state=="AK"),]

#BOXPLOTS/ANOVA/Central Tendency__________________________________________________________________________________________

#CASES
#Boxplot cases
boxplot(Louisiana$tot_cases,Kentucky$tot_cases,Oregon$tot_cases,Oklahoma$tot_cases,Connecticut$tot_cases,Utah$tot_cases,Iowa$tot_cases)
#ANOVA cases
ANOVA_COVID_cases_selected_States <-aov(tot_cases~state, data= Covid_Cases_Over_Time_selected_states)
summary(ANOVA_COVID_cases_selected_States)
#boxplot new cases
boxplot(Louisiana$new_case,Kentucky$new_case,Oregon$new_case,Oklahoma$new_case,Connecticut$new_case,Utah$new_case,Iowa$new_case)
#Central tendency cases
summary(Covid_Cases_Over_Time_selected_states$tot_cases)
stat.desc(Covid_Cases_Over_Time_selected_states$tot_cases,basic = F)
describe(Covid_Cases_Over_Time_selected_states$tot_cases)
hist(Covid_Cases_Over_Time_selected_states$new_case,main = "New Cases Per Day")

#DEATHS
#Boxplot Deaths
boxplot(Louisiana$tot_death,Kentucky$tot_death,Oregon$tot_death,Oklahoma$tot_death,Connecticut$tot_death,Utah$tot_death,Iowa$tot_death)
#ANOVA Deaths
ANOVA_COVID_DEATHS_Selected_States <-aov(tot_death~state, data= Covid_Cases_Over_Time_selected_states)
summary(ANOVA_COVID_DEATHS_Selected_States)
#boxplot new deaths
boxplot(Louisiana$new_death,Kentucky$new_death,Oregon$new_death,Oklahoma$new_death,Connecticut$new_death,Utah$new_death,Iowa$new_death)
#Central tendency deaths
summary(Covid_Cases_Over_Time_selected_states$tot_death)
stat.desc(Covid_Cases_Over_Time_selected_states$tot_death,basic = F)
describe(Covid_Cases_Over_Time_selected_states$tot_death)
hist(Covid_Cases_Over_Time_selected_states$new_death,main = "New Deaths Per Day")

#Correlation and regression______________________________________________________________________________
#Total Cases/deaths
plot(Covid_Cases_Over_Time_selected_states$tot_cases~Covid_Cases_Over_Time_selected_states$tot_death,col = "blue")
abline(lm(Covid_Cases_Over_Time_selected_states$tot_cases~Covid_Cases_Over_Time_selected_states$tot_death),col = "red")
cor.test(Covid_Cases_Over_Time_selected_states$tot_cases,Covid_Cases_Over_Time_selected_states$tot_death)
Covid_tot_cases_tot_dearhs_reg <-lm(Covid_Cases_Over_Time_selected_states$tot_cases~Covid_Cases_Over_Time_selected_states$tot_death)
Covid_tot_cases_tot_dearhs_reg

#New Cases/Deaths
plot(Covid_Cases_Over_Time_selected_states$new_case~Covid_Cases_Over_Time_selected_states$new_death,xlab = "New Cases", ylab= "New Deaths", col = "blue")
abline(lm(Covid_Cases_Over_Time_selected_states$new_case~Covid_Cases_Over_Time_selected_states$new_death),col = "red")
cor.test(Covid_Cases_Over_Time_selected_states$new_case,Covid_Cases_Over_Time_selected_states$new_death)
Covid_new_cases_new_deaths_reg <-lm(Covid_Cases_Over_Time_selected_states$new_case~Covid_Cases_Over_Time_selected_states$new_death)
Covid_new_cases_new_deaths_reg
summary(Covid_new_cases_new_deaths_reg)
predict(Covid_new_cases_new_deaths_reg)

#Extra ANOVA Code and greaphs ______________________________________________________________________________
#Example boxplots
boxplot(Oklahoma$tot_cases,Kansas$tot_cases,col = "red")
boxplot(Oklahoma$tot_death,Kansas$tot_death,col = "red")


#Deaths ANOVA by state
anova_louisiana <- aov(Oklahoma$tot_death~Louisiana$tot_death)
summary(anova_louisiana)

anova_Kentucky <- aov(Oklahoma$tot_death~Kentucky$tot_death)
summary(anova_Kentucky)

anova_Oregon <- aov(Oklahoma$tot_death~Oregon$tot_death)
summary(anova_Oregon)

anova_Connecticut <-aov(Oklahoma$tot_death~Connecticut$tot_death)
summary(anova_Connecticut)

anova_Utah <-aov(Oklahoma$tot_death~Utah$tot_death)
summary(anova_Utah)

anova_Iowa <-aov(Oklahoma$tot_death~Iowa$tot_death)
summary(anova_Iowa)

#Example ANOVA
ANOVA_COVID_cases_selected_States <-aov(Covid_Cases_Over_Time_selected_states$'state' ~Covid_Cases_Over_Time_selected_states$'tot_cases')
summary(ANOVA_COVID_cases_selected_States)

ANOVA_Cases_by_state <-aov(Louisiana$tot_cases,Kentucky$tot_cases,Oregon$tot_cases,Oklahoma$tot_cases,Connecticut$tot_cases,Utah$tot_cases,Iowa$tot_cases)

anova_Cases <- aov(wheat$`Wheat Yields`~wheat$`Time Period`)
anova_Cases <- aov(Oklahoma$tot_cases~Kansas$tot_cases)
anova_Cases
summary(anova_Cases)
#____________________________________________________________________________________________________________

#Multiple States <- Data[which(Data$Month=='OK'|Data$Month=='CA'),]

#ggplot2 bar chart
library(ggplot2)
test <- ggplot(data = Oklahoma) +
  geom_bar(aes(x = submission_date, y = tot_cases), stat = "identity") +
  scale_y_continuous()
test


#TOTAL cases by state labeled
ggplot(data = Covid_deaths_by_April_12_2021)+ 
  geom_bar(aes(x = state, y =tot_cases ), stat = "identity")

ggplot(data = Covid_deaths_by_April_12_2021)+ 
  geom_bar(aes(x = state, y =tot_cases ), stat = "identity")+
  coord_polar()


#TOTAL deaths by state
ggplot(data = Covid_deaths_by_April_12_2021)+
  geom_bar(aes(x = state, y =tot_death), stat = "identity")

ggplot(data = Covid_deaths_by_April_12_2021)+
  geom_bar(aes(x = state, y =tot_death), stat = "identity")+
  coord_polar()

#_______________________________________________________________________________
#Multiple regression

View(Covid_Cases_Over_Time_selected_states)

Covid_Cases_Over_Time_LM <-lm(submission_date ~ tot_cases + tot_death + new_case + new_death, data = Covid_Cases_Over_Time_selected_states)


Covid_Cases_Over_Time_LM

ANOVA_Covid_Cases_Over_Time_LM <-aov(Covid_Cases_Over_Time_LM)

summary(ANOVA_Covid_Cases_Over_Time_LM)

coef(Covid_Cases_Over_Time_LM)
#___________________________________________________________________________________
#stepwise model
#build a constant model
Submission_Date_LM <-lm(submission_date ~1,data=Covid_Cases_Over_Time_selected_states)

Total_Cases_LM <-lm(tot_cases~1,data=Covid_Cases_Over_Time_selected_states)
TOTAL_CASES_FORWARD_STEPWISE <-step(Total_Cases_LM,direction = "forward",
                                    scope = (~tot_death+new_case+new_death),data = Covid_Cases_Over_Time_selected_states, trace = 0)
ANOVA_TOTAL_CASES_STEPWISE <-aov(TOTAL_CASES_FORWARD_STEPWISE)
ANOVA_TOTAL_CASES_STEPWISE
summary(ANOVA_TOTAL_CASES_STEPWISE)
coef(ANOVA_TOTAL_CASES_STEPWISE)
confint(ANOVA_TOTAL_CASES_STEPWISE)
resid(ANOVA_TOTAL_CASES_STEPWISE)
boxplot(residuals(ANOVA_TOTAL_CASES_STEPWISE),col = "tan")
summary(TOTAL_CASES_FORWARD_STEPWISE)

#HW assignment_________________________________________________________________________________________________
#forwards stepwise variables selection

Oklahoma_tot_cases_LM <-lm(tot_cases~1,data = Oklahoma)
OK_Forwards_stepwise <-step(Oklahoma_tot_cases_LM,direction = "forward",
                            scope = (~tot_death+new_case+new_death), data = Oklahoma, trace = 0)
ANOVA_OK_Forwards_stepwise <-aov(OK_Forwards_stepwise)
ANOVA_OK_Forwards_stepwise
summary(ANOVA_OK_Forwards_stepwise)
coef(ANOVA_OK_Forwards_stepwise)
confint(ANOVA_OK_Forwards_stepwise)
resid(ANOVA_OK_Forwards_stepwise)
boxplot(residuals(ANOVA_OK_Forwards_stepwise),col = "green")

summary(OK_Forwards_stepwise)
#_____________________________________________________________________
#forwards stepwise variables selection DOES NOT CURRENTLY WORK
Forwars_Stepwise <-step(Submission_Date_LM,direction = "forward",
                        scope = (~tot_cases+tot_death+new_case+new_death), data = Covid_Cases_Over_Time_selected_states, trace = 0)
#______________________________________________________________________
#CHI-squared
summary(Covid_Cases_Over_Time_selected_states$tot_cases)
summary(Covid_Cases_Over_Time_selected_states$tot_death)

#New Case Chi
summary(Covid_Cases_Over_Time_selected_states$new_case)
Chi_table_New_Case <- table(Chi_Covid_Cases_Over_Time_selected_states$state,Chi_Covid_Cases_Over_Time_selected_states$new_case_quartile)
Chi_table_New_Case
round(prop.table(Chi_table_New_Case,1),2)*100
CHi_Test_New_Case <-chisq.test(Chi_table_New_Case)
CHi_Test_New_Case
CHi_Test_New_Case$expected

#New Death Chi
summary(Covid_Cases_Over_Time_selected_states$new_death)
Chi_table_New_Death <- table(Chi_Covid_Cases_Over_Time_selected_states$state,Chi_Covid_Cases_Over_Time_selected_states$new_death_quartile)
Chi_table_New_Death
round(prop.table(Chi_table_New_Death,1),2)*100
Chi_Test_New_Death <-chisq.test(Chi_table_New_Death)
Chi_Test_New_Death
Chi_Test_New_Death$expected
#Principal Components Analysis_____________________________________________________________________________________

COVID_PCA <-prcomp(Covid_Cases_Over_Time_selected_states,center = TRUE,scale. = TRUE)

Covid_PCA_1_Var <-Covid_PCA_book[,c(3:6)]
Covid_PCA_2 <-prcomp(Covid_PCA_1_Var,center = TRUE,scale. = TRUE)
summary(Covid_PCA_2)
plot(Covid_PCA_2,main = "Covid Infections and Deaths Scree Plot",col = "blue")
Covid_PCA_2
Covid_PCA_loadings <-predict(Covid_PCA_2)
write.csv(Covid_PCA_loadings,file = "Covid_PCA_loadings.csv")
View(Covid_PCA_loadings)
# Cluster analysis__________________________________________________________________________________________________
Covid_PCA_loadings

#Hierarchical clustering
#dissimilarity matrix

DisMatrix <- dist(Covid_PCA_loadings)

#Use distance matrix for clustering

ClusterAnalysis1 <-hclust(DisMatrix)
ClusterAnalysis1
plot(ClusterAnalysis1, main = "COVID-19 Cluster Dendrogram, 
     Total Cases, Total Fatalities with daily New Case and Death Rate ")

#put observations into groups
#need to specify either k groups or h groups
ClusterAnalysis_Cut4 <- cutree(ClusterAnalysis1, k = 5)
ClusterAnalysis_Cut4
ClusterAnalysis_Cut10 <- cutree(ClusterAnalysis1, k = 10)
ClusterAnalysis_Cut10

ClusterAnalysis_Cut25 <-cutree(ClusterAnalysis1, k = 5:10)
ClusterAnalysis_Cut25

rect.hclust(ClusterAnalysis1, k = 2, border = "blue")
rect.hclust(ClusterAnalysis1, k = 4, border = "darkred")
rect.hclust(ClusterAnalysis1, k = 8, border = "green4")


#K-means clustering
Covid_Kmeans <- kmeans(Covid_PCA_loadings, 4)
Covid_Kmeans
summary(Covid_Kmeans)
stat.desc(Covid_Kmeans)

#Graph based on k-means
require(cluster)
clusplot(Covid_PCA_loadings,Covid_Kmeans$cluster,color = TRUE,lines = 3,labels = 1, main = "Covid Infections and Deaths Cluster Analysis")
#Hypothesis testing______________________________________________________________________
#one sample T Test
summary(Covid_Cases_Over_Time_selected_states$new_case)
hist(Covid_Cases_Over_Time_selected_states$new_case, main = "Histogram of New Cases Per Day")
t.test(Covid_Cases_Over_Time_selected_states$new_case,mu=837.2,alternative = "less")

summary(Covid_Cases_Over_Time_selected_states$new_death)
hist(Covid_Cases_Over_Time_selected_states$new_death,main = "Histogram of New Deaths Per Day" )
t.test(Covid_Cases_Over_Time_selected_states$new_death,mu=12.5,alternative = "less")

#Two sample T-Test
summary(Oklahoma$new_case)
summary(Louisiana$new_case)
t.test(Oklahoma$new_case,Louisiana$new_case)

summary(Louisiana$new_death)
summary(Connecticut$new_death)
t.test(Louisiana$new_death, Connecticut$new_death)
