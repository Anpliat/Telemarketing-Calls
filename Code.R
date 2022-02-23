data<- read.csv(file.choose(), header=TRUE)
head(data)
#edit(data)
dim(data)   
is.data.frame(data) 

any(is.na(data))
all(colSums(is.na(data)) != 0)
sum(sapply(data,is.na))
sum(sapply(data,is.null))
sum(sapply(data,is.nan))
sum(sapply(data,is.infinite))
any(duplicated.data.frame(data)==TRUE)  

#------------------------------------------------------
class(data) 
str(data) 
sapply(data,class) 
# factors: job, marital, education, default, housing, loan, contact, month,day_of_week, poutcome, SUBSCRIBED 
# numeric: emp.var.rate, cons.price.idx, euribor3m, nr.employed
# integer: age, duration, campaign, pdays, previous, CODE

#------------------------------------------------------
dim(data)  # 39883    22
data<-data[data$CODE==9,]
dim(data)  #   4019   22

#### Remove the column labeled "CODE"
data$CODE<-NULL  # Delete CODE column
dim(data)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

summary(data)
sapply(data[sapply(data,class)=="factor"],attributes)
factors<-data[sapply(data,class)=='factor']  
sinexeis<-data[sapply(data,class)=='numeric']  
diakrites<-data[sapply(data,class)=='integer'] 
summary(factors)
round(sapply(sinexeis,summary),1)
round(sapply(diakrites,summary),1)


summary(factors)

data$default<-NULL
posost11<-round(100* sum(data$poutcome=="nonexistent") / nrow(data),0)
print(paste("The proportion of nonexistent values in poutcome variable is = ",posost11," %",sep=""))
posost1<-round(100* sum(data$housing=="unknown") / nrow(data),0)
print(paste("The proportion of unknown values in housing and loan variables is = ",posost1," %",sep=""))
posost2<-round(100* sum(data$marital=="unknown") / nrow(data),1)
print(paste("The proportion of unknown values in marital variable is = ",posost2," %",sep=""))
#------------------------------------------------------
#------------------------------------------------------

primary<-c("basic.4y","basic.6y","basic.9y")
library(car)
data$education<-recode(data$education,"data$education[data$education%in%primary]='primary'")
levels(data$education)[5]<-"university"
head(data$education)


lev1<-c("illiterate","primary","high.school","professional.course","university","unknown")
data$education<-factor(data$education,levels=lev1)
head(data$education)

levels(data$day_of_week)
head(data$day_of_week)

lev2<-c("mon","tue","wed","thu","fri")
data$day_of_week<-factor(data$day_of_week,levels=lev2)

head(data$day_of_week)
str(data)
class(data$day_of_week)

factors<-data[sapply(data,class)=='factor']  
summary(factors) 
#------------------------------------------------------
#------------------------------------------------------

levels(data$job)
table(data$job)

White_collar<- c("admin.","entrepreneur","management","self-employed")
Blue_collar<-c("blue-collar","services","technician")
Other <- c("unemployed","housemaid","student","retired","unknown")      

job_old<-data$job
library(car)
data$job<-recode(data$job,"data$job[data$job%in%White_collar]='White_collar'")
data$job<-recode(data$job,"data$job[data$job%in%Blue_collar]='Blue_collar'")
data$job<-recode(data$job,"data$job[data$job%in%Other]='Other'")


lev5<-c("White_collar","Blue_collar","Other")
data$job<-factor(data$job,levels=lev5)
table(data$job)

## contrasts() function. This function will show us how the variables have been dummyfied by R
#and how to interpret them in a model.
sapply(factors,contrasts)
levels(data$SUBSCRIBED)
head(data$SUBSCRIBED)


factors<-data[sapply(data,class)=='factor']  
summary(factors) 


levels(data$month)
spring<-c("mar","apr","may")
summer<-c("jun","jul","aug")
fall<-c("sep","oct","nov")
winter<-c("dec","jan","feb") 
seasons<-data$month
levels(seasons)
head(seasons)

library(car)
seasons<-recode(seasons,"seasons[seasons%in%spring]='spring'")
seasons<-recode(seasons,"seasons[seasons%in%summer]='summer'")
seasons<-recode(seasons,"seasons[seasons%in%fall]='fall'")
seasons<-recode(seasons,"seasons[seasons%in%winter]='winter'")

levels(seasons)
head(seasons)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

diakrites<-data[sapply(data,class)=='integer'] 
round(sapply(diakrites,summary),1)

length(unique(data$pdays))
table(as.factor(data$pdays))
# 999: means client was not previously contacted

#Proportion of missing values 
post<-100* sum(data$pdays==999) / nrow(data)
print(paste("The proportion of 999 value in pdays variable is = ",round(post,0)," %",sep=""))

# 97% of the values of pdays variable are 999, which renders this variable unusable. 
# In order to use it more properly we create a new variable depending on pdays and then
# we remove pdays from our dataset.

p_contact<-data$pdays
table(p_contact)
p_contact <- ifelse(p_contact==999, 1, 0)  # an =999 => 1 ,alliws => 0
table(p_contact)

class(p_contact)
p_contact<-factor(p_contact,levels=c(0,1),labels=c("yes","no"))
class(p_contact)
#------------------------------------------------------
#------------------------------------------------------

####### Transform variables #########
# some variables that were given as integers may have been more useful as categorical variables

# Integers
diakrites<-data[sapply(data,class)=='integer'] 
round(sapply(diakrites,summary),1)

length(unique(diakrites$age))     
length(unique(diakrites$duration)) 
length(unique(diakrites$campaign)) 
length(unique(diakrites$previous))

range(unique(diakrites$age))        
range(unique(diakrites$duration))  
range(unique(diakrites$campaign))   

sort(unique(diakrites$previous))


table(diakrites$previous)
post3<-100* sum(data$previous!=0) / nrow(data)
print(paste("The proportion of 999 value in pdays variable is = ",round(post3,0)," %",sep=""))
#------------------------------------------------------

round(summary(data$duration),1)
range(unique(data$duration)) 
duration_cut<-cut(data$duration,breaks=c(0,300,600,900,max(data$duration)),labels=c("0-5min","5-10min","10-15min",">15min"),include.lowest = T)
table(duration_cut)
head(duration_cut)


round(summary(data$age),1)
range(unique(data$age)) 
length(unique(data$age))
age_cut<-cut(data$age,breaks=c(18,30,45,60,max(data$age)),labels=c("18-30","30-45","45-60",">60"),include.lowest = T)
table(age_cut)


#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

# Numerics
sinexeis<-data[sapply(data,class)=='numeric']  

round(sapply(sinexeis,summary),1)

length(unique(sinexeis$emp.var.rate))   
length(unique(sinexeis$cons.price.idx)) 
length(unique(sinexeis$cons.conf.idx))   
length(unique(sinexeis$euribor3m))       
length(unique(sinexeis$nr.employed))  

range(unique(sinexeis$emp.var.rate))    
range(unique(sinexeis$cons.price.idx))    
range(unique(sinexeis$cons.conf.idx))    
range(unique(sinexeis$euribor3m))        
range(unique(sinexeis$nr.employed)) 

sort(unique(sinexeis$emp.var.rate))  
sort(unique(sinexeis$nr.employed))

#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

##### Normalizing the data
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x)))
}

emp.var.rate<-normalize(data$emp.var.rate)
cons.price.idx<-normalize(data$cons.price.idx)
cons.conf.idx<-normalize(data$cons.conf.idx)
euribor3m<-normalize(data$euribor3m)
nr.employed<-normalize(data$nr.employed)
norm_data<-cbind(emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,nr.employed)

summary(sinexeis)
summary(norm_data)

#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

new_data<-cbind(age_cut,data[,2:7],seasons,data$day_of_week,duration_cut,data$campaign,p_contact,data[,13:14],norm_data,data$SUBSCRIBED)
dim(new_data)
colnames(new_data)[c(9,11,20)]<-c("day_of_week","campaign","SUBSCRIBED")
summary(new_data)
# write.csv(new_data,"C:/.../new_data.csv",col.names = T)


data<-new_data
### separate the response from the predictors
y<-data$SUBSCRIBED
which(colnames(data)=="SUBSCRIBED")
data<-data[,-20]
dim(data)  

model<- glm(y~., data = data, family = binomial(link = "logit"))
summary(model)

#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

####### Box Plot for Continuous Independent Variables #######
win.graph()
par(mfrow=c(1,5))
boxplot(emp.var.rate ~ y,xlab="SUBSCRIBED",ylab="emp.var.rate",col="navy",medcol="red")
boxplot(cons.price.idx ~ y,xlab="SUBSCRIBED",ylab="cons.price.idx",col="navy",medcol="red") 
boxplot(cons.conf.idx ~ y,xlab="SUBSCRIBED",ylab="cons.conf.idx",col="navy",medcol="red")
boxplot(euribor3m ~ y,xlab="SUBSCRIBED",ylab="euribor3m",col="navy",medcol="red")
boxplot(nr.employed ~ y,xlab="SUBSCRIBED",ylab="nr.employed",col="navy",medcol="red")
title("Boxplot of SUBSCRIBED for numeric",outer=TRUE,cex.main =1.5,font=2,line=-2)   #βάζω τίτλο στο παράθυρο διαγραμμάτων


win.graph()
par(mfrow=c(1,2))
boxplot(data$campaign ~ y,xlab="SUBSCRIBED",ylab="campaign",col="navy",medcol="red")
boxplot(data$previous ~ y,xlab="SUBSCRIBED",ylab="previous",col="navy",medcol="red")
title("Boxplot of SUBSCRIBED for integers",outer=TRUE,cex.main =1.5,font=2,line=-2)   #βάζω τίτλο στο παράθυρο διαγραμμάτων
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

################################
########## BAR-PLOTS ###########
################################

factors<-data[sapply(data,class)=='factor']  
#Gia posotikes: basika perigrafika metra
sinexeis<-data[sapply(data,class)=='numeric']  
diakrites<-data[sapply(data,class)=='integer'] 
summary(factors)
round(sapply(sinexeis,summary),1)
round(sapply(diakrites,summary),1)

#-------------------------------------------------------------------------------
############# Plots #############

y1<-factor(as.numeric(y),levels=c(2,1),labels=c("no","yes"))
xrwmata<-c("red","cyan")
barplot(round(prop.table(table(y))*100),main="Barplot for SUBSCRIBED",ylab="Percentage (%)",xlab="SUBSCRIBED",col=xrwmata)
legend('topright', fil=xrwmata, legend=levels(y), ncol=2, bty='n',cex=0.8)


win.graph()
par(mfrow=c(3,2))

levels(data$age_cut)
lev3<-c("18-30","30-45","45-60",">60")
data$age_cut<-factor(data$age_cut,levels=lev3)


xrwmata<-c("red","darkgreen")
barplot(round(prop.table(table(y1,data$age_cut))*100),main="Barplot for age",ylab="Percentage (%)",xlab="Age",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)


lev4<-c("0-5min","5-10min","10-15min",">15min")
data$duration_cut<-factor(data$duration_cut,levels=lev4)
barplot(round(prop.table(table(y1,data$duration_cut))*100),main="Barplot for duration",ylab="Percentage (%)",xlab="Duration",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)


lev41<-c("White_collar","Blue_collar","Other")
data$job<-factor(data$job,levels=lev41)
barplot(round(prop.table(table(y1,data$job))*100),main="Barplot for job",ylab="Percentage (%)",xlab="job",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)


lev41a<-c("divorced","single","married")
data$marital<-factor(data$marital,levels=lev41a)
barplot(round(prop.table(table(y1,data$marital))*100),main="Barplot for marital",ylab="Percentage (%)",xlab="marital",col=xrwmata)
legend('topleft', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)


barplot(round(prop.table(table(y1,data$housing))*100),main="Barplot for housing",ylab="Percentage (%)",xlab="housing",col=xrwmata)
legend('topleft', fil=xrwmata, legend=c("yes","no"),ncol=2, bty='n',cex=0.8)


barplot(round(prop.table(table(y1,data$loan))*100),main="Barplot for loan",ylab="Percentage (%)",xlab="loan",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

win.graph()
par(mfrow=c(3,2))


barplot(round(prop.table(table(y1,data$contact))*100),main="Barplot for contact",ylab="Percentage (%)",xlab="contact",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), ncol=2, bty='n',cex=0.8)

barplot(round(prop.table(table(y1,data$seasons))*100),main="Barplot for seasons",ylab="Percentage (%)",xlab="seasons",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

lev47<-c("mon","tue","wed","thu","fri")
data$day_of_week<-factor(data$day_of_week,levels=lev47)
barplot(round(prop.table(table(y1,data$day_of_week))*100),main="Barplot for day_of_week",ylab="Percentage (%)",xlab="day_of_week",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

barplot(round(prop.table(table(y1,data$p_contact))*100),main="Barplot for p_contact",ylab="Percentage (%)",xlab="p_contact",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

barplot(round(prop.table(table(y1,data$poutcome))*100),main="Barplot for poutcome",ylab="Percentage (%)",xlab="poutcome",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

barplot(round(prop.table(table(y1,data$education))*100),main="Barplot for education",ylab="Percentage (%)",xlab="education",col=xrwmata)
legend('topright', fil=xrwmata, legend=c("yes","no"), bty='n',cex=0.8)

#-----------------------------------------------------------------------
# proportion frequency tables for factors

round(prop.table(table(y,data$age_cut))*100)
round(prop.table(table(y,data$job))*100)
round(prop.table(table(y,data$marital))*100)
round(prop.table(table(y,data$education))*100)
round(prop.table(table(y,data$housing))*100)
round(prop.table(table(y,data$loan))*100)
round(prop.table(table(y,data$contact))*100)
round(prop.table(table(y,data$seasons))*100)
round(prop.table(table(y,data$day_of_week))*100)
round(prop.table(table(y,data$duration_cut))*100)
round(prop.table(table(y,data$p_contact))*100)
round(prop.table(table(y,data$poutcome))*100)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

###################################
########## Correlations ###########
###################################

y_num<-y
levels(y_num)<-c(0,1)
levels(y_num)
head(y_num)
y_num<-as.numeric(y_num)
unique(y_num)
y_num<-ifelse(y_num==1, 0, 1)
unique(y_num)

# correlation coefficients between output variable Y and the input variables
sinexeis<-data[sapply(data,class)=='numeric']  
head(sinexeis)
posotikes<-data[sapply(data,class)=='numeric' | sapply(data,class)=='integer'] 
head(posotikes)


corre0<-t(round(cor(y_num,posotikes),2))
colnames(corre0)<-"y"
corre0

############## Scatter plots ################
scatter1<-function(data){
#install.packages("gclus")
library(gclus)
dta <- data # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,main="Variables Ordered and Colored by Correlation" ) 
}

win.graph()
scatter1(sinexeis)

sin1<-cbind(y_num,sinexeis)
colnames(sin1)[1]<-"SUBSCRIBED"


library(corrplot)
round(cor(sin1),2)
corrplot(round(cor(sin1),2),method="number",type="upper")


round(cor(diakrites<-data[sapply(data,class)=='integer'] ),2)

round(cor(posotikes),2)


win.graph()
library(corrplot)
corrplot(round(cor(posotikes),2),method="number",type="upper")

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

########################################
########## Contigency tables ###########
########################################

lev3<-c("18-30","30-45","45-60",">60")
data$age_cut<-factor(data$age_cut,levels=lev3)

lev4<-c("0-5min","5-10min","10-15min",">15min")
data$duration_cut<-factor(data$duration_cut,levels=lev4)

lev41<-c("White_collar","Blue_collar","Other")
data$job<-factor(data$job,levels=lev41)

lev41a<-c("divorced","single","married")
data$marital<-factor(data$marital,levels=lev41a)

lev47<-c("mon","tue","wed","thu","fri")
data$day_of_week<-factor(data$day_of_week,levels=lev47)


#install.packages("sjPlot")
library(sjPlot)

sjt.xtab(data$age_cut,y,var.labels=c("AGE","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$job,y,var.labels=c("JOB","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$marital,y,var.labels=c("MARITAL","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$education,y,var.labels=c("EDUCATION","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$housing,y,var.labels=c("HOUSING","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$loan,y,var.labels=c("LOAN","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$contact,y,var.labels=c("CONTACT","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$seasons,y,var.labels=c("seasons","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$day_of_week,y,var.labels=c("DAY_OF_WEEK","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$duration_cut,y,var.labels=c("DURATION","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$p_contact,y,var.labels=c("P_CONTACT","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$poutcome,y,var.labels=c("poutcome","SUBSCRIBED"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$housing,data$loan,var.labels=c("housing","loan"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")


sjt.xtab(data$poutcome,data$p_contact,var.labels=c("poutcome","P_CONTACT"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(data$housing,data$loan,var.labels=c("housing","loan"),show.cell.prc=T, show.row.prc=T, show.col.prc=T, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")

#==============================================================================
#==============================================================================
#======================== LOGISTIC REGRESSION MODEL ===========================
#==============================================================================
#==============================================================================

data<- read.csv(file.choose(), header=TRUE)
head(data)
dim(data)  
class(data)
str(data) 
sapply(data,class) 


### separate the response from the predictors
y<-data$SUBSCRIBED
mode(y);class(y)
head(y)
which(colnames(data)=="SUBSCRIBED")
data<-data[,-20] 
dim(data)  


factors<-data[sapply(data,class)=='factor']  
sinexeis<-data[sapply(data,class)=='numeric']  
diakrites<-data[sapply(data,class)=='integer'] 
summary(factors)
round(sapply(sinexeis,summary),1)
round(sapply(diakrites,summary),1)

contrasts(y)
sapply(factors,contrasts)

table(y,factors$age_cut)
table(y,factors$job)
table(y,factors$marital)
table(y,factors$education)
table(y,factors$housing)
table(y,factors$loan)
table(y,factors$contact)
table(y,factors$seasons)
table(y,factors$day_of_week)
table(y,factors$duration_cut)
table(y,factors$p_contact)
table(y,factors$poutcome)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

# Now, our data set is ready. Let's implement Logistic Regression and check our model's accuracy.

############## Model Diagnostics ##############

# The summary gives the beta coefficients, Standard error, z Value and p Value.
# If your model had categorical variables with multiple levels, you will find a row-entry
# for each category of that variable. That is because, each individual category is considered as
# an independent binary variable by the glm().

# In this case it is ok if few of the categories in a multi-category variable 
# don’t turn out to be significant in the model
model<- glm(y~., data = data, family = binomial(link = "logit"))
summary(model)

#--------------------------------------------------------------

############  confint  ############
# confint: obtain confidence intervals for the coefficient estimates

confint(model)            ## CIs using profiled log-likelihood
# confint.default(model)  ## CIs using standard errors

## odds ration scale
## exp(coef(model))
exp(cbind(OR = coef(model), confint(model)))  #gia D.E.

#--------------------------------------------------------------

############  WALD TEST  ############
# We can test for an overall effect of a factor using the wald.test
# The order in which the coefficients are given in the table of coefficients 
# is the same as the order of the terms in the model.

summary(model)

library(aod)
# age:
wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:4) 
# job:
wald.test(b = coef(model), Sigma = vcov(model), Terms = 5:6) 
# marital
wald.test(b = coef(model), Sigma = vcov(model), Terms = 7:8)  
# education
wald.test(b = coef(model), Sigma = vcov(model), Terms = 9:12)  
# seasons
wald.test(b = coef(model), Sigma = vcov(model), Terms = 16:18)
# day_of_week
wald.test(b = coef(model), Sigma = vcov(model), Terms = 19:22) 
# duration_cut
wald.test(b = coef(model), Sigma = vcov(model), Terms = 23:25)
# poutcome
wald.test(b = coef(model), Sigma = vcov(model), Terms = 29:30) 

#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------

### GOF test (Goodness of fit)
# By default, pchisq() gives the proportion of the distribution to the left of the value.
# To get the proportion more extreme than your difference,
# you can specify lower.tail = FALSE or subtract the result from 1

# 1st way
pchisq(model$deviance, model$df.residual, lower.tail=FALSE)  

# 2nd way
with(model, pchisq(deviance, df.residual, lower.tail = F))

#--------------------------------------------------------------
#--------------------------------------------------------------

# The most common test for significance of a binary logistic model is a chi-square test,
# based on the change in deviance when you add your predictors to the null model.


## for model1

# difference in deviance for the two models (i.e., the test statistic)
with(model, null.deviance - deviance)  # 1011.284

# The degrees of freedom for the difference between the two models
# is equal to the number of predictor variables in the mode,
with(model, df.null - df.residual) # 40 (to null exei 1 df logika)


###### 1st way #####
# The approach to testing significance that I’ve seen on a number of web pages 
# involves calculating/compute the p-value manually

# the p-value
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #apor.Ho
# The p-value is almost equal to zero,so we reject the null and conclude that model3 is better than null


###### 2nd way #####
# We can use the anova() command by manually fitting the null model. 
null.model<- glm(y~1, data = data, family = binomial(link = "logit"))
anova(null.model, model, test="Chisq")  # apor.Ho

# We can also use the same syntax to compare two fitted logistic models for the same data,
# say where model2 adds some predictors to model1. => anova(model1,model2, test="Chisq")

## Conclusion:
# The chi-square of (1011.284) with (40) d.f. and an associated p-value<0.001 tells us that:
# our model as a whole fits significantly better than an empty model.


#--------------------------------------------------------------
## Extra: To see the model’s log likelihood:
logLik(model) 

#--------------------------------------------------------------
#--------------------------------------------------------------

######### Pseudo R2 for GLM (Mc Fadden) #########

# While no exact equivalent to the R2 of linear regression exists,
# the McFadden R2 index can be used to assess the model fit.

#install.packages("pscl")
library(pscl)
pR2(model)

#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------


levels(data$age_cut)
lev3<-c("18-30","30-45","45-60",">60")
data$age_cut<-factor(data$age_cut,levels=lev3)

levels(data$duration_cut)
lev4<-c("0-5min","5-10min","10-15min",">15min")
data$duration_cut<-factor(data$duration_cut,levels=lev4)

lev47<-c("mon","tue","wed","thu","fri")
data$day_of_week<-factor(data$day_of_week,levels=lev47)



model<- glm(y~., data = data, family = binomial(link = "logit"))
summary(model)


###### Variables selection ###### 

##########  STEPWISE   ##########

## 1st way

# Stepwise selection according to AIC
m1 <- step(model, direction='both')
getCall(m1)


# Stepwise selection according to BIC
n <- dim(data)[1] 
m2 <- step(model, direction='both', k = log(n))
getCall(m2)
#--------------------------------------------------------------

## 2nd way
# MASS package
# Stepwise Regression from full


# Stepwise selection according to AIC
#install.packages("MASS")
library(MASS)
step1<-stepAIC(model,direction="both",data=data)
step1$anova   


# Stepwise selection according to BIC
n <- dim(data)[1]
step2<-stepAIC(model,direction="both",data=data,k=log(n))
step2$anova 

#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------

# from Stepwise with AIC!!!!
model1c<-glm(y ~ age_cut + job + education + contact + seasons + duration_cut + p_contact 
     + previous + emp.var.rate + cons.conf.idx + euribor3m + nr.employed,
     family = binomial(link = "logit"), data = data)
summary(model1c)


############  WALD TEST gia to AIC model ############
####### Test also that the extra terms in the second model are zero #######

names(coef(m1))
names(coef(m2))
names(coef(m1))[c(2:4,7:14,19,21:23)]

library(aod)
# age:
wald.test(b = coef(model1c), Sigma = vcov(model1c), Terms = 2:4)  #apor.Ho
# job:
wald.test(b = coef(model1c), Sigma = vcov(model1c), Terms = 5:6)  #DEN apor.Ho
# education
wald.test(b = coef(model1c), Sigma = vcov(model1c), Terms = 7:10)  #apor.Ho
# seasons
wald.test(b = coef(model1c), Sigma = vcov(model1c), Terms = 12:14)  #apor.Ho
# duration_cut
wald.test(b = coef(model1c), Sigma = vcov(model1c), Terms = 15:17)  #apor.Ho

#--------------------------------------------------------------

# from Stepwise with BIC!!!!
model1d<-glm( y ~ job + duration_cut + p_contact + emp.var.rate, 
      family = binomial(link = "logit"), data = data)
summary(model1d) 

summary(m1) 
summary(m2) 


######### Pseudo R2 for GLM (Mc Fadden) #########
#install.packages("pscl")
library(pscl)
pR2(model1c)
pR2(model1d)


devi<-cbind(m1$deviance,m2$deviance)
colnames(devi)<-c("with AIC","with BIC")
rownames(devi)<-"Residual deviance"
devi

summary(m1) 
summary(m2) 

#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------

## VIF
# Like in case of linear regression, we should check for multicollinearity in the model. 

library(car)
summary(m1)


colnames(data)
data_m1<-data[,c(1,2,4,7,8,10,12:13,15,17:19)]
colnames(data_m1)


library(car)
sort(vif(m1)[,1])

name<-names(vif(m1)[,1])[vif(m1)[,1]==max(vif(m1)[,1])] #onoma ekei pou pairnei to max vif
name
data16<-data_m1[,-which(colnames(data_m1)==name)]     #bgazw to "euribor3m"
model1<-glm(y~.,family = binomial(link = "logit"),data=data16) 
sort(vif(model1)[,1])

#-----         ---------           ---------         -----------        --------- 
name<-names(vif(model1)[,1])[vif(model1)[,1]==max(vif(model1)[,1])] #onoma ekei pou pairnei to max vif
name
data16<-data16[,-which(colnames(data16)==name)]     #bgazw to "nr.employed"
model1<-glm(y~.,family = binomial(link = "logit"),data=data16) 
sort(vif(model1)[,1])

#---------------------------------------------------
#---------------------------------------------------
#---------------------------------------------------
#---------------------------------------------------

head(data16)
data17<-data16[,-10]
model1b<-glm(y ~ ., family = binomial(link = "logit"), data = data17)
summary(model1b)

############  confint  ############
confint(model1b)          
## odds ration scale
exp(cbind(OR = coef(model1b), confint(model1b)))  #gia D.E.


############  WALD TEST  ############
summary(model1b)

library(aod)
# age:
wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 2:4)  #apor.Ho
# job:
wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 5:6)  #Den apor.Ho(oriaka)
# education:
wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 7:10)  #apor.Ho
# seasons:
wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 12:14)  #apor.Ho
# duration_cut:
wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 15:17)  #apor.Ho



### GOF test (Goodness of fit)
###  Ho: model fits well

# 1st way
pchisq(model1b$deviance, model1b$df.residual, lower.tail=FALSE)  
# 2nd way
with(model1b, pchisq(deviance, df.residual, lower.tail = F))


## ANOVA
# compare two models
anova(m2,model1b,test = "Chisq")  # test model differences with chi-square test
# H0: the full model fits the data at least as well as our model
# H1: our model fits the data better than the m2 model.


######### Pseudo R2 for GLM (Mc Fadden) #########
#install.packages("pscl")
library(pscl)
pR2(model1)
pR2(model1b)


## VIF
library(car)
sort(vif(model1b)[,1])

#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------

getCall(m2)
summary(m2)

############  confint  ############
confint(m2)          
## odds ration scale
exp(cbind(OR = coef(m2), confint(m2)))  #gia D.E.


############  WALD TEST  ############
summary(m2)

library(aod)
# job:
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 2:3)
## duration_cut:
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 4:6)


### GOF test (Goodness of fit)
# 1st way
pchisq(m2$deviance, m2$df.residual, lower.tail=FALSE)  
# 2nd way
with(m2, pchisq(deviance, df.residual, lower.tail = F))



###### 1st way
with(m2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
###### 2nd way
null.model<- glm(y~1, data = data, family = binomial(link = "logit"))
anova(null.model, m2, test="Chisq") 

## ANOVA
anova(m2, test="Chisq")


######### Pseudo R2 for GLM (Mc Fadden) #########
#install.packages("pscl")
library(pscl)
pR2(m2)


## Extra: To see the model’s log likelihood:
logLik(model1) 
logLik(model1b) 
logLik(m2) 
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------

# The probit link function is commonly used for parameters that lie in the unit interval. 
# It is the inverse CDF of the standard normal distribution.

getCall(m2)
prob_model<-glm( y ~ job + duration_cut + p_contact + emp.var.rate, 
                 family = binomial(link = "probit"), data = data)
summary(prob_model)
summary(m2)


######### Pseudo R2 for GLM (Mc Fadden) #########
library(pscl)
pR2(m2)
pR2(prob_model)

deviance(m2)          # logit model 
deviance(prob_model)  # probit model




par(mfrow=c(1,4))
plot(m2)


win.graph()
par(mfrow=c(1,2))
plot(fitted(m2),rstandard(m2),main="St.Residuals vs Fitted")
plot(fitted(m2),resid(m2, type = 'pearson'),main="Pearson Residuals vs Fitted", ylab = 'Residuals (Pearson)')



# Model diagnostics: Residuals against all predictors


### Residuals 
par(mfrow = c(2,4), mar = c(5,5,1,1))

# Pearson residuals
plot(as.numeric(data$job), resid(m2, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = 'job', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)
plot(as.numeric(data$duration_cut), resid(m2, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = 'duration', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)
plot(as.numeric(data$p_contact), resid(m2, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = 'p_contact', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)
plot(data$emp.var.rate, resid(m2, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = 'emp.var.rate', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)

# Deviance residuals
plot(as.numeric(data$job), resid(m2, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = 'job', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)
plot(as.numeric(data$duration_cut), resid(m2, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = 'duration', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)
plot(as.numeric(data$p_contact), resid(m2, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = 'p_contact', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)
plot(data$emp.var.rate, resid(m2, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = 'emp.var.rate', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)


# Box-tidwell test
### keep only the positives values for the box-tidwell test (97.2% of the sample size)
data_start<- read.csv(file.choose(), header=TRUE)
dim(data_start)
data_pos<-data_start[data_start$emp.var.rate>0,]
dim(data_pos)
y_pos<-data_pos$SUBSCRIBED
data_pos<-data_pos[,-20]      


library(car)
getCall(m2)
boxTidwell( as.numeric(y_pos) ~ emp.var.rate, ~ job + duration_cut + p_contact,data = data_pos)
#--------------------------------------------------------------------
#--------------------------------------------------------------------

plot(m2,which=4)

# Note that, not all outliers are influential observations. 
# To check whether the data contains potential influential observations, the standardized residual error can be inspected.
# Data points with an absolute standardized residuals above 3 
# represent possible outliers and may deserve closer attention.

#install.packages("tidyverse")
library("tidyverse")
#install.packages("ggplot2")
library("ggplot2")  
#install.packages("broom")
library(broom)
#install.packages("dplyr") 
library(dplyr)

# Extract model results
model.data <- augment(m2) %>% mutate(index = 1:n()) 
# The data for the top 3 largest values, according to the Cook’s distance, can be displayed as follow:
model.data %>% top_n(3, .cooksd)
# Plot the standardized residuals:
ggplot(model.data, aes(index, .std.resid)) + eom_point(aes(color = y), alpha = .5) +theme_bw()
# Filter potential influential data points with abs(.std.res) > 3:
model.data %>% filter(abs(.std.resid) > 3)