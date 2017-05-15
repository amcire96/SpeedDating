library(readr)
library(class)

# set.seed(1)

Speed_Dating_Data <- read.csv("Speed Dating Data.csv")
# View(Speed_Dating_Data)

columns_kept <- c("gender", "round", "order", "int_corr", "samerace", "age_o", 
                  "dec_o", "attr_o", "sinc_o", "intel_o", "fun_o", "amb_o", 
                  "shar_o", "like_o", 'age', "prob")

filtered_speed_dating <- Speed_Dating_Data[, columns_kept]
sum(is.na(filtered_speed_dating))
# filtered_speed_dating[is.na(filtered_speed_dating),]
filtered_speed_dating <- filtered_speed_dating[complete.cases(filtered_speed_dating), ]
sum(is.na(filtered_speed_dating))
attach(filtered_speed_dating)

library(splines)
gam1=lm(dec_o~ns(int_corr,4)+ns(age_o,5)+ns(age,5)+prob+attr_o+sinc_o
        +intel_o+fun_o+amb_o+shar_o+like_o,data=filtered_speed_dating)

library(gam)
gam.m3=gam(dec_o~s(int_corr,4)+s(age_o,5)+s(age,5)+prob+attr_o+sinc_o
           +intel_o+fun_o+amb_o+shar_o+like_o,data=filtered_speed_dating)

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.gam(gam1, se=TRUE, col="red")


gam.m1=gam(dec_o~s(age,5)+prob,data=filtered_speed_dating)
gam.m2=gam(dec_o~int_corr+s(age,5)+prob,data=filtered_speed_dating)
anova(gam.m1, gam.m2, gam.m3, test="F")

summary(gam.m3)


preds=predict(gam.m2, newdata=filtered_speed_dating)

# #classfication
# 
# gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
# par(mfrow=c(1,3))
# plot(gam.lr,se=T,col="green")
# table(education,I(wage>250))
# 
# gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage, subset=(education!="1. < HS Grad") )
# par(mfrow=c(1,3))
# plot(gam.lr,se=T,col="green")
# 
# #takehomepart
# gam.m4=gam(wage~poly(year,6)+bs(age,5)+education,data=Wage)
# 
# 
# #anova part
# gam.m1=gam(I(wage>250)~year+education,family=binomial,data=Wage, subset=(education!="1. < HS Grad"))
# gam.m2=gam(I(wage>250)~year+age+education,family=binomial,data=Wage, subset=(education!="1. < HS Grad"))
# gam.m3=gam(I(wage>250)~year+s(age,2)+education,family=binomial,data=Wage, subset=(education!="1. < HS Grad"))
# gam.m4=gam(I(wage>250)~year+s(age,5)+education,family=binomial,data=Wage, subset=(education!="1. < HS Grad"))
# gam.m5=gam(I(wage>250)~year+s(age,8)+education,family=binomial,data=Wage, subset=(education!="1. < HS Grad"))
# anova(gam.m1, gam.m2, gam.m3, gam.m4, gam.m5, test="F")

