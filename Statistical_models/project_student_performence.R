data = read.table("student-mat.csv",sep=";",header=TRUE)
data = data[data$G3 != 0,]
men = data[data$sex != "F"]
men = mapvalues(data, data$sex)
women = data[data$sex != "M"]
# print(nrow(data)) # 382 students


#summary(data)
# boxplot(data)

reg<-lm(data$G3~., data)
summary(reg)

new_reg <-lm(data$G3~data$famrel+data$absences+data$G1+data$G2+data$paid+data$health, data)
summary(new_reg)

plot(data$G3, new_reg$res)
shapiro.test(new_reg$res)
qqnorm(new_reg$res)


plot(data$G3 ,new_reg$res)
abline(h=0)
plot(data$paid ,new_reg$res)
abline(h=0)
plot(data$famrel ,new_reg$res)
abline(h=0)
plot(data$health ,new_reg$res)
abline(h=0)
plot(data$absences ,new_reg$res)
abline(h=0)
plot(data$G1 ,new_reg$res)
abline(h=0)
plot(data$G2 ,new_reg$res)
abline(h=0)

new_reg2 <-lm(data$G3~data$famrel+data$absences+data$paid+data$health, data)
summary(new_reg2)

plot(data$G3, new_reg2$res)
shapiro.test(new_reg2$res)
qqnorm(new_reg2$res)


plot(data$G3 ,new_reg2$res)
abline(h=0)
plot(data$paid ,new_reg2$res)
abline(h=0)
plot(data$famrel ,new_reg2$res)
abline(h=0)
plot(data$health ,new_reg2$res)
abline(h=0)
plot(data$absences ,new_reg2$res)
abline(h=0)


shapiro.test(reg$res)
shapiro.test(new_reg$residuals)
plot(data$G3, new_reg$residuals)
new_results = data$G3

new_reg_abs <-lm(data$G3~data$G1+data$G2, data)
summary(new_reg_abs)

plot(data$G3, new_reg_abs$res)
shapiro.test(new_reg_abs$res)
qqnorm(new_reg_abs$res)





shapiro.test(reg$res)
shapiro.test(new_reg$residuals)
plot(data$G3, new_reg$residuals)
new_results = data$G3
#plot(reg$res,type="o")
#abline(h=0)
#boxplot(reg$res)
#ANOVA

grades = data$G1
sex = data$sex
anova.tool <- aov(grades~sex,data)
print(summary(anova.tool))
sex <- 
plot(sex, grades)
