
#Task: Explore various models using European Sales Dataset for SalesPerCapita and Computer Sales Dependant variables.


df<-read.csv("EuropeanSales.csv",header=T)

#Show attributes  
attributes(df)

df

df2 <- df[c(-1)]

#Correlation for all attributes
cor(df[c(-1)])
plot(df[c(-1)])
plot(df$ComputerSales,df$SalesPerCapita )
boxplot(df2)

#model fitting
model1 <- lm(SalesPerCapita ~ ., data = df2)
model1

summary(model1)

#Model attributes and coefficients  
attributes(model1)
model1$coefficients

#Applying AIC
step(lm(SalesPerCapita~.,data=df2), direction='backward')

model2 <-lm(SalesPerCapita ~ Population + GDPperHead + EducationSpending + ComputerSales, data = df2)
summary(model2)



model3 <-lm(ComputerSales~ ., data =df2)
summary(model3)

step(lm(ComputerSales~1, data=df2), direction='forward', scope = ~Population + GDPperHead + EducationSpending + SalesPerCapita)
model4 <-lm(ComputerSales ~ Population + SalesPerCapita, data = df2)
summary(model4)



