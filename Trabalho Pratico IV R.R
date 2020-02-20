load("C:/Users/Utilizador/Desktop/Metodos Quantitativos/Trabalho 4/rend.RData")

library(wooldridge)
attach(rend)
str(rend)


library(plm)
rendp <- pdata.frame(rend, index = c("id_mulher", "id_ano"))


#####  A
# Estimação OLS pooled:
library(plm)
fitOLSPool <- plm(logrend ~ educ + experi + I(experi^2) + raca + sul + sind, data = rendp, model = "pooling")
summary(fitOLSPool)



#####  B
#Estimador cluster
library(lmtest)
fitOLSPool2 <- coeftest(fitOLSPool, vcovHC)
fitOLSPool2 



#####  C
# Estimação de efeitos fixos:
library(plm)
fitFE <- plm(logrend ~ experi + I(experi^2) + sul + sind, data = rendp, model = "within") 
summary(fitFE)

library(car)


library(stargazer)
stargazer(fitOLSPool, fitOLSPool2, fitFE, type = "text", model.names = FALSE, column.labels = c("OLS", "OLS REBUSTO", "FE"))




#####  D
pFtest(fitFE, fitOLSPool)



##### E
library(plm)
fitRE <- plm(logrend ~ educ + experi + I(experi^2) + raca + sul + sind, data = rendp, model = "random") 
summary(fitRE)


library(car)
myHo <- c("educ=0" , "experi=0" , "I(experi^2)=0" , "raca=0" , "sul=0" , "sind=0")
linearHypothesis(fitRE,myHo)


library(stargazer)
stargazer(fitOLSPool, fitOLSPool2, fitFE, fitRE, type = "text", model.names = FALSE, column.labels = c("OLS", "OLS REBUSTO", "FE", "RE"))


##### F
phtest(fitFE, fitRE)

