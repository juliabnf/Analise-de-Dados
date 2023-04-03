#install.packages #Usar isso ao inves de library pra baixar pacotes
library(tidyverse) #manipulacçaão de dados (aka 'data carpentry')
library(gvlma) #teste pra pressupostos do modelo
library(broom) #summarizar resultados de modelos complexos
library(nlme) #modelagem das variancias
library(car) #analise e visualizacao de modelos lineares
library(emmeans) #teste de meédias
library(openxlsx) #abrir arquivos de excel
library(multcomp)#teste de medias

getwd()
setwd("//Users/juliabarranettoferreira/Library/CloudStorage/OneDrive-UniversityofFlorida/PhD/TA/Analise de dados - PPGA-CS")

##############################
#####        DIC         #####
##############################

View(iris)
str(iris)

ggplot(iris) +
  aes(x = Species,
      y = Petal.Length) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = .3)

m1 <- lm(Petal.Length ~ Species, data = iris)
plot(m1)

bartlett.test(Petal.Length ~ Species, data = iris) #heterogeneity
shapiro.test(residuals(m1)) #normality

augment(m1) #package broom
diagnosticos <- augment(m1)

ggplot(diagnosticos) +
  aes(x = Species, 
      y = .std.resid) +
  geom_jitter()

###############################
####  Log transformation  ####
##############################


m2 <- lm(log(Petal.Length) ~ Species, data = iris)
plot(m2)

diagnosticos2 <- augment(m2)
ggplot(diagnosticos2) +
  aes(x = Species, 
      y = .std.resid) +
  geom_jitter()

##############################
#####        DBC         #####
##############################

data<-read.xlsx("K_acumulo.xlsx")
data<-data%>%
  filter(Solucao!="sem")

View(data)
data$Solucao<-as.factor(data$Solucao)
#data$Dose<-as.factor(data$Dose)
data$Bloco<-as.factor(data$Bloco)
str(data)

ggplot(data) +
  aes(x = Dose,
      y = K,
      color=Solucao) +
  geom_smooth(method="lm") +
  geom_jitter(width = .3)#+
facet_grid(Solucao~.)

m.1<- lm(K~Bloco + Dose * Solucao, data=data)

plot(m.1)

diagnosticos3 <- augment(m.1)

ggplot(diagnosticos3) +
  aes(x = Dose, 
      y = .std.resid) +
  geom_jitter()

m.3<- lm(log(K)~Bloco + Dose * Solucao, data=data)
diagnosticos4 <- augment(m.3)

ggplot(diagnosticos4) +
  aes(x = Dose, 
      y = .std.resid) +
  geom_jitter()

################################################
#########.  Uso de estruturas de variancia #####
################################################
m.4<- gls(K~Bloco + Dose * Solucao, data=data,
          weights = varIdent(form = ~1|Dose))#modelo mais complexo
data$Residuos<- residuals(m.4, type="normalized")

ggplot(data) +
  aes(x = Dose, 
      y = Residuos) +
  geom_jitter()

plot(m.4)
qqPlot(residuals(m.4))

m0<- gls(K~Bloco + Dose * Solucao, data=data) #modelo simples
anova(m0,m.4)

anova(m.4)
summary(m.4)#explore mainly the variance function
cld(emmeans(m.4,~Solucao|Dose))


###############################################
########.  Considerando dose como fator.  #####
########         & teste de media         ##### 
###############################################

m.4<- gls(K~Bloco + factor(Dose) * Solucao, data=data,
          weights = varIdent(form = ~1|Dose))#
anova(m.4)

teste<-emmeans(m.4,~Dose:Solucao)
cld(teste, Letters=letters)

teste<-summary(teste) #salva como uma dataframe
write.xlsx(teste, "Teste de medias.xlsx")
