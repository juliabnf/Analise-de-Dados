library(openxlsx)
library(tidyverse)
library (car)
library(broom)
library(lme4)
library(nlme)
library(lmerTest)
library(emmeans)
library(multcompView)
library(multcomp)
library(viridis)#palette

data<- read.xlsx("Lixiviação.xlsx", sheet="Analysis")
head(data)
data$TRT<-as.factor(data$TRT)
data$Block<-as.factor(data$Block)
data$Leaching <- as.factor (data$Leaching)


#============================= Cumulative =========================='
cumulative<- data%>%
  filter(Ktot != "NA")

ktot<- lm(Ktot~ Block + TRT,
          data=cumulative, na.action = na.omit)

augment(ktot) #package broom
diagnosticos <- augment(ktot)

ggplot(diagnosticos) +
  aes(x = TRT, 
      y = .std.resid) +
  geom_jitter()

qqPlot(residuals(ktot))

cld(emmeans(ktot,~ TRT), Letters=letters,adjust="tukey", reverse=T)

#===================== Plot cumulative =========================;

plot1<-cld(emmeans(ktot,~ TRT), Letters=letters,adjust="tukey", reverse=T)
str(plot1)

ggplot(plot1, aes(x=TRT, y = emmean, fill=TRT))+
  theme_bw()+
  geom_bar(stat = 'identity', color = 'black')+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.3)+
  labs(y = expression(paste("Cum. K leaching (mg ", L^{-1},")")),
       x="", fill="Fertilizante",
       title = "") +
  scale_y_continuous(expand = c(0,0), limits = c(0,180))+
  scale_fill_viridis(discrete = TRUE) 
  

#============================ ANOVA RESIDUAL MODELING ======================================'

ggplot(data) +
  aes(y = Kleach,
      x = TRT,
      color=Leaching) +
#  geom_smooth(method="lm") +
  geom_jitter(width = .3)#+
facet_grid(.~Leaching)

m1<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
        #control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)
m2<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
          correlation = corAR1(form=~1|Block/TRT),
#control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)
m3<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
        correlation = corCompSymm(form=~1|Block/TRT),
        #control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)
m4<-lme(Kleach~ TRT*Leaching, random= ~1|Block/TRT,
        correlation = corLin(form=~1|Block/TRT),
        control=lmeControl(maxIter=1000, msMaxIter=1000, niterEM=1000, opt='optim'),
        data=data, na.action = na.omit)
anova(m1,m2,m3,m4)

data$Residuals1<- residuals(m1, type="normalized")
data$Residuals2<- residuals(m2, type="normalized")

ggplot(data) +
  aes(y = Residuals1,
      x = TRT,
      color=Leaching) +
  geom_jitter(width = .3)

ggplot(data) +
  aes(y = Residuals2,
      x = TRT,
      color=Leaching) +
  geom_jitter(width = .3)

qqPlot(residuals(m1))
qqPlot(residuals(m2))

anova(m1)
anova(m2)#a interacao é menos forte, ja que o efeito do tempo é considerado

cld(emmeans(m1,~ TRT|Leaching), Letters=letters,adjust="bonf", reverse=T)#differences among trt
cld(emmeans(m2,~ TRT|Leaching), Letters=letters,adjust="bonf", reverse=T)# is better with the RM model

plot2<-cld(emmeans(m2,~ TRT|Leaching), Letters=letters,adjust="bonf", reverse=T)# is better with the RM model

ggplot(plot2, aes(x=Leaching, y=emmean, 
                        group=TRT, color=TRT)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.1)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Potassium Leaching") +
  theme_bw() +
  labs(y = expression(paste(" Leaching (mg ", L^{-1}, ")")), 
       x = 'Leaching event',
       color = "Fertilizer")

