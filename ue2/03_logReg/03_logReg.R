
#logistische Regression
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
# library(InformationValue) # not in CRAN any more
library(pscl)
library(generalhoslem)

#Daten laden
dat<-read.table("diabetes.csv", header = TRUE, sep = ",")
#Datenstruktur ansehen
str(dat)

dat$diabetes[dat$diabetes==0] = "neg"
dat$diabetes[dat$diabetes==1] = "pos"
dat$diabetes<-as.factor(dat$diabetes)

# konvertier 0 zu NAs und entferne NAs
dat[dat==0] <- NA
dat<-dat[complete.cases(dat),]


# einfaches Modell ####
m1 <- glm(diabetes ~ glucose, data = dat, family = binomial)

# Zusammenfassung vom Model
summary(m1)
confint(m1) #KI

#odds von glucose
odds_m1 <- exp(m1$coefficients)
exp(confint(m1)) #Ki

# Daten f?r Vorhersagen 
test.data<- dat
#test.data<-data.frame(glucose=seq(0,300,10)) eigene Testdaten

#Wahrscheinlichkeiten berechnen
probabilities_m1 <- m1 %>% predict(test.data, type = "response")

#logodds berechnen
logodds_m1 <- m1 %>% predict(test.data, type = "link") # f?r log(odds)

#Zuordnung bei 50% cut-off
predicted.classes <- ifelse(probabilities_m1 > 0.5, "pos", "neg")

# Modellgenauigkeit
mean(predicted.classes == test.data$diabetes)
#misClassError(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5) # missclassifcation error

#G?tekriterium
AIC(m1)
BIC(m1)

#mcfadden pseudo R2
pR2(m1)

#Sensitivit?t und Spezifit?t

#InformationValue (old)
# sensitivity(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5)
# specificity(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5)

# with caret
caret::sensitivity(data=factor(predicted.classes), reference = factor(dat$diabetes))
caret::specificity(data=factor(predicted.classes), reference = factor(dat$diabetes))


#Confusion matrix (old)
#confusionMatrix(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5)

#Creating confusion matrix with caret package

cm <- caret::confusionMatrix(data=factor(predicted.classes), reference = factor(dat$diabetes), positive ='pos')
cm


#hosmer und lemeshow
logitgof(dat$diabetes,fitted(m1))


#Residuenkontrolle
plot(m1, which=2, col=c("blue")) # Normal Q-Q Plot

#cooks distance
plot(m1, which=4, col=c("blue")) # Normals Q-Q Plot

#Darstellung
op<-data.frame(logodds=logodds_m1,
               odds=exp(logodds_m1),
               probabilities=probabilities_m1,
               glucose=dat$glucose,
               prediction=predicted.classes)


#Darstellung: Glukose vs logodds
ggplot(op,aes(glucose, logodds)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm",  se=T) +
  labs(
    title = "Das logistische Regressionsmodel", 
    x = "Plasma Glukose Konzentration",
    y = "Log(odds)"
  )

#Darstellung: logodds vs Wahrscheinlichkeit
ggplot(op,aes(logodds, probabilities)) +
  geom_point(alpha = 0.2) + geom_vline(xintercept = 0) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se=F)

#Darstellung: Glukose vs Wahrscheinlichkeit
ggplot(op,aes(glucose, probabilities)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se=F) +
  labs(
    title = "Das logistische Regressionsmodel", 
    x = "Plasma Glukose Konzentration",
    y = "Diabetes Odds"
  )

#Darstellung: Glukose vs Odds
ggplot(op,aes(glucose, odds)) +
  geom_point(alpha = 0.2) + 
  labs(
    title = "Das logistische Regressionsmodel", 
    x = "Plasma Glukose Konzentration",
    y = "Diabetes Odds"
  )


#mit fixen modellparametern rechnen (siehe Vorlesung Folie 14)
m1save<-m1
m1$coefficients[[1]]<--7
m1$coefficients[[2]]<-0.05

#logodds
m1$coefficients[[1]]+m1$coefficients[[2]]*100
m1$coefficients[[1]]+m1$coefficients[[2]]*101
m1$coefficients[[1]]+m1$coefficients[[2]]*102


#odds
exp(m1$coefficients[[1]]+m1$coefficients[[2]]*100)
exp(m1$coefficients[[1]]+m1$coefficients[[2]]*101)
exp(m1$coefficients[[1]]+m1$coefficients[[2]]*102)


exp(m1$coefficients[[1]]+m1$coefficients[[2]]*100)*exp(0.05)^2

#mit 
m1<-m1save


#
# volles Modell ####

model <- glm(diabetes ~ ., data = dat, family = binomial)

#bestes Modell automatisch bestimmen
m2<-stepAIC(model,direction = "backward")
m3<-stepAIC(model,direction = "forward")
m4<-stepAIC(model,direction = "both")

#logodds
summary(model)
confint(model)

#OR
exp(coef(model))

AIC(model)
BIC(model)

#multicolinearity
car::vif(model)

#mcfadden R2
pR2(model)

#hosmer und lemeshow goodness of fit
logitgof(dat$diabetes,fitted(model))

#test prediction 
test.data<-dat
probabilities <- model %>% predict(test.data, type = "response") # f?r Prob
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Modellgenauigkeit
mean(predicted.classes == test.data$diabetes)
#misClassError(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5) # missclassifcation error

#ROC Plot

library(ROCit)
ROCit_obj <- rocit(score=probabilities,class=dat$diabetes)
plot(ROCit_obj)
#plotROC(ifelse(dat$diabetes == "pos", 1, 0), probabilities)

#sensitivity(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5)
#specificity(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5)

#confusionMatrix(ifelse(dat$diabetes == "pos", 1, 0), probabilities,threshold=0.5)

#normalverteilung der residuen
plot(model, which=2, col=c("blue")) # Normal Q-Q Plot

#influential values: cook's distance
plot(model, which = 4, id.n = 3)

model.data<-broom::augment(model) # matrix mit cooks distance und std.resid
model.data <- augment(model) %>% mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)

#std.resid anzeigen
index = 1:336
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = diabetes), alpha = .5) +
  theme_bw()

#Werte > 3 stdaw filtern
model.data %>% filter(abs(.std.resid) > 3) 


#Linearit?t der Pr?diktoren ?berpr?fen

# nur f?r numerische Pr?diktoren 
mydata <- dat %>% dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)

# logit berechnen
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(predictor.value,logit))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_x")


