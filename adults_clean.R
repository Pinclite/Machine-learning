# ------------------------------ANALIZA ATRIBUTA-------------------
# ucitavanje skupa podataka i provera strukture atributa
adults = read.csv("adult.csv")
summary (adults)
str (adults)

# s obzirom na to da nedostajucih vrednosti (?) ima u nezanemarljivoj kolicini 
# one ce u modelu biti zadrzane kao takve i cinice posebnu kategoriju

# ispitivanje znacajnosti kategorickih atributa
library(ggplot2)
library(dummies)
library(bnlearn)

# Workclass
ggplot(data = adults, mapping = aes(x = Workclass, fill = Income)) +
  geom_bar(position = "fill", width = 0.4) +
  ylab("Percentage of examinees") + xlab("Workclass") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

# Marital status
ggplot(data = adults, mapping = aes(x = Marital.status, fill = Income)) +
  geom_bar(position = "fill", width = 0.4) +
  ylab("Percentage of examinees") + xlab("Marital status") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

# Occupation
ggplot(data = adults, mapping = aes(x = Occupation, fill = Income)) +
  geom_bar(position = "fill", width = 0.4) +
  ylab("Percentage of examinees") + xlab("Occupation") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

# Relationship
ggplot(data = adults, mapping = aes(x = Relationship, fill = Income)) +
  geom_bar(position = "fill", width = 0.4) +
  ylab("Percentage of examinees") + xlab("Relationship") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

# Race
ggplot(data = adults, mapping = aes(x = Race, fill = Income)) +
  geom_bar(position = "fill", width = 0.4) +
  ylab("Percentage of examinees") + xlab("Race") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

# Sex
ggplot(data = adults, mapping = aes(x = Sex, fill = Income)) +
  geom_bar(position = "fill", width = 0.4) +
  ylab("Percentage of examinees") + xlab("Sex") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

# Native country
ggplot(data = adults, mapping = aes(x = Native.country, fill = Income)) +
  geom_bar(position = "fill", width = 0.4) +
  ylab("Percentage of examinees") + xlab("Native country") +
  theme_bw() + scale_y_continuous(labels = scales::percent)


# ispitivanje znacajnosti numerickih atributa
# Age
ggplot(data = adults, aes(x = Age, fill = Income)) + geom_density(alpha = 0.2)

# Fnlwgt
ggplot(data = adults, aes(x = Fnlwgt, fill = Income)) + geom_density(alpha = 0.2)

# Education num
ggplot(data = adults, aes(x = Education.num, fill = Income)) + geom_density(alpha = 0.2)

# Capital gain
ggplot(data = adults, aes(x = Capital.gain, fill = Income)) + geom_density(alpha = 0.2)

# Capital loss
ggplot(data = adults, aes(x = Capital.loss, fill = Income)) + geom_density(alpha = 0.2)

# Hours per week
ggplot(data = adults, aes(x = Hours.per.week, fill = Income)) + geom_density(alpha = 0.2)


# Nakon izvrsene analize znacajnosti atributa, utvrdjeno je Native country, Race i Fnlwgt nemaju uticaj na izlaznu promenljivu,
# tako da ce ove varijable biti uklonjene.

# Takodje, atribut Education predstavlja leksicku interpretaciju stepena obrazovanja (Education num)
# Posto je pogodnija numericka interpretacija ovog atributa, Education ce takodje biti uklonjen.
adults$Native.country = NULL
adults$Race = NULL
adults$Fnlwgt = NULL
adults$Education = NULL


# formiranje novog skupa podataka sa dummy kategorickim varijablama
remove(adults.work)

# Age
adults.work = data.frame(adults$Age)
names(adults.work) = "Age"
# Workclass
adults.Workclass = data.frame(dummy(adults$Workclass))
adults.work = cbind(adults.work, adults.Workclass)
# Education num
adults.work = cbind(adults.work, adults$Education.num)
names(adults.work)[11] = "Education.num"
# Marital status
adults.Marital.status = data.frame(dummy(adults$Marital.status))
adults.work = cbind(adults.work, adults.Marital.status)
# Occupation
adults.Occupation = data.frame(dummy(adults$Occupation))
adults.work = cbind(adults.work, adults.Occupation)
# Relationship
adults.Relationship = data.frame(dummy(adults$Relationship))
adults.work = cbind(adults.work, adults.Relationship)
# Sex
adults.Sex = data.frame(dummy(adults$Sex))
adults.work = cbind(adults.work, adults.Sex)

# S obzirom na cinjenicu da vise od 90% opservacija imaju vrednosti atributa Capital gain i Capital loss 0, 
# neophodno je pretvoriti ih u kategoricke atribute i izvrsiti diskretizaciju vrednosti koje su vece od 0.
adults.Capital.gain = data.frame(adults$Capital.gain)
adults.Capital.gain.0 = as.data.frame(adults.Capital.gain[which(adults.Capital.gain > 0),])
names(adults.Capital.gain.0)[1] = "abc"
adults.Capital.gain.0$abc = as.numeric(adults.Capital.gain.0$abc)
discretized.gain = discretize(data = as.data.frame(adults.Capital.gain.0$abc), method = 'quantile', breaks = 3)
adults.Capital.gain.0 = discretized.gain
adults.Capital.gain[which(adults.Capital.gain > 0),] = adults.Capital.gain.0
unique(adults.Capital.gain.0)
adults.Capital.gain[which(adults.Capital.gain == 1),] = "[114-4390]"
adults.Capital.gain[which(adults.Capital.gain == 2),] = "(4390-7690]"
adults.Capital.gain[which(adults.Capital.gain == 3),] = "(7690-100000]"

adults.Capital.loss = data.frame(adults$Capital.loss)
adults.Capital.loss.0 = as.data.frame(adults.Capital.loss[which(adults.Capital.loss > 0),])
names(adults.Capital.loss.0)[1] = "abc"
adults.Capital.loss.0$abc = as.numeric(adults.Capital.loss.0$abc)
discretized.loss = discretize(data = as.data.frame(adults.Capital.loss.0$abc), method = 'quantile', breaks = 3)
adults.Capital.loss.0 = discretized.loss
adults.Capital.loss[which(adults.Capital.loss > 0),] = adults.Capital.loss.0
unique(discretized.loss)
adults.Capital.loss[which(adults.Capital.loss == 1),] = "[155-1740]"
adults.Capital.loss[which(adults.Capital.loss == 2),] = "[1740-1980]"
adults.Capital.loss[which(adults.Capital.loss == 3),] = "[1980-4360]"

# Capital gain
adults.gain = data.frame(dummy(adults.Capital.gain$adults.Capital.gain))
adults.work = cbind(adults.work, adults.gain)
adults.work = adults.work[,c(1:41,45,44,42,43)]
# Capital loss
adults.loss = data.frame(dummy(adults.Capital.loss$adults.Capital.loss))
adults.work = cbind(adults.work, adults.loss)
adults.work = adults.work[,c(1:45,49,46:48)]
# Hours per week
adults.work$Hours.per.week = adults$Hours.per.week
# Income
adults.work$Income = adults$Income

names(adults.work)[42:49] = c("Capital.gain.0", "Capital.gain.114..4390", "Capital.gain.4390..7690", "Capital.gain.7690..100000",
                          "Capital.loss.0", "Capital.loss.155..1740", "Capital.loss.1740..1980", "Capital.loss.1980..4360")

# ---------------------BALANSIRANJE PODATAKA------------------
library(caret)
library(ROSE)
library(rpart)
library(grid)
library(gridExtra)

# Provera raspodele izlazne varijable Income
prop.table(table(adults.work$Income))

# Formiranje trening i test skupa podataka 
set.seed(5)
train.indices = createDataPartition(adults.work$Income, p = 0.8, list = FALSE)
train.adults.rose = adults.work[train.indices,]
test.adults.rose = adults.work[-train.indices,]

# Uravnotezavanje trening podataka razlicitim metodama
table(train.adults.rose$Income)

# Originalni skup podataka
treeimb <- rpart(Income ~ ., data = train.adults.rose)
pred.treeimb <- predict(treeimb, newdata = test.adults.rose)
roc.curve(test.adults.rose$Income, pred.treeimb[,2])
# AUC = 0.841

# Over-sampling
data_balanced_over <- ovun.sample(Income ~ ., data = train.adults.rose, method = "over",N = 45000)$data
table(data_balanced_over$Income)
tree.over <- rpart(Income ~ ., data = data_balanced_over)
pred.tree.over <- predict(tree.over, newdata = test.adults.rose)
roc.curve(test.adults.rose$Income, pred.tree.over[,2])
# AUC = 0.860

# Under-sampling
data_balanced_under <- ovun.sample(Income ~ ., data = train.adults.rose, method = "under",N = 16000)$data
table(data_balanced_under$Income)
tree.under <- rpart(Income ~ ., data = data_balanced_under)
pred.tree.under <- predict(tree.under, newdata = test.adults.rose)
roc.curve(test.adults.rose$Income, pred.tree.under[,2])
# AUC = 0.836

# Both Over-sampling and Under-sampling
data_balanced_both <- ovun.sample(Income ~ ., data = train.adults.rose, method = "both", p=0.5, seed = 1)$data
table(data_balanced_both$Income)
tree.both <- rpart(Income ~ ., data = data_balanced_both)
pred.tree.both <- predict(tree.both, newdata = test.adults.rose)
roc.curve(test.adults.rose$Income, pred.tree.both[,2])
# AUC = 0.837

# ROSE
data.rose <- ROSE(Income ~ ., data = train.adults.rose, seed = 1)$data
table(data.rose$Income)
tree.rose <- rpart(Income ~ ., data = data.rose)
pred.tree.rose <- predict(tree.rose, newdata = test.adults.rose)
roc.curve(test.adults.rose$Income, pred.tree.rose[,2], add.roc = TRUE)
# AUC = 0.500

# Tabela poredjenja rezultata koriscenih metoda
balancing.results = data.frame(rbind("0.841", "0.837", "0.836", "0.860", "0.500"), 
                      row.names = c("Original", "Over", "Under", "Both", "ROSE"))
names(balancing.results)[1] = "Area under the curve (AUC)"
grid.table(balancing.results)

# Bice korisceni trening podaci dobijeni kombinacijom Over-sampling i Under-sampling metoda, jer je pokazala najbolje rezultate
adults.training = data_balanced_both
adults.test = test.adults.rose

# Cuvanje trening podataka u posebnu varijablu za svaki od algoritama
adults.knn = adults.training
adults.tree = adults.training
adults.nb = adults.training
adults.logreg = adults.training


# ---------------------- Funkcija za evaluacione metrike ---------------------
# U ovom primeru pozitivnu klasu predstavljaju visoke zarade, a negativnu niske zarade
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc = sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}


# --------------KNN ALGORITAM----------------
library(e1071)
library(class)

# Priprema podataka za potrebe kNN algoritma
str(adults.knn)

# Provera autlajera kod numerickih varijabli Age, Education num i Hours per week
apply (X = adults.knn[,c(1,11,50)], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))
par(mfrow = c(1,3))
boxplot(adults.knn$Age, main = "Age")
# Age ima samo gornje autlajere
boxplot(adults.knn$Education.num, main = "Education num")
# Education.num ima donje autlajere
boxplot(adults.knn$Hours.per.week, main = "Hours per week")
# Hours per week ima autlajere sa obe strane

# Vinzorizacija

# AGE
sort(boxplot.stats(adults.knn$Age)$out)
quantile(adults.knn$Age, probs = seq(0.9,1,0.025))
# probs = 0.975
new.max.age = as.numeric(quantile(adults.knn$Age, probs = 0.975))
adults.knn$Age[adults.knn$Age > new.max.age] = new.max.age

# EDUCATION NUM
sort(boxplot.stats(adults.knn$Education.num)$out)
quantile(adults.knn$Education.num, probs = seq(0,0.1,0.025))
new.min.edunum = as.numeric(quantile(adults.knn$Education.num, probs = 0.025))
adults.knn$Education.num[adults.knn$Education.num < new.min.edunum] = new.min.edunum

# HOURS PER WEEK
sort(boxplot.stats(adults.knn$Hours.per.week[adults.knn$Hours.per.week<60 & 
                                               adults.knn$Hours.per.week>30])$out)
# granice su na <= 32 i >= 53
# uklanjanje donjih autlajera
quantile(adults.knn$Hours.per.week, probs = seq(0.1,0.2,0.025))
new.min.hours = as.numeric(quantile(adults.knn$Hours.per.week, probs = 0.125))
adults.knn$Hours.per.week[adults.knn$Hours.per.week < new.min.hours] = new.min.hours
# uklanjanje gornjih autlajera
quantile(adults.knn$Hours.per.week, probs = seq(0.8,0.9,0.025))
new.max.hours = as.numeric(quantile(adults.knn$Hours.per.week, probs = 0.875))
adults.knn$Hours.per.week[adults.knn$Hours.per.week > new.max.hours] = new.max.hours

# provera raspodele (5000 vrednosti iz uzorka je granica za primenu shapiro testa)
apply (X = adults.knn[c(1:5000),c(1,11,50)], MARGIN = 2, FUN = shapiro.test)

# Nijedna od numerickih varijabli nije u normalnoj raspodeli tako da se koristi metoda standardizacije 
# kako bi se njihove vrednosti svele na isti numericki opseg.
adults.knn.st = as.data.frame(apply(X = adults.knn[,c(1,11,50)], MARGIN = 2, FUN = 
                                      function(x) scale(x, center = median(x), scale = IQR(x))))

# Standardizovane kolone bez autlajera se vracaju u pocetni dataset
adults.knn$Age = adults.knn.st$Age
adults.knn$Education.num = adults.knn.st$Education.num
adults.knn$Hours.per.week = adults.knn.st$Hours.per.week


# Postupak kros-validacije
prop.table(table(adults.training$Income))
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.k = seq(from = 3, to = 25, by = 2))
# Zbog ogromne kolicine podataka i kompleksnosti modela, metoda train se izvrsava duze od 2h
knn.cv = train (Income~., data = adults.knn, method = "knn", trControl = numFolds, tuneGrid = cpGrid)
knn.cv
plot(knn.cv)
# Utvrdjeno je da je najbolje k = 3

# kNN
knn.pred = knn (train = adults.knn[,-51], test = adults.test[,-51], cl = adults.knn$Income, k = 3)
head(knn.pred)

# Matrica konfuzije
knn.cm = table (true = adults.test$Income, predicted = knn.pred)
knn.cm

# Evaluacione metrike
knn.eval = compute.eval.metrics(knn.cm)
knn.eval = round(knn.eval,4)
knn.eval

# --------------STABLO ODLUCIVANJA---------------
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)

# tree1 - jednostavna predikcija (default parametar kompleksnosti)
tree1 = rpart(Income~., data = adults.tree, method = "class")
tree1
rpart.plot(tree1)
tree1.pred = predict (object = tree1, newdata = adults.test, type = "class")

# Matrica konfuzije
tree1.cm = table(true = adults.test$Income, predicted = tree1.pred)
tree1.cm

# Evaluacione metrike
tree1.eval = compute.eval.metrics(tree1.cm)
tree1.eval = round(tree1.eval,4)
tree1.eval

# racunanje optimalne vrednosti za parametar kompleksnosti (cp) primenom postupka kros-validacije
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(from = 0.001, to = 0.05, by = 0.001))
tree2.cv = train (Income ~ ., data = adults.knn, method = "rpart", control = rpart.control(minsplit = 10), 
                  trControl = numFolds, tuneGrid = cpGrid)
tree2.cv
plot(tree2.cv)
# najbolje cp = 0.001 (ista vrednost kao i default)

# Ispitivanje procenjene znacajnosti atributa
varImp(tree1)

#---------------NAIVNI BAJES------------------
library(bnlearn)
library(ggplot2)
library(e1071)
library(caret)
library(pROC)

# Za algoritam naivnog Bajesa bice iskoriscen skup podataka bez dummy varijabli radi postizanja boljih rezultata
adults.nb = adults

summary(adults.nb)
str(adults.nb)

# Provera raspodele numerickih atributa
apply (X = adults.nb[c(1:5000),c(1,3,10)], MARGIN = 2, FUN = shapiro.test)

# Vrsi se diskretizacija, jer nijedan od navedenih atributa ne podleze normalnoj raspodeli
to.discretize = c("Age", "Education.num", "Hours.per.week")
adults.nb$Age = as.numeric(adults.nb$Age)
adults.nb$Education.num = as.numeric(adults.nb$Education.num)
adults.nb$Hours.per.week = as.numeric(adults.nb$Hours.per.week)

discretized = discretize(data = adults.nb[,to.discretize], method = 'quantile', breaks = c(5,4,2))
adults.nb$Age = discretized$Age
adults.nb$Education.num = discretized$Education.num
adults.nb$Hours.per.week = discretized$Hours.per.week

# Atributi capital gain i Capital loss se transformisu u faktor varijable, pri tom se vrednostima vecim od 0 dodeljuje 1
adults.nb$Capital.gain = ifelse(adults.nb$Capital.gain > 0, yes = 1, no = 0)
adults.nb$Capital.loss = ifelse(adults.nb$Capital.loss > 0, yes = 1, no = 0)
adults.nb$Capital.gain = as.factor(adults.nb$Capital.gain)
adults.nb$Capital.loss = as.factor(adults.nb$Capital.loss)

str(adults.nb)

# Kreiranje trening i test skupova podataka
set.seed(5)
train.indices = createDataPartition(adults.nb$Income, p = 0.8, list = FALSE)
train.adults.nb = adults.nb[train.indices,]
test.adults.nb = adults.nb[-train.indices,]


# Naive Bayes algoritam (formiranje modela i predikcija na osnovu njega) - default vrednost parametra threshold
nb1 = naiveBayes(Income~., data = train.adults.nb)
nb1
nb1.pred = predict(nb1, newdata = test.adults.nb, type = 'class')

# Matrica konfuzije
nb1.cm = table (true = test.adults.nb$Income, predicted = nb1.pred)
nb1.cm

# Evaluacione metrike
nb1.eval = compute.eval.metrics(nb1.cm)
nb1.eval = round(nb1.eval,4)
nb1.eval


# Poboljsani model (ROC kriva) - optimalna vrednost parametra threshold
nb2 = naiveBayes(Income~ ., data = adults.nb)
nb2.pred = predict(nb2, newdata = test.adults.nb, type = "raw")
nb2.pred

# Formiranje ROC krive
nb2.roc = roc(response = as.numeric(test.adults.nb$Income), predictor = nb2.pred[,2])
nb2.roc
nb2.roc$auc
# AUC = 0.6232

# Iscrtavanjem ROC krive dolazi se do optimalne vrednosti parametra threshold
par(mfrow = c(1,1))
plot.roc(nb2.roc, print.thres = TRUE, print.thres.best.method = "youden")
# youden - maksimizira vrednost specificity + sensitivity (podjednaka vaznost tacnosti predvidjanja pozitivne i negativne klase)
# sensitivity = 0.841, specificity = 0.757, best threshold = 0.301

# Dodela klasa na osnovu izracunatih verovatnoca
nb2.pred2 = ifelse (test = nb2.pred[,2] >= 0.301, yes = ">50K", no = "<=50K")
nb2.pred2 = as.factor(nb2.pred2)

# Matrica konfuzije
nb2.cm2 = table(true = test.adults.nb$Income, predicted = nb2.pred2)
nb2.cm2

# Evaluacione metrike
nb2.eval = compute.eval.metrics(nb2.cm2)
nb2.eval = round(nb2.eval,4)
nb2.evaltest.adults.nb

#---------------LOGISTICKA REGRESIJA------------------
library(car)

# model logisticke regresije radi sa numerickim podacima tako da cemo iskoristiti iste sredjene tabele kao i za kNN algoritam
adults.logreg = adults.knn
str(adults.logreg)

# prvo se formira model gde su ukljucene sve nezavisne promenljive kako bi se utvrdilo koje treba ukloniti
glm.fit = glm(Income ~ ., data = adults.logreg, family = binomial)
glm.fit
summary(glm.fit)

# Izbacuju se iz modela atributi koji su potpuno medjusobno korelisani (NA vrednoti iz glm.fit) i formira se novi model
glm.fit2 = glm(Income ~ . - Workclass.Without.pay - Marital.status.Widowed - Occupation.. 
               - Occupation.Transport.moving - Relationship.Wife - Sex.Male 
               - Capital.gain.7690..100000 - Capital.loss.1980..4360, data = adults.logreg, family = binomial)
glm.fit2
summary(glm.fit2)

# Proverava se multikolinearnost (izbacuju se svi atributi cija korena vrednost linearne medjuzavisnosti 
# sa drugim atributima premasuje vrednost 2) i formira se novi pouzdaniji model glm.fit3
sqrt(vif(glm.fit2))
glm.fit3 = glm(Income ~ Age + Workclass.Never.worked + Education.num + Marital.status.Married.AF.spouse +
                 Marital.status.Married.spouse.absent + Marital.status.Separated + Occupation.Adm.clerical +
                 Occupation.Armed.Forces + Occupation.Craft.repair + Occupation.Exec.managerial +
                 Occupation.Farming.fishing + Occupation.Handlers.cleaners + Occupation.Machine.op.inspct +
                 Occupation.Other.service + Occupation.Priv.house.serv + Occupation.Prof.specialty +
                 Occupation.Protective.serv + Occupation.Sales + Occupation.Tech.support + Relationship.Other.relative +
                 Relationship.Own.child + Sex.Female + Capital.loss.0 + Capital.loss.155..1740 +
                 Capital.loss.1740..1980 + Hours.per.week, data = adults.logreg, family = binomial)
glm.fit3
summary(glm.fit3)

# Predvidjanje verovatnoca na osnovu formiranog modela
glm.probs = predict(glm.fit3, newdata = adults.test, type = "response")
glm.probs

# Dodela klasa na osnovu dobijenih verovatnoca
glm.pred = ifelse(glm.probs < 0.5, "<=50K", ">50K")

# Matrica konfuzije
glm.cm = table (true = adults.test$Income, predicted = glm.pred)
glm.cm

# Evaluacione metrike
glm.eval = compute.eval.metrics(glm.cm)
glm.eval = round(glm.eval,4)
glm.eval


# Kada se iz modela glm.fit3 izbace svi atributi ciji je nivo znacaja izuzetno nizak (p-vrednost veca od 0.05), 
# ostaju 4 varijable na osnovu kojih se formira novi model
glm.fit4 = glm(Income ~ Age + Education.num + Marital.status.Married.AF.spouse +
                 Marital.status.Married.spouse.absent + Marital.status.Separated + Occupation.Adm.clerical +
                 Occupation.Craft.repair + Occupation.Exec.managerial +
                 Occupation.Farming.fishing + Occupation.Handlers.cleaners +
                 Occupation.Other.service + Occupation.Priv.house.serv + Occupation.Prof.specialty +
                 Occupation.Protective.serv + Occupation.Sales + Occupation.Tech.support + Relationship.Other.relative +
                 Relationship.Own.child + Sex.Female + Capital.loss.0 + Capital.loss.155..1740 +
                 Capital.loss.1740..1980 + Hours.per.week, data = adults.logreg, family = binomial)
glm.fit4
summary(glm.fit4)

# Predvidjanje verovatnoca na osnovu formiranog modela
glm.probs2 = predict(glm.fit4, newdata = adults.test, type = "response")
glm.probs2

# Dodela klasa na osnovu dobijenih verovatnoca
glm.pred2 = ifelse(glm.probs2 < 0.5, "<=50K", ">50K")

# Matrica konfuzije
glm.cm2 = table (true = adults.test$Income, predicted = glm.pred2)
glm.cm2

# Evaluacione metrike
glm.eval2 = compute.eval.metrics(glm.cm2)
glm.eval2 = round(glm.eval2,4)
glm.eval2

# Ispitivanje znacaja atributa
varImp(glm.fit4, scale = FALSE)

#-------------REZULTATI----------------
# Tabela sa konacnim rezultatima za svaki od modela
results = data.frame(rbind(knn.eval, tree1.eval, nb1.eval, nb2.eval, glm.eval2), 
                     row.names = c("knn", "tree", "nb1", "nb2", "logreg"))
results