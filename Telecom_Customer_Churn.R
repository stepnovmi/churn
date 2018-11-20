# Установка нужных пакетов (если ещё не установлены)

#install.packages(c("eeptools","e1071","plyr","corrplot","ggplot2","gridExtra","ggthemes","recipes","caret","MASS","randomForest","party","dplyr","data.table"))

# Подгрузка нужных библиотек
library(dplyr)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(recipes)
library(caret)
library(MASS)
library(randomForest)
library(party)

library(eeptools)
library(data.table)
library(e1071)

# Очистка рабочего пространства 
rm(list = ls())
options(warn=-1)
# Подгрузка исходного .csv. Предварительно в исходном .csv были заменены русские слова (Активен, Блокирован, Закрыт, Приоставновлен)
# на английские для избегания проблем с кодировкой

churn <- read.csv2('main_input.csv',na.strings=c("","NA"))
os <- read.csv('DS_ex2_os_04.18.csv',na.strings=c("","NA"))
cc <- read.csv("DS_ex2_callcenter_04.18.csv",col.names = c("SUBS_ID","CC"),na.strings=c("","NA"))

# Совмещение основной базы и информации об ОС и звонках в колл-центр

churn <- join(churn,os, by ="SUBS_ID")
churn <- join(churn,cc, by ="SUBS_ID")

# Очистка базы от пустых ячеек (к сожалению, точно смоделировать отток при наличии пустых ячеек почти невозможно)

sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn), ]


# Замена всех статусов кроме "Active" на "Inactive" и сохранение этого столбца как фактора - подготовка целевого параметра прогнозирования
# Тесты модели показали, что точность прогнозирования фактора выше чем значений TRUE/FALSE
churn$STATUS <- as.factor(mapvalues(churn$STATUS, 
                                    from=c("Active"),
                                    to= c("No")))
churn$STATUS <- as.factor(mapvalues(churn$STATUS, 
                                    from=c("Blocked"),
                                    to= c("Yes")))
churn$STATUS <- as.factor(mapvalues(churn$STATUS, 
                                    from=c("Paused"),
                                    to= c("Yes")))
churn$STATUS <- as.factor(mapvalues(churn$STATUS, 
                                           from=c("Closed"),
                                           to= c("Yes")))
churn$STATUS<- as.factor(churn$STATUS)

# Пол также нужно сохранить как фактор, т.к. иначе пол "1" будет считаться как что-то большее, чем "0"
churn$GNDR_ID <- as.factor(churn$GNDR_ID)

# В процессе тестирования модели оказалось, что значимая разница возникает только в факте "обращался в Колл-центр/не обращался в колл-центр"
# Это просиходит потому, что не обращавшихся сильно больше, чем обращавшихся
# Поэтому следует принять как фактор данную характеристику, со значениями "обращался в колл-центр/не обращался"

churn$fctrcc  <- as.factor(ifelse(churn$CC >0,1,0))

# После преобразования исходный столбец удаляется
churn$CC <- NULL

# Работа с параметром "Дата рождения": во-первых, его нужно перевести в стандартный вид и сохранить в типе данных "Дата"
# Во-вторых, оперировать датой рождения малоинформативно для прогноза. Поэтому преобразовываем дату рождения в возраст абонента
churn$DATE_OF_BIRTH <-gsub(" .*","",churn$DATE_OF_BIRTH)
churn$DATE_OF_BIRTH <- as.Date(churn$DATE_OF_BIRTH, "%d.%m.%Y")
churn$DATE_OF_BIRTH <- age_calc(churn$DATE_OF_BIRTH, enddate = Sys.Date(), units = "years", precise = TRUE)

# Посмотрим корелляционную матрицу всех числовых параметров модели
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# Видно, что  во-первых, есть однозначная корелляция между числом оплат и расходами. Соответственно один из параметров необходимо убрать
# также в рамках тестов модели было выявлено, что абсолютые значения оплаты и расходов не несут ценности для модели
# В связи с этим они были заменены на 2 параметра: средняя оплата (Оплата, деленная на число оплат) и разница между оплатой и расходами

churn <- churn  %>% mutate(DIFF = CHARGE/PAYMENT -1) %>% mutate(PP = PAYMENT/COUNT.PT.PAY_ID.)
churn$PAYMENT <- NULL
churn$CHARGE <- NULL
#churn$COUNT.PT.PAY_ID.<- NULL
 
# Вновь посмотрим корелляционную матрицу всех числовых параметров модели
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# Теперь проблемы сильной корелляции между параметрами нет, однако возраст сильно выбивается из остальных параметров (на основе тестов)
# Поэтому принимается решение заменить значения возраста возрастными группами

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 18){
    return('Child')
  }else if(tenure > 18 & tenure <= 25){
    return('Young')
  }else if (tenure > 25 & tenure <= 60){
    return('Adult')
  }else if (tenure > 60){
    return('Old')
  
  }
}
churn$age_group <- sapply(churn$DATE_OF_BIRTH,group_tenure)
churn$DATE_OF_BIRTH <- NULL

# Финальная корелляционная матрица всех числовых параметров модели. Средняя оплата не имеет корелляции с остальными параметрами, но в дальнейшем будет очень значима для модели
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# Далее следует посмотреть на статистику нечисловых показателей
p1 <- ggplot(churn, aes(x=GNDR_ID)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=age_group)) + ggtitle("Age Group") + xlab("Age Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=OS)) + ggtitle("OS") + xlab("OS") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=fctrcc)) + ggtitle("Contact Center usage") + xlab("Contact Center usage") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2,p3,p4, ncol = 4)


# На графике видно, что в данных есть абоненты с возрастом меньше года. 
# Стоит принять это за ошибку внесения инфомрации и удалить из выборки

churn$age_group <- as.factor(mapvalues(churn$age_group, 
                                     from=c("Child"),
                                     to= NA))
churn <- churn[complete.cases(churn), ]

# Вновь посмотрим на статистику нечисловых показателей и убедимся, что теперь все данные логичны

p1 <- ggplot(churn, aes(x=GNDR_ID)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=age_group)) + ggtitle("Age Group") + xlab("Age Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=OS)) + ggtitle("OS") + xlab("OS") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=fctrcc)) + ggtitle("Contact Center usage") + xlab("Contact Center usage") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2,p3,p4, ncol = 4)

# На этом этап подготовки датасета для анализа завершен
##########################################################################################################################################
# Cледующий этап: разбивка выборки на тестовую и обучающую. 70% наблюдений должны попасть в обучающую выборку
# Распределение классов для прогнозирования сохраняется в тех же пропорциях

intrain <- createDataPartition(churn$STATUS,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]

# На данном этапе следует исключить SUBS_ID из датасетов, чтобы он не стал параметром регрессии
subsid <- data.frame(testing$SUBS_ID)
testing$SUBS_ID <- NULL
training$SUBS_ID <- NULL

##########################################################################################################################################
# Расчет логистической регрессии

LogModel <- glm(STATUS~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

# Выявлено большое количество важных переменных, а это говорит, что коррелляция действительно есть.
# На данном этапе становится видно, что больше всего снижает residual deviance параметр "Число Оплат",
# а наивысшее значение качества Pr(>Chi) дает средняя оплата. Также важна разница между оплатой и расходами

anova(LogModel, test="Chisq")

# Используем полученную модель для прогноза на тествоых данных
testing$STATUS <- as.character(testing$STATUS)
testing$STATUS[testing$STATUS=="No"] <- "0"
testing$STATUS[testing$STATUS=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$STATUS)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); table(testing$STATUS, fitted.results > 0.5)
testing$STATUS[testing$STATUS=="0"] <- "No"
testing$STATUS[testing$STATUS=="1"] <- "Yes"

# Полученный результат уже очень высокий, 89%. Это было ожидаемо в связи с высокой значимостью выбранных параметров
# Попробуем улучшить его с помощью метода decision tree
##########################################################################################################################################
exp(cbind(OR=coef(LogModel), confint(LogModel)))
# Анализ вероятности наступления события еще раз указывает на огромную важность средней оплаты, равно как и разница между оплатой и расходами и число оплат
tree <- ctree(STATUS~COUNT.PT.PAY_ID.+PP+DIFF+fctrcc, training)
plot(tree)
# Получившееся дерево решений
##########################################################################################################################################
# Далее происходит прогнозирование на основе получившегося дерева
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$STATUS)

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$STATUS)
tab2 <- table(Predicted = pred_tree, Actual = testing$STATUS)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
# Точность модели составила 93%
##########################################################################################################################################
# Попробуем увеличить точность путем использования алгоритма Random forest.
# Обучение модели:
rfModel <- randomForest(STATUS ~., data = training)
print(rfModel)


# Прогнозирование и оценка результата:
pred_rf <- predict(rfModel, testing)

caret::confusionMatrix(table(pred_rf, testing$STATUS)) 
plot(rfModel)
# Попробуем улучшить результат. На графике получившейся модели видно, что падение ошибки прекращается после 100 деревьев. Посмотрим какое значение mtry выбрать
t <- tuneRF(training[, -8], training[, 8], stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)
# Минимальное значение ошибки при mtry = 1, зададим его
rfModel_new <- randomForest(STATUS ~., data = training, ntree = 100, mtry = 1, importance = TRUE, proximity = TRUE)
print(rfModel_new)

pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(table(pred_rf_new, testing$STATUS))

# Получившаяся модель даже без коррецкии параметров дает результат в 94%, чего более чем достаточно для поставленной задачи
##########################################################################################################################################
# далее следует записать получившийся результат в  .csv притянув обратно значения SUBS_ID
pred_rn <- data.frame(pred_rf)
testing <-cbind(testing,pred_rn)
testing <-cbind(testing,subsid)
testing$STATUS <- as.factor(mapvalues(testing$STATUS, 
                                             from=c("No"),
                                             to= c("Active")))
testing$STATUS <- as.factor(mapvalues(testing$STATUS, 
                                      from=c("Yes"),
                                      to= c("Inactive")))
testing$pred_rf <- as.factor(mapvalues(testing$pred_rf, 
                                      from=c("No"),
                                      to= c("Active")))
testing$pred_rf<- as.factor(mapvalues(testing$pred_rf, 
                                      from=c("Yes"),
                                      to= c("Inactive")))
testing$pred_rf_new <- NULL
write.csv(testing,"test_result.csv",row.names = FALSE)

# По итогам моделирования можно сделать следующие важные выводы:
# 1. Данный датасет вероятно имеет недостаточное количество параметров.
# Рекомендуется добавить такие параметры как например "время, которое клиент с оператором", "метод оплаты"
# 2. Выборка достаточно "однобока" - значений "Неактивен" слишком мало по отношению к "Активен". 
# Это невозможно исправить стандартными методами (удаление части записей, которых больше), так как тогда число наблюдений будет слишком мало.
# Для дальнейшего анализа рекомендуется подготавливать более сбалансированный датасет. 
# 3. Следует уточнить, что считать "ушедшим" клиентом. Либо использовать тайм лаг. Сейчас получается, что у большого числа "ушедших" клиентов значимые параметры характерны для полного отсутствия активности.
# Возможно, стоит брать данные клиентов, о которых известно, что через N месяцев они ушли. Это существенно повысит бизнес-применимость модели.
