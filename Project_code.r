# Эконометрика, учебный год 2022-2023
# Образовательная программа "Экономика", НИУ ВШЭ, г. Санкт-Петербург
# Код для проекта по теме "Влияние уровня коррупции в стране на индекс счастья населения"
# Выполнила команда №2


# Подготовка к работе ==========================================================
## Загрузка пакетов ------------------------------------------------------------

library(dplyr) # манипуляции с данными
library(tidyr) # манипуляции с данными
library(ggplot2) # визуализация данных
library(stargazer)
library(lmtest)
library(margins)
library(GGally)
library(sandwich)
library(car)
library(broom)
library(ggpubr)
library(stargazer)

### Установка рабочей директории
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Общая тема оформления для всех графиков
theme_set(theme_bw(base_size = 12))

## Загрузка данных -------------------------------------------------------------
DataFrame <- read.csv("DataFrame.csv", header = TRUE, sep = ";")
## Работа с данными ------------------------------------------------------------
DataFrame <- DataFrame[1:134,1:12]

## Описательные статистики -----------------------------------------------------
summary(DataFrame)

stargazer(DataFrame, 
          type = "html",
          out = "descrstat.htm",
          title = "Описательные статистики",
          notes = "Число наблюдений = 134",
          notes.align = 'r',
          digits = 2,
          omit.summary.stat = "n",
          median = TRUE,
          covariate.labels = c("Индекс счастья","Восприятие коррупции","Логарифм ВВП на душу населения",
                               "Социальная поддержка","Личные свободы","Продолжительность жизни",
                               "Индекс свободы прессы","Доступ к интернету","Чистота выборов",
                               "WGI","Индекс хрупкости государств"))

#Средний показатель индекс счастья 5,44 из 10 => чуть больше половины населения не полностью удовлетворены жизнью
#Средний показатель коррупции 0,74 из 1 => большинство людей считают коррупцию распространенной

# Графики ======================================================================

## Переменная интереса и зависимая переменная ----------------------------------
DataFrame %>%
  ggplot(aes(corruption, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Восприятие коррупции") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья от уровня восприятия коррупции")

# Зависимость переменных похожа на линейную: чем больше коррупция, тем меньше счастье

## Прокси качества институтов --------------------------------------------------
s1 <- DataFrame %>%
  ggplot(aes(fragile, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Индекс хрупкости государств") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья от качества институтов")

s2 <- DataFrame %>%
  ggplot(aes(wgi, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Worldwide Governance Indicators") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья от качества управления")

ss2 <- ggarrange(s1, s2, ncol = 2)
ss2

## Другие переменные -----------------------------------------------------------
s3 <- DataFrame %>%
  ggplot(aes(logGDPpc, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Логарифм ВВП на душу населения, US $") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья от общего благосостояния")

s4 <- DataFrame %>%
  ggplot(aes(life_expactancy, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Продолжительность жизни, лет") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья от продолжительности жизни")

s5 <- DataFrame %>%
  ggplot(aes(social_support, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Социальная поддержка") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья от социальных контактов")

s6 <- DataFrame %>%
  ggplot(aes(freedom_private, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Свобода делать жизненный выбор") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья в зависимости от личных свобод")

s7 <- DataFrame %>%
  ggplot(aes(press_freedom, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Индекс свободы прессы") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья в зависимости от уровня цензуры")

s8 <- DataFrame %>%
  ggplot(aes(internet_access, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Доступ к Интернету") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья в зависимости от доступа в сеть")

s9 <- DataFrame %>%
  ggplot(aes(clean_elections, happiness)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Чистота выборов") + 
  ylab("Индекс счастья населения") + 
  ggtitle("Распределение индекса счастья в зависимости от честности выборов")

ss2 <- ggarrange(s3, s4, s5, s6, ncol = 2, nrow=2)
ss2

ss3 <- ggarrange(s7, s8, s9, ncol = 2, nrow=2)
ss3


# Регрессионный анализ =========================================================
## Спецификации ----------------------------------------------------------------
spec1 <- happiness~1+corruption+logGDPpc+social_support+freedom_private+life_expactancy+
  press_freedom+internet_access+clean_elections
# базовая спецификация без нелиненых эффектов

spec2 <- happiness~1+corruption+I(corruption^2)+logGDPpc+social_support+freedom_private+life_expactancy+
  internet_access+wgi+wgi:corruption+logGDPpc:corruption
#Спецификация с добавлением квадрата коррупции и перекрестными эффектами: коррупции с ВВП и качеством институтов управления

spec3 <- happiness~1+corruption+I(corruption^2)+logGDPpc+social_support+freedom_private+life_expactancy+
  internet_access+fragile+fragile:corruption+logGDPpc:corruption
#Спецификация аналогичная 2-ой, но с перекрестным эффектом коррупции с хрупкостью государства

## OLS -------------------------------------------------------------------------
ols1 <- lm(spec1, data = DataFrame)
ols2 <- lm(spec2, data = DataFrame)
ols3 <- lm(spec3, data = DataFrame)


cov1 <- vcovHC(ols1, type = "HC0")
cov2 <- vcovHC(ols2, type = "HC0")
cov3 <- vcovHC(ols3, type = "HC0")

## Робастные оценки ------------------------------------------------------------
cf1 <- coeftest(ols1, df=Inf, vcov=cov1)
cf2 <- coeftest(ols2, df=Inf, vcov=cov2)
cf3 <- coeftest(ols3, df=Inf, vcov=cov3)


## Результаты                                                                                                                                                                                                                                                                                                                                                                                                                                     зультаты ------------------------------------------------------------

stargazer(cf1, cf2, cf3,
          type = "html",
          out = "results.html", 
          title = "Результаты оценивания",
          column.labels = c("Модель 1", "Модель 2", "Модель 3"),
          notes = c("В скобках даны робастные стандартные ошибки", 
                    "Все регрессии содержат контрольные переменные:..."))
#Коррупция значима во всех моделях. При добавлении в модель нелинейных эффектов,
#Переменная интереса начинает оказывать большее влияние на индекс счастья.
#Кросс-эффекты значимы

# Проверка гипотез =============================================================
#1 Увеличение уровня коррупции в стране оказывает значимое отрицательное влияние на индекс счастья
cf1

# t-статистика = -3.7978 выше критического значения -2.32 => отвергаем нулевую гипотезу 
# об отсутствии влияния коррупции на индекс счастья на уровне значимости 1%

# Предельные эффекты ===========================================================
#Хрупкое государство с большим ввп
me1 <- ols3 %>% margins_summary(variables = "corruption",
                                at = list(fragile=93.3,logGDPpc=10.545,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols3, type = "HC0"),
                                vce = "delta")
me1
m1 <- me1 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Хрупкое государство с большим ВВП на душу")

m1
#Хрупкое государство с маленьким ввп
me2 <- ols3 %>% margins_summary(variables = "corruption",
                                at = list(fragile=93.3,logGDPpc=5.32,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols3, type = "HC0"),
                                vce = "delta")
me2
m2 <- me2 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Хрупкое государство с маленьким ВВП на душу")

m2
#Не хрупкое государство с большим ввп
me3 <- ols3 %>% margins_summary(variables = "corruption",
                                at = list(fragile=54.62,logGDPpc=10.545,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols3, type = "HC0"),
                                vce = "delta")
me3
m3 <- me3 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Нехрупкое государство с большим ВВП на душу")

m3
#Не хрупкое государство с маленьким ввп
me4 <- ols3 %>% margins_summary(variables = "corruption",
                                at = list(fragile=54.62,logGDPpc=5.32,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols3, type = "HC0"),
                                vce = "delta")
me4
m4 <- me4 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Нехрупкое государство с маленьким ВВП на душу")

m4

ame3 <- ggarrange(m1, m2, m3, m4, ncol = 2, nrow = 2) 
ame3


#Слабое по уровню управления государство с большим ввп
me5 <- ols2 %>% margins_summary(variables = "corruption",
                                at = list(wgi=21.75,logGDPpc=10.545,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols2, type = "HC0"),
                                vce = "delta")
me5
m5 <- me5 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Слабое государство с большим ВВП на душу")

m5
#Слабое по уровню управления государство с маленьким ввп
me6 <- ols2 %>% margins_summary(variables = "corruption",
                                at = list(wgi=21.75,logGDPpc=5.32,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols2, type = "HC0"),
                                vce = "delta")
me6
m6 <- me6 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Слабое государство с маленьким ВВП на душу")

m6
#Сильное по уровню управления государство с большим ввп
me7 <- ols2 %>% margins_summary(variables = "corruption",
                                at = list(wgi=69.99,logGDPpc=10.545,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols2, type = "HC0"),
                                vce = "delta")
me7
m7 <- me7 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Сильное государство с большим ВВП на душу")

m7
#Сильное по уровню управления государство с маленьким ввп
me8 <- ols2 %>% margins_summary(variables = "corruption",
                                at = list(wgi=69.99,logGDPpc=5.32,corruption=seq(from = 0, to = 1, by = 0.01)),
                                vcov = vcovHC(ols2, type = "HC0"),
                                vce = "delta")
me8
m8 <- me8 %>%
  ggplot()+
  geom_ribbon(aes(x=corruption, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5)+
  geom_line(aes(corruption, AME))+
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
  xlab("Восприятие коррупции") + 
  ylab("AME (предельный эффект)") + 
  ggtitle("Сильное государство с маленьким ВВП на душу")

m8

ame2 <- ggarrange(m5, m6, m7, m8, ncol = 2, nrow = 2) 
ame2

# Предельный эффект является значимым и степень влияния коррупции на индекс счастья
# варьируется в зависимости от собственных значений, а также уровня богатства и качества институтов
