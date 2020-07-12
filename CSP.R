library(stargazer)
library(tidyverse)
library(mlr)
library(caret)
library(clubSandwich)
library(plm)
library(wooldridge)
library(broom)
library(estimatr)

df <- read.csv("https://raw.githubusercontent.com/JoelHenr/CS-Final-Project/master/FRED2.csv")
pdim(df)
df.within <- df %>% select(DATE,GDP_PCH,BUSLOANS_PCH,T10Y2Y_PCH,DRCLACBS_PCH,MEHOINUSA646N_PCH,UNRATE_PCH,PCE_PCH,FEDFUNDS_PCH) %>%
group_by(DATE) %>% summarize(
   mean.GDP = mean(GDP_PCH),
  var.GDP = var(GDP_PCH),
  mean.LOAN = mean(BUSLOANS_PCH),
  var.LOAN = var(BUSLOANS_PCH),
  mean.YIEL = mean(T10Y2Y_PCH),
  var.YIEL = var(T10Y2Y_PCH),
  mean.DEL = mean(DRCLACBS_PCH),
  var.DEL = var(DRCLACBS_PCH),
  mean.HOUSINC = mean(MEHOINUSA646N_PCH),
  var.HOUSINC = var(MEHOINUSA646N_PCH),
  mean.UNEMP = mean(UNRATE_PCH),
  var.UNEMP = var(UNRATE_PCH),
  mean.PCE = mean(PCE_PCH),
  var.PCE = var(PCE_PCH),
  mean.FED = mean(FEDFUNDS_PCH),
  var.FED = var(FEDFUNDS_PCH)
  )

df.within %>% as.data.frame %>% stargazer(type="text")

est <- lm(GDP_PCH ~ BUSLOANS_PCH + T10Y2Y_PCH + DRCLACBS_PCH +
          MEHOINUSA646N_PCH + UNRATE_PCH + PCE_PCH + FEDFUNDS_PCH, data = df)

stargazer(est, type = "text")

est.r <- lm_robust(GDP_PCH ~ BUSLOANS_PCH + T10Y2Y_PCH + DRCLACBS_PCH +
                   MEHOINUSA646N_PCH + UNRATE_PCH + PCE_PCH + FEDFUNDS_PCH, data = df)
summary(est.r)

est.cr <- lm_robust(GDP_PCH ~ BUSLOANS_PCH + T10Y2Y_PCH + DRCLACBS_PCH +
            MEHOINUSA646N_PCH + UNRATE_PCH + PCE_PCH + FEDFUNDS_PCH, data = df, clusters = df$DATE)
summary(est.cr)
