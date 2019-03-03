library(lmtest)
library(dplyr)
library(broom)
library(ggplot2)
library(forecast)
library(strucchange)
library(tseries)
library(readxl)
library(vcd)
library(vars)
library(rio)
library(urca)
library(funModeling) 
library(Hmisc)
library(CARS)
library(readxl)



#Загрузка данных
full_data <-read_excel("C:/Users/oabek/Desktop/default.xlsx")
head(full_data[,6:26])
live <- read_excel("C:/Users/oabek/Desktop/live.xlsx")
head(live)
default <- subset(full_data, default==1)
head(default)

my_data <- rbind(live, default)

head(my_data)


#Сводная информация
summary(my_data[,6:26])

#Визуализация
plot_num(my_data[,6:26])

plot(my_data$assets)
ggplot(my_data, aes(x=assets))+ geom_histogram() + facet_grid(default ~ .)

#Визуализация по группам дефолт/выжившие

my_data$default <- factor(my_data$default)


ggplot(default, aes(x=assets))+ geom_histogram()



#Из выживших отобрать n строк
unique_id <- unique(my_data$ID)


randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}

#Построение модели для 50 случайных выборок
#Выбор лучшей по тесту ХЛ

set.seed(567)
myResults <- list()
df.split <- split(live, live$ID)

hl_pval <- c()
model_list <- list()
hl <- list()



for(i in 1:50){
  df.sample <- lapply(df.split, randomRows, 1)
  df.final <- do.call("rbind", df.sample)
  randomly_live <- randomRows(df.final, nrow(default))
  data1 <- rbind(default, randomly_live)
  model_list[[i]] = glm(default ~ log(assets)+sk_a+hh_dep_a+hh_dep_liab+gov_sec_a+
                nongov_sec_a+cor_acc_a+res_to_cb_a+foreign_liab_a+
                credit_to_banks_A+nbs_credit_a+credit_to_hh_a+
                mbk_a+res_a+delayed_nbs_credit+overdue_liab+
                liab_bank_ratio+marketdebt_liab+foreign_assets_a +H1, data = data1,
              family = 'binomial')
  hl[[i]] <- hoslem.test(model_list[[i]]$y, fitted(model_list[[i]]), g=10)
  hl_pval[i] <- hl[[i]]$p.value
  }



summary(model_list[[which.max(hl_pval)]])

max(hl_pval)

