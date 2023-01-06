## Pacotes

library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(ggplot2)

## Carregando os dados

sleep_raw <- read_csv("fitabase_data/sleepDay_merged.csv")
daily_raw <- read_csv("fitabase_data/dailyActivity_merged.csv")
weight_raw <- read_csv("fitabase_data/weightLogInfo_merged.csv")

## Pré-visualização

lapply(list(sleep_raw, daily_raw, weight_raw), skim)

lapply(list(sleep_raw, daily_raw, weight_raw), str)

lapply(list(sleep_raw, daily_raw, weight_raw), function(x) n_distinct(x$Id))

## Limpar e formatar 

lapply(list(sleep_raw, daily_raw, weight_raw), function(x) sum(duplicated(x)))

sleep_raw <- sleep_raw %>%
  distinct() %>%
  drop_na()

daily_raw <- daily_raw %>%
  distinct() %>%
  drop_na()

weight_raw <- weight_raw %>%
  select(Id, Date, WeightKg, BMI)

lapply(list(sleep_raw, daily_raw, weight_raw), function(x) sum(duplicated(x)))

sleep_raw <- sleep_raw %>%
  clean_names()

daily_raw <- daily_raw %>%
  clean_names()

weight_raw <- weight_raw %>%
  clean_names()

daily_raw <- daily_raw %>%
  rename(date = activity_date) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

sleep_raw <- sleep_raw %>%
  rename(date = sleep_day) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p"))

weight_raw <- weight_raw %>%
  rename(date = date) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p"))

fitdata <- merge(daily_raw, sleep_raw, by = c("id", "date"))

fitdata <- fitdata %>% 
  left_join(weight_raw, by = c("id", "date"))

# Análise
fitdata %>%
  select(very_active_minutes,
         fairly_active_minutes, 
         lightly_active_minutes, 
         sedentary_minutes) %>% 
  summary()

fitdata %>%
  select(total_minutes_asleep) %>% 
  summary()

fitdata %>%
  select(weight_kg, bmi) %>% 
  summary()

# Padrões de Atividade Física ao Longo do Tempo

#transform the data frame into a long format, where each row represents a single value of a variable
fitdata_pivot <- 
fitdata %>% 
  pivot_longer(cols = c(sedentary_minutes, 
                        very_active_minutes, 
                        fairly_active_minutes, 
                        lightly_active_minutes), 
               names_to = "tipo_de_atividade", values_to = "minutos") 
  
fitdata_pivot %>% 
  group_by(date, tipo_de_atividade) %>%
    summarise(avg_minutes = mean(minutos)) %>% 
    ggplot(aes(x=date, y = avg_minutes, color = tipo_de_atividade, group = tipo_de_atividade))+
    geom_line(size = 0.8)+
    labs(title = "Média de Minutos de Atividade por Dia", 
         x = "Data", y = "Média de Minutos de Atividade") + 
    theme_minimal(base_size = 12) +  
    theme(plot.title = element_text(face = "bold", size = 14)) + 
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +  
    scale_y_continuous(limits = c(0, 850), breaks = seq(0, 850, by = 250))+ 
  scale_color_manual(values = c("red", "blue", "green", "orange"),
                     labels =
                       c( "Moderademente Ativo",
                          "Levemente Ativo",
                          "Sedentário",
                          "Muito Ativo"),
                     name = "Tipo de Atividade")

fitdata_pivot %>%
  group_by(date, tipo_de_atividade) %>%
  summarise(avg_minutes = mean(minutos)) %>%
  mutate(dia_da_semana = ifelse(wday(date) %in% c(1, 7), "Fim de Semana", "Semana")) %>% # Adicione uma nova coluna "dia da semana"
  ggplot(aes(x=date, y = avg_minutes, color = tipo_de_atividade, group = tipo_de_atividade))+
  geom_line(size = 0.8)+
  labs(title = "Média de Minutos de Atividade por Dia",
       x = "Data", y = "Média de Minutos de Atividade") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 850), breaks = seq(0, 850, by = 250))+
  scale_color_manual(values = c("red", "blue", "green", "orange"),
                     labels =
                       c( "Moderademente Ativo",
                          "Levemente Ativo",
                          "Sedentário",
                          "Muito Ativo"),
                     name = "Tipo de Atividade") +
  facet_wrap(~dia_da_semana) # Adicione a faceta "dia da semana"



fitdata %>% 
  ggplot(aes(x = total_minutes_asleep)) +
  geom_point(aes(y = very_active_minutes, color = "Muito Ativo")) +
  geom_point(aes(y = fairly_active_minutes, color = "Moderadamente Ativo")) +
  geom_point(aes(y = lightly_active_minutes, color = "Levemente Ativo")) +
  geom_point(aes(y = sedentary_minutes, color = "Sedentário")) +
  labs(title = "Correlação entre Minutos de Atividade e Minutos Dormindo", 
       x = "Minutos Dormindo", 
       y = "Minutos de Atividade")

cor(fitdata$total_minutes_asleep, fitdata$very_active_minutes)
cor(fitdata$total_minutes_asleep, fitdata$fairly_active_minutes)
cor(fitdata$total_minutes_asleep, fitdata$lightly_active_minutes)
cor(fitdata$total_minutes_asleep, fitdata$sedentary_minutes)

correlation(
  data = fitdata,
  select = "total_minutes_asleep",
  select2 = "sedentary_minutes",
  method = "pearson",
  alternative = "two.sided"
) 

