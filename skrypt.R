library(tidyverse)

data <- read.csv("countries of the world.csv", dec = ',', sep = ',')
colnames(data) <-c("Country","Region","Pop","Area","Popdens",
                  "Coastline","Migration","InfMortality","GDP",
                  "Literacy","Phone","Arable","Crops","Other",
                  "Climate","Birthrate","Deathrate","Agri","Industry","Service")
summary(data)
data <- data[complete.cases(data),]

data %>% group_by(Region) %>% filter(Migration > 0) %>% count()

data <- data %>% mutate(Country = str_trim(Country),Region = str_trim(Region))

data %>% group_by(Region) %>% summarise(mean_GDP = mean(GDP),
                                        net_Migration = sum(Migration)
                                        ) %>% arrange(desc(mean_GDP),net_Migration)

data %>% filter(Migration > 0) %>% group_by(Region) %>% filter(Popdens == max(Popdens)) %>% arrange(Popdens)

#y = a + b*x + E #epsioln - zmienna losowa b - wpływ współczynnik

model <- lm(GDP ~ Migration,data)
summary(model)

model <- lm(Area ~ Migration,data)
summary(model)

cor(data$GDP,data$Migration)^2

#data[,1] = data$Country

#jak stałą ????

#dodawanie zmiennych 
model <- lm(GDP ~ Migration + Area,data)
summary(model)
#wszystko model <- lm(GDP ~ .,data)
#zamiana kategorycznych na liczbowe

data_num <- data[,3:20]
model <- lm(GDP ~ .,data_num)
summary(model)

n <- nrow(data)
count <- 0
for(i in 1 : 1000){
  random <- rnorm(n)
  df <- data.frame(data$GDP, Random = random) #tworzymy tabelke
  model <- lm(data$GDP ~ Random,df)
  summ <- summary(model)
  if(summ$coefficients[2,4] < 0.05){
    count <- count + 1
  } #zliczamy ile razy trafimy na istotną
}
#konstrukcja testu istotności - zmienne mogą byc przypadkowo istotne  - hipoteza zerowa - bo niezależne - korelacja równa 0
#model$coefficients
#sum$coefficients