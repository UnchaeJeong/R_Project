install.packages('foreign')
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
raw_welfare <- read.spss(file = 'Koweps_hpc10_2015_beta1.sav', to.data.frame = T)
welfare <- raw_welfare
class(welfare)

str(welfare)
View(welfare)
dim(welfare)
tail(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
str(welfare)

# 09-2 성별에 따른 월급 차이

class(welfare$sex)
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 1, "male","female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income) + xlim(0,1000)

summary(welfare$income)
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare$income))

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_col()

# 09-3 나이와 월급의 관계

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

summary(welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income)

ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line()

# 09-4 연령대에 따른 월급 차이

welfare <- welfare %>% 
  mutate(ageg = ifelse(age <30, 'young',
                       ifelse(age <= 59, 'middle','old')))
table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income

ggplot(data=ageg_income, aes(x = ageg, y=mean_income)) + geom_col() + scale_x_discrete(limits = c('young','middle','old'))

# 연령대 및 성별 월급 차이

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data=sex_income, aes(x = ageg, y = mean_income, fill = sex)) + geom_col(position="dodge") + scale_x_discrete(limits = c('young','middle','old'))


#나이 및 성별 월급 차이 분석하기
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age)
ggplot(data=sex_age, aes(x=age, y=mean_income, col = sex)) + geom_line()

# 09-6 직업별 월급 차이
class(welfare$code_job)
table(welfare$code_job)
library(readxl)
list_job <- read_excel('한국복지패널데이터_etc/Koweps_Codebook.xlsx', col_name = T, sheet = 2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, id = 'code_job')
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

job_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data=top10, aes(x=reorder(job, mean_income), y = mean_income)) + geom_col() + coord_flip()

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

ggplot(data=bottom10, aes(x = reorder(job, -mean_income), y = mean_income)) + geom_col() +coord_flip() + ylim(0,850)

# 09-7 성별 직업 빈도 
head(welfare)

job_male <- welfare %>% 
  filter(!is.na(job) & sex == 'male') %>% 
  group_by(job) %>% 
  summarise(n= n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male         

ggplot(data = job_male, aes(x = reorder(job,n), y=n)) + geom_col() + coord_flip()

job_female <- welfare %>% 
  filter(!is.na(job) & sex == 'female') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

ggplot(data=job_female, aes(x = reorder(job, n), y=n)) + geom_col() + coord_flip()

# 09-8 종교 유무에 따른 이혼율


