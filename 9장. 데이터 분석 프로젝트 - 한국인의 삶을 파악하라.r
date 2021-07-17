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
# 아래와 같음  welfare$income <- ifelse((welfare$income == 0 | welfare$income == 9999), NA, welfare$income)
table(is.na(welfare$income))

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data=sex_income, aes(x=sex, y=mean_income)) + geom_col()

# 09-3 나이와 월급의 관계
# 몇 살 때 월급을 가장 많이 받을까?
#
# 분석 절차
# 1. 변수 검토 및 전처리
#    나이, 월급
# 2. 변수 간 관계 분석
#    나이에 따른 월급 평균표 만들기
#    그래프 만들기

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
# 분석 절차
# 변수 검토 및 전처리
# 연령대 : 초년 :30세 미만, 중년 : 30~59세, 노년 : 60이상
# 월급

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

# 09-5 연령대 및 성별 월급 차이

sex_income <- welfare %>% 
  filter(!is.na(income)) %>%  # 결측치 빼고 
  group_by(ageg,sex) %>%      # 총 6개로 구분 청중장년층(30 * 성별(2)
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
library(ggplot2)
list_job <- read_excel('한국복지패널데이터_etc/Koweps_Codebook.xlsx', col_names = T, sheet = 2)
head(list_job)
dim(list_job)
dim(welfare)

welfare <- left_join(welfare, list_job, id = 'code_job') # 열추가
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10) # 없던 테이블을 만들어 붙임.

# 직업별 월급 차이 분석하기
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data = top10, aes(x = reorder(job,mean_income), y = mean_income)) + 
  geom_col() +
  coord_flip()

# 하위 10위 추출
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10
# 같음. 비교 해보자
bottom10 <- job_income %>% 
  arrange(-mean_income) %>% 
  tail(10)
bottom10

ggplot(data=bottom10, aes(x = reorder(job, -mean_income),
                          y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0,850)

# 09-7 성별 직업 빈도
# 성별로 어떤 직업이 가장 많을까?
# 1. 성별 직업 빈도표 만들기
# 남성 직업 빈도 상위 10개 추출
job_male <- welfare %>% 
  filter(!is.na(job) & sex =='male') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male  

# 여자를 구해보자
job_female <- welfare %>% 
  filter(!is.na(job) & sex =='female') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female  

# 그래프를 만들어보자
# 남성 직업 빈도 상위 10개 직업

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

class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, 'yes', 'no')
table(welfare$religion)

class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage ==1, 'marriage',
                                 ifelse(welfare$marriage ==3, 'divorce',NA))
table(welfare$group_marriage)

qplot(welfare$group_marriage)

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
religion_marriage

# 이혼율 표를 만들겠습니다.
divorce <- religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(religion, pct)
divorce

ggplot(data=divorce, aes(x = religion, y = pct)) +
  geom_col()

# 1. 연령대별 이혼율 표 만들기
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

ageg_marriage

str(welfare$ageg)

# 연령대별 이혼율 그래프 만들기
ageg_divorce <- ageg_marriage %>% 
  filter(ageg != 'young' & group_marriage == 'divorce') %>% 
  select(ageg, pct)
ageg_divorce

#2. 연령대별 이혼율 그래프를 그립시다.
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) + geom_col()


# 3. 연령대 및 종교 유무에 따른 이혼율 표 만들기

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg !='young') %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))
ageg_religion_marriage

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != 'young') %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(ageg, religion, pct)

df_divorce

# 4. 연령대 및 종교 유무에 따른 이혼율 그래프 만들기
ggplot(data = df_divorce, aes(x = ageg, y=pct, fill = religion)) + geom_col(position = 'dodge')

# p.254. 09-9 지역별 연령대 비율 - 노년층이 많은 지역은 어디일까?

class(welfare$code_region)
table(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region = c('서울','수도권','부울경','대구경북','대전충남','강원충북','광주전남제주'))
list_region

# 지역명 변수 추가
welfare <- left_join(welfare, list_region, id = 'code_region')

# 지역별 연령대 비율 분석하기

# 1. 지역별 연령대 비율표 만들기
region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))
head(region_ageg)

region_ageg <- welfare %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100,2))
region_ageg

# 2. 그래프 만들기
ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg)) + geom_col() + coord_flip()

# 3. 노년층 비율 높은 순으로 막대 정렬하기
list_order_old <- region_ageg %>% 
  filter(ageg == 'old') %>% 
  arrange(pct)
list_order_old

# 지역별 순서 변수 만들기
order <- list_order_old$region
order

ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg)) + geom_col() + coord_flip() + scale_x_discrete(limits = order)

# 4. 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c('old','middle','young'))
class(region_ageg$ageg)
levels(region_ageg$ageg)

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() + 
  scale_x_discrete(limits = order)
