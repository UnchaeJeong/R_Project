library(dplyr)
exam <- read.csv("data/csv_exam.csv")
exam
dim(exam)
summary(exam)
str(exam)

exam %>% filter(class == 1)

exam %>% filter(class == 2)

exam %>% filter(class != 1)

exam %>% filter(math >= 50)

#여러 조건을 충족하는 행 추출

exam %>% filter(class ==1, math >=50)

exam %>% filter(class != 2 | english >=60)

exam %>% filter(class == 1 | class == 3 | class == 4)

exam %>% filter(class %in% c(1,3,5))

class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)
mean(class1$math)
mean(class2$math)

# 혼자서 해보기
#q1.
mpg <- as.data.frame(ggplot2::mpg)
dim(mpg)
mpg_a <- mpg %>% filter(displ <=4)
mpg_b <- mpg %>% filter(displ >=5)
mean(mpg_a$hwy)
mean(mpg_b$hwy)

head(mpg)
#q2.
mpg_audi <- mpg %>% filter(manufacturer == 'audi')
mpg_toyota <- mpg %>% filter(manufacturer == 'toyota')
mean(mpg_audi$cty)
mean(mpg_toyota$cty)


#q3.
mpg_new <- mpg %>% filter(manufacturer %in% c('chevrolet','honda', 'ford'))
mean(mpg_new$hwy)

exam %>% select(class, english, math)

exam %>% select(-math, -english)

exam %>% filter(class == 1) %>% select(math, english)
exam %>% 
  filter(class != 1) %>% 
  select(-math, -english)

exam %>% 
  select(-math) %>% 
  head(10)
mpg <- as.data.frame(ggplot2::mpg)

df <- mpg %>% select(class, cty)
df
head(df)
df_suv <- df %>% filter(class == 'suv')
df_compact <- df %>% filter(class == 'compact')
mean(df_suv$cty)
mean(df_compact$cty)

exam %>% arrange(math)
exam %>% arrange(desc(math))
head(exam) %>% arrange(math)
exam %>% arrange(class, math)
