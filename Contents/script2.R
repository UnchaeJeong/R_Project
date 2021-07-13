library(dplyr)
exam <- read.csv('data/csv_exam.csv')
exem <- read.csv('data/csv_exam.csv')
rm(exem) # 글로벌 변수 지우기

tail(exam)
class(exam)
str(exam)

exam %>% filter(class ==1)
a <- exam %>% filter(class ==1)
class(a) # 데이터 형식df

exam %>% filter(class ==1)
exam %>% filter(class ==2)
exam %>% filter(class !=2)
# 리턴형이 데이터 프레임형식으로 나온다.

exam %>%  filter(class ==1 & math > 50)

exam %>% filter(class ==2 | math>50) # 거짓이면 뒤에도 확인
# or일때는 많이 해당하는 걸 앞에 두고
# and일때는 적게 해당하는 걸 앞에 두는 것이 속도 면에서 빠르다.

exam %>% filter(class==1|class==2|class==3)
exam %>% filter(class!=4 &class!=5)

exam %>% filter(class %in% c(1,3,5))

# 모니터에 뿌리기만함. 저장하고 싶다면 변수지정하면 됨.

mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
a<-mpg %>% filter(displ <=4)
b<-mpg %>% filter(displ >=5)
mean(a$hwy)
mean(b$hwy)

audi <- mpg %>% filter(manufacturer == 'audi')
toyota <- mpg %>% filter(manufacturer == 'toyota')

mean(audi$cty)
mean(toyota$cty)

mpg_new <- mpg %>% filter(manufacturer %in% c('chevrolet',
                                              'ford','honda'))
head(mpg_new)
mean(mpg_new$hwy)

# p.134 06-3 필요한 변수만 추출하기

class(exam)
a <- exam %>% select(math)
class(a)
exam %>%  select( -math, -english)

exam %>%  filter(class == 1 | class ==2)  %>% select(english)
class(exam) #select가 받는 값이 df인지 뭔지 알면 함수를 계속
## 이어서 쓸 수 있음.


exam %>% arrange(math)

mpg
mpg %>% filter(manufacturer == 'audi') %>% 
  arrange(desc(hwy)) %>% 
  head(5)

# p.142. 06-5 파생변수 추가하기.

exam_new <- exam %>% 
  mutate(total = math + english + science) %>% 
  head        
exam_base <- exam
exam_base$total <- exam$math + exam$english + exam$science
exam_base$mean <-  exam$math + exam$english + exam$science
head(exam_base)
exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>% 
  head(3)

exam %>%  # dplyr 데이터 넘길 때 %>% 사용 data.frame으로 넘어감
## 결과값도 df로 넘어온다는 것.
  mutate(test = ifelse(science >=60,'pass','fail')) %>% 
  head(5)
head(exam_base)
exam_base$test<- ifelse(exam_base$science >= 60, 'pass','fail')
head(exam_base)

exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head

# p.144. 알아두면 좋아요 (컴퓨터 버벅거려서 하둡이란 분산처리시스템이 있음)


exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(desc(total)) %>% 
  head
# 이건 휘발되어버림 지울필요없으니 메모리 효율이 좋음.
# 위에서 mutate를 통해 만든 함수를 밑에 함수에서 사용할 수 있음

head(exam_base)
exam_base$total <- exam_base$math + exam_base$english + exam_base$science
head(exam_base)
exam_base %>% arrange(total)
exam_base %>% arrange(desc(total))
# 이건 저장되어 버려서 메모리 효율이 떨어짐.
