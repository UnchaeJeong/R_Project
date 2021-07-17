# 11 지도 시각화

install.packages("maps")
install.packages("mapproj")
install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)

library(tibble) # 도시명을 추가하고 싶어서 쓴 것.
crime <- rownames_to_column(USArrests,var = "state") # 컬럼값으로 끄집어 온 것. 차이점 확인. 행번호 생김.
crime$state <- tolower(crime$state)
head(crime)

str(crime)

#install.packages("maps")
library(ggplot2)
states_map <- map_data("state")
str(states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T)

# 11-2 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기

install.packages("stringi")

devtools::install_github("cardiomoon/kormaps2014", force = TRUE)
library(kormaps2014)
str(changeCode(korpop1))

library(dplyr)
korpop1 <- rename(korpop1, pop = 총인구_명, name=행정구역별_읍면동)
str(korpop1) # 윈도우가 cp 949라서 못읽음. 
str(korpop1$name) # 깨져서 나옴
korpop1$name <- iconv(korpop1$name, "UTF-8","CP949") # 내부는 utf8인데 윈도우가 cp949로 읽어서 해석함.  name만 cp 949로 바꿔서 읽을 수 있게 됨.
str(korpop1$name)

str(changeCode(kormap1))

ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1, 
             interactive = T)

str(changeCode(tbc))
str(tbc)
tbc$name <- iconv(tbc$name, "UTF-8", "CP949")
str(tbc)

ggChoropleth(data = tbc,
             aes(fill = NewPts, # 결핵 환자수로 변신
                 map_id = code,
                 tooltip = name),
             map = kormap1, 
             interactive = T)


# 12 인터랙티브 그래프
install.packages("plotly")
library(plotly)

library(ggplot2)

p <- ggplot(data=mpg, aes(x=displ, y=hwy, col = drv)) +geom_point()
ggplotly(p)


# 12-2 dygraphs패키지로

install.packages('dygraphs')
library(dygraphs)
economics <- ggplot2::economics
head(economics)
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)

dygraph(eco)
dygraph(eco) %>% dyRangeSelector()

