install.packages('devtools')
library(devtools)
library(KoNLP)
library(dplyr)
install_github('haven-jeon/NIADic/NIADic', force = TRUE)
install.packages("multilinguer")
library(multilinguer)

useNIADic()

# 데이터 불러오기
txt <- readLines("Data/hiphop.txt")
head(txt)

# 특수문자 제거하기
install.packages("stringr")
library(stringr)
# 제거
txt <- str_replace_all(txt, "\\W"," ")

# 가장 많이 사용된 단어 알아보기
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다.")

# 기사에서 명사 추출
nouns <- extractNoun(txt)
#
wordcourt <- table(unlist(nouns))
df_word <- as.data.frame(wordcourt, stringsAsFactors = F)
df_word <- rename(df_word, word = Var1, freq = Freq)

# 두글자 이상
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

# 워드 클라우드 만들기
# 1. 패키지 준비하기
# 패키지 설치
install.packages("wordcloud")
# 패키지 로드
library(wordcloud)
library(RColorBrewer)
#
pal <- brewer.pal(8,'Dark2')
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)
pal <- brewer.pal(9,"Blues")[5:9]
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

# 10-2 국정원 트윗

twitter <- read.csv("Data/twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
# 변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)

# 특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

# 2. 단어빈도표
# 트윗에서 명사 추출
nouns <- extractNoun(twitter$tw)
class(nouns)

wordcourt <- table(unlist(nouns))
df_word <- as.data.frame(wordcourt, stringsAsFactors = F)

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)

top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top20

library(ggplot2)
order <- arrange(top20,freq)$word
ggplot(data = top20, aes(x=word, y=freq)) +
  ylim(0,2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label=freq), hjust = -0.3)

# 워드클라우드 만들기
pal <- brewer.pal(8, 'Dark2')
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)

pal <- brewer.pal(8, 'Blues')[5:9]
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)
          
