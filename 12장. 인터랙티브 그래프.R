# p.289 12장. 인터랙티브 그래프 12-1 plotly패키지로 인터랙티브 그래프 만들기

install.packages('plotly')
library(plotly)

library(ggplot2)
p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) + geom_point()
ggplotly(p)
