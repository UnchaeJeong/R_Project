install.packages('multilinguer')
library(multilinguer)
install_jdk()
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", 
                        upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
library(dplyr)

install.packages("NIADic")




install.packages('devtools')
library(devtools)
install_github('haven-jeon/NIADic/NIADic', build_vignettes = FALSE)

useNIADic()
