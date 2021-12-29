packages <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
              "nlme","lmtest","fastDummies","msm","lmeInfo","jtools")

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  instalador <- packages[!packages %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()
  }
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}
