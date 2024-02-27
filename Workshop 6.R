install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
beetles <- read.table("dung_beetles.csv", sep=",",header=T)
View(beetles)
str(beetles)
?select
beetles %>% select(1:68) #Selects species columns by numbers
beetles %>% select(c(Month, contains('_'))) #Selects month column, and all other columns which contain _
beetles %>% select(!Month) #Selects everything, apart from the column month, using ! (meaning apart from)
beetles %>% select(!c(Month, Site)) #Selects everything apart from the month and site column
