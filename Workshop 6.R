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

beetles %>% 
  filter(Onthophagus_sideki > 10) #Removes rows that have less than 10 Onthophagus sideki, using the filter function
beetles %>% 
  filter(Onthophagus_sideki & Ochicanthon_woroae > 10) #Removes the rows for two of the species rows which are less than 10
beetles %>%
  filter(Ochicanthon_woroae > 15, Month == 'July') #Selects only the rows that this species has greater than 15 samples in the month of July


