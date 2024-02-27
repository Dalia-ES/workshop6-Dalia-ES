install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
beetles <- read.table("dung_beetles.csv", sep=",",header=T, na.strings=c('X.1','X.2','X.3','X.4','X.5','X.6','X.7','X.8','X.9','X.10','X.11','X.12'))
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

beetles %>% rename(c(Copris_agnus=Copis_agnus,
                     Copris_ramosiceps=Copis_ramosiceps)) #Individually renames vectors using the rename function
?rename
fixcopris <- function(x) {gsub("opis","opris",x)} #Creates a function tht replaces opis with opris
beetles %>% rename_with(fixcopris) #Renames the species with the rename_with function, using the function I created
beetles %>%
  rename_with(~gsub("opis", "opris",.), starts_with("Copis")) #Alternative way to do the above without creating a new function. ~ means 'as a function of'
beetles <- beetles %>%
  rename_with(~gsub("opis", "opris",.), starts_with("Copis")) %>%
  pivot_longer(cols=3:last_col(), names_to='Species', values_to='Count') #Condenses the table, moving all species into their own column
beetles

beetles <- beetles %>%
  rename_with(tolower, everything()) #Converts capitalized column names to lower case
beetles

beetles <- beetles %>%
  separate_wider_delim('species', '_', names=c('genus', 'species'), too_few=c('align_end')) #Separates genus and species names, and makes a new column for them
beetles

?separate_wider_delim