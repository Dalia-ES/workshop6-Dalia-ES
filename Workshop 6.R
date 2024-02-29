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

beetles %>%
  separate_wider_delim('species', '_', names=c('genus', 'species'), too_few=c('align_end')) #Separates genus and species names, and makes a new column for them
beetles

beetles %>% mutate("species"=gsub("_"," ",species)) #Uses the mutate function to remove the underscores of the values in the species column

casesdf <- read.table("WMR2022_reported_cases_3.txt",
                      sep="\t",
                      header=T,
                      na.strings=c("")) %>% 
  fill(country) %>% 
  pivot_longer(cols=c(3:14),
               names_to="year",
               values_to="cases") %>%
  pivot_wider(names_from = method,
              values_from = cases)
casesdf
casesdf <- casesdf %>% rename(c("suspected"="Suspected cases",
                                "examined"="Microscopy examined",
                                "positive"="Microscopy positive")) #Uses the rename function to rename the columns
str(casesdf,vec.len=2)
casesdf <- casesdf %>% mutate("year"=as.numeric(gsub("X","",year))) #Removes X infront of year columns and changes it to a numeric value
casesdf
str(casesdf)
casesdf %>% mutate("country"=gsub("[0-9]","",country)) #Removes all numbers from country column
casesdf %>% mutate("suspected"=as.numeric(gsub("[^0-9]","",suspected))) #Removes all numbers from suspected column and treats it as a numeric value
clean_number <- function(x) {as.numeric(gsub("[^0-9]","",x))} #Makes a function which cleans numbers and casts them to a numeric value
?across
casesdf %>% mutate(across(c(suspected,examined,positive),clean_number)) #Across function lets you apply the same function to many columns
casesdf %>% mutate(across(!country,clean_number)) #Does the same as above, highlighting everything but the country column
casesdf %>% 
  mutate(test_positivity = round(clean_number(positive) / clean_number(examined), 2)) #Makes a new column which divides one column. values by the other, rounding those values by 2 and treating both of those coliumns as numeric

casesdf <- casesdf %>% mutate(country = as.factor(country)) #Uses as.factor and mutate functions to convert country column to a factor
levels(casesdf$country) #Looks at categories of the country factor
casesdf <- casesdf %>% 
  mutate(country = gsub("Eritrae",
                        "Eritrea",
                        country)) %>%
  mutate(country = as.factor(country)) #Fixes a mispelling and converts country to a factor

write.table(casesdf, "WMR2022_reported_cases_clean.txt",
            sep="\t",
            col.names = T,
            row.names = F,
            quote = F) #Writes the table to a file called WRM2022_reported_cases_clean with plain text, tab delimited, with a header line, no quotation marks, and no row names

#Big Challenge which imports and cleans the above text file
clean_number <- function(x) {as.numeric(gsub("[^0-9]","",x))}

casesdf <- read.table("WMR2022_reported_cases_3.txt",
                      sep="\t",
                      header=T,
                      na.strings=c("")) %>% 
  fill(country) %>% 
  pivot_longer(cols=c(3:14),
               names_to="year",
               values_to="cases") %>%
  pivot_wider(names_from = method,
              values_from = cases) %>% 
  rename(c("suspected" = "Suspected cases",
           "examined" = "Microscopy examined",
           "positive" = "Microscopy positive")) %>% 
  mutate(year=as.numeric(gsub("X","",year))) %>% 
  mutate(across(c(suspected,
                  examined,
                  positive),clean_number)) %>% 
  mutate(test_positivity = round(positive / examined,2)) %>% 
  mutate(country = gsub("Eritrae",
                        "Eritrea",
                        country)) %>%
  mutate(country = as.factor(country)) 
casesdf