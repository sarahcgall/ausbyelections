#=========================LIBRARY============================#
library(tidyverse)
library(rvest)
library(htmltab)
library(rlist)
library(XML)
library(lubridate)
library(janitor)
#============================================================#



#============================================================#
#                         SCRAPE
#============================================================#
#Scrape data
wiki <- htmltab("https://en.wikipedia.org/wiki/List_of_Australian_federal_by-elections", which = 1, 
                colNames = c("Division","Date","Cause","Former member","RM1","Party of former member","Winner","RM2","Party of new member"))

#Remove colour columns
wiki <- wiki %>%
  select(Division, Date, Cause, `Former member`, `Party of former member`, Winner, `Party of new member`)

#Remove titles
wiki <- wiki %>%
  mutate(Division = ifelse(str_detect(wiki$Division, "Parliament"), "Parliament", Division)) %>%
  filter(Division != "Parliament" & Division != "No by-elections held") %>%
  mutate(Date = str_replace_all(Date, "\\(", ""), Date = str_replace_all(Date, "\\)",""))

#============================================================#
#Scrape results per wiki page

#Parse URLs
pages <- c("2020_Eden-Monaro_by-election","2018_Wentworth_by-election", "2018_Braddon_by-election", "2018_Fremantle_by-election", "2018_Longman_by-election", "2018_Mayo_by-election", "2018_Perth_by-election", "2018_Batman_by-election", "2017_Bennelong_by-election", "2017_New_England_by-election",
           "2015_North_Sydney_by-election","2015_Canning_by-election","2014_Griffith_by-election","2009_Higgins_by-election", "2009_Bradfield_by-election","2008_Lyne_by-election","2008_Mayo_by-election","2008_Gippsland_by-election","2005_Werriwa_by-election","2002_Cunningham_by-election",
           "2001_Aston_by-election","2001_Ryan_by-election","2000_Isaacs_by-election","1999_Holt_by-election","1997_Fraser_by-election","1996_Lindsay_by-election","1996_Blaxland_by-election","1995_Wentworth_by-election","1995_Canberra_by-election","1994_Kooyong_by-election",
           "1994_Mackellar_by-election","1994_Warringah_by-election","1994_Bonython_by-election","1994_Fremantle_by-election","1994_Werriwa_by-election","1992_Wills_by-election","1991_Menzies_by-election","1989_Gwydir_by-election","1988_Oxley_by-election","1988_Groom_by-election",
           "1988_Port_Adelaide_by-election","1988_Adelaide_by-election","1986_Scullin_by-election","1984_Corangamite_by-election","1984_Hughes_by-election","1984_Richmond_by-election","1983_Moreton_by-election","1983_Bruce_by-election","1983_Wannon_by-election","1982_Flinders_by-election",
           "1982_Lowe_by-election","1981_Wentworth_by-election","1981_Boothby_by-election","1981_Curtin_by-election","1981_McPherson_by-election","1979_Grayndler_by-election","1978_Werriwa_by-election","1977_Cunningham_by-election","1975_Bass_by-election","1973_Parramatta_by-election",
           "1971_Murray_by-election","1970_Chisholm_by-election","1970_Australian_Capital_Territory_by-election","1969_Bendigo_by-election","1969_Gwydir_by-election","1969_Curtin_by-election","1968_Higgins_by-election","1967_Capricornia_by-election","1967_Corio_by-election","1966_Kooyong_by-election",
           "1966_Dawson_by-election","1965_Riverina_by-election","1964_Robertson_by-election","1964_Angas_by-election","1964_Parramatta_by-election","1964_Denison_by-election","1963_East_Sydney_by-election","1963_Grey_by-election","1962_Batman_by-election","1960_Higinbotham_by-election",
           "1960_Calare_by-election","1960_Balaclava_by-election","1960_Bendigo_by-election","1960_Hunter_by-election","1960_La_Trobe_by-election","1958_Parramatta_by-election","1957_Richmond_by-election","1956_Wentworth_by-election","1956_Barker_by-election","1956_Cunningham_by-election",
           "1955_Cook_by-election","1953_Gwydir_by-election","1953_Corangamite_by-election","1953_Lang_by-election","1953_Dalley_by-election","1952_Bradfield_by-election","1952_Werriwa_by-election","1952_Flinders_by-election","1952_Lyne_by-election","1951_Balaclava_by-election",
           "1951_Macquarie_by-election","1946_Henty_by-election","1946_Wimmera_by-election","1945_Fremantle_by-election","1941_Boothby_by-election","1940_Swan_by-election","1940_Kalgoorlie_by-election","1940_Corio_by-election","1939_Wilmot_by-election","1939_Griffith_by-election",
           "1938_Wakefield_by-election","1937_Gwydir_by-election","1936_Darling_Downs_by-election","1936_Kennedy_by-election","1935_Fawkner_by-election","1935_Newcastle_by-election","1933_Flinders_by-election","1932_East_Sydney_by-election","1931_East_Sydney_by-election","1931_Parkes_by-election",
           "1929_Franklin_by-election","1929_Balaclava_by-election","1928_Wide_Bay_by-election","1928_Martin_by-election","1927_Warringah_by-election","1927_Dalley_by-election","1926_Eden-Monaro_by-election","1922_Yarra_by-election","1921_Parramatta_by-election","1921_West_Sydney_by-election",
           "1921_Maranoa_by-election","1920_Kalgoorlie_by-election","1920_Ballaarat_by-election","1919_Echuca_by-election","1918_Corangamite_by-election","1918_Swan_by-election","1918_Flinders_by-election","1917_Grampians_by-election","1917_Darwin_by-election","1915_Wide_Bay_by-election",
           "1915_Dalley_by-election","1915_Grampians_by-election","1915_Bendigo_by-election","1914_Adelaide_by-election","1913_Kalgoorlie_by-election","1912_Werriwa_by-election","1911_Boothby_by-election","1911_North_Sydney_by-election","/1911_Batman_by-election","1910_Kooyong_by-election",
           "1909_Wakefield_by-election","1908_Adelaide_by-election","1907_Echuca_by-election","1904_Riverina_by-election","1904_Melbourne_by-election","1904_Wilmot_by-election","1903_East_Sydney_by-election","1902_Tasmania_by-election","1901_Darling_Downs_by-election")
url <- list()
for (i in 1:length(pages)){
  urls <- paste0("https://en.wikipedia.org/wiki/", pages[i])
  url[[i]] <- urls
  
}

#Parse titles
captionall <- c("[6]","[5]","[4]","[4]","[4]","[4]","[4]","[3]","[3]","[3]",
                "[4]","[6]","[4]","[3]","[3]","","[2]","","","",
                "","","","","","","","","","",
                "","","","","","","","","","",
                "","","","","","","","[1]","","",
                "","[1]","","","","","","","","",
                "","","","","[2]","","[2]","[2]","[2]","[2]",
                "[2]","","","","","","","","","",
                "","","","","","","","","","",
                "","","","","","","[2]","","","",
                "","[2]","","","","","","","","",
                "","","","","[2]","","","","","",
                "","","","","","","[1]","","","",
                "","","","","","","","","","",
                "","","","","","","","","","",
                "","","","","","","","","")
caption <- list()
for (m in 1:length(captionall)){
  captions <- paste0('//*[@id="mw-content-text"]/div/table', captionall[m], '/caption')
  caption[[m]] <- captions
  
}

#Parse results table
xpathall <- c("[6]","[5]","[4]","[4]","[4]","[4]","[4]","[3]","[3]","[3]",
              "[4]","[6]","[4]","[3]","[3]","","[2]","","","",
              "","","","","","","","","","",
              "","","","","","","","","","",
              "","","","","","","","[1]","","",
              "","[1]","","","","","","","","",
              "","","","","[2]","","[2]","[2]","[2]","[2]",
              "[2]","","","","","","","","","",
              "","","","","","","","","","",
              "","","","","","","[2]","","","",
              "","[2]","","","","","","","","",
              "","","","","[2]","","","","","",
              "","","","","","","[1]","","","",
              "","","","","","","","","","",
              "","","","","","","","","","",
              "","","","","","","","","")
xpath <- list()
for (n in 1:length(xpathall)){
  xpaths <- paste0('//*[@id="mw-content-text"]/div/table', xpathall[n], '/tbody')
  xpath[[n]] <- xpaths
  
}

#Merge two lists
url <- mapply(c, url, xpath, caption, SIMPLIFY = FALSE)

#=====================First Pref=============================#
#1)Scrape first preference result from each page;
#2)Clean data; and 
#3)Add it to a data frame
data_firstpref <- lapply(url, function(i){
  webpages <- read_html(i[1])
  
  caption <- webpages %>% html_nodes(xpath = i[3]) %>% html_text()
  caption <- as.data.frame(caption) %>%
    mutate(Year = str_extract(caption, "\\d{4}")) %>%
    mutate(Division = caption, Division = str_replace(Division, "\\d{4}", ""))
  caption$Division <- gsub(" by-election.*", "", caption$Division)
  
  table <- webpages %>% html_nodes(xpath = i[2]) %>% html_text()
  table <- gsub("\nTotal formal votes.*", "", table)
  table <- table %>%
    str_split_fixed("\n\n\n", n = 30) %>%
    str_subset("\n\n") %>%
    str_split_fixed("\n\n", n = 5)
  
  table <- as.data.frame(table) %>%
    row_to_names(row_number = 1) %>%
    mutate(Year = caption$Year, Division = caption$Division) %>%
    mutate(Votes = as.numeric(str_remove_all(Votes, ",")),
           `%` = as.numeric(as.character(`%`)),
           `±` = as.character(`±`),
           `±` = ifelse(str_detect(`±`, "[+]") == FALSE, 
                        substring(`±`, 2), `±`),
           `±` = as.numeric(ifelse(str_detect(`±`, "[+]") == FALSE, 
                        paste0("-", `±`), `±`)))
  
  table <- table %>%
    select(Year, Division, Party, Candidate, Votes, `%`, `±`)

})
firstpref <- do.call(rbind, data_firstpref)

#==========================Totals============================#
#1)Scrape totals from each page;
#2)Clean data; and 
#3)Add it to a data frame
data_totals <- lapply(url, function(i){
  webpages <- read_html(i[1])
  
  caption <- webpages %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]/caption') %>% html_text()
  caption <- as.data.frame(caption) %>%
    mutate(Year = str_extract(caption, "\\d{4}")) %>%
    mutate(Division = caption, Division = str_replace(Division, "\\d{4}", ""))
  caption$Division <- gsub(" by-election.*", "", caption$Division)
  
  table <- webpages %>% html_nodes(xpath = i[2]) %>% html_text()
  table <- gsub(".*(Total)", "", table)
  table <- gsub("\nTwo-party-preferred result.*", "", table)
  table <- table %>%
    str_split_fixed("\n[:upper:]", n = 3) %>%
    str_subset("\n\n") %>%
    str_split_fixed("\n\n", n = 4)
  
  table[1, 1] <- "Formal Votes"
  table[2, 1] <- "Informal Votes"
  table[3, 1] <- "Turnout"
  
  table <- as.data.frame(table) %>%
    mutate(Year = caption$Year, Division = caption$Division) %>%
    transmute(Type = as.character(V1), 
              Votes = as.numeric(str_remove_all(V2, ",")), 
              `%` = as.numeric(as.character(V3)),
              `±` = as.character(V4),
              `±` = ifelse(str_detect(`±`, "[+]") == FALSE, 
                           substring(`±`, 2), `±`),
              `±` = as.numeric(ifelse(str_detect(`±`, "[+]") == FALSE, 
                                      paste0("-", `±`), `±`)),
              `±` = ifelse(is.na(`±`), 0, `±`)) %>%
    mutate(Year = caption$Year, Division = caption$Division)
  
  table <- table %>%
    select(Year, Division, Type, Votes, `%`, `±`)
  
})
totals <- do.call(rbind, data_totals)

#==========================2CP===============================#
#1)Scrape 2CP from each page;
#2)Clean data; and 
#3)Add it to a data frame
data_twocp <- lapply(url, function(i){
  webpages <- read_html(i[1])
  
  caption <- webpages %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]/caption') %>% html_text()
  caption <- as.data.frame(caption) %>%
    mutate(Year = str_extract(caption, "\\d{4}")) %>%
    mutate(Division = caption, Division = str_replace(Division, "\\d{4}", ""))
  caption$Division <- gsub(" by-election.*", "", caption$Division)
  
  table <- webpages %>% html_nodes(xpath = i[2]) %>% html_text()
  table <- gsub(".*\n(Two-party-preferred result)", "", table)
  table <- gsub(".*\n(Two-candidate-preferred result)", "", table)
  table <- table %>%
    str_split_fixed("\n\n\n", n = 30) %>%
    str_subset("\n\n") %>%
    str_split_fixed("\n\n", n = 5)
  
  table <- table[-3, ]
  
  table <- as.data.frame(table) %>%
    transmute(Party = as.character(V1),
              Candidate = as.character(V2),
              Votes = as.numeric(str_remove_all(V3, ",")), 
              `%` = as.numeric(as.character(V4)),
              `±` = as.character(V5),
              `±` = ifelse(str_detect(`±`, "[+]") == FALSE, 
                           substring(`±`, 2), `±`),
              `±` = as.numeric(ifelse(str_detect(`±`, "[+]") == FALSE, 
                                      paste0("-", `±`), `±`))) %>%
    mutate(Year = caption$Year, Division = caption$Division)
  
  table <- table %>%
    select(Year, Division, Party, Candidate, Votes, `%`, `±`)
  
})
twocp <- do.call(rbind, data_twocp)

#==========================2PP===============================#
#1)Scrape 2PP from each page;
#2)Clean data; and 
#3)Add it to a data frame
data_twopp <- lapply(url, function(i){
  webpages <- read_html(i[1])
  
  caption <- webpages %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]/caption') %>% html_text()
  caption <- as.data.frame(caption) %>%
    mutate(Year = str_extract(caption, "\\d{4}")) %>%
    mutate(Division = caption, Division = str_replace(Division, "\\d{4}", ""))
  caption$Division <- gsub(" by-election.*", "", caption$Division)
  
  table <- webpages %>% html_nodes(xpath = i[2]) %>% html_text()
  table <- gsub(".*\n(Two-party-preferred result)", "", table)
  table <- gsub("(Two-candidate-preferred result).*", "", table)
  table <- table %>%
    str_split_fixed("\n\n\n", n = 30) %>%
    str_subset("\n\n") %>%
    str_split_fixed("\n\n", n = 5)
  
  table <- table[-3, ]
  
  table <- as.data.frame(table) %>%
    transmute(Party = as.character(V1),
              Candidate = as.character(V2),
              Votes = as.numeric(str_remove_all(V3, ",")), 
              `%` = as.numeric(as.character(V4)),
              `±` = as.character(V5),
              `±` = ifelse(str_detect(`±`, "[+]") == FALSE, 
                           substring(`±`, 2), `±`),
              `±` = as.numeric(ifelse(str_detect(`±`, "[+]") == FALSE, 
                                      paste0("-", `±`), `±`))) %>%
    mutate(Year = caption$Year, Division = caption$Division)
  
  table <- table %>%
    select(Year, Division, Party, Candidate, Votes, `%`, `±`)
  
})
twopp <- do.call(rbind, data_twopp)

#============================================================#



  