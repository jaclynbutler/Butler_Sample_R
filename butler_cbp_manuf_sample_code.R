
#Sample R Code for StratoDem Analytics
#Analyst: Jaclyn Butler
#Dataset: County Business Patterns (CBP)
  #Citation: Fabian Eckert, Teresa C. Fort, Peter K. Schott, 
  # and Natalie J. Yang. "Imputing Missing Values in the US Census Bureau's
  # County Business Patterns." NBER Working Paper #26632, 2021. Retrieved from
  # http://www.fpeckert.me/cbp/

library(tidyverse)

#===============================================================================
#Import NAICS Harmonized CBP x Industry Panel Dataset
  #Note: unzipped raw csv is 1,282,873 KB
url <- "http://fpeckert.me/cbp/Imputed%20Files/efsy_panel_naics.csv.zip"
temp <- tempfile()
temp2 <- tempfile()

download.file(url, temp)
unzip(zipfile = temp, exdir = temp2)
raw_cbp75_18 <- read.csv(file.path(temp2, "efsy_panel_naics.csv"))
unlink(c(temp,temp2))

#===============================================================================
#Create sample subset in case raw_cbp75_18 is too unwieldy
sample50k_cbp75_18 <- sample_n(raw_cbp75_18, 50000)

getwd()
path <- "./"
write.csv(sample50k_cbp75_18, paste0(path, "/sample50k_cbp75_18.csv"), row.names = FALSE)

#===============================================================================
#Format dataset to select industries based on NAICS codes
  #2012 NAICS reference: https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/naics-descriptions/naics2012.txt
manuf80_18 <- raw_cbp75_18 %>%
  as_tibble() %>%
  #Filter to ~decadal intervals to match census data
    filter(year==1980 | year==1990 | year==2000 | year==2010 | year==2018) %>%
  #Drop non-manuf values  
    subset(substr(naics12,1,1)=="3") %>%
  #Create FIPS Identifiers   
    mutate(fips_num=((fipstate*1000)+fipscty)) %>% 
    mutate(fips_str=ifelse(fips_num<10000, paste0("0",fips_num), as.character(fips_num))) %>% 
  #Nondurable and durable manufacturing subsectors specified via NAICS
    #Source: https://www.census.gov/manufacturing/m3/get_forms/instructionman.pdf    
    mutate(subsect=
           ifelse(grepl("^311",naics12), "nondur",
           ifelse(grepl("^312",naics12), "nondur", 
           ifelse(grepl("^313",naics12), "nondur", 
           ifelse(grepl("^314",naics12), "nondur", 
           ifelse(grepl("^315",naics12), "nondur", 
           ifelse(grepl("^316",naics12), "nondur", 
           ifelse(grepl("^321",naics12), "dur", 
           ifelse(grepl("^322",naics12), "nondur", 
           ifelse(grepl("^323",naics12), "nondur", 
           ifelse(grepl("^324",naics12), "nondur", 
           ifelse(grepl("^325",naics12), "nondur", 
           ifelse(grepl("^326",naics12), "nondur", 
           ifelse(grepl("^327",naics12), "dur", 
           ifelse(grepl("^331",naics12), "dur", 
           ifelse(grepl("^332",naics12), "dur", 
           ifelse(grepl("^333",naics12), "dur", 
           ifelse(grepl("^334",naics12), "dur", 
           ifelse(grepl("^335",naics12), "dur", 
           ifelse(grepl("^336",naics12), "dur", 
           ifelse(grepl("^337",naics12), "dur", 
           ifelse(grepl("^339",naics12), "dur",
        #TODO: confirm what "----^" values total to 
           ifelse(grepl("----^",naics12), "q_var",
           "indus_nonmanuf"))))))))))))))))))))))) %>%
  #Drop the "----^" observations for now
  mutate(test_q_var=ifelse(subsect=="q_var", TRUE, FALSE)) %>%
  filter(subsect!="q_var") %>%
  select(fipstate, fipscty, fips_num, fips_str, year, naics12, emp, subsect)

#===============================================================================
#Collapse into county summary dataset
manuf_wide <- manuf80_18 %>%
  group_by(fips_str) %>%
  summarize(
    emp_nondur80 = (sum(year==1980 & subsect=="nondur")),
    emp_dur80 = (sum(year==1980 & subsect=="dur")),
    emp_nondur90 = (sum(year==1990 & subsect=="nondur")),
    emp_dur90 = (sum(year==1990 & subsect=="dur")),
    emp_nondur00 = (sum(year==2000 & subsect=="nondur")),
    emp_dur00 = (sum(year==2000 & subsect=="dur")),    
    emp_nondur10 = (sum(year==2010 & subsect=="nondur")),
    emp_dur10 = (sum(year==2010 & subsect=="dur")),    
    emp_nondur18 = (sum(year==2018 & subsect=="nondur")))

#===============================================================================
#Manuf long dataset for graphs
manuf_long <- manuf80_18 %>%
  filter(subsect=="dur" | subsect=="nondur") %>%
  group_by(subsect, year) %>%
  summarize(
    subsect_emp = sum(emp)) %>%
  mutate(manuf_cnt = sum(subsect_emp)) 

#===============================================================================
#Line graph
ggplot (data = manuf_long, aes(x=year, y=subsect_emp,group=subsect)) +
  geom_line(aes(linetype=subsect)) +
  geom_point (aes(shape=subsect)) +
  labs(title= "Employees by Manufacturing Subsector",
       x="Year", 
       y="Number Employed")

#Stacked bar chart
ggplot(data = manuf_long, aes(fill=subsect, y=subsect_emp, x=year)) + 
  geom_bar(position="fill", stat="identity") +
  labs(title= "Manufacturing Sector Composition",
       x="Year", 
       y="Percent Employed")

#===============================================================================
#Export a sample file to local working dir
write.csv(manuf_wide, paste0(path, "/manuf80_18_wide.csv"), row.names = FALSE)
