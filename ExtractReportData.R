# load packages
source("Ressources/dependencies.R")
source("Ressources/helperFunctions.R")

# Download files from IIHF #####
# base URL
baseURL<-"https://www.iihf.com/en/statichub/4823/annual-report"

# List with all links to pdf files on IIHF webpage
URLS<-read_html(baseURL)%>%
  html_nodes(xpath="//div[@class='s-content']/p/a")%>%
  html_attr("href")

# Remove unwanted reports
reportsToRemove<-paste0(
  c(
    paste0("annualreport",seq(2019,year(Sys.Date()))),  # Removed annual reports from 2019 onwards (are named season summary starting in 2019)
    paste0("fr_",c(2017,2018))  # Removed financial reports (years 2017 and 2018)
  )
  
)

URLS_final<-URLS[!str_detect(URLS, paste0(reportsToRemove, collapse="|"))]  # Remove the according report links from list

# Function to download files to local directory
downloadReports<-function(URL){
  
  # Extract name of report
  name<-as.vector(str_extract_all(URL, "(?<=report/).*(?=.pdf)", simplify = T))
  
  # Extract year of report name
  year<-as.vector(str_extract_all(name, "\\d+", simplify = T))
  
  # Download pdf file and save it as report.pdf
  download.file(URL, destfile = paste0("Reports/iihf",year,".pdf"), mode = "wb")
  
  # Progress
  print(paste0("Report from ",year, " downloaded"))
  
}

#Vectorize function
downloadReports = Vectorize(downloadReports, SIMPLIFY = F)

#Create directory for downloaded files
dir.create("Reports")

# Download files and save them locally
downloadReports(URLS_final)

# clear environment
rm(list=ls())
gc()


# Extract data from reports ####

# Extract entire text from pdf
text<-pdf_text("Reports/iihf2021.pdf")

# Extract player survey pages as vector
start_page<-which(str_detect(text, "IIHF Survey of Players|IIHF SURVEY OF PLAYERS"))[2]  # page number of survey start
end_page<-which(str_detect(text, "CIA World Factbook"))  # page number of survey end
survey_page<-paste(text[start_page:end_page], collapse = "")  # concatenate survey pages to one vector

# Create and clean list with each page line as separate entry
page_list<-as.list(str_split(survey_page, "\n")[[1]])%>%  # Create list with lines of page as entries
  str_squish%>%  # Remove excessice empty spaces between character
  lapply(handle_landscaped)%>%  # Check if data in landscape format
  unlist()%>% 
  str_remove_all("(?<=\\,)\\s")%>%  # Remove empty space between participated WM's
  str_remove_all("\\s(?=I|V)")%>%  # Remove empty spaces in front of roman numbers 
  str_replace_all("Championships/Olympic Games", "Championships")%>%  # In olympic game year, remove olympic games from name 
  str_remove_all("(?<=Outdoor ).+(?=Championships)")%>%  # Only one empty space between Outdoor and Championships
  str_replace_all("Male FemaleIndoor", "MaleRefs FemaleRefs Indoor")%>%  # Rename variables for extraction later
  str_remove_all("(?<=\\d)\\,(?=\\d)")%>%  # Remove all commas between numbers (normally thousander separators)
  str_remove_all("(?<=\\d)\\.(?=\\d)")%>%  # Remove all points between numbers (normally thousander separators)
  str_replace_all("(?<=\\s\\d{1,6})\\s(?=\\d{1,10}$)", " None ")%>%  # Insert None if a country has played in no championship
  lapply(correct_names)%>%
  #Remove report specific errors
  str_remove_all("(?<=WM|WW)\\s(?=\\d{2}IQ)")%>%  # Error in championship list for austria (iihf2014)
  str_replace_all("WM18 WWIA", "WM18,WWIA")  # Error in championship list for slovakia (iihf2018)
  
# Subset List
header<-page_list[str_detect(page_list,"Registered")][1] # Header of table (if two pages select first header)
country_list<-page_list[str_detect(page_list, "\\d+\\s\\d+\\s\\d+")] # Actual data from table 

# Prepare header to be inserted into tibble
header<-c("Country", as.vector(str_split(header, " ", simplify = T)))
header[length(header)+1]<-"Population"

# Tibble with data
data_tibble<-tibble(x=unlist(country_list))%>%
  separate(x, header, sep=" ")

# Prepare tibble for analysis
final_data<-data_tibble%>%
  mutate(Country=str_replace_all(Country, "_", " "))%>%  # Replace underscore in country names again
  mutate(Championships=str_remove_all(Championships, "\\*"))%>%  # Remove asteriks in string
  mutate(Championships=str_remove_all(Championships, "A|B|Q"))%>%  # Remove information about division groups or qualification
  mutate_at(vars(header[c(-1,-10)]), as.numeric)%>%  # Transform to numeric variables
  mutate(Championships=str_split(Championships, ","))%>%  # split Championships column in list if sublists
  unnest(Championships)%>%  # Unnest list to create entry for each championship participation
  add_column(values=rep(1))%>%  # Create artifical dummy for pivot longer afterwards
  distinct()%>%  # Remove potential duplicates
  pivot_wider(names_from = Championships, values_from = values, values_fill=0)%>%  # Create dummy columns for each championship
  select(-None)%>%  # Remove None dummy column
  rename(IndoorRinks=Indoor,
         OutdoorRinks=Outdoor)

# Reorder variables based on championships
length(colnames(final_data))==length(sort_variables(colnames(final_data)))
colnames(final_data)<-sort_variables(colnames(final_data)) 
  
  


