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
text<-pdf_text("Reports/iihf2011.pdf")

# Extract player survey page as vector
survey_page<-text[str_detect(text, "IIHF Survey of Players")][2] 

# Create and clean list with each page line as separate entry
page_list<-as.list(str_split(survey_page, "\n")[[1]])%>%  # Create list
  str_squish%>%  # Remove excessice empty spaces between character
  str_remove_all("(?<=\\,)\\s")%>%  # Remove empty space between participated WM's
  str_remove_all("\\s(?=I|V)")%>%  # Remove empty spaces in front of roman numbers 
  str_remove_all("(?<=Outdoor ).+(?=Championships)")%>%  # Only one empty space between Outdoor and Championships
  str_replace_all("Male FemaleIndoor", "MaleRefs FemaleRefs Indoor")%>%  # Rename variables for extraction later
  str_replace_all("(?<=\\s\\d{1,5})\\s(?=\\d{1,3}\\,)", " None ")%>%  # Insert NA if not WM participation
  lapply(correctNames)
  
# Subset List
header<-page_list[str_detect(page_list,"Registered")] # Header of table
country_list<-page_list[str_detect(page_list, "\\d+\\s\\d+")] # Actual data from table 

# Prepare header to be inserted into tibble
header<-c("Country", as.vector(str_split(header, " ", simplify = T)))
header[length(header)+1]<-"Population"

# Tibble with data
data_tibble<-tibble(x=unlist(country_list))%>%
  separate(x, header, sep=" ")

# Prepare tibble for analysis
final_data<-data_tibble%>%
  mutate(Country=str_replace_all(Country, "_", " "))%>%  # Replace underscore in country names again
  mutate(Population=str_remove_all(Population, ","))%>%  # Remove comma in population number
  mutate(Championships=str_remove_all(Championships, "\\*"))%>%  # Remove asteriks in string
  mutate_at(vars(header[c(-1,-10)]), as.numeric)%>%  # Transform to numeric variables
  mutate(Championships=str_split(Championships, ","))%>%  # split Championships column in list if sublists
  unnest(Championships)%>%  # Unnest list to create entry for each championship participation
  add_column(values=rep(1))%>%  # Create artifical dummy for pivot longer afterwards
  pivot_wider(names_from = Championships, values_from = values, values_fill=0)%>%  # Create dummy columns for each championship
  select(-None)%>%  # Remove None dummy column
  rename(IndoorRinks=Indoor,
         OutdoorRinks=Outdoor)

# Reorder variables based on championships
colnames(final_data)<-sortVariables(colnames(final_data)) 
  
  


