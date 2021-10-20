# load packages
source("Ressources/dependencies.R")

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

# Load helper functions
source("Ressources/helperFunctions.R")

# locate and extract filenames from local directory
fileList<-list.files("Reports")

#initate empty list
tibbleList<-list()

# Looping over all files to extract data
for(i in fileList){
  
  # Print progress
  print(paste0("Processing report ",i))
  
  # Extract entire text from pdf
  text<-pdf_text(paste0("Reports/",i))
  
  # Extract player survey pages as vector
  startPage<-which(str_detect(text, "IIHF Survey of Players|IIHF SURVEY OF PLAYERS"))[2]  # page number of survey start
  endPage<-which(str_detect(text, "CIA World Factbook"))  # page number of survey end
  surveyPage<-paste(text[startPage:endPage], collapse = "")  # concatenate survey pages to one vector
  
  # Create and clean list with each page line as separate entry
  pageList<-as.list(str_split(surveyPage, "\n")[[1]])%>%  # Create list with lines of page as entries
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
    str_replace_all("Under-20", "U20")%>%  
    str_replace_all("U-20", "U20")%>%
    lapply(correct_names)%>%  # correct country names (add underscore so that entries are seperable by white space in a further step)
    #Remove report specific errors
    str_remove_all("(?<=WM|WW)\\s(?=\\d{2}IQ)")%>%  # Error in championship list for Austria (iihf2014)
    str_replace_all("WM18 WWIA", "WM18,WWIA")%>%  # Error in championship list for Slovakia (iihf2018)
    str_replace_all(",OGWW,", ",OGW,")  # Error in championship list for Sweden (iihf2014)
  
  # Subset List
  header<-pageList[str_detect(pageList,"Registered")][1] # Header of table (if two pages select first header)
  countryList<-pageList[str_detect(pageList, "\\d+\\s\\d+\\s\\d+")] # Actual data from table 
  
  # Prepare header to be inserted into tibble
  header<-c("Country", as.vector(str_split(header, " ", simplify = T)))
  header[length(header)+1]<-"Population"
  
  # Tibble with data
  surveyData<-tibble(x=unlist(countryList))%>%
    separate(x, header, sep=" ")
  
  # Prepare tibble for analysis
  surveyDataFinal<-surveyData%>%
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
           OutdoorRinks=Outdoor)%>%
    add_column(Year = rep(as.numeric(str_extract_all(i, "\\d+", simplify = T))))%>%  #A dd Year column
    relocate(Year)  # Year as first column in df
  
  # Assign tibble to a list
  tibbleList[[i]]<-surveyDataFinal
  
  
}
  
# Combine tibbles in list to one large tibble and fill NA's
finalData<-bind_rows(tibbleList)%>%
  replace(is.na(.), 0)

# Reorder variables based on championships
colnames(finalData)<-sort_variables(colnames(finalData)) 
  
# Write observations to csv
write.csv(finalData, file="IIHFSurveysCombined.csv", row.names = F)
  
# clear workspace
rm(list=ls())
gc()


