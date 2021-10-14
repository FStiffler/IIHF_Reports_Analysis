# load packages
source("dependencies.R")

# download files from IIHF #####
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
