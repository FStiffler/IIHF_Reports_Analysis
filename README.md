# IIHF Report Analysis

## Introduction

Each year, the International Ice Hockey Federation (IIHF) publishes a 
report about the events of the previous hockey season. The report also 
contains data on the number of hockey players in each country. The goal 
of this project is to extract this information and make it available for 
data analysis.

## Description of Repository

### Ressources Folder

Folder which contains the [dependecies](dependencies.R) and 
[helperFunctions](helperFunctions.R) files. The dependencies file automatically installs required packages, if they are not installed yet, and loads the packages. The helperFunctions contains functions which simplify data preparation. Both files are automatically sourced when running [extractReportData](extractReportData.R) so that packages and functions are available. 

### Reports Folder (not on Github)

Folder into which the IIHF Reports are automatically downloaded. The folder is created when running the file [extractReportData](extractReportData.R). The reports are saved as pdf files and can be opened normally with any pdf reader. 

### [extractReportData](extractReportData.R) File

File to extract the IIHF survey of players data from each report and prepare it for further analysis. 
