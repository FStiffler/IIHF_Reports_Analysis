correct_names<-function(x){
  
  #' @description Helper function to correct country names in the page list for further processing
  #' @param x character. Vector with country information
  #' @returns Line with adjusted names wherever necessary 
  
  # detect pattern "namecomponent1 namecomponent2" in front of number
  if(str_detect(x, "^[A-Za-z]+\\s[A-Za-z]+(?=\\s\\d)")){
    
    country<-str_extract(x, "^[A-Za-z]+\\s[A-Za-z]+(?=\\s\\d)")  # detect full name
    remaining<-str_split(x, "^[A-Za-z]+\\s[A-Za-z]+(?=\\s\\d)")[[1]][2]  # detect rest of string
    country<-str_replace_all(country, "\\s", "\\_")  # replace white space by underscore
    line<-paste0(country, remaining)  # concatenate the two strings to create new string
    return(line)  # return new string
    
  }
  
  # detect pattern "namecomponent1 namecomponent2 namecomponent3" in front of number
  else if(str_detect(x, "^[A-Za-z]+\\s[A-Za-z]+\\s[A-Za-z]+(?=\\s\\d)")){
    
    country<-str_extract(x, "^[A-Za-z]+\\s[A-Za-z]+\\s[A-Za-z]+(?=\\s\\d)")
    remaining<-str_split(x, "^[A-Za-z]+\\s[A-Za-z]+\\s[A-Za-z]+(?=\\s\\d)")[[1]][2]
    country<-str_replace_all(country, "\\s", "\\_")
    line<-paste0(country, remaining)
    return(line)
  }
  
  # detect pattern "namecomponent1 & namecomponent3" in front of number
  else if(str_detect(x, "^[A-Za-z]+\\s\\&\\s[A-Za-z]+(?=\\s\\d)")){
    
    country<-str_extract(x, "^[A-Za-z]+\\s\\&\\s[A-Za-z]+(?=\\s\\d)")
    remaining<-str_split(x, "^[A-Za-z]+\\s\\&\\s[A-Za-z]+(?=\\s\\d)")[[1]][2]
    country<-str_replace_all(country, "\\s", "\\_")
    line<-paste0(country, remaining)
    return(line)
    
  }
  
  
  # detect pattern "namecomponent1 namecomponent3,namecomponent4" in front of number
  else if(str_detect(x, "^[A-Za-z]+\\s[A-Za-z]+\\,[A-Za-z]+(?=\\s\\d)")){
    
    country<-str_extract(x, "^[A-Za-z]+\\s[A-Za-z]+(?=\\,)")
    remaining<-str_split(x, "^[A-Za-z]+\\s[A-Za-z]+\\,[A-Za-z]+(?=\\s\\d)")[[1]][2]
    country<-str_replace_all(country, "\\s", "\\_")
    line<-paste0(country, remaining)
    return(line)
    
  }
  
  else{
    return(x)
  }
  
}

sort_variables<-function(x){
  
  #' @description Sorts variables in a meaningfull way based on championship names.
  #' @param x character. Vector containing all column names
  #' @returns Vector with ordered column names based on championship names. 
  
  new<-c(
    x[1:10],
    sort(x[!is.na(str_match(x, "WM$"))]),  # men championship top division
    sort(x[!is.na(str_match(x, "WM(I{1,3}|IV|V|VI)$"))]),  # men championships lower divisions
    sort(x[!is.na(str_match(x, "WM20"))]),  # U20 men championships
    sort(x[!is.na(str_match(x, "WM18"))]),  # U18 men championships
    sort(x[!is.na(str_match(x, "OGM"))]),  # men olympic games
    sort(x[!is.na(str_match(x, "WW$"))]),  # women championship top division
    sort(x[!is.na(str_match(x, "WW(I{1,3}|IV|V|VI)$"))]),  # men championships lower divisions
    sort(x[!is.na(str_match(x, "WW18"))]),  # U18 men championships
    sort(x[!is.na(str_match(x, "OGW$"))])  # Women olympic games
  )
  
  return(new)
}

handle_landscaped<-function(x){
  
  #' @description Seperates country information when the player survey page is in landscape format. In this case, information of two countries might be contained in the same page line
  #' @param x character. A string containing the information of one page line
  #' @returns List containing the separated country information for each page line if necessary.
    
  # Header is duplicated. Separate and only return one instance of header
  if(str_detect(x, "Registered")){
      
    return(str_split(x, "\\s(?=Registered)")[[1]][1])
      
  }
    
  # Separate strings which contain information about two countries
  else if(str_detect(x, "\\d{2}(\\,|\\.)\\d{3}\\s[A-Za-z]")){
    
    first<-str_extract(x, "^.+?\\d{2}(\\,|\\.)\\d{3}(?=\\s[A-Za-z])")  # information of first country
    second<-str_trim(str_split(x, "^.+?\\d{2}(\\,|\\.)\\d{3}(?=\\s[A-Za-z])")[[1]][2])  # information of second country
    return(list(first,second))
      
  } 
    
  # Remove summarized numbers from country information on same line (Denmark in iihr2021 for example). 
  # Explanation of approach:
  # The summarized numbers are of at list 4 digits. 
  # No country has 4 digit numbers in population,  indoor and outdoor rinks without also having participated 
  # in at least one championship. Therefore, search for 3 consecutive 4 or more digit numbers at end of string.
  else if(str_detect(x, "(\\d{1,3}(\\.|\\.)\\d{3})\\s(\\d{1,3}(\\.|\\.)\\d{3})\\s(\\d{1,3}(\\.|\\.)\\d{3})$")){  
      
    return(str_extract(x, "^.+?\\d{2}(\\,|\\.)\\d{3}"))  # Extract information about country
      
      
  }
    
  else {
    return(x)
  }
      

    
}
    
