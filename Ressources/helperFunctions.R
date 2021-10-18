# Helper function to correct country names in the page list for further processing
correct_names<-function(x){
  
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
  
  else return(x)
  
}

# Sort variables in a meaningfull way based on championships 
sort_variables<-function(x){
  
  new<-c(
    x[1:10],
    sort(x[!is.na(str_match(x, "WM$"))]),  # men championship top division
    sort(x[!is.na(str_match(x, "WM(I{1,3}|IV|V|VI)$"))]),  # men championships lower divisions
    sort(x[!is.na(str_match(x, "WM20"))]),  # U20 men championships
    sort(x[!is.na(str_match(x, "WM18"))]),  # U18 men championships
    sort(x[!is.na(str_match(x, "WW$"))]),  # women championship top division
    sort(x[!is.na(str_match(x, "WW(I{1,3}|IV|V|VI)$"))]),  # men championships lower divisions
    sort(x[!is.na(str_match(x, "WW18"))])  # U18 men championships
  )
  
  return(new)
}
