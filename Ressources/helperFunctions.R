# Helper function to correct country names in the page list for further processing
correctNames<-function(x){
  
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