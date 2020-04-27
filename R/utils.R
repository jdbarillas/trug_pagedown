create_text <- function(text_df) {

  template = "
<img src={picture}  width='65' height='65'></img>  

**Country:** {name}  
**Authority:** {authority}  
**Date:** {date}  
**Fine:** €{prettyNum(price, big.mark = ',', scientific = FALSE)}  
**Organization Fined:** {controller}  
**Article Violated:** {article_violated}  
**Type:** {type}  
**Source:** [Link]({source})


**Summary:** {summary}

*********

"
  text_df %>% 
    mutate(as_date = as.Date(date, format = "%m/%d/%Y")) %>% 
    arrange(desc(as_date)) %>% 
    glue_data( template)
  
}

create_text_filtered <- function(text_df, country) {
  
  template = "
<img src={picture}  width='65' height='65'></img>  

**Country:** {name}  
**Authority:** {authority}  
**Date:** {date}  
**Fine:** €{prettyNum(price, big.mark = ',', scientific = FALSE)}  
**Organization Fined:** {controller}  
**Article Violated:** {article_violated}  
**Type:** {type}  
**Source:** [Link]({source})


**Summary:** {summary}

*********

"
  text_df_filtered <- text_df %>% 
    mutate(as_date = as.Date(date, format = "%m/%d/%Y")) %>% 
    arrange(desc(as_date)) 
  
  if (country != "All") {
    text_df_filtered <- text_df_filtered %>% 
      filter(name == country)
  }
  
  text_df_filtered %>% 
    glue_data( template)
  
}

create_df_filtered <- function(text_df, country) {
  
  text_df_filtered <- text_df %>% 
    mutate(as_date = as.Date(date, format = "%m/%d/%Y")) %>% 
    arrange(desc(as_date)) 
  
  if (country != "All") {
    text_df_filtered <- text_df_filtered %>% 
      filter(name == country)
  }
  
  text_df_filtered
}

pretty_num <- function(x) {
  if(is.infinite(x)) x = 0
  paste0("€", prettyNum(x, big.mark = ',', scientific = FALSE))
}

pretty_date <- function(x) {
  format.Date(x, "%B %d, %Y")
}
