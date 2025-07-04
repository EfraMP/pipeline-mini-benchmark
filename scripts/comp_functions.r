# Auxiliary functions to convert units

## Function to convert units to MB
get_MB <- function(x) {
  ## Trim whitespace
  x %<>% as.character() %>% 
    trimws()
  
  ## Gets numeric value
  val <- str_extract(x, "[0-9]+(\\.[0-9]+)?") %>% 
    as.numeric()
  
  ## Gets unit
  unit <- str_extract(x, "[A-Za-z]+") %>% 
    toupper()
  
  ## Gets MB
  value_mb = case_when(
    unit %in% c("KB", "K")  ~ val / 1024,
    unit %in% c("MB", "M")  ~ val,
    unit %in% c("GB", "G") ~ val * 1024,
    unit %in% c("TB", "T") ~ val * 1024 * 1024,
    TRUE ~ NA_real_
  )
  
  ## Return megabytes
  return(value_mb)
}


## Function to get time information into seconds
get_seconds <- function(x) {
  x %<>% as.character()
  
  ## Gets time with regex
  h <- x %>% 
    str_extract(., "\\d+(?=h)") %>% 
    as.numeric()
  m <- x %>% 
    str_extract(., "\\d+(?=m)") %>% 
    as.numeric()
  ## Regex to allow decimal seconds
  s <- x %>% 
    str_extract(., "\\d+(?:\\.\\d+)?(?=s)") %>% 
    as.numeric()
  
  # Replace NAa with 0
  h[is.na(h)] <- 0
  m[is.na(m)] <- 0
  s[is.na(s)] <- 0
  
  ## Return seconds
  return(h * 3600 + m * 60 + s)
}
