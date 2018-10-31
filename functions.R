

################
## create function get data from hh
################

hh.getjobs <- function(query, paid = FALSE)
{
  # Makes a call to hh API and gets the list of vacancies based on the given search queries
  
  df <- data.frame(
    query = character() # query to hh API
    , URL = character() # all Path vacancy
    , id = numeric()
    , name = character() 
    , city = character()
    , published = character()
    , company = character() 
    , responsibility = character()
    , requerement = character()
    , stringsAsFactors = FALSE
  ) 

  for (q in query)
  {
    for (pageNum in 0:99) {
      try(
        
        {
          data <- fromJSON(paste0("https://api.hh.ru/vacancies?search_field=name&text=\""
                                  , q
                                  , "\"&search_field=name"
                                  , "&only_with_salary=", paid
                                  ,"&page="
                                  , pageNum))
          ## change rbind to fast rbind
          df <- rbind(df, data.frame(
            q,
            data$items$url,
            as.numeric(data$items$id),
            data$items$name,
            data$items$area$name,
            data$items$published_at,
            data$items$employer$name,
            data$items$snippet$responsibility,
            data$items$snippet$requirement,
            stringsAsFactors = FALSE))
          
        })
      
      print(paste0("Downloading page:", pageNum + 1, "; query = \"", q, "\""))
    }
    
  }
  
  names <- c("query", "URL", "id", "Name", "City", 
             "Published", "Company", "Responsibility", "Requirement")
  colnames(df) <- names
  
  return(df)
}


####################
## create function get more data use url vacancy
####################
hh.getAddData <- function(df)
{
  df$experience <- NA
  df$currency <- NA
  df$from <- NA
  df$to <- NA
  df$gross <- NA
  df$key_skills <- NA
  
  for (myURL in df$URL) {
    try( 
      {
        data <- fromJSON(myURL)
        
        df[df$URL == myURL, "experience"] <- data$experience$name
        df[df$URL == myURL, "currency"] <- data$salary$currency
        df[df$URL == myURL, "from"] <- data$salary$from
        df[df$URL == myURL, "to"] <- data$salary$to
        df[df$URL == myURL, "gross"] <- data$salary$gross
        df[df$URL == myURL, "key_skills"] <- data$key_skills$name
      }
    )
    
    print(paste0("Filling in ", which(df$URL == myURL, arr.ind = TRUE), "from ", nrow(df)))
    
  }
  
  return(df)
}





