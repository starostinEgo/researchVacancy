

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

hh.getSalaryExp <- function(df)
{
  df$experience <- NA
  df$currency <- NA
  df$from <- NA
  df$to <- NA
  df$gross <- NA
  
  for (myURL in df$URL) {
    try( 
      {
        data <- fromJSON(myURL)
        
        df[df$URL == myURL, "experience"] <- ifelse(is.null(data$experience$name),NA,data$experience$name)
        df[df$URL == myURL, "currency"] <- ifelse(is.null(data$salary$currency),NA,data$salary$currency) 
        df[df$URL == myURL, "from"] <- ifelse(is.null(data$salary$from),NA,data$salary$from) 
        df[df$URL == myURL, "to"] <- ifelse(is.null(data$salary$to),NA,data$salary$to) 
        df[df$URL == myURL, "gross"] <- ifelse(is.null(data$salary$gross),NA,data$salary$gross) 
      }
    )
    
    print(paste0("Filling in ", which(df$URL == myURL, arr.ind = TRUE), " from ", nrow(df)))
    
  }
  
  return(df)
}

####################
## create function get more data use url vacancy and skils
####################

hh.getskills <- function(allurls)
{
  analyst.skills <- data.frame(
    id = character(), # id 
    skill = character() # 
  )
  
  for (myURL in allurls) {
    
    data <- fromJSON(myURL)
    
    if (length(data$key_skills) > 0)
      analyst.skills <- rbind(analyst.skills, cbind(data$id, data$key_skills))
    
    print(paste0("Filling in "
                 , which(allurls == myURL, arr.ind = TRUE)
                 , " out of "
                 , length(allurls)))
    
  }
  
  names(analyst.skills) <- c("id", "skill")
  analyst.skills$skill <- tolower(analyst.skills$skill)
  
  return(analyst.skills)
}





