

################
## create function get data from hh
################
hh.getresids <- function(query, paid = FALSE, area = 2)
{
  # Makes a call to hh API and gets the list of resume ids based on the given search queries
  
  for (q in query)
  {
    searchURL <- paste0("https://hh.ru/search/resume?text="
                        , q
                        , "&logic=normal&pos=position&exp_period=all_time&items_on_page=100&order_by=relevance&area="
                        , area
                        ,"&label=only_with_salary&clusters=true&relocation=living"
                        ,"&page=")
    allids <- NULL
    for (pageNum in 0:30) {
      try(
        
        {
          #Вытащим id резюме 
          hDoc <- read_html(paste0(searchURL, as.character(pageNum)))
          
          ids <- html_nodes(hDoc, css = 'a') %>% as.character() 
          # Выделим все аттрибуты ссылок на странице
          ids <- as.vector(ids) %>% `[`(str_detect(ids, fixed('/resume/'))) %>%
            str_extract(pattern = '/resume/.{38}') %>% str_sub(str_count('/resume/') + 1)
          ids <- ids[!str_detect(ids, "advanced")]
          
          print(paste0("Downloading page:", pageNum + 1, "; query = \"", q, "\""))
          
          allids <- append(allids, ids)
        })
    }
  }
  
  return(allids)
}

hh.getresumes <- function(ids, name)
{
  
  # ids <- head(ids, 10) # to speed up debugging
  
  df <- data.frame(
    id = numeric() # id резюме
    , Name = character() # название резюме
    , City = character()
    , Currency = character()
    , Salary = numeric() # зарплата
    , Published = character()
    , Level = character()
    , stringsAsFactors = FALSE
  )
  
  for (id in ids)
  {
    try(
      {
        data <- read_html(paste0("https://spb.hh.ru/resume/", id))
        
        tmp <- str_split(
          str_remove(pattern = "\u00A0",
                     string = 
                       html_text(
                         html_node(x = data, xpath = '//*[@class="resume-block__salary"]')
                       )), pattern = " "
        )
        
        salary <- as.numeric(tmp[[1]][1])
        
        currency <- enc2utf8(tmp[[1]][2])
        
        city <- enc2utf8(html_text(
          html_node(x = data, xpath = '//*[@itemprop="addressLocality"]')
        ))
        
        title <- enc2utf8(html_text(
          html_node(x = data, xpath = '//*[@class="resume-block__title-text "]')
        ))
        
        
        df <- rbind(df, data.frame(
          id = id,
          Name = title,
          City = city,
          Currency = currency,
          Salary = salary,
          Published = as.character(Sys.Date()),
          Level = NA,
          stringsAsFactors = FALSE))
        
        print("please just wait some more...")
      }
      
    )
    
  }
  
  return(df)
}

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
  
  names <- c("query", "URL", "id", "name", "city", 
             "published", "company", "responsibility", "requirement")
  colnames(df) <- names
  
  return(df)
}

####################
## create function get more data use url vacancy: salary and experiens
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

##############################
## function create usd,Eur and other
##############################

quotations.update <- function()
{
  # Parses the most up-to-date qutations data provided by the Central Bank of Russia
  # and returns a table with currency rate against RUR
  
  doc <- XML::xmlParse("http://www.cbr.ru/scripts/XML_daily.asp")
  
  quotationsdf <- XML::xmlToDataFrame(doc, stringsAsFactors = FALSE)
  
  quotationsdf <- select(quotationsdf, -Name) 
  
  quotationsdf$NumCode <- as.numeric(quotationsdf$NumCode)
  quotationsdf$Nominal <- as.numeric(quotationsdf$Nominal)
  quotationsdf$Value <- as.numeric(sub(",", ".", quotationsdf$Value))
  
  quotationsdf$Value <- quotationsdf$Value / quotationsdf$Nominal
  quotationsdf <- quotationsdf %>% select(CharCode, Value)
  
  return(quotationsdf)
  
}

##############################
## function convert usd,eur to rur
##############################

convert.currency <- function(targetCurrency = "RUR", df, quotationsdf)
{
  cond <- (!is.na(df$currency) & df$currency == "BYR") 
  df[cond, "currency"] <- "BYN"
  
  currencies <- unique(na.omit(df$currency[df$currency != targetCurrency]))
  
  if (!is.null(df$from))
  {
    for (currency in currencies)
    {
      condition <- (!is.na(df$from) & df$currency == currency)
      
      try(
        df$from[condition] <- 
          df$from[condition] * quotationsdf$Value[quotationsdf$CharCode == currency]
      )
    }
  }
  
  if (!is.null(df$to))
  {
    for (currency in currencies)
    {
      condition <- !is.na(df$to) & df$currency == currency
      
      try(
        df$to[condition] <- 
          df$to[condition] * quotationsdf$Value[quotationsdf$CharCode == currency]
      )
    }
  }
  
  return(df%>% select(-currency))
}

gross.to.net <- function(df, resident = TRUE)
{
  if (resident == TRUE)
    coef <- 0.87
  else
    coef <- 0.7
  
  if (!is.null(df$gross))
    
  {
    
    if (!is.null(df$from)) 
    {
      index <- na.omit(as.numeric(rownames(df[!is.na(df$from) & df$gross == TRUE,])))
      
      df$from[index] <- df$from[index] * coef
    }
    
    if (!is.null(df$to)) 
    {
      index <- na.omit(as.numeric(rownames(df[!is.na(df$to) & df$gross == TRUE,])))
      
      df$to[index] <- df$to[index] * coef
    }
    
    df <- df %>% select(-gross)
  }
  
  return(df)
}

##############################
## function convert name in vacanstion to classification
##############################

get.positions <- function(df)
{
  df$lvl <- NA
  
  df[grep(pattern = "lead|senior|старший|ведущий|главный", 
          x = df$name, ignore.case = TRUE), "lvl"] <- "senior"
  
  df[grep(pattern = "junior|младший|стажер|стажёр", 
          x = df$name, ignore.case = TRUE), "lvl"] <- "junior"
  
  df[is.na(df$lvl), "lvl"] <- "middle"
  
  return(df)
}

##############################
## function use information about part salery to create all salary
##############################

select.paid <- function(df, suggest = TRUE)
{
  # Returns a data frame with average salaries between To and From
  # optionally, can suggest To or From value in case only one is specified
  
  if (suggest == TRUE)
  {
    df <- df %>% filter(!is.na(from) | !is.na(to))
    
    magic.coefficient <- # shows the average difference between max and min salary 
      
      round(mean(df$to/df$from, na.rm = TRUE), 1)
    
    df[is.na(df$to),]$to <- df[is.na(df$to),]$from * magic.coefficient
    df[is.na(df$from),]$from <- df[is.na(df$from),]$to / magic.coefficient
    
  }
  
  else
  {
    df <- na.omit(df)
  }
  
  df$salary <- rowMeans(x = df %>% select(from, to))
  
  df$salary <- ceiling(df$salary / 10000) * 10000
  
  return(df %>% select(-from, -to) %>% filter(salary <= 1000000))
}


################################
## full description vacancy
################################

hh.get.full.description <- function(df)
{
  df$full.description <- NA
  
  for (myURL in df$URL) {
    try(
      {
        data <- fromJSON(myURL)
        
        if (length(data$description) > 0)
        {
          df$full.description[which(df$URL == myURL, arr.ind = TRUE)] <- data$description
        }
        
        print(paste0("Filling in "
                     , which(df$URL == myURL, arr.ind = TRUE)
                     , " out of "
                     , length(df$URL)))
      }
    )
  }
  
  df$full.description <- tolower(df$full.description)
  
  return(df)
}