# Loading Libraries & Functions ----

library(XML)
library(rvest)
library(htmltools)
library(RCurl)
library(shinyjs)
library(shiny)
library(jsonlite)
library(dplyr)
library(stringr)
library(shinythemes)
library(shinydashboard)
library(readr)
library(stringr)
library(purrr)
library(tidyverse)
library(tibble)

###Guardian API: "b80008a9-a43c-4047-a0ad-47f0cd8efb20"
###NYTIMES_API <- "KCOgDvSOdjGOQeF0kfQJ7Ch4IqMhEG42"
###newsapi <- "0017f268ff0b414bbb505d701dddacb0"


ft_function <- function(query, from_date, to_date) {
  
  message("...WARNING...Financial Times Function is unlikely to return full texts...")
  
  # Prerequisites ----
  
  base_link <- "https://www.ft.com/search?q="
  
  sort <- "&sort=date"
  
  query_link <- paste0(base_link,
                       URLencode(query),
                       "&dateFrom=",
                       from_date,
                       "&dateTo=",
                       to_date,
                       sort)
  link_read <- read_html(query_link)
  
  page_num <- link_read %>%
    html_nodes(xpath = '//*[@id="site-content"]/div/div[*]/div/span/text()[2]') %>%
    html_text() %>%
    parse_number()
  
  if (length(page_num) == 0){
    return(paste0("...No Result, Please Modify Search Parameters..."))
    
  }
  message("...Collecting articles about ", URLdecode(query), "...")
  
  
  # Creating Containers ----
  
  title <- list()
  link <- list()
  date <- list()
  article <- vector()
  newspaper <- vector()
  
  
  # Data Collection & Status Messages [No Articles] ----
  
  hits <- page_num * 25  
  
  if (hits > 1000) {
    message(
      "...This search resulted in ",
      hits,
      " articles. Financial times will only display 1000 results per query. Use multiple queries to get more results... "
    )
    page_num <- 40
  }
  
  
  message("...Now scraping ", page_num, " pages", " and up to ", page_num*25, " articles... ")
  for (i in 1:page_num) {
    
    link_read <- read_html(paste0(base_link,
                                  URLencode(query),
                                  "&page=",
                                  i,
                                  "&dateFrom=",
                                  from_date,
                                  "&dateTo=",
                                  to_date,
                                  sort))
    
    
    title[[i]] <- link_read %>%
      html_nodes(xpath = '//*[@id="site-content"]/div/ul/li[*]/div/div/div/div[1]/div[2]/a') %>%
      html_text()
    
    
    link[[i]] <- link_read %>%
      html_nodes(xpath = '//*[@id="site-content"]/div/ul/li[*]/div/div/div/div[1]/div[2]/a') %>%
      html_attr("href")
    
    
    date[[i]] <- link_read %>%
      html_nodes(xpath = '//*[@id="site-content"]/div/ul/li[*]/div/div/div/div[1]/div[3]/time') %>%
      html_attr("datetime")
    
    Sys.sleep(.3)
    
    message("...Working on Page ", i,"... ")
    
  }
  
  
  news_data_frame <- tibble(title = unlist(title), link = unlist(link), date = unlist(date))
  
  # Collecting Article ----
  
  for (i in 1:length(news_data_frame$link)) {
    
    if (str_detect(news_data_frame$link[i], c("content|video")) == TRUE) {
      news_data_frame$link[i] <-
        paste0("https://www.ft.com", news_data_frame$link[i])
    
      }
    
    if (str_detect(news_data_frame$link[i], "content") == TRUE) {
      
      article_read <- read_html(news_data_frame$link[i])
      message("...Gathering Article #", i, "...")
      text <- article_read %>%
        html_nodes(xpath = '//*[@id="site-content"]/div[3]/div[2]') %>%
        html_text()
      if (length(text) != 0){ article[i] <- text} else {
        article[i] <- "NA"
      
        }
      
      Sys.sleep(.3)
    
      } else {
      
        article[i] <-
        "MANUAL"
      message(
        "...This is not a standard article page, please check this link manually: ",
        news_data_frame$link[i]," ... "
      )
    
      }
  }
  
  # Creating Newspaper Column ---- 
  
  for (n in 1:length(article)) {
    newspaper[n] <- "Financial Times"
  }
  
  # Preparting Data for Download ----
  
  news_data_frame <- add_column(news_data_frame, newspaper = newspaper, article = article) %>%
    select(newspaper, title, link, date, article)
  
  message("...Scraping complete! Once the table appears, please press the download button for a .csv file...")
  
  news_data_frame
  
}

guardian_function <- function(from_date, to_date, query, api) {
  
    # Prerequisites ----
    
    base_link <- "https://content.guardianapis.com/search?section=world&show-fields=bodyText&q="
    from <- "&from-date="
    to <- "&to-date="
    page <- "&page="
    api <- trimws(api)
    
    url <- paste0(base_link, query, from, from_date, to, to_date, page, "1", "&api-key=", api)
    guardian_json <- fromJSON(url)
    
    # Creating Containers ----
    newspaper <- list()
    title <- list()
    section <- list()
    date <- list()
    link <- list()
    article <- list()
    
    # Status Messages ----
    message("...Collecting articles about ", URLdecode(query), "...")
    message("...Total Pages: ", guardian_json[["response"]][["pages"]], "...")
    message("...Completion in around ", ceiling((guardian_json[["response"]][["pages"]] + 60) / 45), " minutes", "...")
    
    # Data Collection ----
    for (page_num in 1:guardian_json[["response"]][["pages"]]) {
      
      message("...Gathering page ", page_num, "/", guardian_json[["response"]][["pages"]], "...")
      
      page_url <- paste0(base_link, query, from, from_date, to, to_date, page, page_num, "&api-key=", api)
      
      page_json <- fromJSON(page_url)
      
      title[[page_num]] <- page_json[["response"]][["results"]][["webTitle"]]
      
      section[[page_num]] <-
        page_json[["response"]][["results"]][["section"]]
      
      link[[page_num]] <- page_json[["response"]][["results"]][["webUrl"]]
      
      article[[page_num]] <-
        trimws(page_json[["response"]][["results"]][["fields"]][["bodyText"]])
      
      date[[page_num]] <-
        page_json[["response"]][["results"]][["webPublicationDate"]]
      
      Sys.sleep(.3)
    }
    
    # Newspaper Column ----
    for (n in 1:length(unlist(title))) {
      newspaper[[n]] <- "Guardian"
    }
    
    # Preparing Data data for download ----
    news_data_frame <- tibble(newspaper = newspaper, title = unlist(title), link = unlist(link), date = unlist(date), article = unlist(article), section = unlist(section))
    
    message("...Scraping complete! Please press the download button for a .csv file...")
    
    news_data_frame
    
    
  }

nyt_function <-
  function(from_date, to_date, query, api) {
    # Prerequisites ----
    
    base_link <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",
        query, "&begin_date=",
        from_date, "&end_date=",
        to_date, "&facet_filter=true&api-key=", trimws(api)
      )
    
    nyt_json <- fromJSON(base_link)
    pages <- ceiling(nyt_json$response$meta$hits[1] / 10)
    if (pages > 200) {
      message("...You're trying to gather ", pages, "pages...")
      pages <- 200
      message("...NYT API Allows for 200 Pages Before Kicking you Out...")
    } else {
      pages <- pages
    }
    
    # Status Messages ----
    
    message("...Collecting articles about ", URLdecode(query), "...")
    
    message("...Total Pages: ", pages, "...")
    message("...Completion in ", ceiling((pages * 6 + 120) / 60), " minutes", "...")
    
    # Containers ----
    
    newspaper <- list()
    title <- list()
    section <- list()
    date <- list()
    link <- list()
    article <- vector()
    
    # Data Collection (Pages) ----
    
    for (page in 1:pages) {
      page_link <-
        paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=", query,
          "&begin_date=", from_date,
          "&end_date=", to_date,
          "&page=",page,
          "&facet_filter=true&api-key=", trimws(api))
      
      page_json <- fromJSON(page_link)#Constructing the url
      
      message("...Gathering page ", page, "/", pages,"...")
      
      title[[page]] <- page_json[["response"]][["docs"]][["headline"]][["main"]]
      
      section[[page]] <- page_json[["response"]][["docs"]][["section_name"]]
      
      link[[page]] <- page_json[["response"]][["docs"]][["web_url"]]
      
      date[[page]] <- page_json[["response"]][["docs"]][["pub_date"]]
      
      Sys.sleep(6) #Waiting to ensure that the Api doesn't kick us out
    }
    
    article_prep <- unlist(link)
    
    
    
    # Data Collection (Articles) ----
    
    for (n in 1:length(article_prep)) {
      
      if (str_detect(article_prep[n], c("video")) == FALSE) {
        
        get_link <- read_html(article_prep[n])
        parse_link <- htmlParse(get_link)

        extract_article <- unlist(xpathSApply(parse_link, path = '//*[@id="story"]/section/div', xmlValue))
        article[n] <- paste(trimws(extract_article), collapse = "")
      } else {
        article[n] <- NA
      }
      
      
      message("...Article ", n, "/", length(article_prep), " Retrieved", "...")
      
      Sys.sleep(.2)
    }
    
    # Newspaper Column ----
    
    for (n in 1:length(unlist(title))) {
      newspaper[[n]] <- "New York Times"
    }
    
    # Constructing the Table ----
    
    news_data_frame <- tibble(newspaper = newspaper, title = unlist(title), link = unlist(link), date = unlist(date), article = article, section = unlist(section))
    
    message("...Scraping complete! Please press the download button for a .csv file...")
    
    news_data_frame

  }


news_api_free <- function(query = NULL, qInTitle = NULL, domains = NULL, 
                                  excludeDomains = NULL, from = NULL, to = NULL, 
                                  language = NULL, query1 = NULL, sources = NULL, 
                                  sortBy = NULL, api = NULL) {
    
    if (is.null(sources) == FALSE) {
      sources <- paste0("sources=", sources, "&")
    }
    
    if (is.null(query) == FALSE) {
      query1 <- paste0("q=", URLencode(query), "&")
    }
    
    if (is.null(domains) == FALSE) {
      domains <- paste0("domains=", URLencode(domains), "&")
    }
    
    if (is.null(excludeDomains) == FALSE) {
      excludeDomains <-
        paste0("excludeDomains=", URLencode(excludeDomains), "&")
    }
    
    if (is.null(from) == FALSE) {
      from <- paste0("from=", from, "&")
    }
    
    if (is.null(to) == FALSE) {
      to <- paste0("to=", to, "&")
    }
    
    if (is.null(language) == FALSE) {
      language <- paste0("language=", URLencode(language), "&")
    }
    
    if (is.null(sortBy) == FALSE) {
      sortBy <- paste0("sortBy=", URLencode(sortBy), "&")
    }
    
    base_link <- "https://newsapi.org/v2/everything?"
    
    
    initial_link <- paste0(base_link, sources, domains, excludeDomains, query1, qInTitle, language, from, to, sortBy, "apiKey=", api)
    
    # Containers ----
    
    date <- list()
    title <- list()
    newspaper <- list()
    link <- list()
    article <- list()
    
    # Number of Pages ----
    message("...Collecting articles about ", URLdecode(query), "...")
    
    pages <- fromJSON(initial_link)
    
    maxPages <- ceiling(pages[["totalResults"]] / 20)
    message(".....USING FREE API; MAX PAGES = 5.....")
    message("...Collecting Articles About ", query, "...")
    message("...Total Pages: ", maxPages, "...")
    
    
    # Contents ----
    for (page in 1:5) { #limited due to Free API.
      page_link <-
        paste0(base_link, sources, domains, excludeDomains, query1, qInTitle, language, from, to, sortBy, "page=", page, "&apiKey=", api)
      
      
      
      
      
      page_parse <- fromJSON(page_link)
      message("...Retrieving Page ", page, "/", maxPages,"...")
      
      
      date[[page]] <- page_parse[["articles"]][["publishedAt"]]
      newspaper[[page]] <- page_parse[["articles"]][["source"]][["name"]]
      title[[page]] <- page_parse[["articles"]][["title"]]
      link[[page]] <- page_parse[["articles"]][["url"]]
      article[[page]] <- page_parse[["articles"]][["content"]]
      
      
      Sys.sleep(.3)
    }
    
    
    # Preparing Data data for download ----
    news_data_frame <- tibble(newspaper = unlist(newspaper), title = unlist(title), link = unlist(link), date = unlist(date), article = unlist(article))
    
    message("...Scraping complete! Please press the download button for a .csv file...")
    
    news_data_frame
    
  }

news_api_paid <- function(query = NULL, qInTitle = NULL, domains = NULL, 
           excludeDomains = NULL, from = NULL, to = NULL, 
           language = NULL, query1 = NULL, sources = NULL, 
           sortBy = NULL, api = NULL) {
    
    if (is.null(sources) == FALSE) {
      sources <- paste0("sources=", sources, "&")
    }
    
    if (is.null(query) == FALSE) {
      query1 <- paste0("q=", URLencode(query), "&")
    }
    
    if (is.null(domains) == FALSE) {
      domains <- paste0("domains=", URLencode(domains), "&")
    }
    
    if (is.null(excludeDomains) == FALSE) {
      excludeDomains <-
        paste0("excludeDomains=", URLencode(excludeDomains), "&")
    }
    
    if (is.null(from) == FALSE) {
      from <- paste0("from=", from, "&")
    }
    
    if (is.null(to) == FALSE) {
      to <- paste0("to=", to, "&")
    }
    
    if (is.null(language) == FALSE) {
      language <- paste0("language=", URLencode(language), "&")
    }
    
    if (is.null(sortBy) == FALSE) {
      sortBy <- paste0("sortBy=", URLencode(sortBy), "&")
    }
    
    message("...Warning, paid API key required...")
    
    base_link <- "https://newsapi.org/v2/everything?"
    
    
    initial_link <- paste0(base_link, sources, domains, excludeDomains, query1, qInTitle, language, from, to, sortBy, "apiKey=", api)
    
    # Containers ----
    
    date <- list()
    title <- list()
    newspaper <- list()
    link <- list()
    article <- list()
    
    # Number of Pages ----
    pages <- fromJSON(initial_link)
    message("...Collecting articles about ", URLdecode(query), "...")
    
    maxPages <- ceiling(pages[["totalResults"]] / 20)
    message(".....USING FREE API; MAX PAGES = 5.....")
    message("...Collecting Articles About ", query, "...")
    message("...Total Pages: ", maxPages, "...")
    
    
    # Contents ----
    for (page in 1:maxPages) { 
      page_link <-
        paste0(base_link, sources, domains, excludeDomains, query1, qInTitle, language, from, to, sortBy, "page=", page, "&apiKey=", api)
      
      
      
      
      
      page_parse <- fromJSON(page_link)
      message("...Retrieving Page ", page, "/", maxPages,"...")
      
      
      date[[page]] <- page_parse[["articles"]][["publishedAt"]]
      newspaper[[page]] <- page_parse[["articles"]][["source"]][["name"]]
      title[[page]] <- page_parse[["articles"]][["title"]]
      link[[page]] <- page_parse[["articles"]][["url"]]
      article[[page]] <- page_parse[["articles"]][["content"]]
      
      
      Sys.sleep(.3)
    }
    
    
    # Preparing Data data for download ----
    news_data_frame <- tibble(newspaper = unlist(newspaper), title = unlist(title), link = unlist(link), date = unlist(date), article = unlist(article))
    
    message("...Scraping complete! Please press the download button for a .csv file...")
    
    news_data_frame
    
  }

headline_daily_function <- function(query, cut_off){
  
    base_link <- "http://hd.stheadline.com/search?keyword="
    page_link <- paste0(base_link, URLencode(query))
    parse_page <- read_html(page_link)
    
    hits <- parse_page %>%
      html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[1]/div/small') %>%
      html_text(trim = TRUE) %>% parse_number()
    
    page_num <- ceiling(hits / 10)
    
    
    # Containers ----
    title <- list()
    date <- list()
    article <- vector()
    link <- list()
    newspaper <- vector()

    message("...Collecting articles about ", URLdecode(query), "...")
    
    # Collecting Metadata ----
      
    message("...This query resulted in ",
            ceiling(hits/10),
            " pages...")
    
    message("...Collecting metadata and searching pages for Cut-Off Date...")
    
    for (i in 1:ceiling(hits/10)) {
      message("...Page ", i, "/", ceiling(hits/10),"...")
      query_link <- paste0(base_link, URLencode(query), "&page=", i)
      parse_link <- read_html(query_link)
      date[[i]] <- parse_link %>%
        html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[2]/div[*]/div[2]/p[2]/span/text()') %>%
        html_text() %>% parse_date() %>% as.character()
      
      
      title[[i]] <- parse_link %>%
        html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[2]/div[*]/div[1]') %>%
        html_text(trim = TRUE)
      
      link[[i]] <- parse_link %>%
        html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[2]/div[*]/div[1]/h4/a') %>%
        html_attr("href")
      
      if (date[[i]][10] < cut_off) {
        
        
        for (n in 1:length(unlist(date))) {
          newspaper[[n]] <- "Headline Daily"
        }
        
        prelim_news_data_frame <- tibble(newspaper = newspaper, title = unlist(title), link = unlist(link), date = unlist(date)) %>%
          filter(date >= cut_off)
        
        message("...Filtered content posted before ", ymd(cut_off),"...")
        
        break
        
        
      }
      
    }
    

    # Gathering Articles ----
    
    
    message("...Now collecting ", length(prelim_news_data_frame$link)," articles...")

    
    for (i in 1:length(prelim_news_data_frame$link)) {
      
      prelim_news_data_frame$link[i] <-
        paste0("http://hd.stheadline.com", prelim_news_data_frame$link[i])
      
      parse_article <- read_html(prelim_news_data_frame$link[i])
      message("...", i,"/",length(prelim_news_data_frame$link),"...")
      
      article_text <- parse_article %>%
        html_nodes(xpath = '//*[@id="news-content"]') %>%
        html_text(trim = TRUE)
      
      
      if (length(article_text) != 0) {
        article[i] <- article_text[article_text != ""]
        
      } else {
        
        article[i] <- "NA"
        
      }
    }
    
    
    
    # Preparing Data data for download ----
    news_data_frame <-  prelim_news_data_frame %>%
      mutate(article = article)
    
    message("...Scraping complete! Please press the download button for a .csv file...")
    
    news_data_frame
    
  }



hkfp_function <- function(page  = 50, query, cut_off) {
  
  
  
  # Prerequisites ----
    
    base_link <- "https://www.hongkongfp.com/page/"
    
    query_link <- paste0(base_link,page,"/?s=", URLencode(query))
    message("...Searching for Cut-off Date...")
    empty_page_catch <- tryCatch(read_html(query_link) %>%
                           html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[1]/header/h2/a')%>%
                           html_attr('href'),
                         error = function(e){NA})
    
    
    while (is.na(empty_page_catch) == TRUE) {
      n = 1
      message("...Processing ", n, "/", "50-ish...")
      page = page-1
      
      query_link <- paste0(base_link,page,"/?s=", URLencode(query))
      
      empty_page_catch <- tryCatch(read_html(query_link) %>%
                             html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[1]/header/h2/a')%>%
                             html_attr('href'),
                           error = function(e){NA})
      
      
    }
    
    
    message("...Collecting articles about ", URLdecode(query), "...")
    
    
    #  Containers ----
    
    title <- list()
    date <- list()
    article <- vector()
    link <- list()
    newspaper <- vector()
    
    
    # Data Collection (Metadata) ----
    
    message("...This query resulted in ",
            page,
            " pages...")
    
    message("...Collecting metadata...")
    
    for (i in 1:page) {
      message("...Working on Page ", i,"...")
      query_link <- paste0(base_link,i,"/?s=", URLencode(query))

      link_parse <- read_html(query_link)
      date[[i]] <- link_parse %>%
        html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[*]/header/div/div/span') %>%
        html_text() %>% parse_date(format = "%d %B %Y %H:%M") %>% as.character()
      
      
      title[[i]] <- link_parse %>%
        html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[*]/header/h2/a') %>%
        html_text(trim = TRUE) 
      
      link[[i]] <- link_parse %>%
        html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[*]/header/h2/a') %>%
        html_attr("href")
      
      message("...Metadata collected...")
      if (date[[i]][1:150] < cut_off) {
        
        
        
        for (n in 1:length(unlist(date))) {
          newspaper[[n]] <- "Hong Kong Free Press"
        }
        prelim_news_data_frame <- tibble(newspaper = newspaper, title = unlist(title), link = unlist(link), date = unlist(date)) %>%
          filter(date >= cut_off)
        
        
        message("...Filtered content posted before ", ymd(cut_off),"...")
        
        break
        
  
          }
          
        }
        
        
    
    # Data Collection (Articles) ----
    
    message("...Now collecting ", length(prelim_news_data_frame$link)," articles...")
    
    for (hit in 1:length(prelim_news_data_frame$link)) {
      Article_Read <- read_html(prelim_news_data_frame$link[hit])
      message("...", hit,"...")
      Texting <- Article_Read %>%
        html_nodes(xpath = '/html/body/div[1]/div/div[2]/div/main/article/div/p') %>%
        html_text(trim = TRUE) %>% paste(collapse = " ")
      #str_split(Texting,"相關新聞")
     
      if (length(Texting) != "") {
        article[hit] <- str_sub(Texting, end=-233)
      } else {
        article[hit] <- "NA"
      }
    }
    
    # Preparing Data data for download ----
    news_data_frame <-  prelim_news_data_frame %>%
      mutate(article = article)
    
    message("...Scraping complete! Please press the download button for a .csv file...")
    
    news_data_frame
    
    
  }


# Define UI for data download app -----
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyjs(),
  # App title ----
  titlePanel("News Data Collection"),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      shiny::actionButton(
        inputId = 'ab1',
        label = "Guardian API Documentation",
        icon = icon("th"),
        onclick = "window.open('https://open-platform.theguardian.com/documentation/')"
      ),
      shiny::actionButton(
        inputId = 'ab2',
        label = "NYT API Documentation",
        icon = icon("th"),
        onclick = "window.open('https://developer.nytimes.com/docs/articlesearch-product/1/overview/')"
      ),
      shiny::actionButton(
        inputId = 'ab3',
        label = "News API Documentation",
        icon = icon("th"),
        onclick = "window.open('https://newsapi.org/docs ')"
      ),
      
      # Input: Choose dataset ----
      selectInput(
        "dataset",
        "Choose Source:",
        choices = c(
          "No Selection",
          "Guardian",
          "New York Times",
          "NewsAPI Free",
          "NewsAPI Paid",
          "Financial Times",
          "Headline Daily",
          "Hong Kong Free Press"
        )
      ),
      
      # Input: Dates ----
      
      ## Guardian
      conditionalPanel(
        condition = "input.dataset == 'Guardian'",
        dateInput("start1", "Start Date", format = "yyyy-mm-dd")
      ),
      conditionalPanel(condition = "input.dataset == 'Guardian'",
                       dateInput("end1", "End Date", format = "yyyy-mm-dd")),
      
      ## New York Times
      conditionalPanel(
        condition = "input.dataset == 'New York Times'",
        dateInput("start2", "Start Date", format = "yyyymmdd")
      ),
      conditionalPanel(condition = "input.dataset == 'New York Times'",
                       dateInput("end2", "End Date", format = "yyyymmdd")),
      
      ## NewsAPI
      conditionalPanel(
        condition = "input.dataset == 'NewsAPI Free' || input.dataset == 'NewsAPI Paid' ",
        dateInput("start3", "Start Date", format = "yyyy-mm-dd")
      ),
      conditionalPanel(condition = "input.dataset == 'NewsAPI Free' || input.dataset == 'NewsAPI Paid' ",
                       dateInput("end3", "End Date", format = "yyyy-mm-dd")),
      
      ## Guardian
      conditionalPanel(
        condition = "input.dataset == 'Financial Times'",
        dateInput("start4", "Start Date", format = "yyyy-mm-dd")
      ),
      conditionalPanel(condition = "input.dataset == 'Financial Times'",
                       dateInput("end4", "End Date", format = "yyyy-mm-dd")),
      
      # Headline Daily
      
      conditionalPanel(condition = "input.dataset == 'Headline Daily'",
                       dateInput("end5", "Cut-Off Date", format = "yyyy-mm-dd")),
      
      # Hong Kong Free Press
      
      conditionalPanel(condition = "input.dataset == 'Hong Kong Free Press'",
                       dateInput("end6", "Cut-Off Date", format = "yyyy-mm-dd")),
      
      # Input: Query ----
      
      conditionalPanel(condition = "input.dataset != 'No Selection'",
                       textInput("query", "Enter Search Term")),
      
      # Input: News API Modifiers ----
      
      conditionalPanel(
        condition = "input.dataset == 'NewsAPI Free' || input.dataset == 'NewsAPI Paid' ",
        selectInput(
          "sources",
          "Choose Sources (Optional)",
          choices = c(
            "",
            "the-washington-post",
            "bbc-news",
            "the-telegraph",
            "the-wall-street-journal",
            "xinhua-net"
          )
        )
      ),
      
      # Input: API ----
      ## Guardian
      conditionalPanel(condition = "input.dataset == 'Guardian'",
                       textInput("APIG", "Enter Guardian API Key")),
      ## New York Times
      conditionalPanel(condition = "input.dataset == 'New York Times'",
                       textInput("APIN", "Enter New York Times API Key")),
      ## NewsAPI
      conditionalPanel(condition = "input.dataset == 'NewsAPI Free'",
                       textInput("NAPIF", "Enter New API Developer Key")),
      
      conditionalPanel(condition = "input.dataset == 'NewsAPI Paid'",
                       textInput("NAPIP", "Enter New API Business Key")),
      
      
      
      # Input: Buttons ----
      ## Build Data Set
      actionButton(inputId = "clicks",
                   label = "Build Dataset"),
      ## Reset Everything
      actionButton(inputId = "reset",
                   label = "Reset"),
      
      ## Download Button
      downloadButton("downloadData", "Download Data"),
      textOutput("text")),
    
    # Main panel for displaying outputs ----
    #See if Possible to Add Console Output
    mainPanel(tableOutput("table"))
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output, session) {
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(
      input$dataset,
      "Guardian" = guardian_function(
        from_date = input$start1,
        to_date = input$end1,
        query = URLencode(input$query),
        api = input$APIG
      ),
      "New York Times" = nyt_function(
        query = URLencode(input$query),
        from_date = input$start2,
        to_date = input$end2,
        api = input$APIN
      ),
      "NewsAPI Free" = news_api_free(
        from = input$start3,
        to = input$end3,
        query = input$query,
        api = input$NAPIF,
        sources = input$sources
      ),
      "NewsAPI Paid" = news_api_paid(
        from = input$start3,
        to = input$end3,
        query = input$query,
        api = input$NAPIP,
        sources = input$sources
      ),
      "Financial Times" = ft_function(
        from_date = input$start4,
        to_date = input$end4,
        query = input$query
      ),
      "Headline Daily" = headline_daily_function(
        query = URLencode(input$query),
        cut_off = input$end5
      ),
      "Hong Kong Free Press" = hkfp_function(
        query = URLencode(input$query),
        cut_off = input$end6
      )
    )
  })
  
  
  # Table of selected dataset ----
  ## Renders the table with the provided information once the $clicks is clicked
  observeEvent(input$clicks, {
    output$table <- renderTable({
      withCallingHandlers({
        shinyjs::html("text", "")
        datasetInput()
      },
      message = function(m) {
        shinyjs::html(
          id = "text",
          html = paste(m$message, sep = "<\n>"),
          add = TRUE,
        )
      })
    })
  })
  ##Hides the Click Button
  observeEvent(input$clicks, {
    hide("clicks")
    shinyjs::disable("dataset")
    shinyjs::disable("start1")
    shinyjs::disable("start2")
    shinyjs::disable("end1")
    shinyjs::disable("query")
    shinyjs::disable("end2")
    shinyjs::disable("APIN")
    shinyjs::disable("APIG")
    shinyjs::disable("NAPIP")
    shinyjs::disable("NAPIF")
    shinyjs::disable("sources")
    shinyjs::disable("start3")
    shinyjs::disable("end3")
    shinyjs::disable("start4")
    shinyjs::disable("end4")
    shinyjs::disable("end5")
    shinyjs::disable("end6")
    
  })
  
  # Reset ----
  ## Resets everything
  observeEvent(input$reset, {
    shinyjs::reset("dataset")
    shinyjs::reset("start1")
    shinyjs::reset("start2")
    shinyjs::reset("end1")
    shinyjs::reset("end2")
    shinyjs::reset("APIN")
    shinyjs::reset("APIG")
    shinyjs::reset("query")
    shinyjs::reset("NAPIF")
    shinyjs::reset("NAPIP")
    shinyjs::reset("sources")
    shinyjs::reset("start3")
    shinyjs::reset("end3")
    shinyjs::reset("start4")
    shinyjs::reset("end4")
    shinyjs::reset("end5")
    shinyjs::reset("end6")
    shinyjs::show("clicks")
    shinyjs::enable("dataset")
    shinyjs::enable("start1")
    shinyjs::enable("start2")
    shinyjs::enable("end1")
    shinyjs::enable("end2")
    shinyjs::enable("query")
    shinyjs::enable("APIN")
    shinyjs::enable("APIG")
    shinyjs::enable("NAPIF")
    shinyjs::enable("NAPIP")
    shinyjs::enable("sources")
    shinyjs::enable("start3")
    shinyjs::enable("end3")
    shinyjs::enable("start4")
    shinyjs::enable("end4")
    shinyjs::enable("end5")
    shinyjs::enable("end6")
    
    
    output$table <- NULL
    output$text <- NULL
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, noquote(input$query),".csv",sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}


# Create Shiny app ----
shinyApp(ui, server)
