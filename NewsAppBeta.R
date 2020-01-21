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



###Guardian API: "b80008a9-a43c-4047-a0ad-47f0cd8efb20"
###NYTIMES_API <- "KCOgDvSOdjGOQeF0kfQJ7Ch4IqMhEG42"
###newsapi <- "0017f268ff0b414bbb505d701dddacb0"

FinancialTimesFunction <-
  function(x,
           y,
           z) {
    basiclink <- "https://www.ft.com/search?q="
    sorted <- "&sort=date"
    pageslink <- paste0(basiclink,
                        URLencode(x),
                        "&dateFrom=",
                        y,
                        "&dateTo=",
                        z,
                        sorted)
    FTread <- read_html(pageslink)
    FT_Prime <- FTread %>%
      html_nodes(xpath = '//*[@id="site-content"]/div/div[*]/div/span/text()[2]') %>%
      html_text()
    if (length(FT_Prime) == 0){
      return(paste0("No Result, Please Modify Search Parameters"))
    }
    FT_Pages <- FT_Prime
    Pages <- parse_number(FT_Pages)
    FTtitle <- list()
    FTlink <- list()
    FTdate <- list()
    FTarticle <- vector()
    Newspaper <- vector()
    hits <- Pages * 25
    if (hits > 1000) {
      message(
        " ...This search resulted in ",
        hits,
        " articles. Financial times will only display 1000 results per query. Use multiple queries to get more results... "
      )
      Pages <- 40
    }
    message(" ...Now scraping ", Pages, " pages ", " and ", Pages*25, " articles... ")
    for (i in 1:Pages) {
      Link <-
        paste0(basiclink,
               URLencode(x),
               "&page=",
               i,
               "&dateFrom=",
               y,
               "&dateTo=",
               z,
               sorted)
      FTread <- read_html(Link)
      
      FTtitle[[i]] <- FTread %>%
        html_nodes(xpath = '//*[@id="site-content"]/div/ul/li[*]/div/div/div/div[1]/div[2]/a') %>%
        html_text()
      
      FTlink[[i]] <- FTread %>%
        html_nodes(xpath = '//*[@id="site-content"]/div/ul/li[*]/div/div/div/div[1]/div[2]/a') %>%
        html_attr("href")
      
      FTdate[[i]] <- FTread %>%
        html_nodes(xpath = '//*[@id="site-content"]/div/ul/li[*]/div/div/div/div[1]/div[3]/time') %>%
        html_attr("datetime")
      Sys.sleep(.25)
      message(" ...Working on Page ", i,"... ")
      
    }
    
    FTDatasetTest <-
      as.data.frame(cbind(Newspaper,unlist(FTtitle), unlist(FTlink), unlist(FTdate)), stringsAsFactors = FALSE)
    
    
    for (i in 1:length(FTDatasetTest$V2)) {
      if (str_detect(FTDatasetTest$V2[i], c("content|video")) == TRUE) {
        FTDatasetTest$V2[i] <-
          paste0("https://www.ft.com", FTDatasetTest$V2[i])
      }
      
      if (str_detect(FTDatasetTest$V2[i], "content") == TRUE) {
        FT_Article_Read <- read_html(FTDatasetTest$V2[i])
        message("...Gathering Article #", i)
        Text <- FT_Article_Read %>%
          html_nodes(xpath = '//*[@id="site-content"]/div[3]/div[2]') %>%
          html_text()
        if (length(Text) != 0){ FTarticle[i] <- Text} else {
          FTarticle[i] <- "NA"
        }
        
        Sys.sleep(.3)
      } else {
        FTarticle[i] <-
          "MANUAL"
        message(
          " ...This is not a standard article page, please check this link manually: ",
          FTDatasetTest$V2[i]," ... "
        )
      }
      
    }
    for (n in 1:length(FTarticle)) {
      Newspaper[n] <- "Financial Times"
    }
    FTDatasetTest <-
      as.data.frame(cbind(Newspaper, FTDatasetTest, FTarticle), stringsAsFactors = FALSE)
    colnames(FTDatasetTest) <- c("Newspaper", "Titles","Link","Date","Article")
    FTDatasetTest
  }


GuardianFunction <-
  function(x = "2014-01-01",
           y = "2019-12-31",
           i = "Hong%20Kong",
           z = "Your API Key") {
    # Prerequisites ----
    GBase <-
      "https://content.guardianapis.com/search?section=world&show-fields=bodyText&q="
    From <- "&from-date="
    To <- "&to-date="
    Page <- "&page="
    Blocks <- "&show-blocks=all"
    url <-
      paste0(GBase, i, From, x, To, y, Page, "1", "&api-key=", z)
    tt <- fromJSON(url)
    Newspaper <- list()
    Titles <- list()
    SectionName <- list()
    PublicationDate <- list()
    URL <- list()
    FullText <- list()
    
    # Status Messages ----
    message("Collecting Articles About ", i, "...")
    message("Total Pages ", tt[["response"]][["pages"]], "...")
    message("Completion in ", (tt[["response"]][["pages"]] + 30) / 60, " minutes", "...")
    
    # Data Collection ----
    for (t in 1:tt[["response"]][["pages"]]) {
      message("Gathering Page ", t, "/", tt[["response"]][["pages"]], "...")
      url1 <-
        paste0(GBase, i, From, x, To, y, Page, t, "&api-key=", z)
      json <- fromJSON(url1)
      Titles[[t]] <- json[["response"]][["results"]][["webTitle"]]
      SectionName[[t]] <-
        json[["response"]][["results"]][["sectionName"]]
      URL[[t]] <- json[["response"]][["results"]][["webUrl"]]
      FullText[[t]] <-
        trimws(json[["response"]][["results"]][["fields"]][["bodyText"]])
      PublicationDate[[t]] <-
        json[["response"]][["results"]][["webPublicationDate"]]
      Sys.sleep(.3)
    }
    
    # Newspaper Column ----
    for (n in 1:length(unlist(Titles))) {
      Newspaper[[n]] <- "Guardian"
    }
    
    # Constructing the Table ----
    Table1 <- cbind(
      unlist(Newspaper),
      unlist(Titles),
      unlist(SectionName),
      unlist(URL),
      unlist(PublicationDate),
      unlist(FullText)
    )
    Table1 <- as.data.frame(Table1)
    colnames(Table1) <- c("Newspaper", "Titles","Section","Link","Date","Article")
    message("READY FOR DOWNLOAD")
    Table1
  }

TimesFunction <-
  function(x = 20140101,
           y = 20191231,
           i = "Hong%20Kong",
           z = "Your API Key") {
    # Prerequisites ----
    
    url <-
      paste0(
        "http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",
        i,
        "&begin_date=",
        x,
        "&end_date=",
        y,
        "&facet_filter=true&api-key=",
        z
      )
    initialQuery <- fromJSON(url)
    maxPages <- ceiling(initialQuery$response$meta$hits[1] / 10)
    if (maxPages > 200) {
      message("You're trying to gather ", maxPages, "pages.")
      maxPages <- 200
      message(".....NYT API Allows for 200 Pages Before Kicking you Out....")
    } else {
      maxPages <- maxPages
    }
    
    # Status Messages ----
    
    message("Collecting Articles About ", i, "...")
    message("Total Pages: ", maxPages, "...")
    message("Completion in ", (maxPages * 6 + 120) / 60, " minutes", "...")
    
    # Containers ----
    
    Newspaper <- list()
    Titles <- list()
    SectionName <- list()
    PublicationDate <- list()
    URL <- list()
    Snippet <- list()
    FullArticle <- vector()
    
    # Data Collection (Pages) ----
    
    for (t in 1:maxPages) {
      url <-
        paste0(
          "http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",
          i,
          "&begin_date=",
          x,
          "&end_date=",
          y,
          "&page=",
          t,
          "&facet_filter=true&api-key=",
          z
        )
      json <- fromJSON(url)#Constructing the url
      message("Retrieving Page ", t, "/", maxPages)
      Titles[[t]] <-
        json[["response"]][["docs"]][["headline"]][["main"]]
      SectionName[[t]] <-
        json[["response"]][["docs"]][["section_name"]]
      URL[[t]] <- json[["response"]][["docs"]][["web_url"]]
      PublicationDate[[t]] <-
        json[["response"]][["docs"]][["pub_date"]]
      Sys.sleep(6) #Waiting to ensure that the Api doesn't kick us out
    }
    LinkPrerp <- unlist(URL)
    # Data Collection (Articles) ----
    
    for (n in 1:length(LinkPrerp)) {
      if (str_detect(LinkPrerp[n], c("video")) == FALSE) {
        step1 <- read_html(LinkPrerp[n])
        news_html <- htmlParse(step1)
        #ttext <-  tryCatch(trimws(unlist(xpathSApply(news_html, '//*[@id="story"]/section/div', xmlValue),error = function(e){NA})))
        
        catching <- unlist(xpathSApply(news_html, path = '//*[@id="story"]/section/div', xmlValue))
        FullArticle[n] <- paste(trimws(catching), collapse = "")
      } else {
        FullArticle[n] <- NA
      }
      
      
      message("Article ", n, "/", length(LinkPrerp), " Retrieved", "...")
      Sys.sleep(.2)
    }
    
    # Newspaper Column ----
    
    for (n in 1:length(unlist(Titles))) {
      Newspaper[[n]] <- "New York Times"
    }
    
    # Constructing the Table ----
    Table1 <-
      cbind(
        unlist(Newspaper),
        unlist(Titles),
        unlist(SectionName),
        unlist(URL),
        unlist(PublicationDate),
        unlist(FullArticle)
      )
    Table1 <- as.data.frame(Table1)
    colnames(Table1) <- c("Newspaper", "Titles","Section","Link","Date","Article")
    message("READY FOR DOWNLOAD")
    Table1
  }


NewsAPIEverythingFree <-
  function(query = NULL,
           qInTitle = NULL,
           domains = NULL,
           excludeDomains = NULL,
           from = NULL,
           to = NULL,
           language = NULL,
           query1 = NULL,
           sources = NULL,
           sortBy = NULL,
           api = NULL) {
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
    BaseLink <- "https://newsapi.org/v2/everything?"
    url <-
      paste0(
        BaseLink,
        sources,
        domains,
        excludeDomains,
        query1,
        qInTitle,
        language,
        from,
        to,
        sortBy,
        "apiKey=",
        api
      )
    #return(url)
    jason <- fromJSON(url)
    publication <- list()
    title <- list()
    source <- list()
    links <- list()
    content <- list()
    maxPages <- ceiling(jason[["totalResults"]] / 20)
    message(".....USING DEVELOPER API; MAX PAGES = 5.....")
    message("Collecting Articles About ", query, "...")
    message("Total Pages: ", maxPages, "...")
    
    for (x in 1:5) {
      url2 <-
        paste0(
          BaseLink,
          sources,
          domains,
          excludeDomains,
          query1,
          qInTitle,
          language,
          from,
          to,
          sortBy,
          "page=",
          x,
          "&apiKey=",
          api
        )
      #return(url2)
      jason2 <- fromJSON(url2)
      message("Retrieving Page ", x, "/", maxPages)
      publication[[x]] <- jason2[["articles"]][["publishedAt"]]
      source[[x]] <- jason2[["articles"]][["source"]][["name"]]
      title[[x]] <- jason2[["articles"]][["title"]]
      links[[x]] <- jason2[["articles"]][["url"]]
      content[[x]] <- jason2[["articles"]][["content"]]
      Sys.sleep(.3)
    }
    framing <-
      cbind(
        unlist(source),
        unlist(title),
        unlist(links),
        unlist(publication),
        unlist(content)
      )
    framing <- as.data.frame(framing)
    colnames(Dataseting) <- c("Source","Titles", "Links","Dates","Articles")
    message("READY FOR DOWNLOAD")
    framing
  }

NewsAPIEverythingPaid <-
  function(query = NULL,
           qInTitle = NULL,
           domains = NULL,
           excludeDomains = NULL,
           from = NULL,
           to = NULL,
           language = NULL,
           query1 = NULL,
           sources = NULL,
           sortBy = NULL,
           api = NULL) {
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
    BaseLink <- "https://newsapi.org/v2/everything?"
    url <-
      paste0(
        BaseLink,
        sources,
        domains,
        excludeDomains,
        query1,
        qInTitle,
        language,
        from,
        to,
        sortBy,
        "apiKey=",
        api
      )
    #return(url)
    jason <- fromJSON(url)
    publication <- list()
    title <- list()
    source <- list()
    links <- list()
    content <- list()
    maxPages <- ceiling(jason[["totalResults"]] / 20)
    message("Collecting Articles About ", query, "...")
    message("Total Pages: ", maxPages, "...")
    
    for (x in 1:maxPages) {
      url2 <-
        paste0(
          BaseLink,
          sources,
          domains,
          excludeDomains,
          query1,
          qInTitle,
          language,
          from,
          to,
          sortBy,
          "page=",
          x,
          "&apiKey=",
          api
        )
      #return(url2)
      jason2 <- fromJSON(url2)
      message("Retrieving Page ", x, "/", maxPages)
      publication[[x]] <- jason2[["articles"]][["publishedAt"]]
      source[[x]] <- jason2[["articles"]][["source"]][["name"]]
      title[[x]] <- jason2[["articles"]][["title"]]
      links[[x]] <- jason2[["articles"]][["url"]]
      content[[x]] <- jason2[["articles"]][["content"]]
      Sys.sleep(.3)
    }
    framing <-
      cbind(
        unlist(source),
        unlist(title),
        unlist(links),
        unlist(publication),
        unlist(content)
      )
    framing <- as.data.frame(framing)
    colnames(Dataseting) <- c("Source","Titles", "Links","Dates","Articles")
    message("READY FOR DOWNLOAD")
    framing
  }

HeadlineDailyFunction <-
  function(x, y) {
    basiclink <- "http://hd.stheadline.com/search?keyword="
    pageslink <- paste0(basiclink, URLencode(x))
    Pageread <- read_html(pageslink)
    
    hits <- Pageread %>%
      html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[1]/div/small') %>%
      html_text(trim = TRUE) %>% parse_number()
    
    Pages <- ceiling(hits / 10)
    Titles <- list()
    Dates <- list()
    Articles <- vector()
    Links <- list()
    Newspaper <- vector()
    message("...This search resulted in a total of ",
            hits,
            " articles. .")
    
    message("...Now scraping Pages and Searching for Cut-Off Date...")
    
    for (i in 1:30) {
      message("Working on Page ", i)
      Link <- paste0(basiclink, URLencode(x), "&page=", i)
      message(Link)
      LinkRead <- read_html(Link)
      Dates[[i]] <- LinkRead %>%
        html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[2]/div[*]/div[2]/p[2]/span/text()') %>%
        html_text() %>% parse_date() %>% as.character()
      
      
      Titles[[i]] <- LinkRead %>%
        html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[2]/div[*]/div[1]') %>%
        html_text(trim = TRUE)
      
      Links[[i]] <- LinkRead %>%
        html_nodes(xpath = '/html/body/div[1]/div[2]/div[2]/div/div/div[1]/div/div[1]/div/div/div/div[2]/div[*]/div[1]/h4/a') %>%
        html_attr("href")
      
      if (Dates[[i]][10] < y) {
        message("...Detected Cutoff Date. Collected ",i," Pages of Articles...")
        for (n in 1:10) {
          if (Dates[[i]][n] < y) {
            Dates[[i]][n] <- NA
            Titles[[i]][n] <- NA
            Links[[i]][n] <- NA
          }
        }
        Dates[[i]] <- Dates[[i]][!is.na(Dates[[i]])]
        Titles[[i]] <- Titles[[i]][!is.na(Titles[[i]])]
        Links[[i]] <- Links[[i]][!is.na(Links[[i]])]
        break
      }
      
    }
    Dataseting <-
      as.data.frame(cbind(unlist(Titles), unlist(Links), unlist(Dates)), stringsAsFactors = FALSE)
    for (i in 1:length(Dataseting$V2)) {
      Dataseting$V2[i] <-
        paste0("http://hd.stheadline.com", Dataseting$V2[i])
      
      Article_Read <- read_html(Dataseting$V2[i])
      message("Gathering Article #", i)
      Texting <- Article_Read %>%
        html_nodes(xpath = '//*[@id="news-content"]') %>%
        html_text(trim = TRUE)
      #str_split(Texting,"相關新聞")[1]
      if (length(Texting) != 0) {
        Articles[i] <- Texting[Texting != ""]
      } else {
        Articles[i] <- "NA"
      }
    }
    for (n in 1:length(Articles)) {
      Newspaper[[n]] <- "Headline Daily"
    }
    
    Dataseting <-
      as.data.frame(cbind(Newspaper, Dataseting, Articles), stringsAsFactors = FALSE)
    colnames(Dataseting) <- c("Newspaper","Titles", "Links","Dates","Articles")
    Dataseting
  }



HKFunctionPress <-
  function(n  = 50, x, y) {
    basiclink <- "https://www.hongkongfp.com/page/"
    pageslink <- paste0(basiclink,n,"/?s=", URLencode(x))
    
    catching <- tryCatch(read_html(pageslink) %>%
                           html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[1]/header/h2/a')%>%
                           html_attr('href'),
                         error = function(e){NA})
    while (is.na(catching) == TRUE) {
      n = n-1
      pageslink <- paste0(basiclink,n,"/?s=", URLencode(x))
      catching <- tryCatch(read_html(pageslink) %>%
                             html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[1]/header/h2/a')%>%
                             html_attr('href'),
                           error = function(e){NA})
      message("...Processing...")
    }
    
    hits <- n*150
    Titles <- list()
    Dates <- list()
    Articles <- vector()
    Links <- list()
    Newspaper <- vector()
    message("...This search resulted in a total of ",
            hits,
            " articles. .")
    
    message("...Now scraping Pages and Searching for Cut-Off Date...")
    
    for (i in 1:n) {
      message("Working on Page ", i)
      Link <- paste0(basiclink,i,"/?s=", URLencode(x))
      message(Link)
      LinkRead <- read_html(Link)
      Dates[[i]] <- LinkRead %>%
        html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[*]/header/div/div/span') %>%
        html_text() %>% parse_date(format = "%d %B %Y %H:%M") %>% as.character()
      
      
      Titles[[i]] <- LinkRead %>%
        html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[*]/header/h2/a') %>%
        html_text(trim = TRUE) 
      
      Links[[i]] <- LinkRead %>%
        html_nodes(xpath = '//*[@id="primary"]/div/div[2]/div/article[*]/header/h2/a') %>%
        html_attr("href")
      
      message(length(Dates[[1]]))
      if (Dates[[i]][1:150] < y) {
        message("...Detected Cutoff Date. Collected ",i," Pages of Articles...")
        for (k in 1:150) {
          if (Dates[[i]][k] < y) {
            Dates[[i]][k] <- NA
            Titles[[i]][k] <- NA
            Links[[i]][k] <- NA
          }
        }
        Dates[[i]] <- Dates[[i]][!is.na(Dates[[i]])]
        Titles[[i]] <- Titles[[i]][!is.na(Titles[[i]])]
        Links[[i]] <- Links[[i]][!is.na(Links[[i]])]
        break
      }
      
    }
    Dataseting <-
      as.data.frame(cbind(unlist(Titles), unlist(Links), unlist(Dates)), stringsAsFactors = FALSE)
    for (i in 1:length(Dataseting$V2)) {
      Article_Read <- read_html(Dataseting$V2[i])
      message("Gathering Article #", i)
      Texting <- Article_Read %>%
        html_nodes(xpath = '/html/body/div[1]/div/div[2]/div/main/article/div/p') %>%
        html_text(trim = TRUE) %>% paste(collapse = " ")
      #str_split(Texting,"相關新聞")
     
      if (length(Texting) != "") {
        Articles[i] <- str_sub(Texting, end=-233)
      } else {
        Articles[i] <- "NA"
      }
    }
    for (n in 1:length(Articles)) {
      Newspaper[[n]] <- "Hong Kong Free Press"
    }
    Dataseting <-
      as.data.frame(cbind(Newspaper,Dataseting, Articles), stringsAsFactors = FALSE)
    colnames(Dataseting) <- c("Newspaper","Titles", "Links","Dates","Articles")
    Dataseting
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
      "Guardian" = GuardianFunction(
        x = input$start1,
        y = input$end1,
        i = URLencode(input$query),
        z = input$APIG
      ),
      "New York Times" = TimesFunction(
        i = URLencode(input$query),
        x = input$start2,
        y = input$end2,
        z = input$APIN
      ),
      "NewsAPI Free" = NewsAPIEverythingFree(
        from = input$start3,
        to = input$end3,
        query = input$query,
        api = input$NAPIF,
        sources = input$sources
      ),
      "NewsAPI Paid" = NewsAPIEverythingPaid(
        from = input$start3,
        to = input$end3,
        query = input$query,
        api = input$NAPIP,
        sources = input$sources
      ),
      "Financial Times" = FinancialTimesFunction(
        y = input$start4,
        z = input$end4,
        x = input$query
      ),
      "Headline Daily" = HeadlineDailyFunction(
        x = URLencode(input$query),
        y = input$end5
      ),
      "Hong Kong Free Press" = HKFunctionPress(
        x = URLencode(input$query),
        y = input$end6
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
