#### EFFICIENT GLASSDOOR SCRAPER ####

# Packages
library(gdscrapeR)
library(dplyr)
library(pillar)
library(stringr)    # pattern matching functions

library(httr)  #get HTML document: GET()
library(xml2)  #convert to XML document: read_html()
library(rvest) #select & extract text from XML: html_nodes() & html_text() html_attr()
library(purrr) #iterate scraping and return data frame: map_df()

# Set URL
baseurl <- "https://www.glassdoor.co.in/Reviews/xxxxxxxx-xxxxx-xxxxxx-"
companyNum <- "xxxxxx"
sort <- ".htm?sort.sortType=RD&sort.ascending=false"

# Set URL of another company - to compare the reviews
#baseurl <- "https://www.glassdoor.co.in/Reviews/xxxxxxxx-xxxxx-xxxxxx-"
#companyNum <- "xxxxxx"
#sort <- ".htm?sort.sortType=RD&sort.ascending=false"

# How many total number of reviews? It will determine the maximum page results to iterate over.
pg <- read_html(paste(baseurl, companyNum, sort, sep = ""))
totalReviews <- pg %>% html_nodes("#NodeReplace") %>%
  html_node("main > div > div > h2 > span") %>%
  html_text() %>% cat() %>%
  sub("Found", "", .) %>%
  sub("reviews", "", .) %>%
  sub(" ", "", .) %>%
  as.integer()

# Input Parameters
reviewsperpage <- 10 # should be always = 10, do not change this number
startpage <- 1
#endpage <- as.integer(ceiling(totalReviews/10))    #10 reviews per page, round up to whole number
endpage <- 6
batchsize <- 3

# Create data frames for: Date, Summary, Rating, Title, Pros, Cons, Advice, Helpful
# Function Reviews Scrapper
fngdreviewsscrapper <- function(s1,e1) {map_df(s1:e1, function(i) {
  # Page Number
  cat(" P", i, sep = "")
  pg <- read_html(GET(paste(baseurl, companyNum, "_P", i, sort, sep = "")))
  
  # Review ids in page
  reviewids <- pg %>% html_nodes("#ReviewsFeed") %>% html_nodes(".empReview.cf") %>% 
    html_attr("id") %>% data.frame(rev.reviewids = ., stringsAsFactors = F) 
    
    dfrows <- map_df(1:nrow(reviewids), function(r) {
      r <- reviewids[r,"rev.reviewids"]
      rvw <- pg %>% html_node(paste("#",r,sep=""))
      
      # Review id
      reviewid <- r %>% data.frame(rev.reviewid = ., stringsAsFactors = F)
  
      # Date
      date <- rvw %>% html_node("div.hreview") %>% html_node(".date.subtle.small") %>%
        html_text() %>% data.frame(rev.date = ., stringsAsFactors = F)

      # Helpful
      #pg %>% html_nodes(".tight") %>%
      helpful <- rvw %>% html_node("div.row.mt") %>% html_node(".gd-ui-button.css-ij39oc") %>%
        html_text() %>% data.frame(rev.helpf = ., stringsAsFactors = F)

      # Summary
      summary <- rvw %>% html_node("div.row.mt") %>% html_node(".summary.strong.mt-0") %>%
        html_text() %>% data.frame(rev.sum = ., stringsAsFactors = F)

      # Rating
      rating <- rvw %>% html_node("div.row.mt") %>% html_node("span.gdStars.gdRatings .value-title") %>%
        html_attr("title") %>% data.frame(rev.rating = ., stringsAsFactors = F)

      # SubRatingtitle
      subratingtitle <- rvw %>% html_node("div.row.mt") %>% html_node("div.mr-xsm.d-lg-inline-block > span > div > ul") %>%
        html_text() %>% data.frame(rev.subratingtitle = ., stringsAsFactors = F)
      
      # SubRating
      sr=1
      worklifebalance <- data.frame(rev.worklifebalance = NA_character_, stringsAsFactors = F)
      culturenvalues <- data.frame(rev.culturenvalues = NA_character_, stringsAsFactors = F)
      careeroppor <- data.frame(rev.careeroppor = NA_character_, stringsAsFactors = F)
      compnbenefits <- data.frame(rev.compnbenefits = NA_character_, stringsAsFactors = F)
      seniormgmt <- data.frame(rev.seniormgmt = NA_character_, stringsAsFactors = F)
      
      if(!is.na(subratingtitle))
      {
        #Work/Life Balance
        if(str_detect(subratingtitle,"Work/Life Balance",negate=F))
        {
          worklifebalance <- rvw %>% html_node("div.row.mt") %>% html_node(paste("div.mr-xsm.d-lg-inline-block > span > div > ul > li:nth-child(", sr,") > span", sep="")) %>%
            html_attr("title") %>% data.frame(rev.worklifebalance = ., stringsAsFactors = F)
          sr <- sr + 1
        }
        
        #Culture & Values
        if(str_detect(subratingtitle,"Culture & Values",negate=F))
        {
          culturenvalues <- rvw %>% html_node("div.row.mt") %>% html_node(paste("div.mr-xsm.d-lg-inline-block > span > div > ul > li:nth-child(", sr,") > span", sep="")) %>%
            html_attr("title") %>% data.frame(rev.culturenvalues = ., stringsAsFactors = F)
          sr <- sr + 1
        }
        
        #Career Opportunities
        if(str_detect(subratingtitle,"Career Opportunities",negate=F))
        {
          careeroppor <- rvw %>% html_node("div.row.mt") %>% html_node(paste("div.mr-xsm.d-lg-inline-block > span > div > ul > li:nth-child(", sr,") > span", sep="")) %>%
            html_attr("title") %>% data.frame(rev.careeroppor = ., stringsAsFactors = F)
          sr <- sr + 1
        }
        
        #Compensation and Benefits
        if(str_detect(subratingtitle,"Compensation and Benefits",negate=F))
        {
          compnbenefits <- rvw %>% html_node("div.row.mt") %>% html_node(paste("div.mr-xsm.d-lg-inline-block > span > div > ul > li:nth-child(", sr,") > span", sep="")) %>%
            html_attr("title") %>% data.frame(rev.compnbenefits = ., stringsAsFactors = F)
          sr <- sr + 1
        }
        
        #Senior Management
        if(str_detect(subratingtitle,"Senior Management",negate=F))
        {
          seniormgmt <- rvw %>% html_node("div.row.mt") %>% html_node(paste("div.mr-xsm.d-lg-inline-block > span > div > ul > li:nth-child(", sr,") > span", sep="")) %>%
            html_attr("title") %>% data.frame(rev.seniormgmt = ., stringsAsFactors = F)
          sr <- sr + 1
        }
      }
      
      # Title
      title <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0 > div:nth-child(3) > div > span > span.authorJobTitle.middle.reviewer") %>%
        html_text() %>% data.frame(rev.title = ., stringsAsFactors = F)
      
      # Location
      location <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0 > div:nth-child(3) > div > span > span:nth-child(2) > span") %>%
        html_text() %>% data.frame(rev.location = ., stringsAsFactors = F)
      
      # Recommends
      recommends <- rvw %>% html_node("div.row.mt") %>% html_node("div.row.reviewBodyCell.recommends") %>%
        html_text() %>% data.frame(rev.recommends = ., stringsAsFactors = F)
      
      # CEOApprove
      #ceoapproves <- rvw %>% html_node("div.row.mt") %>% html_node("div.row.reviewBodyCell.recommends") %>%
        #html_text() %>% data.frame(rev.ceoapproves = ., stringsAsFactors = F)

      # Maintext
      maintext <- rvw %>% html_node("div.row.mt") %>% html_node("p.mainText.mb-0") %>%
        html_text() %>% data.frame(rev.maintext = ., stringsAsFactors = F)

      # Pros
      #pg %>% html_nodes(".description .mt-md:nth-child(1)") %>%
      #pros <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0 > div:nth-child(6) > p:nth-child(2)") %>%
      pros <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0") %>%
        html_text() %>% data.frame(rev.pros = ., stringsAsFactors = F)

      # Cons
      #pg %>% html_nodes(".description .mt-md:nth-child(2)") %>%
      #cons <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0 > div:nth-child(7) > p:nth-child(2)") %>%
      #cons <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0") %>%
        #html_text() %>% data.frame(rev.cons = ., stringsAsFactors = F)

      # Advice
      #pg %>% html_nodes(".description") %>%
      #advice <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0 > div:nth-child(8) > p:nth-child(2)") %>%
      #advice <- rvw %>% html_node("div.row.mt") %>% html_node("div.col-sm-11.pl-sm-lg.mx-0") %>%
        #html_text() %>% data.frame(rev.advice = ., stringsAsFactors = F)
      
      # Combine columns in data frame
      #dfrow <- data.frame(reviewid, date, helpful, summary, rating, title, location, recommends, ceoapproves,maintext, pros, cons, advice)
      dfrow <- data.frame(reviewid, date, helpful, summary, rating, subratingtitle, worklifebalance, culturenvalues, careeroppor, compnbenefits, seniormgmt, title, location, recommends, maintext, pros)
      #dfrow <- data.frame(reviewid, date, title)
      
    })
    
  })
  
  # Close connection
  #Sys.sleep(2) #pause to let connection work
  #close.connection(pg)
  #gc()
  
}


# Loop through webpages and scrape data
dfgdreview <- data.frame()
dfgdreviews <- data.frame()
s1 <- startpage
while(endpage >= s1){
  e1 <- s1 + batchsize - 1 # Batchwise 
  e1 <- if (e1 <= endpage) e1 else endpage 
  
  # Call gdreviewsscrapper function and create Dataframe
  # Batch dataframe
  dfgdreview <- fngdreviewsscrapper(s1,e1) 

  # Export to csv - batchwise data
  write.csv(dfgdreview, paste("df-gds-results-",companyNum,"-P",s1,"-P",e1,".csv", sep=""), row.names = F)
  
  # Next Batch
  s1 <- e1 + 1
  
  # Overall dataframe
  dfgdreviews <- bind_rows(dfgdreviews, dfgdreview)
}

# Export to csv - overall data
write.csv(dfgdreviews, paste("df-gds-results-",companyNum,".csv", sep=""), row.names = F)
