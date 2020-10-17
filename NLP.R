#companyNum <- "xxxxxx"
companyNum <- "xxxxxx"

# Import dataset
dataset_gd = read.csv(paste('C:/Users/xxxxxx/xxxxxx/GD-Reviews/df-gds-results-', companyNum,'.csv', sep=""))

library(stringr)
library(stopwords)
library(tidytext)
library(tm)
library(tokenizers)

# Cleanup Columns
# Date
#dataset_gd$rev.date <- as.Date(strptime(dataset_gd$rev.date,"%B %d, %Y"))
dataset_gd$rev.date <- dataset_gd$rev.date

# Helpful
#dataset_gd$rev.helpf <- as.character(dataset_gd$rev.helpf)
dataset_gd$rev.helpf <- str_replace_all(dataset_gd$rev.helpf,"Helpful","")
dataset_gd$rev.helpf <- str_replace_all(dataset_gd$rev.helpf,"\\(","")
dataset_gd$rev.helpf <- str_replace_all(dataset_gd$rev.helpf,"\\)","")
dataset_gd$rev.helpf <- as.numeric(as.character(dataset_gd$rev.helpf))
dataset_gd$rev.helpf[is.na(dataset_gd$rev.helpf)] <- 0

# Summary
dataset_gd$rev.sum <- as.character(dataset_gd$rev.sum)
dataset_gd$rev.sum <- str_replace_all(dataset_gd$rev.sum,"\"","")
dataset_gd$rev.sum[str_length(dataset_gd$rev.sum)==0] <-"Not Available"

# Rating
# No changes for Rating
dataset_gd$rev.rating <- dataset_gd$rev.rating

# Subrating
# No changes for SubRating
dataset_gd$rev.worklifebalance <- dataset_gd$rev.worklifebalance
dataset_gd$rev.culturenvalues <- dataset_gd$rev.culturenvalues
dataset_gd$rev.careeroppor <- dataset_gd$rev.careeroppor
dataset_gd$rev.compnbenefits <- dataset_gd$rev.compnbenefits
dataset_gd$rev.seniormgmt <- dataset_gd$rev.seniormgmt

# Title
# Emp Status
dataset_gd$emp.status[str_detect(dataset_gd$rev.title,"Current Employee",negate=F)] <- "Current Employee"
dataset_gd$emp.status[str_detect(dataset_gd$rev.title,"Former Employee",negate=F)] <- "Former Employee"
dataset_gd$emp.status[is.na(dataset_gd$emp.status)] <- "Not Available"

# Emp Title
dataset_gd$emp.title <- str_trim(dataset_gd$rev.title)
dataset_gd$emp.title <- str_replace(dataset_gd$emp.title,dataset_gd$emp.status,"")
dataset_gd$emp.title <- str_replace(dataset_gd$emp.title," - ","")
st_en_tt <- str_locate(dataset_gd$emp.title, "in ")
dataset_gd$emp.title  <- str_trim(str_sub(dataset_gd$emp.title, start = 0, end = ifelse(is.na(st_en_tt[,1]) , str_length(dataset_gd$emp.title), st_en_tt[,1]-2)))
dataset_gd$emp.title[str_length(dataset_gd$emp.title)==0] <- "Not Available"

# Emp Location
dataset_gd$emp.location <- str_trim(dataset_gd$rev.title)
dataset_gd$emp.location <- str_replace(dataset_gd$emp.location,dataset_gd$emp.status,"")
dataset_gd$emp.location <- str_replace(dataset_gd$emp.location,dataset_gd$emp.title,"")
dataset_gd$emp.location <- str_replace(dataset_gd$emp.location," - ","")
dataset_gd$emp.location <- str_trim(str_replace(dataset_gd$emp.location,"in ",""))
# dataset_gd$emp.location <- str_trim(dataset_gd$rev.location)
dataset_gd$emp.location[dataset_gd$emp.location == ""] <- "Not Available"

# Recommends
dataset_gd$Bus.recommends <- "Not Available"
dataset_gd$Bus.recommends[str_detect(dataset_gd$rev.recommends,"Recommends",negate=F)] <- "Recommends"
dataset_gd$Bus.recommends[str_detect(dataset_gd$rev.recommends,"Doesn't Recommend",negate=F)] <- "Doesn't Recommend"

# Outlook
dataset_gd$Bus.outlook <- "Not Available"
dataset_gd$rev.outlook <- dataset_gd$rev.recommends
dataset_gd$Bus.outlook[str_detect(dataset_gd$rev.outlook,"Positive Outlook",negate=F)] <- "Positive Outlook"
dataset_gd$Bus.outlook[str_detect(dataset_gd$rev.outlook,"Neutral Outlook",negate=F)] <- "Neutral Outlook"
dataset_gd$Bus.outlook[str_detect(dataset_gd$rev.outlook,"Negative Outlook",negate=F)] <- "Negative Outlook"

# CEO Approves
dataset_gd$Bus.ceoapproves <-"Not Available"
dataset_gd$rev.ceoapproves <- dataset_gd$rev.recommends
dataset_gd$Bus.ceoapproves[str_detect(dataset_gd$rev.ceoapproves,"Approves of CEO",negate=F)] <- "Approves of CEO"
dataset_gd$Bus.ceoapproves[str_detect(dataset_gd$rev.ceoapproves,"No opinion of CEO",negate=F)] <- "No opinion of CEO"
dataset_gd$Bus.ceoapproves[str_detect(dataset_gd$rev.ceoapproves,"Disapproves of CEO",negate=F)] <- "Disapproves of CEO"

# Stopwords
sw_list <- stopwords("en")
sw_rexp <- paste(unlist(sw_list), collapse=" | ")
print(sw_rexp)

# pros
dataset_gd$Bus.pros <- str_trim(as.character(dataset_gd$rev.pros))
st_en_pr <- str_locate(dataset_gd$Bus.pros, "Pros")
st_pr <- ifelse(is.na(st_en_pr[,1]) , str_length(dataset_gd$Bus.pros), st_en_pr[,2]+1)
st_en_cn <- str_locate(dataset_gd$Bus.pros, "Cons")
dataset_gd$Bus.pros  <- str_trim(str_sub(dataset_gd$Bus.pros, start = st_pr, end = st_en_cn[,1]-1))
dataset_gd$Bus.pros <- str_replace(dataset_gd$Bus.pros,"Pros","")

dataset_gd$Bus.pros_cln <- tolower(dataset_gd$Bus.pros) #to lower case
dataset_gd$Bus.pros_cln <- str_replace_all(dataset_gd$Bus.pros_cln, "[^A-Za-z, ]"," ") #special characters
dataset_gd$Bus.pros_cln <- str_replace_all(dataset_gd$Bus.pros_cln, sw_rexp," ") #stopwords
dataset_gd$Bus.pros_cln <- str_replace(gsub("\\s+", " ", str_trim(dataset_gd$Bus.pros_cln)),"A","a") #extra spaces
dataset_gd$Bus.pros_cln[str_length(dataset_gd$Bus.pros_cln)==0] <- ""

# Cons
dataset_gd$Bus.cons <- str_trim(as.character(dataset_gd$rev.pros))
st_en_cn <- str_locate(dataset_gd$Bus.cons, "Cons")
st_cn <- ifelse(is.na(st_en_cn[,1]) , str_length(dataset_gd$Bus.cons), st_en_cn[,2]+1)
st_en_adv <- str_locate(dataset_gd$Bus.cons, "Advice to Management")
st_en_shr <- str_locate(dataset_gd$Bus.cons, "Share on Facebook")
dataset_gd$Bus.cons  <- str_trim(str_sub(dataset_gd$Bus.cons, start = st_cn, end = ifelse(is.na(st_en_adv[,1]), st_en_shr[,1]-1, st_en_adv[,1]-1)))
dataset_gd$Bus.cons <- str_replace(dataset_gd$Bus.cons,"Cons","")

dataset_gd$Bus.cons_cln <- tolower(paste(dataset_gd$Bus.cons," ")) #to lower case
dataset_gd$Bus.cons_cln <- str_replace_all(dataset_gd$Bus.cons_cln, "[^a-zA-Z]"," ") #special characters
dataset_gd$Bus.cons_cln <- str_replace_all(dataset_gd$Bus.cons_cln, sw_rexp," ") #stopwords
dataset_gd$Bus.cons_cln <- str_replace(gsub("\\s+", " ", str_trim(dataset_gd$Bus.cons_cln)),"A","a") #extra spaces
dataset_gd$Bus.cons_cln[str_length(dataset_gd$Bus.cons_cln)==0] <- ""

# Advice
dataset_gd$Bus.advice <- str_trim(as.character(dataset_gd$rev.pros))
st_en_adv <- str_locate(dataset_gd$Bus.advice, "Advice to Management")
st_adv <- ifelse(is.na(st_en_adv[,1]) , str_length(dataset_gd$Bus.advice), st_en_adv[,2]+1)
st_en_shr <- str_locate(dataset_gd$Bus.advice, "Share on Facebook")
dataset_gd$Bus.advice  <- str_trim(str_sub(dataset_gd$Bus.advice, start = st_adv, end = st_en_shr[,1]-1))
dataset_gd$Bus.advice <- str_replace(dataset_gd$Bus.advice,"Advice to Management","")

dataset_gd$Bus.advice_cln <- tolower(paste(dataset_gd$Bus.advice," ")) #to lower case
dataset_gd$Bus.advice_cln <- str_replace_all(dataset_gd$Bus.advice_cln, "[^a-zA-Z]"," ") #special characters
dataset_gd$Bus.advice_cln <- str_replace_all(dataset_gd$Bus.advice_cln, sw_rexp," ") #stopwords
dataset_gd$Bus.advice_cln <- str_replace(gsub("\\s+", " ", str_trim(dataset_gd$Bus.advice_cln)),"A","a") #extra spaces
dataset_gd$Bus.advice_cln[str_length(dataset_gd$Bus.advice_cln)==0] <- ""

# Sentiment Analysis
#library(tidytext)
library(sentimentr)
library(dplyr)
library(magrittr)
library(stringr)
library(xtable)
library(zoo)

get_sentiments(lexicon = "nrc")%>%
  count(sentiment, sort = TRUE)

# Function to convert list of character vector to dataframe
fn_list2df <- function(lcv) {
  id <- index(lcv)
  len <- lengths(lcv)
  df <- data.frame()
  for(i in id)
    for(l in 1:len[i])
      df <- bind_rows(df, data.frame(element_id=i, sentence_id=l, text=lcv[[i]][l], stringsAsFactors=F))
  df
}

# pros
pros_sentiment <- 
  as.character(dataset_gd$Bus.pros) %>% 
  get_sentences() %>%
  sentiment_by(by=NULL) 
pros_Highlight <-
  as.character(dataset_gd$Bus.pros) %>% 
  get_sentences() %>%
  sentiment_by(by=NULL) %>%
  highlight()

# pros Sentencewise Sentiment
pros_sentencewise_sentiment <-
  as.character(dataset_gd$Bus.pros) %>% 
  get_sentences() %>%
  fn_list2df()
pros_sentencewise_sentiment$word_count <- sentiment_by(get_sentences(pros_sentencewise_sentiment$text),by=NULL)$word_count
pros_sentencewise_sentiment$ave_sentiment <- round(sentiment_by(get_sentences(pros_sentencewise_sentiment$text),by=NULL)$ave_sentiment,2)


# Cons
cons_sentiment <- 
  as.character(dataset_gd$Bus.cons) %>% 
  get_sentences() %>%
  sentiment_by(by=NULL) 
cons_Highlight <-
  as.character(dataset_gd$Bus.cons) %>% 
  get_sentences() %>%
  sentiment_by(by=NULL) %>%
  highlight()

# cons Sentencewise Sentiment
cons_sentencewise_sentiment <-
  as.character(dataset_gd$Bus.cons) %>% 
  get_sentences() %>%
  fn_list2df()
cons_sentencewise_sentiment$word_count <- sentiment_by(get_sentences(cons_sentencewise_sentiment$text),by=NULL)$word_count
cons_sentencewise_sentiment$ave_sentiment <- round(sentiment_by(get_sentences(cons_sentencewise_sentiment$text),by=NULL)$ave_sentiment,2)


# Advice
advice_sentiment <- 
  as.character(dataset_gd$Bus.advice) %>% 
  get_sentences() %>%
  sentiment_by(by=NULL) 
advice_Highlight <-
  as.character(dataset_gd$Bus.advice) %>% 
  get_sentences() %>%
  sentiment_by(by=NULL) %>%
  highlight()

# advice Sentencewise Sentiment
advice_sentencewise_sentiment <-
  as.character(dataset_gd$Bus.advice) %>% 
  get_sentences() %>%
  fn_list2df()
advice_sentencewise_sentiment$word_count <- sentiment_by(get_sentences(advice_sentencewise_sentiment$text),by=NULL)$word_count
advice_sentencewise_sentiment$ave_sentiment <- round(sentiment_by(get_sentences(advice_sentencewise_sentiment$text),by=NULL)$ave_sentiment,2)

# Ave Sentiment - Round to 2 decimal places
dataset_gd$pros_sa_Ave_Sentiment <- round(pros_sentiment$ave_sentiment,2)
dataset_gd$cons_sa_Ave_Sentiment <- round(cons_sentiment$ave_sentiment,2)
dataset_gd$advice_sa_Ave_Sentiment <- round(advice_sentiment$ave_sentiment,2)

# Export to csv
write.csv(dataset_gd, paste('dataset_gd_', companyNum,'.csv', sep=""), row.names = F)
write.csv(pros_sentencewise_sentiment, paste('dataset_gd_', companyNum,'_sa_pros.csv', sep=""), row.names = F)
write.csv(cons_sentencewise_sentiment, paste('dataset_gd_', companyNum,'_sa_cons.csv', sep=""), row.names = F)
write.csv(advice_sentencewise_sentiment, paste('dataset_gd_', companyNum,'_sa_advice.csv', sep=""), row.names = F)
