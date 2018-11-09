library(data.table) 

##initial
alltags <- fread("./statwords.csv")


#==== preliminary processing====

##The function of tags transform
##For one song, merge all of tags into one string with the format like this:
##[tag1]*popularity1+[tag2]*popularity2+...+[tagN]*popularityN
##e.g  "happy*3;party*2" will be transformed into "happy happy happy party party"
itemsMultiTrans <- function(items){
  itemlist <- tstrsplit(items, "\\*")
  itemdf <- as.data.frame(itemlist, stringsAsFactors = F,col.names = c("item","popularity"))
  if(!is.integer(itemdf$popularity)) print(itemdf)
  #for single word
  #itemdf$multiItem <- strrep(paste0(itemdf$item," "),as.integer(itemdf$popularity) )
  #for phrase
  itemdf$multiItem <- strrep(paste0(itemdf$item,";"),as.integer(itemdf$popularity) )
  itemdf <- itemdf[itemdf$popularity!=0,]
  
  itemsstr<-paste(itemdf$multiItem,collapse =" ")
  itemsstr
}

##=== process tags and merge into string ====
#lowercase-> split-> tags multipled with popularity and merge
obsv <- str_to_lower(alltags$tags)
obsv <- str_split(obsv,pattern = ";")      #list
obsv1 <- sapply(obsv, itemsMultiTrans)    #char vector

##data backup
alltags$transtags <- obsv1
fwrite(alltags,file = "alltranstags_phrase.csv")



#==== tags analysis====

##initial 
train <- fread("alltranstags_phrase.csv")

##use space tokenizer for pharase analysis by separating items through ";"
tokens <- space_tokenizer(trimws(train$transtags), sep=";")
it_train <- itoken(tokens, 
                   ids = train$track_id,   
                   progressbar = T) 

##create vocabulary
##set stop words and remove them from vocabulary
##use 'snowball' package and remove negative word
stop_words<- stopwords(language = "en", source = "snowball")
stop_words<-stop_words[-81:-98]
stop_words<-stop_words[-147:-149]
#stop_words2<- stopwords(language = "en", source = "smart")
system.time(vocab <- create_vocabulary(it_train,stopwords = stop_words))

##remove low-freq and high-freq words
pruned_vocab = prune_vocabulary(vocab,   
                                #term_count_min = 2,   
                                doc_proportion_max = 0.8,  
                                doc_proportion_min = 0.005)  

##set up corpus vector
vectorizer <- vocab_vectorizer(pruned_vocab)

##generate DTM(doc-term matrix). Here we can understand it as song-term matrix
system.time(dtm_train <- create_dtm(it_train, vectorizer))


##generate TCM(term-co-occurrence matrix).
system.time(tcm_train <- create_tcm(it_train, vectorizer, skip_grams_window = 5L,
                                    skip_grams_window_context = c("symmetric", "right", "left")))
