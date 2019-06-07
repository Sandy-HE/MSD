library(text2vec)  
library(data.table) 
library(stringr)
library(tibble)
library(stopwords)
library(dplyr)
library(ggplot2)


##initial
alltags <- fread("alltranstags_phrase.csv")
train <- fread("alltranstags_phrase.csv")

##prepare train set and test set
##divide into train set and test set with the proportion of 4:1
#all_ids <- alltags$track_id 
#train_ids <- sample(all_ids, nrow(alltags)/5*4)
#test_ids <- setdiff(all_ids, train_ids) 
#train = alltags[alltags$track_id %in% train_ids,]  
#test = alltags[alltags$track_id %in% test_ids,]  

#==== text2vec: generate term-co-occurrence matrix ====
##use word_tokenizer for single word analysis
#tok_fun <- word_tokenizer
#it_train <- itoken(train$transtags,   
#                  preprocessor = prep_fun,   
#                  tokenizer = tok_fun,   
#                  ids = train$track_id,   
#                  progressbar = T)  

##use space tokenizer for pharase analysis by separating items through ";"
tokens <- space_tokenizer(train$transtags, sep=";")
it_train <- itoken(tokens, 
                   ids = train$track_id,   
                   progressbar = T) 

##set stop words and remove them from vocabulary
#stop_words <- c("the","to","of","and","a","in","this","-","for","it","that","on","is")
stop_words<- stopwords(language = "en", source = "snowball")
stop_words<-stop_words[-81:-98]
stop_words<-stop_words[-147:-149]
#stop_words2<- stopwords(language = "en", source = "smart")
system.time(vocab <- create_vocabulary(it_train ,stopwords = stop_words))

##remove low-freq and high-freq words
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 1000,   
                                doc_proportion_max = 0.8,  
                                doc_proportion_min = 0.0002)  

##set up corpus vector
vectorizer <- vocab_vectorizer(pruned_vocab)

##generate DTM(doc-term matrix). Here we can understand it as song-term matrix
system.time(dtm_train <- create_dtm(it_train, vectorizer))


#==== generate TCM(term-co-occurrence matrix). ====
system.time(tcm_train <- create_tcm(it_train, vectorizer, skip_grams_window = 5L,
                                    skip_grams_window_context = c("symmetric","right","left")))

#dtm_train_l1_norm = normalize(dtm_train, "l1")

##check the order of documents in both dtm and original dataset
#identical(rownames(dtm_train), train$track_id) 


#====LSA model====
#tfidf = TfIdf$new()
#lsa = LSA$new(n_topics = 10)
#track_embedding = dtm_train %>% 
#  fit_transform(tfidf) %>% 
#  fit_transform(lsa)
#temp <- lsa$components

#====LDA model====
#In LDA model, when setting topic number is 10, it looks good in 4 quadrants.
#But when setting topic number is 20 or 50, it becomes worse and worse. 
#Most topics tend to cluster to one position

#lda_model = LDA$new(n_topics = 30, doc_topic_prior = 0.1, topic_word_prior = 0.01)
#track_topic_distr = 
#  lda_model$fit_transform(x = dtm_train, n_iter = 1000, 
#                          convergence_tol = 0.001, n_check_convergence = 25, 
#                          progressbar = FALSE)
#library(LDAvis)
#lda_model$plot()

#temp <- lda_model$get_top_words(n = 30, topic_number = c(1L, 5L, 10L), lambda = 0.6)
#temp <- lda_model$get_top_words(n = 10, lambda = 0.6)
#topic50_top30 <- as.data.frame(temp, stringsAsFactors = FALSE)


#==These steps saves visualization to dir
#DIR = "LDAvis_topic50"
#lda_model$plot(out.dir = DIR)
#temp1<-lda_model$topic_word_distribution
#topic50_term_matrix <- as.data.frame(temp1, stringsAsFactors = FALSE)
#====The end of LDA model====

#====Glove model===
#This model will use tcm as input, output is word vectors
#You can adjust vector size
glove = GlobalVectors$new(word_vectors_size = 150, vocabulary = pruned_vocab, x_max = 10, learning_rate=0.15)
wv_main = glove$fit_transform(tcm_train, n_iter = 25, convergence_tol = 0.005)
wv_context = glove$components
word_vectors = wv_main + t(wv_context)


#Save word vectors if neccesary, because when you change vector size, the word vector is changed time by time. 
#If you want to get stable result. Just save your result.
wordvec_df <- as.data.frame(word_vectors)
wordvec_df$term <- rownames(word_vectors)
#fwrite(as.data.frame(word_vectors), "wordvec_<size>.csv")
fwrite(wordvec_df, "wordvec_7685_new_vec150.csv")


#one test case for checking validation of word vector 
#temp2 <- word_vectors["happy", , drop = FALSE]
#cos_sim = sim2(x = word_vectors, y = temp2, method = "cosine", norm = "l2")
#head(sort(cos_sim[,1], decreasing = TRUE),20)
#====The end of Glove model====

#====calculate word-word similarity====
#1.get subset of word vectors based on emotion terms list
eterms_df <- fread("./emotion_terms_list.csv")

#option1: if use glove processing result directly
#@wordvec_df <- as.data.frame(word_vectors)
#@wordvec_df$term <- rownames(wordvec_df)
#option2: read repository data
#@wordvec_df <- as.data.frame(fread("wordvec_7685_new.csv"))

#====merge some same meaning terms====
#Solution1: merge terms, then get sub word vectors
termassimilation <- function(item, df){
  termkey <- item[1]
  termset <- item[2]
  df$term[str_detect(df$term, termset)] <- termkey
  df <- aggregate(df[,-151], by=list(term=df$term), mean)
  df[df$term %in% termkey,]
}

system.time(result <- apply(eterms_df,1,termassimilation, df=wordvec_df))  #a list of data frames
a <- bind_rows(result)
rownames(a) <- a$term
a <- a[,-1]
sub_wordvec <- as.matrix(a)

#Save merged result for future use
#a$term <- rownames(a)
#fwrite(a, file="7685_merge_result_100terms_150vecsize.csv")

#Get the ready-use data
#a<-as.data.frame(fread("7685_merge_result_101terms.csv"))
#rownames(a) <- a$term
#please check which column is term, then specify the column number
#a <- a[,-1]
#sub_wordvec <- as.matrix(a)


#Solution2: not merge terms, get sub word vectors
#rownames(wordvec_df)<-wordvec_df$term
#word_vectors <- as.matrix(wordvec_df[,-4])
#sub_wordvec <- subset(word_vectors, rownames(word_vectors) %in% eterms_df$term)

#2.calculate pairwise-rows cosine similarity and generate a similarity matrix
termsim_mt <-lsa::cosine(t(sub_wordvec))
#====The end of calculating similarity====

#====MDS model====
#In MDS model, we use dissimilarity instead of similarity.
#When I use similarity in the begining, the model result is 0or1 dim. It is meaningless.
mds_terms.names = rownames(sub_wordvec)

#get dissimilarity
termdis_mt <- max(termsim_mt)-termsim_mt

#@termdis_df<- as.data.frame(termdis_mt)
#@fwrite(termdis_df, "./termdisdata.csv")
#MDS model, K is the dimension of target matrix
#=====MDS model1====
#@@@@@
system.time(mds_terms<-cmdscale(termdis_mt, k=3,eig = T))

#R square validation
plot(cumsum(mds_terms$eig) / sum(mds_terms$eig),
     type="h", lwd=5, las=1,
     xlab="Number of dimensions",
     ylab=expression(R^2))
plot(mds_terms$eig,
     type="h", lwd=5, las=1,
     xlab="Number of dimensions",
     ylab="Eigenvalues")
mds_terms_new<- mds_terms$points
mdsnew_sim <-lsa::cosine(t(mds_terms_new))
mdsnew_dis <- max(mdsnew_sim)-mdsnew_sim
r <- cor(c(termdis_mt),c(mdsnew_dis))
rsquare <- r*r


#Visualization 
mdsterm_df <- as.data.frame(mds_terms$points)
#@@@@visualization style1
plot(mdsterm_df$V1,mdsterm_df$V2,type='p',pch=16,col="grey")
text(mdsterm_df$V1,mdsterm_df$V2,mds_terms.names,adj=c(0,1),cex=.7)

#@@@@visualization style2
#mds_terms.names <- rownames(mdsterm_df)
countdf <- pruned_vocab[pruned_vocab$term %in% mds_terms.names, c("term","term_count") ]
submds <- mdsterm_df[rownames(mdsterm_df) %in% countdf$term,]
#countdf$term_count <- scale(log(countdf$term_count))
countdf$term <- factor(countdf$term,levels = mds_terms.names)
countdf<- countdf[order(countdf$term),]
temp2 <- cbind(submds,countdf)
ggplot(temp2, aes(x=MDS1,y=MDS2, size=factor(term_count),color="black",label=term))+
  geom_point(color="gray",size=2)+
  geom_text(color="blue")
#mds_terms.size <- as.factor(temp$term_count)

#====MDS model 2====
#library(vegan)
mds_model<-metaMDS(termdis_mt, k=3)
stressplot(mds_model,termdis_mt)
mds_terms <- mds_model$points

plot(mds_terms, type= 'n')
text(mds_terms,mds_terms.names,cex=.7)


#====MDS model 3====
library(MASS)

mds_model <- isoMDS(termdis_mt, k=3)
mds_terms <- mds_model$points
plot(mds_terms, type= 'n')
mds_terms.names <- rownames(mds_terms)
text(mds_terms,mds_terms.names,cex=.7)
#====MDS model 4====
#Now it does not work
#mds_model <- wcmdscale(termdis_mt, k=3)
#stressplot(mds_terms)

#====procrustes analysis====
library(vegan)
library("plotrix")

#====scale classical coordinate data====
#If we do not run this step, the plots in rotated Y is very closed to each other.
#To let rotatedY be sparse in the [-1,1] circle, we can scale X
#And use scaledX as targetX to do procruste analysis with Y, 
#then the rotated Y is sparsed better.

#This coordinate value is obtained based on scherer's 2D model(2005)
#I use matlab to get these value.
scherer_cord = as.data.frame(fread("scherer_emotion_coord.csv"))
cord_min = min(min(scherer_cord$x),min(scherer_cord$y))
cord_max = max(max(scherer_cord$x),max(scherer_cord$y))
scherer_cord$norm1_x = (scherer_cord$x)/230
scherer_cord$norm1_y = (scherer_cord$y)/230
rownames(scherer_cord) <- scherer_cord$term

scherer_cord_new <- scherer_cord[(rownames(scherer_cord) %in% rownames(sub_wordvec)),]

#scherer_cord_new1 is scaledX, scherer_cord_new2 is not scaled. 
#You can compare the procrustes results.
scherer_cord_new1 <- scale(scherer_cord_new[,-c(1,2,3)])
scherer_cord_new2 <- as.matrix(scherer_cord_new[,-c(1,2,3)])
rownames(mdsterm_df) <- mds_terms.names
target <- mdsterm_df[rownames(scherer_cord_new),]
target_ex <- mdsterm_df[setdiff(rownames(sub_wordvec),rownames(scherer_cord_new)),]

#fwrite(target, file = "Y.csv")
#fwrite(target_ex, file = "other.csv")
#fwrite(scherer_cord_new[,-c(1,2,3)], file = "X.csv")

#X is the classical model, Y is my model
proc <- procrustes(scherer_cord_new1, target)
summary(proc)
max(proc$Yrot)
temp <- proc$Yrot
proc$Yrot <- scale(temp,scale = FALSE)
## S3 method for class 'procrustes'
plot(proc,kind = 0)
plot(c(-1.1,1.1), c(-1.1,1.1), type='n', asp=1,main = "Emotion Dimensional Model")
draw.circle(0, 0, 1, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)
arrows(c(-1.1,0),c(0,-1.1),c(1.1,0),c(0,1.1), length=0.1)

points(proc$Yrot,pch=16, col="blue")
text(proc$Yrot,adj=c(0,1),rownames(proc$Yrot), cex = .7)
newdata <- predict(proc,target_ex)
#newdata <- scale(newdata,scale = FALSE)
points(newdata, pch=8, col="green")
text(newdata,adj=c(0,1),rownames(newdata), cex=.7)
points(scherer_cord_new2,pch=1, col="red")
arrows(proc$Yrot[,1],proc$Yrot[,2], scherer_cord_new2[,1],scherer_cord_new2[,2],length=0.1,col="gray")
text(scherer_cord_new[,c(4,5)],adj=c(0,1), rownames(proc$Yrot), cex=.7, col= "gray")

text(x=0.22,y=1.1, "Arousal", font=2)
text(x=1.2,y=0.1, "Valence", font=2)
text(x=1.15,y=-0.05, "positive" , cex=.7, color="grey", font=3)
text(x=-1.15,y=-0.05, "negative" , cex=.7, color="grey", font=3)
text(x=-0.15,y=1.05, "active" , cex=.7, color="grey", font=3)
text(x=-0.15,y=-1.05, "inactive" , cex=.7, color="grey", font=3)

