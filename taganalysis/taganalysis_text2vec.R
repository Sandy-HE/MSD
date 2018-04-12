library(text2vec)  
library(data.table) 
library(stringr)
library(tibble)



#initial
alltags <- fread("./statwords.csv")
#use top40 frequent words calculated in the experiment before
tematrix <- fread("tagsemomatrix.csv")
topwords <- tematrix$V1

#divide into train set and test set with the proportion of 4:1
all_ids <- alltags$track_id 
train_ids <- sample(all_ids, nrow(alltags)/5*4)
test_ids <- setdiff(all_ids, train_ids) 

# The function of tags transform
#For one song, merge all of tags into one string with the format like this:
#[tag1]*popularity1+[tag2]*popularity2+...+[tagN]*popularityN
itemsMultiTrans <- function(items){
  itemlist <- tstrsplit(items, "\\*")
  itemdf <- as.data.frame(itemlist, stringsAsFactors = F,col.names = c("item","popularity"))
  if(!is.integer(itemdf$popularity)) print(itemdf)
  itemdf$multiItem <- strrep(paste0(itemdf$item," "),as.integer(itemdf$popularity) )
  itemdf <- itemdf[itemdf$popularity!=0,]
  
  itemsstr<-paste(itemdf$multiItem,collapse =" ")
  itemsstr
}

#==== process tags and merge into string ====
#lowercase-> split-> tags multipled with popularity and merge
obsv <- str_to_lower(alltags$tags)
obsv <- str_split(obsv,pattern = ";")      #list
obsv1 <- sapply(obsv, itemsMultiTrans)    #char vector

#data backup
alltags$transtags <- obsv1
fwrite(alltags,file = "alltranstags.csv")


#prepare train set and test set
train = alltags[alltags$track_id %in% train_ids,]  
test = alltags[alltags$track_id %in% test_ids,]  

#==== text2vec: generate term-co-occurance matrix ====
tok_fun <- word_tokenizer  
it_train <- itoken(train$transtags,   
                  #preprocessor = prep_fun,   
                  tokenizer = tok_fun,   
                  ids = train$track_id,   
                  progressbar = T)  

#set stop words and remove them from vocabulary
stop_words <- c("the","to","of","and","a","in","this","-","for","it","that","on","is")
system.time(vocab <- create_vocabulary(it_train,stopwords = stop_words))

#remove low-freq and high-freq words
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 10,   
                                doc_proportion_max = 0.5,  
                                doc_proportion_min = 0.001)  

#set up corpus vector
vectorizer <- vocab_vectorizer(pruned_vocab)

#generate DTM(doc-term matrix). Here we can understand it as song-term matrix
system.time(dtm_train <- create_dtm(it_train, vectorizer))


#==== generate TCM(term-co-occurance matrix). ====
system.time(tcm_train <- create_tcm(it_train, vectorizer, skip_grams_window = 5L,
           skip_grams_window_context = c("symmetric", "right", "left")))


#prune matrix only containing topN-freq tags 
tcm_sample <- tcm_train[rownames(tcm_train) %in% topwords,colnames(tcm_train) %in% topwords]
tcm_sample_mat <- as.matrix(tcm_sample)

tcm_sample_diag <- diag(diag(tcm_sample_mat))
tcm_sample_whole <- tcm_sample_mat + t(tcm_sample_mat) - tcm_sample_diag*2
tcm_sample_whole <- round(tcm_sample_whole,2)
tcm_sample_df <- as.data.frame(tcm_sample_whole)

tags <- rownames(tcm_sample_df)
tcm_sample_df<- cbind(tags,tcm_sample_df)
fwrite(tcm_sample_df, file = "tcm_sample_pruned_vocab.csv")

#==== visualization ====
library(igraph)
set.seed(10)
#sampling a small data size
samplemat <- tcm_sample_whole[1:10,1:10]
sampletags <- as.data.frame(rownames(samplemat))
colnames(sampletags) <- "tags"
sampledf <- graph.adjacency(samplemat,weighted=TRUE)
sampledf <- get.data.frame(sampledf)

#use the whole data size
topwordsnode <- as.data.frame(topwords,stringsAsFactors = F)
topwordsnode <- topwordsnode %>% rowid_to_column("id")
topwordsedge <- graph.adjacency(tcm_sample_whole,weighted=TRUE)
topwordsedge <- get.data.frame(topwordsedge)

#====igraph package usage ====
#This is one visualization style, but not what I want
#network <- graph_from_data_frame(d = sampledf, vertices = sampletags, directed = FALSE)
#plot(network,layout = layout_with_graphopt)
network <- graph_from_data_frame(d = topwordsedge, vertices = topwordsnode, directed = T)
plot(network, edge.arrow.size = 0.2,layout = layout_with_graphopt)
#==== End of igraph package usage ====

#==== network package usage ====
library(network)
tagsnetwork <- network(topwordsedge, vertex.attr = topwordsnode, matrix.type = "edgelist", ignore.eval = FALSE)
plot(tagsnetwork, vertex.cex = 3,mode = "circle")
#==== End of network package usage ====


#==== tidygraph and ggraph ====
library(tidygraph)
nettidy <- tbl_graph(nodes = sampletags, edges = sampledf, directed = T)
nettidy <- tbl_graph(nodes = topwordsnode, edges = topwordsedge, directed = T)
library(ggraph)
ggraph(nettidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  scale_edge_width(range = c(0.02, 0.1)) +
  geom_node_text(aes(label = topwords), repel = TRUE) +
  labs(edge_width = "correlation strength") +
  theme_graph()
#==== End of tidygraph and ggraph ====

#==== ggnetwork usage ====
library(ggplot2)
library(ggnetwork)
ggplot(topwordsedge, aes(from, to)) +
  geom_edges(aes(color = weight)) +
  geom_nodes(color = "grey50") +
  geom_nodelabel(aes(size = weight,label=from),
                 color = "grey20") +
  scale_size_continuous(range = c(2, 8)) +
  scale_color_gradient2(low = "grey25", midpoint = 0.75, high = "black") +
  guides(size = FALSE, color = FALSE) + 
  theme_blank()


