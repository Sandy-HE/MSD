library(text2vec)  
library(data.table) 
library(stringr)
library(tibble)



##initial
alltags <- fread("./statwords.csv")
#use top40 frequent words calculated in the experiment before
tematrix <- fread("tagsemomatrix_29.csv")
topwords <- tematrix$V1

##divide into train set and test set with the proportion of 4:1
all_ids <- alltags$track_id 
train_ids <- sample(all_ids, nrow(alltags)/5*4)
test_ids <- setdiff(all_ids, train_ids) 

##The function of tags transform
##For one song, merge all of tags into one string with the format like this:
##[tag1]*popularity1+[tag2]*popularity2+...+[tagN]*popularityN
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

#==== process tags and merge into string ====
#lowercase-> split-> tags multipled with popularity and merge
obsv <- str_to_lower(alltags$tags)
obsv <- str_split(obsv,pattern = ";")      #list
obsv1 <- sapply(obsv, itemsMultiTrans)    #char vector

##data backup
alltags$transtags <- obsv1
fwrite(alltags,file = "alltranstags_phrase.csv")


##prepare train set and test set
train = alltags[alltags$track_id %in% train_ids,]  
test = alltags[alltags$track_id %in% test_ids,]  

#==== text2vec: generate term-co-occurance matrix ====
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
stop_words <- c("the","to","of","and","a","in","this","-","for","it","that","on","is")
system.time(vocab <- create_vocabulary(it_train,stopwords = stop_words))

##remove low-freq and high-freq words
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 10,   
                                doc_proportion_max = 0.5,  
                                doc_proportion_min = 0.001)  

##set up corpus vector
vectorizer <- vocab_vectorizer(pruned_vocab)

##generate DTM(doc-term matrix). Here we can understand it as song-term matrix
system.time(dtm_train <- create_dtm(it_train, vectorizer))


#==== generate TCM(term-co-occurance matrix). ====
system.time(tcm_train <- create_tcm(it_train, vectorizer, skip_grams_window = 5L,
           skip_grams_window_context = c("symmetric", "right", "left")))


##prune matrix only containing topN-freq tags,'topwords' dataset determines which phrases are kept
tcm_sample <- tcm_train[rownames(tcm_train) %in% topwords,colnames(tcm_train) %in% topwords]
tcm_sample_mat <- as.matrix(tcm_sample)
##for whole train set
#tcm_sample_mat <- as.matrix(tcm_train)

##clean and normalize the term-cooccurrance matrix
tcm_sample_diag <- diag(diag(tcm_sample_mat))
#tcm_sample_whole <- tcm_sample_mat + t(tcm_sample_mat) - tcm_sample_diag*2
tcm_sample_whole <- tcm_sample_mat - tcm_sample_diag
tcm_sample_whole <- round(tcm_sample_whole,2)   # this is matrix
words <- rownames(tcm_sample_whole)
words <- order(words)
tcm_sample_whole <- tcm_sample_whole[words,words]

##shrink the whole weight scale
#tcm_sample_whole_log <- log(tcm_sample_whole+1)
#tcm_sample_whole <- round(tcm_sample_whole_log,2)

##reduce edges for simplified network visualization
tcm_sample_whole_interm <- ifelse(tcm_sample_whole==0, NA, tcm_sample_whole)
md <- median(tcm_sample_whole_interm, na.rm = T)
tcm_sample_less_edges <- ifelse(tcm_sample_whole<md, 0, tcm_sample_whole)

##data backup
#tcm_sample_df <- as.data.frame(tcm_sample_whole, stringsAsFactors = F)
#tags <- rownames(tcm_sample_df)
#tcm_sample_df<- cbind(tags,tcm_sample_df)
#fwrite(tcm_sample_df, file = "tcm_sample_pruned_vocab.csv"

#==== visualization ====
#set.seed(10)

##build up nodes and edges for the whole sample data or less-edges data
topwordsnode <- as.data.frame(rownames(tcm_sample_whole),stringsAsFactors = FALSE)
#topwordsnode <- as.data.frame(rownames(tcm_sample_less_edges),stringsAsFactors = FALSE)
colnames(topwordsnode) <- "tags"
topwordsnode$id <- rownames(topwordsnode)
topwordsedge <- graph.adjacency(tcm_sample_whole,weighted=TRUE)
#topwordsedge <- graph.adjacency(tcm_sample_less_edges,weighted=TRUE)
topwordsedge <- get.data.frame(topwordsedge)

##backup data
fwrite(topwordsnode, file="items_node.csv")
fwrite(topwordsedge, file="items_edge.csv")

#====igraph package usage ====
##This is one visualization style
library(igraph)
igrahnet <- graph_from_data_frame(d = topwordsedge, vertices = topwordsnode, directed = F)

##show style1
#change parameter 'mfrow' to add more layout graph in one view
par(mfrow=c(1,3), mar=c(0,0,0,0))
#typical force-directed graph
plot(igrahnet, layout=layout_with_fr)
plot(igrahnet, layout=layout_with_kk)
plot(igrahnet, layout=layout_with_drl)
#force-directed graph, but seems not to be typical
plot(igrahnet, layout=layout_with_graphopt)
plot(igrahnet, layout=layout_with_gem)
#other layout style for your reference
plot(igrahnet, layout=layout_with_lgl)
plot(igrahnet, layout=layout_with_mds)
plot(igrahnet, layout=layout_with_dh)

#show style2
#l <-layout_with_fr(igrahnet)
#l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
#rescale plot graph
#plot(igrahnet,rescale=FALSE,layout=l*1.5)
#plot(igrahnet,layout=l)

#plot(igrahnet, edge.arrow.size = 0.2,niter=200, layout = layout_with_graphopt)
#==== End of igraph package usage ====


#==== tidygraph and ggraph ====
library(tidygraph)
library(ggraph)
nettidy <- tbl_graph(nodes = topwordsnode, edges = topwordsedge, directed = T)

#here we can set layout to drl/kk/fr/..., similar to igraph
ggraph(nettidy, layout = "drl") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  scale_edge_width(range = c(0.02, 0.1)) +
  geom_node_text(aes(label = tags), repel = TRUE) +
  labs(edge_width = "correlation strength") +
  theme_graph()
#==== End of tidygraph and ggraph ====

#==== network package usage ====
library(network)

##build up tagsnetwork
tagsnetwork <- network(topwordsedge, vertex.attr = topwordsnode, matrix.type="edgelist",loops=F, multiple=F, ignore.eval = F)

##define some attributes for edge and vertex
set.edge.attribute(tagsnetwork, "weight", topwordsedge$weight)
t <- as.edgelist(tagsnetwork, attrname = "weight")%>%
  as.tnet %>%
  degree_w
set.vertex.attribute(tagsnetwork, "degree_w", t[, "output" ])

l <- tagsnetwork %v% "degree_w"

l <- network.vertex.names(tagsnetwork)
set.vertex.attribute(tagsnetwork, "label", l)
#tagsnetwork <- network(tcm_sample_whole, matrix.type="adjacency")
#plot(tagsnetwork, vertex.cex = 3,mode = "circle")
#==== End of network package usage ====

#==== ggnetwork usage ====
library(ggplot2)
library(ggnetwork)
library(intergraph)
library(tnet)

#reuse tagsnetwork
#layout could be fruchtermanreingold/kamadakawai/circle
#gn <- ggnetwork(tagsnetwork)
gn <- ggnetwork(tagsnetwork, layout = "fruchtermanreingold", cell.jitter = 0.75)
gn <- ggnetwork(tagsnetwork, layout = "target", niter = 100)
ggplot(gn, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(aes(color = log(weight))) +
  geom_nodes(color = "grey50") +
  geom_nodelabel(aes(label=label,size = degree_w),
                 color = "grey20",label.size = NA) +
  scale_size_continuous(range = c(2, 8)) +
  scale_color_gradient2(low = "grey25", midpoint = 0.75, high = "black") +
  guides(size = FALSE, color = FALSE) + 
  theme_blank()

#==== ndtv: dynamic network visualization ====
library("ndtv")

#reuse tagsnetwork

len <- ncol(tcm_sample_whole)
times <- nrow(topwordsedge)
vs <- data.frame(onset=0, terminus=times+1, vertex.id=1:len)
es <- data.frame(onset=1:times, terminus=times+1, 
                 head=as.matrix(tagsnetwork, matrix.type="edgelist")[,1],
                 tail=as.matrix(tagsnetwork, matrix.type="edgelist")[,2])

tagsnetwork.dyn <- networkDynamic(base.net=tagsnetwork, edge.spells=es, vertex.spells=vs)
compute.animation(tagsnetwork.dyn, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=times, interval=1, 
                                 aggregate.dur=1, rule='any'))

render.d3movie(tagsnetwork.dyn, usearrows = F, displaylabels = T, bg="#ffffff", 
               vertex.border="#111111", vertex.col =  "#aaaaaa",
               vertex.cex = log(tagsnetwork %v% "degree_w")/10, 
               label.cex = 0.5,
               edge.lwd = log(tagsnetwork %e% "weight")/5, edge.col = '#55555599',
               vertex.tooltip = tagsnetwork %v% 'tags',
               edge.tooltip =tagsnetwork %e% "weight",
               launchBrowser=F, filename="Media-Network.html",
               #render.par=list(tween.frames = 30, show.time = F),
               plot.par=list(mar=c(0,0,0,0),xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
               )  


