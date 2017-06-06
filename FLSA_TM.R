FLSA_TM <-  function(InputFile,Num_Topics,Num_Words,WW,Destination){
  

  
library(tm)
library(svd)
library(topicmodels)
library(lsa)
library(topsis)
library(fclust)
library(devtools)
library(irlba)
library(skmeans)
library(data.table)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tictoc)


require(tm)
require(svd)
require(topicmodels)
require(lsa)
require(topsis)
require(fclust)
require(devtools)
require(irlba)
require(skmeans)
require(data.table)
require(SnowballC)
require(wordcloud)
require(RColorBrewer)
require(tictoc)


tic()
  
cat("Please Wait","\n")


openfile <- file(InputFile,open="r")
readfile <- readLines(InputFile)

noc <- Num_Topics
twords <- Num_Words

a <- Destination

corpus <- Corpus(VectorSource(readfile), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stemming = FALSE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE ))
matrix0 <- as.matrix(dtm)
matrix1 <- matrix0

cat("Document Term Frequency Matrix Was Created","\n")

if (WW ==1){ x <- lw_tf(matrix1)* gw_idf(matrix1)}
else if (WW == 2) {x <- lw_tf(matrix1)* gw_idf(matrix1)}
else if (WW == 3){x <- lw_tf(matrix1)* gw_entropy(matrix1)}
else if (WW == 4){x <- lw_tf(matrix1)* gw_entropy(matrix1)}
else {x <- lw_tf(matrix1)* gw_idf(matrix1)}


cat("Selected Word Weighting Method Was Implemented", "\n")



#p(w)       
pword <- 0
sum_of_columns <- colSums(x)
sum_of_matrix <- sum(x)
for(i in 1:nrow(data.matrix(sum_of_columns))){pword[i]=sum_of_columns[i]/sum_of_matrix}

#p(d) probability of document
pdoc <- 0
no_of_documents <- nrow(matrix1)
for(i in 1:no_of_documents){pdoc[i]=1/no_of_documents}

#p(w|d)
rows = nrow(x)
columns = ncol(x)
sum_of_rows = rowSums(x)
pwgd = matrix(rep(0),nrow = rows,ncol = columns)
for (i in 1:rows){pwgd[i,]=x[i,]/sum_of_rows[i]}

#deduction technique
dR2 <- irlba(x, 2)

file.remove(paste(c,paste("Probability_of_Topics_for_Documents.txt",sep=""), sep=""))
file.remove(paste(c,paste("Probability_of_Words_for_Topics.txt",sep=""), sep=""))
file.remove(paste(c,paste("Top_words_per_Topics.txt",sep=""), sep=""))


file.remove(paste(a,"FLSA_Outputs",sep=""))
dir.create(paste(a,"FLSA_Outputs",sep=""))

c <- paste(a,"FLSA_Outputs/",sep="")
ptd <- paste(c,paste("Probability_of_Topics_for_Documents.txt",sep=""), sep="")
ptw <- paste(c,paste("Probability_of_Words_for_Topics.txt",sep=""), sep="")

q <- paste(c,paste("Top_words_per_Topics.txt",sep=""), sep="")


#clustering p(T|D)
ptgw <- skmeans(dR2$u,k = noc, m = 1.1,control = list(nruns = 2, verbose = TRUE));  #k in the number of cluster and for accessign the matrix we need $membership




#p(T,D)
A = matrix(rep(0), nrow = nrow(ptgw$membership), ncol = ncol(ptgw$membership))
for (i in 1:nrow(A)){A[i,] = as.matrix(ptgw$membership[i,]*pdoc[i])}

#p(d|t)
B = matrix(rep(0),nrow=nrow(A),ncol = ncol(A))
Sum_of_columns = colSums(A)
for (j in 1:ncol(B)){B[,j]=A[,j]/Sum_of_columns[j]}

#p(w|t)
tpwdt <- t(pwgd)
pwgt <- tpwdt %*% B
toc()

cat("Done, Please Wait for Creating Outputs", "\n")

write.table(as.matrix(ptgw$membership), ptd, row.names = FALSE, col.names = FALSE)

cat("Probability_of_Topics_for_Documents.txt was Created", "\n")

write.table(as.matrix(pwgt), ptw, row.names = FALSE, col.names = FALSE)

cat("Probability_of_Words_for_Topics.txt was Created", "\n")




t_matrix1 <- t(matrix1)
aa2 = matrix(rep(0), nrow = nrow(pwgt), ncol = ncol(pwgt))
for (i in 1:ncol(pwgt)){aa2[,i] <- as.matrix(order(pwgt[,i], decreasing = TRUE))}

sink(q,append=TRUE)
for(i in 1:noc){cat("Topic ",i,row.names(t_matrix1)[aa2[,i]][1:twords],"\n")}
sink()

cat("Top_words_per_Topics.txt was Created", "\n")


cat("Please Check FLSA_Outputs Folder", "\n")

}








# getting fle and readind it line by line
fileName <- "/Users/amir/Documents/Data/GNIP-SCflood/CleanedData/Vishal-Flood/only tweets/2015-10-03.txt" 

openfile <- file(fileName,open="r")
readfile <- readLines(openfile)

noc = 100                # enter the number of topic or cluster you want to create i am creating 20 clusters and displaying the top 20 words for each topic 
twords = 20   # top words for each topic want to display

a <- "/Users/amir/Desktop/Test/"            # Destination folder where you want to store or create file

    
corpus <- Corpus(VectorSource(readfile), readerControl=list(language="en"))


dtm <- DocumentTermMatrix(corpus, control = list(stemming = FALSE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE ))
matrix0 <- as.matrix(matrix0)
matrix1 <- matrix0





#x <- lw_tf(matrix1)* gw_idf(matrix1)
x <- lw_tf(matrix1)* gw_entropy(matrix1)
#x <- lw_tf(matrix1)* gw_normalisation(matrix1)


#p(w)       
pword <- 0
sum_of_columns <- colSums(x)
sum_of_matrix <- sum(x)
for(i in 1:nrow(data.matrix(sum_of_columns))){pword[i]=sum_of_columns[i]/sum_of_matrix}

#p(d) probability of document
pdoc <- 0
no_of_documents <- nrow(matrix1)
for(i in 1:no_of_documents){pdoc[i]=1/no_of_documents}

#p(w|d)
rows = nrow(x)
columns = ncol(x)
sum_of_rows = rowSums(x)
pwgd = matrix(rep(0),nrow = rows,ncol = columns)
for (i in 1:rows){pwgd[i,]=x[i,]/sum_of_rows[i]}

#deduction technique
dR2 <- irlba(x, 2)

#clustering p(T|D)
ptgw <- skmeans(dR2$u,k = noc, m = 1.1,control = list(nruns = 2, verbose = TRUE))  #k in the number of cluster and for accessign the matrix we need $membership

#p(T,D)
A = matrix(rep(0), nrow = nrow(ptgw$membership), ncol = ncol(ptgw$membership))
for (i in 1:nrow(A)){A[i,] = as.matrix(ptgw$membership[i,]*pdoc[i])}

#p(d|t)
B = matrix(rep(0),nrow=nrow(A),ncol = ncol(A))
Sum_of_columns = colSums(A)
for (j in 1:ncol(B)){B[,j]=A[,j]/Sum_of_columns[j]}

#p(w|t)
tpwdt <- t(pwgd)
pwgt <- tpwdt %*% B

file.remove(paste(a,"Top_words_per_Topics.txt",sep=""))
q <- paste(a,"Top_words_per_Topics.txt",sep="")

sink(q,append=TRUE)
for (i in 1:noc){cat ("", "",row.names(t(matrix1))[order(pwgt[,i],decreasing = TRUE)[1:twords]],"\n")}
sink()

