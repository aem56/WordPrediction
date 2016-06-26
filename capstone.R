library(tm)
library(SnowballC)
library(wordcloud)
library(stringi)
library(slam)
library(data.table)
library(stringr)
set.seed(5000)

#Load Data
con<-file("en_US.twitter.txt", "r")
twitter<-readLines(con, encoding="UTF-8", skipNul=TRUE)
close(con)
con<-file("en_US.blogs.txt", "r")
blogs<-readLines(con, encoding="UTF-8", skipNul=TRUE)
close(con)
con<-file("en_US.news.txt", "r")
news<-readLines(con, encoding="UTF-8", skipNul=TRUE)
close(con)

#Create 5% samples and merge together
twittersample<-sample(twitter, size = round(length(twitter)*0.01))
blogssample<-sample(blogs, size = round(length(blogs)*0.01))
newssample<-sample(news, size = round(length(news)*0.01))
sample<-c(twittersample, blogssample, newssample)

#Load profane words
profaneWords<-read.table("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", encoding = "UTF-8")

#Make sure sample if ASCII text only (otherwise have problems with emoticons later)
sample<-sapply(sample,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Transform samples into 'corpus' class
sampleCorpus<-Corpus(VectorSource(sample), readerControl=list(reader=readPlain, language="en_US", load=TRUE))

#Remove unused variables
rm(twitter,blogs,news,twittersample,blogssample,newssample, sample)

#Clean Corpos
sampleCorpus<-tm_map(sampleCorpus, removeNumbers)
sampleCorpus<-tm_map(sampleCorpus, content_transformer(tolower), lazy = T)
sampleCorpus<-tm_map(sampleCorpus, removeWords, profaneWords[,1])
sampleCorpus<-tm_map(sampleCorpus, content_transformer(stripWhitespace))

#Ngram Tokenize
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE, skip_word_number = FALSE) {
    stopifnot(is.numeric(n), is.finite(n), n > 0)
    
    #' To avoid :: calls
    stri_split_boundaries <- stringi::stri_split_boundaries
    stri_join <- stringi::stri_join
    
    options <- stringi::stri_opts_brkiter(
        type="word", skip_word_none = skip_word_none, skip_word_number = skip_word_number
    )
    
    #' Tokenizer
    #' 
    #' @param x character
    #' @return character vector with n-grams
    function(x) {
        stopifnot(is.character(x))
        
        # Split into word tokens
        tokens <- unlist(stri_split_boundaries(x, opts_brkiter=options))
        len <- length(tokens)
        
        if(all(is.na(tokens)) || len < n) {
            # If we didn't detect any words or number of tokens is less than n return empty vector
            character(0)
        } else {
            sapply(
                1:max(1, len - n + 1),
                function(i) stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
            )
        }
    }
}
Uni<-TermDocumentMatrix(sampleCorpus, control=list(tokenize=content_transformer(ngram_tokenizer(1)),
                                                   wordLengths=c(1,Inf)))
Bi<-TermDocumentMatrix(sampleCorpus, control=list(tokenize=content_transformer(ngram_tokenizer(2)),
                                                  wordLengths=c(1,Inf)))
Tri<-TermDocumentMatrix(sampleCorpus, control=list(tokenize=content_transformer(ngram_tokenizer(3)),
                                                   wordLengths=c(1,Inf)))
Quad<-TermDocumentMatrix(sampleCorpus, control=list(tokenize=content_transformer(ngram_tokenizer(4)),
                                                    wordLengths=c(1,Inf)))

rm(sampleCorpus, profaneWords)

#Process Ngrams
UniFreq<-rowapply_simple_triplet_matrix(Uni, sum)
BiFreq<-rowapply_simple_triplet_matrix(Bi, sum)
TriFreq<-rowapply_simple_triplet_matrix(Tri, sum)
QuadFreq<-rowapply_simple_triplet_matrix(Quad, sum)

WordSplitter<-function(ngramFreq, word){
    sapply(strsplit(names(ngramFreq), " "), function(x) x[word])
}

UniDT<-data.table(Unigram=names(UniFreq), Freq=UniFreq)
BiDT<-data.table(Bigram=names(BiFreq), Freq=BiFreq, 
                 First=WordSplitter(BiFreq, 1), 
                 Second=WordSplitter(BiFreq, 2))
TriDT<-data.table(Trigram=names(TriFreq), Freq=TriFreq, 
                  First=paste(WordSplitter(TriFreq, 1), WordSplitter(TriFreq, 2)), 
                  Second=WordSplitter(TriFreq, 3))
QuadDT<-data.table(Quadgram=names(QuadFreq), Freq=QuadFreq, 
                   First=paste(WordSplitter(QuadFreq, 1), WordSplitter(QuadFreq, 2), WordSplitter(QuadFreq, 3)), 
                   Second=WordSplitter(QuadFreq, 4))

UniDT<-UniDT[order(UniDT$Freq, decreasing = TRUE),]
BiDT<-BiDT[order(BiDT$Freq, decreasing = TRUE),]
TriDT<-TriDT[order(TriDT$Freq, decreasing = TRUE),]
QuadDT<-QuadDT[order(QuadDT$Freq, decreasing = TRUE),]

#Word Predictor
Predictor<-function(text,numPred){
    
    cleanText<-removePunctuation(text)
    cleanText<-removeNumbers(text)
    cleanText<-tolower(text)
    cleanText<-unlist(str_split(cleanText," "))
    
    NumWords<-length(cleanText)
    
    UniSubset<-UniDT[1:numPred,]
    BiSubset<-BiDT[grepl(paste0("^",cleanText[NumWords],"$"), BiDT$First),]
    TriSubset<-TriDT[grepl(paste0("^",paste(cleanText[NumWords-1], cleanText[NumWords]),"$"), TriDT$First),]
    QuadSubset<-QuadDT[grepl(paste0("^",paste(cleanText[NumWords-2], cleanText[NumWords-1], 
                                              cleanText[NumWords]),"$"), QuadDT$First),]
    
    if(NumWords>=3 & nrow(QuadSubset) != 0){
        UniSubset<-UniSubset[0,]
        BiSubset<-BiSubset[0,]
        TriSubset<-TriSubset[0,]
    }
    else if(nrow(TriSubset) != 0 & NumWords>=2){
        UniSubset<-UniSubset[0,]
        BiSubset<-BiSubset[0,]
        QuadSubset<-QuadSubset[0,]
    }
    else if(nrow(BiSubset) != 0 & NumWords>=1){
        UniSubset<-UniSubset[0,]
        TriSubset<-TriSubset[0,]
        QuadSubset<-QuadSubset[0,]
    }
    else{
        BiSubset<-BiSubset[0,]
        TriSubset<-TriSubset[0,]
        QuadSubset<-QuadSubset[0,]
    }
    
    UniSubset$Score<-(UniSubset$Freq/nrow(UniSubset))
    BiSubset$Score<-(BiSubset$Freq/nrow(BiSubset))
    TriSubset$Score<-(TriSubset$Freq/nrow(TriSubset))
    QuadSubset$Score<-(QuadSubset$Freq/nrow(QuadSubset))
    
    Predictions<-data.frame(Word=c(UniSubset$Unigram, BiSubset$Second, TriSubset$Second, QuadSubset$Second),
                            Score=c(UniSubset$Score, BiSubset$Score, TriSubset$Score, QuadSubset$Score))
    Predictions<-aggregate(Score~Word, data=Predictions, FUN=sum)
    Predictions<-Predictions[order(Predictions$Score, decreasing = TRUE),]
    Predictions<-Predictions[1:numPred,]
    Predictions
}

save(UniDT, BiDT, TriDT, QuadDT, file="ngrams.RData")


