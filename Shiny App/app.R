library(shiny)
library(tm)
library(data.table)
library(stringr)
load("ngrams.RData")

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


ui <- shinyUI(fluidPage(
   
   titlePanel("N-gram based word predictor"),
   h4("Written by Aristide Mooyaart"),
   hr(),
   
   sidebarLayout(
      sidebarPanel(
          textInput("text", label = h3("Text input:"), value = "Best predictor in the"),
          h6("Type some words above to get the top 5 predictions for the next word and the score assigned 
                   by the prediction algoirthm"),
          h3("How does this work?"),
          h6("The prediction algorithm is based on a n-gram model with a 'Stupid Backoff' (Brants et al (2007)) 
             based prediction alogithm. The n-gram models were constructed from sampled twitter, news and blogs data 
             provided by Coursera. The data and algoirthms are designed to balance speed with accuracy for applications
             like mobile phones."),
          hr()
      ),
      
      mainPanel(
         h3("Top 5 predictions:"),
         tableOutput("pred")
      )
   )
))

server <- shinyServer(function(input, output) {
   pred<-reactive({Predictor(input$text, 5)})
   output$pred<-renderTable({pred()}, include.rownames=FALSE)
})

# Run the application 
shinyApp(ui = ui, server = server)

