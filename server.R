# Load/install necessary packages
library(devtools)
library(shiny)
library(wordcloud)
library(mallet)
library(ggplot2)
library(RColorBrewer)
library(plyr)
library(plotly)
library(reshape2)

library(LDAvis)
pal2 <- brewer.pal(8,"Dark2")
num.top.words<- 30
project_path <- "./class"
numberOftopics <- 20

shinyServer(function(input, output) {
  

  docs <- reactive({ 
    path <- paste0(project_path,input$select)
    return(mallet.read.dir(path))
    })
  doc.topics <- reactive({return(mallet.doc.topics(models(), smoothed=T, normalized=T))})
  dat.date<-reactive({    
    ## Import dataset with date
    date_path <- paste0("./train/",input$select,"/",input$select,".csv")
    dat_date <- read.csv(date_path, sep = ",", header=TRUE)
    dat_date$id <- unlist(lapply(dat_date$id, function(x) paste0("0",as.character(x))))
    return(dat_date)
  })
  models <- reactive({
    mallet.instances <- mallet.import(docs()$id, docs()$text,"./stopwords.txt",token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
    ## Create a topic trainer object.
    topic.model <- MalletLDA(numberOftopics)
    ## Load our documents. We could also pass in the filename of a
    ##  saved instance list file that we build from the command-line tools.
    topic.model$loadDocuments(mallet.instances)
    ## Optimize hyperparameters every 20 iterations,
    ##  after 50 burn-in iterations.
    topic.model$setAlphaOptimization(20, 50)
    ## Now train a model. Note that hyperparameter optimization is on, by default.
    ##  We can specify the number of iterations. Here we'll use a large-ish round nu
    topic.model$train(200)
    ## NEW: run through a few iterations where we pick the best topic for each token,
    ##  rather than sampling from the posterior distribution.
    topic.model$maximize(10)
    ## Get the probability of topics in documents and the probability of words in topics.
    ## By default, these functions return raw word counts. Here we want probabilities,
    ##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
    return(topic.model)
    })
  
  topic_top_words <- reactive({
    topic.words <- mallet.topic.words(models(), smoothed=T, normalized=T)
    topic.top.words <- mallet.top.words(models(), topic.words[input$slider,], num.top.words = num.top.words)
    return(topic.top.words)
  })
  docs.clean<- reactive({
    docs <- docs()
    docs$id <- gsub("\\..*","",docs$id)
    docs$id <- sapply(docs$id, function(x) strsplit(x,"/")[[1]][2])
    return(docs)
  })
  topics.trend <- reactive({
    ## Import dataset with date
    dat_date<-dat.date()
    docs <- docs.clean()
    ## Process dataset with text. Remove .txt after id
    mdata <- merge(docs,dat_date,by="id")
    all_tops <- list()
    for(i in 1:numberOftopics)
    {
      all_tops<- cbind(all_tops,doc.topics()[,i]*100)
    }
    mdata.final <- data.frame(all_tops, 
                        abstract = docs$text, 
                        id <- mdata$id, 
                        shortdate <- mdata$date)
    names(mdata.final)<-c(1:numberOftopics,"abstract","id","shortdate")
    date <- as.Date(as.character(mdata$date),"%Y%m%d",origin="1970-01-01")
    mdata.final$shortdate <- strftime(date, origin = "1970-01-01", format="%Y/%m")
    return(mdata.final)
  })
  
  topic.trend <- reactive({
    docs <- docs.clean()
    dat_date<-dat.date()
    ## Process dataset with text. Remove .txt after id
    mdata <- merge(docs,dat_date,by="id")
    percent <- doc.topics()[,input$slider]*100
    d <- data.frame(id=mdata$id,date=mdata$date,percent=percent)
    date <- as.Date(as.character(mdata$date),"%Y%m%d",origin="1970-01-01")
    d$shortdate <- strftime(date, origin = "1970-01-01", format="%Y/%m")
    percent_m <- aggregate(percent~shortdate, data=d,sum)
    freq_m <- data.frame(table(d$shortdate))
    names(freq_m)<-c("shortdate","freq")
    total <- merge(percent_m,freq_m, by="shortdate")
    return(total)
  })

  topic.docs <- reactive({
    docs<-docs.clean() 
    ## Process dataset with text. Remove .txt after id
    mdata <- merge(docs,dat.date(),by="id")
    percent <- doc.topics()[,input$slider]*100
    d <- cbind(docs$text,percent)
    d <- data.frame(d)
    names(d) <- c("Abstract","Percent")
    for (i in 1:nrow(topic_top_words())){
      d$Abstract<- gsub(as.character(topic_top_words()[i,][1]),paste0("<b><span style='color:red'>",as.character(topic_top_words()[i,][1]),"</span></b>"),d[,1])}
    d$Abstract <- gsub("_", " ", d$Abstract)
    d <- d[with(d,order(Percent,decreasing = TRUE)),]
    return(d)
  })

  output$plot1 <- renderPlot({
      wordcloud(topic_top_words()$words, topic_top_words()$weights, c(4,.8), rot.per=0.11, random.order=F,colors=pal2)
  })
  output$plot2 <- renderPlot({
     ggplot(topic_top_words(),aes(x=words,y=weights))+ geom_bar(stat="identity",aes(fill=weights)) +coord_flip()
  })
  output$plot3 <- renderPlot({
    topic.words <- mallet.topic.words(models(), smoothed=T, normalized=T)
    topic.labels <- mallet.topic.labels(models(), topic.words, 3)
    plot(mallet.topic.hclust(doc.topics(), topic.words, 0.5), labels=topic.labels)
  })

  output$value <- renderPrint({
    input$select
  })
  output$range <- renderPrint({
    input$slider
  })
  output$alltrend <- renderPlotly({
    df <-topics.trend()
    freq_m <- data.frame(table(df$shortdate))
    names(freq_m)<-c("shortdate","freq")
    df.final <- freq_m
    for(i in 1:20){
      df.subset <- df[,c(i,"abstract","id","shortdate")]
      names(df.subset)<-c("percent","abstract","id","shortdate")
      df.subset$percent <- unlist(df.subset$percent)
      percent_m <- aggregate(percent~shortdate, data=df.subset,sum)
      total <- merge(percent_m,freq_m, by="shortdate")
      df.final <- cbind(df.final,i= total$percent/total$freq)
    }
    names(df.final)<-c("shortdate","freq",1:20)
    mdata<-melt(df.final,id=c("shortdate","freq"))
    mdata$id <- as.integer(mdata$variable)
    
    sp <- ggplot(mdata, aes(x=shortdate, y=value,color=variable,group=variable)) + geom_line()+ scale_y_continuous("")+ scale_x_discrete("")
    
    # Divide by levels of "sex", in the vertical direction
    sp + facet_wrap(~variable,ncol=2,scales="free")+ theme_bw()+theme(axis.title=element_blank(), 
                                                           axis.text=element_blank(),
                                                           axis.ticks=element_blank(),
                                                          strip.text = element_blank()
                                                           ) 

    ggplotly()
  })
  output$trend<-renderPlotly({
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showgrid = FALSE
    )
    plot_ly(topic.trend(), x = shortdate, y = percent/freq,
            mode = "lines+markers",line=list(shape="spline"))%>%
      layout(xaxis = ax)
  })
  output$weights <- renderDataTable({
    datatable(topic.docs(), escape = FALSE, filter = 'bottom', extensions = 'Responsive')
  },escape=FALSE,options = list(lengthMenu = c(10,15), pageLength = 10))
  
  output$ldavis <-renderVis({
    phi <- mallet.topic.words(models(), smoothed = TRUE, normalized = TRUE)
    # Now get the smoothed estimates of the document-topic distributions:
    topic.words <- mallet.topic.words(models(), smoothed = TRUE, normalized = FALSE)
    vocab <- models()$getVocabulary()
    word.freqs <- mallet.word.freqs(models())
    term.freqs <- word.freqs$term.freq
    doc.tokens <- data.frame(id=c(1:nrow(doc.topics())), tokens=0)
    for(i in vocab){
      # Find word if word in text
      matched <- grepl(i,docs()$text)
      doc.tokens[matched,2] =doc.tokens[matched,2] +  1
    }
    json <-createJSON(phi = phi, 
                      theta = doc.topics(), 
                      doc.length = doc.tokens$tokens, 
                      vocab = vocab,
                      term.frequency = term.freqs)
  })
})