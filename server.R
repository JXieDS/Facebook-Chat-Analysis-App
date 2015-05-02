
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("XML")
library("scales")
library("reshape")
library("dplyr")
library("BH")
library(shiny)
source("functions.r")
library(stringr)
library("tm")
library("SnowballC")
library("ggplot2")
library("RWeka")
library(qdap)
library("wordcloud")
library("lubridate")
options(shiny.maxRequestSize=50*1024^2) # 50 MB max. Upload size
#Mac
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
#Windows
Sys.setlocale("LC_ALL","English")
shinyServer(function(input, output) {
  
  chatDF <- reactive({   
    if(is.data.frame(web())){
      data.frame("Name"=factor(c("Nothing","To Show")),"Date"=c(as.Date("2000-01-01"),as.Date("2000-01-02")),"message"=c("there is no data","please select the file"),"hour"=c(1,2),stringsAsFactors = FALSE)
    }else{
      nThreads <- getNodeSet(isolate(web()), "//*/div[@class='thread'] ")
      if(isolate(input$all)){
        ids <- seq(1,length(nThreads))
      }else{
        isolate(id_list())->filtered_df
        #necessary because input could be substring of other thread
        filtered_df$participants <- paste(":", filtered_df$participants, ":")
        inputparticipants <- paste(":", (input$checkboxlistids),":")
        ids <- filtered_df[filtered_df$participants %in% inputparticipants,]$id
      }
      print(ids)
      if(length(ids)>0){
        chatDF<-apply(as.matrix(as.integer(ids)),1,FUN = function(x){createChatDF(chatstr = getChildrenStrings(nThreads[[x]]))})
        chatDF <- do.call("rbind", chatDF)
      }else{
        data.frame("Name"=factor(c("Nothing","To Show")),"Date"=c(as.Date("2000-01-01"),as.Date("2000-01-02")),"message"=c("there is no data","please select the file"),"hour"=c(1,2),stringsAsFactors = FALSE)
      }
    }
  })
  chatTables <- reactive({
    getChatTables(chatDF(),input$stop_english, input$stop_german)
  })
  output$mindate <- renderText({
    as.character(min(chatDF()$Date))
  })
  output$maxdate <- renderText({
    as.character(max(chatDF()$Date))
  })
  output$daterange <- renderUI({
   dateRangeInput("daterange",label = "Date range",start = min(chatDF()$Date),end = max(chatDF()$Date),min = min(chatDF()$Date),max=max(chatDF()$Date))
  })
  output$checkboxlist<- renderUI({
    dfp <- id_list() 
    if(sum(dim(dfp))!=2){ 
      # unique is possible here because the lookup for the ids over the selected participants is done via %IN%
   checkboxGroupInput("checkboxlistids",label = NULL,choices = unique(dfp$participants),inline=FALSE)
    }else{
      "No threads found."
    }
  })
  output$timespan <- renderText({
    as.integer(max(chatDF()$Date)-min(chatDF()$Date))
  })
  output$msgcount <- renderText({
    as.character(dim(chatDF())[1])
  })
  output$meanwords <- renderDataTable({
    df <- chatDF()
    if(dim(df)[1]==2){
      data.frame("no data")
    }else{
      df$wc <- wc(df$message)
      df <- df[,c(1,4)]
      group_by(df, Name)->newd
      summarise(newd, "Mean words"=mean(wc,na.rm = TRUE), "SD words"=sd(wc,na.rm = TRUE), "Messages"=n())
    }
  })

  web <- reactive({
    inputfiledf <- input$file
    if(is.data.frame(inputfiledf)){
      htmlTreeParse(inputfiledf[1,]$datapath ,error=function (...) {},useInternalNodes = TRUE)
    }else{
      data.frame("no.Data" = "Select a File")
  }})
  output$userplot <- renderPlot({
    print(ggplot(head(as.data.frame(table(chatDF()$Name)), n=30), aes(x=Var1,y=Freq)) + geom_bar(stat="Identity", fill="Blue") +geom_text(aes(label=Freq), vjust=-0.10,) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Participants")+ylab("Messages"))
  })
  output$tab1 <- renderDataTable({(chatTables()$t1)})
  output$tab2 <- renderDataTable({(chatTables()$t2)})
  output$tab3 <- renderDataTable({(chatTables()$t3 )})
  output$tab4 <- renderDataTable({(chatTables()$t4 )})
  output$wc1 <- renderPlot({wordcloud(chatTables()$t1[,2],chatTables()$t1[,3],max.words = 50,scale = c(1,2),colors=brewer.pal(6,"Dark2"))})
  output$wc2 <- renderPlot({wordcloud(chatTables()$t2[,2],chatTables()$t2[,3],max.words = 75,colors=brewer.pal(6,"Dark2"))})
  output$wc3 <- renderPlot({wordcloud(chatTables()$t3[,2],chatTables()$t3[,3],max.words = 50,colors=brewer.pal(6,"Dark2"))})
  output$wc4 <- renderPlot({wordcloud(chatTables()$t4[,2],chatTables()$t4[,3],max.words = 50,colors=brewer.pal(6,"Dark2"))})
  
  output$hourplot <- renderPlot({
    df <- chatDF()
    df$hour <- hour(df$Date)
    table_hour <- as.data.frame(table(df$hour))
    print(ggplot(table_hour, aes(x=Var1,y=Freq)) + geom_bar(stat="Identity", fill="Blue") +xlab("Hour")+ylab("Activity")+scale_x_discrete(breaks=as.factor(c(seq(6,24), seq(0,5)))))
  })
  output$timeplot <- renderPlot({
    df<-chatDF()
    df$day <- as.Date(df$Date)
    if(input$weekagg){
      df$yweek <- paste(year(df$day),week(df$day), "1", sep = " ")
      df<-as.data.frame(table(df$yweek))
      df$Var1 <- as.Date(as.POSIXlt(df$Var1, format = "%Y %U %u"))
      df <- df[order(df$Var1,decreasing = TRUE),]
    }else{
      df<-as.data.frame(table(df$day))
      df$Var1 <- as.Date(df$Var1)  
    }
    print(ggplot(df, aes(x=Var1,y=Freq,group=1)) + geom_line()+xlab("Date")+ylab("Messages")+scale_x_date(labels = date_format("%b %Y")))
   })
  output$timeuserplot<- renderPlot({
    df<-chatDF()
    df$day <- as.Date(df$Date)
    as.data.frame(table(df$day, df$Name))->crosstab
    colnames(crosstab)<-c("Day", "User","Frequency")
    melt(crosstab, id.vars = c("Day","User"))->newd
    newd$Day <- as.Date(newd$Day)
    newd$yweek <- paste(year(newd$Day),week(newd$Day), "1", sep = " ")
    newd <- newd[,c(2,4,5)]
    group_by(newd, User, yweek)->newd
    summarise(newd, messages=sum(value))->newd
    newd$yweek <- as.Date(as.POSIXlt(newd$yweek, format = "%Y %U %u"))
    newd_plot <- newd[newd$yweek >= input$daterange[1] & newd$yweek <= input$daterange[2],]
    print(ggplot(data = newd_plot, aes(x=yweek, y=messages, fill=User))+geom_area(stat="identity")+scale_x_date(labels = date_format("%d %b %Y"))+xlab("Time")+theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  output$file <- renderDataTable({id_list()})
  
  unique_participants <- reactive({
    dfp <- isolate(id_list())
    str_split(dfp$participants ,pattern = ",")->participants_list
    lapply(participants, function(x) gsub("^\\s+|\\s+$","",x))->participants_list
    unlist(participants_list)->participants
    unique(participants)
  })
  output$fancyfilter <- renderUI({
    selectInput('in6', label = 'Users present in Thread', unique_participants(), multiple=TRUE, selectize=TRUE)
  })
  output$fancyexcludefilter <- renderUI({
    selectInput('in7', label = 'Users NOT present in Thread', unique_participants(), multiple=TRUE, selectize=TRUE)
  })
  output$checkboxgroupselected <- renderPrint({
    input$checkboxlistids
  })
  id_list<- reactive({
    if(is.data.frame(web())){
      web()
    }else{
      nThreads <- getNodeSet(web(), "//*/div[@class='thread'] ")
      unlist(lapply(nThreads,FUN = function(x) xmlValue(x[[1]])))->participants
      as.data.frame( participants,stringsAsFactors = F)->dfp
      dfp$id <- rownames(dfp)
      dfp$people <- str_count(dfp$participants, ",")+1
      str_split(dfp$participants ,pattern = ",")->participants_list
      lapply(participants_list, function(x) gsub("^\\s+|\\s+$","",x))->participants_list
      if(is.null(input$in6)){
        if(is.null(input$in7)){
          dfp[ dfp$people >= as.integer(input$people[1]) & dfp$people <= as.integer(input$people[2]) ,c(1,2)]    
        }else{
          dfp[participantFilter(participants_list, input$in6)!=1 & dfp$people >= as.integer(input$people[1]) & dfp$people <= as.integer(input$people[2]) ,c(1,2)]     
        }
      }else{
        if(is.null(input$in7)){
          dfp[participantFilter(participants_list, input$in6)==1 & dfp$people >= as.integer(input$people[1]) & dfp$people <= as.integer(input$people[2]) ,c(1,2)]
        }else{
          dfp[participantFilter(participants_list, input$in6)==1 & participantFilter(participants_list, input$in7)!=1  & dfp$people >= as.integer(input$people[1]) & dfp$people <= as.integer(input$people[2]) ,c(1,2)] 
        }
        }
      }
    
  })

})
