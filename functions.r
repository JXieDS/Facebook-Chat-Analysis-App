participantFilter <- function(participants_list, filter_participants){
  i = 1
  mask = 0
  while(i<=length(filter_participants)){
    mask <- mask + unlist(lapply(participants_list, function(x) sum(x==filter_participants[i])))
    i = i + 1
  }
  mask <- mask / length(filter_participants)
  mask
  
}

createChatDF <- function(chatstr){
  as.data.frame(chatstr, stringsAsFactors = F)->df1
  colnames(df1)<-c("strings1")
  df1[1,1]->participants_chat
  as.data.frame(df1[-1,], stringsAsFactors = FALSE)->df1
  colnames(df1)<-c("strings1")
  df1$meta <- as.logical(as.integer(row.names(df1)) %% 2)
  df1[df1$meta, ]$strings1->metadf1
  df1[!df1$meta, ]$strings1->chatdf1
  
  chatdataframe<-data.frame("user"=metadf1, "message"=chatdf1,stringsAsFactors = FALSE)
  chatdataframe$user <- gsub("Monday",replacement = ";Monday",chatdataframe$user)
  chatdataframe$user <- gsub("Tuesday",replacement = ";Tuesday",chatdataframe$user)
  chatdataframe$user <- gsub("Wednesday",replacement = ";Wednesday",chatdataframe$user)
  chatdataframe$user <- gsub("Thursday",replacement = ";Thursday",chatdataframe$user)
  chatdataframe$user <- gsub("Friday",replacement = ";Friday",chatdataframe$user)
  chatdataframe$user <- gsub("Saturday",replacement = ";Saturday",chatdataframe$user)
  chatdataframe$user <- gsub("Sunday",replacement = ";Sunday",chatdataframe$user)
  
  data.frame(do.call('rbind',strsplit(chatdataframe$user,split = ";")))->metadata
  colnames(metadata)<-c("Name","Date")
  cbind(metadata, chatdataframe)->chatdataframe
  chatdataframe$Date <- strptime(chatdataframe$Date,"%A, %B %d, %Y at %I:%M%p")
  chatdataframe$user <- NULL
  chatdataframe <- chatdataframe[order(chatdataframe$Date),]
  print(paste0("Participants: ", participants_chat))
  print(paste0("Messages: ", as.character(dim(chatdataframe)[1])))
  rm(metadf1)
  rm(chatdf1)
  chatdataframe
}

getChatTables <- function(chatDF, stop_english, stop_german){
  chatDF$message <- tolower(chatDF$message)
  if(stop_german){
    chatDF$message <- removeWords(chatDF$message, stopwords("german"))  
  }
  if(stop_english){
    chatDF$message <- removeWords(chatDF$message, stopwords("english"))  
  }

  chatDF$message <- removePunctuation(chatDF$message, preserve_intra_word_dashes = TRUE)
  chatDF$message <- removeNumbers(chatDF$message)
  NGramTokenizer(chatDF$message, Weka_control(min=1,max=1))->ng1
  NGramTokenizer(chatDF$message, Weka_control(min=2,max=2))->ng2
  NGramTokenizer(chatDF$message, Weka_control(min=3,max=3))->ng3
  NGramTokenizer(chatDF$message, Weka_control(min=4,max=4))->ng4
  
  tab_ng1 <- data.frame(table(ng1))
  tab_ng2 <- data.frame(table(ng2))
  tab_ng3 <- data.frame(table(ng3))
  tab_ng4 <- data.frame(table(ng4))
  
  tab_ng1 <- tab_ng1[order(tab_ng1$Freq,decreasing = TRUE),]
  tab_ng2 <- tab_ng2[order(tab_ng2$Freq,decreasing = TRUE),]
  tab_ng3 <- tab_ng3[order(tab_ng3$Freq,decreasing = TRUE),]
  tab_ng4 <- tab_ng4[order(tab_ng4$Freq,decreasing = TRUE),]
  
  colnames(tab_ng1)<- c("Gram", "Frequeny")
  colnames(tab_ng2)<- c("Gram", "Frequeny")
  colnames(tab_ng3)<- c("Gram", "Frequeny")
  colnames(tab_ng4)<- c("Gram", "Frequeny")
  
  tab_ng1$Rank <- seq(1,dim(tab_ng1)[1])
  tab_ng2$Rank <- seq(1,dim(tab_ng2)[1])
  tab_ng3$Rank <- seq(1,dim(tab_ng3)[1])
  tab_ng4$Rank <- seq(1,dim(tab_ng4)[1])
  
  list(t1 = tab_ng1[,c(3,1,2)],t2= tab_ng2[,c(3,1,2)],t3= tab_ng3[,c(3,1,2)],t4= tab_ng4[,c(3,1,2)])
}

