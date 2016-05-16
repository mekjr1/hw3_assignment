library(shiny)
library(ggplot2)
library(ggvis)
library('caret')
library('ROCR')



perf <- function(set_1, target, numV){
  print (target)
  if (is.null(set_1)){
    set_1="set1"
  }
  names<-c()
  f1s<-c()
  aucs<-c()
  sensitivities<-c()
  specificities<-c()
  significancies<-c()
  
  filenames <- list.files(set_1, pattern="*.csv", full.names=TRUE)
  #read one file at a time from a list of files
  for(file in filenames[1:numV]){
    
    name<-gsub(".csv", "", basename(file))
    
    #read the file and convert it to a matrix
    dfile<-read.table(file, header=T,sep=",")
    #print(dfile)
    cM<-table(truth=dfile$reference,prediction=dfile$prediction)
    cM= as.matrix(cM)
    #if target is male, change the order of the matrix, the default favors female
    if(target == "male"){
      cM = matrix(c(cM[2,2],cM[2,1],cM[1,2],cM[1,1]),ncol=2,byrow=T)
      rownames(cM)=c("male","female")
      colnames(cM)=c("male","female")
    }
    
    precision = cM[1,1] /(cM[1,1]+cM[1,2])
    recall = cM[1,1] / (cM[1,1]+cM[2,1])
    F1 = ((2*precision*recall)/(precision+recall))
    specificity=(cM[2,2]/ (cM[2,2]+cM[2,1]))
    sensitivity=(cM[1,1]/(cM[1,1]+cM[1,2]))
    
    predic <- prediction(dfile$pred.score,dfile$reference)
    perfor <- performance(predic, "tpr", "fpr")
    plot(perfor, colorize=T)
    
    AUC <- performance(predic, "auc")
    AUC <- unlist(AUC@y.values)
    signf = fisher.test(cM)
    #print(signf$p.value)
    if(signf$p.value < 0.05 || AUC > 0.5)
    {
      sig="yes"
    }else{
      sig="no"
    }
    
    sensitivities<-c(sensitivities, sensitivity)
    specificities<-c(specificities, specificity)
    significancies<-c(significancies,sig)
    f1s<-c(f1s, F1)
    aucs<-c(aucs, AUC)
    names<-c(names,name)  
  }
  
  #prepare the output frame to write to a file, according to query items
  
  #highest_holder<- data.frame(methods=names,  sensitivity=sensitivities,specificity=specificities,stringsAsFactors = F)
  highest_holder<- data.frame(methods=names, f1=f1s, auc=aucs, sensitivity=sensitivities, specificity=specificities, significance=significancies,stringsAsFactors = F)
  
  names<-c(names,'highest')
  
  out_data<-data.frame(method=names, stringsAsFactors = F)
  out_data["sensitivities"]<-c(sensitivities,highest_holder[which.max(highest_holder$sensitivity), ][1,1])
  out_data["specificities"]<-c(specificities,highest_holder[which.max(highest_holder$specificity), ][1,1])
  out_data["aucs"]<-c(aucs,highest_holder[which.max(highest_holder$auc), ][1,1])
  out_data["significancies"]<-c(significancies,highest_holder[which.max(highest_holder$significance), ][1,1])
  out_data["f1s"]<-c(f1s,highest_holder[which.max(highest_holder$f1), ][1,1])
  
  #return (out_data)
  return(list(highest_holder,out_data))
}



# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # output$select_set <- renderUI({
  #   selectInput("dataset", "Data set",  as.list(data_sets))
  # })
  
  # Plot the data
  
  mtc <- reactive({ perf(input$select_set, input$select_gen, input$n)[[1]]}) 
 
  mtc %>%
    ggvis( ~specificity,~sensitivity,size := 200,fill:= "orange", opacity := 0.2, stroke := "green",
          strokeWidth := 10) %>%
    layer_points() %>%
    set_options(
                renderer = "canvas")%>%
    add_tooltip(all_values,"hover") %>%
    bind_shiny("plot")
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    
    paste0("Method:", x$methods,"<br>","Sensitivity:", x$sensitivity, "<br>","Specificity:", x$specificity, collapse = "<br />")
  }
  
  
  
  output$mytable <- renderTable({
    
    mtc()
    
  })
  mtc2 <- reactive({ perf(input$select_set, input$select_gen, input$n)[[2]]}) 
  output$maxmet <- renderTable({ 
    #tail(mtc2(),1)
    tail(mtc2()[,c("f1s","aucs","specificities", "sensitivities")] ,1)
  })
})


