library(shiny)
library(ggplot2)
library(ggvis)
library('caret')
library('ROCR')



perf <- function(set_1){
  if (is.null(set_1)){
    set_1="set1"
  }
  names<-c()
  # f1s<-c()
  # aucs<-c()
  sensitivities<-c()
  specificities<-c()
  #significancies<-c()
  target<-'female'
  filenames <- list.files(set_1, pattern="*.csv", full.names=TRUE)
  #read one file at a time from a list of files
  for(file in filenames){
    print(file)
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
    
    specificity=(cM[2,2]/ (cM[2,2]+cM[2,1]))
    sensitivity=(cM[1,1]/(cM[1,1]+cM[1,2]))
    print(specificity)
    
    sensitivities<-c(sensitivities, sensitivity)
    specificities<-c(specificities, specificity)
    #significancies<-c(significancies,sig)
    #aucs<-c(aucs, AUC)
    names<-c(names,name)  
  }
  #prepare the output frame to write to a file, according to query items
  
  highest_holder<- data.frame(methods=names,  sensitivity=sensitivities,specificity=specificities,stringsAsFactors = F)
  print(highest_holder)
  names<-c(names,'highest')
  
  out_data<-data.frame(method=names, stringsAsFactors = F)
  out_data["sensitivies"]<-c(sensitivities,highest_holder[which.max(highest_holder$sensitivity), ][1,1])
  out_data["specificities"]<-c(specificities,highest_holder[which.max(highest_holder$specificity), ][1,1])
  
  print(out_data)
  return(highest_holder)
}



# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
data_sets <- c("set1","set2", "set3", "set4", "set5")
# output$select_set <- renderUI({
#   selectInput("dataset", "Data set",  as.list(data_sets))
# })

# Plot the data
mtc <- reactive({ perf(input$select_set)[1:input$n, ]})  
mtc %>%
  ggvis(~sensitivity, ~specificity) %>%
  layer_points() %>%
  bind_shiny("plot")

#mtc2 <- reactive({ perf(input$select_set) })  
# Output the data
output$mytable <- renderTable({
  
    mtc()[, c("specificity", "sensitivity")] 
 
})
})


