library(shiny)
library(ggplot2)
library(ggvis)
library('caret')
library('ROCR')

names<-c()
# f1s<-c()
# aucs<-c()
sensitivities<-c()
specificities<-c()
#significancies<-c()
target<-'female'
filenames <- list.files('set1', pattern="*.csv", full.names=TRUE)
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
  
  # precision = cM[1,1] /(cM[1,1]+cM[1,2])
  # recall = cM[1,1] / (cM[1,1]+cM[2,1])
  # F1 = ((2*precision*recall)/(precision+recall))
  
  specificity=(cM[2,2]/ (cM[2,2]+cM[2,1]))
  sensitivity=(cM[1,1]/(cM[1,1]+cM[1,2]))
  print(specificity)
  
  # predic <- prediction(dfile$pred.score,dfile$reference)
  # perfor <- performance(predic, "tpr", "fpr")
  # plot(perfor, colorize=T)
  # 
  # AUC <- performance(predic, "auc")
  # AUC <- unlist(AUC@y.values)
  # signf = fisher.test(cM)
  #print(signf$p.value)
  # if(signf$p.value < 0.05 || AUC > 0.5)
  # {
  #   sig="yes"
  # }else{
  #   sig="no"
  # }
  #f1s<-c(f1s, F1)
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


# out_data[q]<-c(f1s,highest_holder[which.max(highest_holder$f1), ][1,1])
out_data["sensitivies"]<-c(sensitivities,highest_holder[which.max(highest_holder$sensitivity), ][1,1])
out_data["specificities"]<-c(specificities,highest_holder[which.max(highest_holder$specificity), ][1,1])
# out_data[q]<-c(aucs,highest_holder[which.max(highest_holder$auc), ][1,1])
# out_data[q]<-c(significancies,highest_holder[which.max(highest_holder$significance), ][1,1])

print(out_data)

#dataset<-out_data
# shinyServer(function(output,session){
# 
#   dataset<-reactive(out_data)
#   output$plot <- renderPlot({
#     p<-ggplot(dataset(),aes_string(x=input$sensitivities,y=input$specifities))+geom_point()
#     print (p)
#     
#   }, height=700)
# })
img.width <-450
img.height<-300

scatter.ggvis <- out_data%>%ggvis(x=~sensitivities,y=~specificities)%>%layer_points()%>%set_options(width=img.width, height = img.height)
scatter.ggvis