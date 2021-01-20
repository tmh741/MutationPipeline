displayvals = function(fit){
vals2 = as.matrix(fit)
dim(vals2)
displayval2=data.frame(names=rep(NA,ncol(vals2)),value=rep(NA,ncol(vals2)),madsd=rep(NA,ncol(vals2)),dim1=rep(NA,ncol(vals2)),dim2=rep(NA,ncol(vals2)),variable=rep(NA,ncol(vals2)))
for(i in 1:ncol(vals2)){
  displayval2$names[i] = colnames(vals2)[i]
  displayval2$value[i] = median(vals2[,i])
  displayval2$madsd[i] = mad(vals2[,i])
  displayval2$dim1[i] = gsub("[^0-9.-]","",strsplit(colnames(vals2)[i],",")[[1]])[1]
  displayval2$dim2[i] = gsub("[^0-9.-]","",strsplit(colnames(vals2)[i],",")[[1]])[2]
  displayval2$variable[i] = gsub("[^a-zA-Z]","",colnames(vals2)[i])
}
displayval2$dim1 = as.numeric(displayval2$dim1)
displayval2$dim2 = as.numeric(displayval2$dim2)
return(displayval2)}

plottheta = function(displayval,model,docnames,address=""){
  matrix = dcast(displayval %>% filter(variable=="theta"), dim1 ~ dim2)[,-1]
  colnames(matrix) = paste("sig",1:ncol(matrix),sep="")
  rownames(matrix) = docnames
  matrix$names = 1:nrow(matrix)
  matrix$group = 1:nrow(matrix)
  
  plotlist = list()
  
  for (k in 1:K)
    plotlist[[k]] = ggplot(matrix) + aes_string(x="names",y=colnames(matrix)[k]) + geom_bar(stat="identity") + theme(axis.text.x = element_blank(),legend.position="none") + xlab("") + ylab(paste("Signature",k))
  
  p = grid.arrange(plotlist[[1]],plotlist[[2]],plotlist[[3]],ncol=1)
  ggsave(filename=paste(model,"theta",Sys.Date(),".png",sep=""),plot=p)
}

plotphi = function(displayval, model,mutnames,address=""){
  matrix = dcast(displayval %>% filter(variable=="phi"), dim1 ~ dim2)[,-1]
  rownames(matrix) = paste("sig",1:nrow(matrix),sep="")
  colnames(matrix) = mutnames
  matrix = data.frame(t(matrix),names=colnames(matrix))
  
  plotlist = list()
  
  for (k in 1:K)
    plotlist[[k]] = ggplot(matrix) + aes_string(x="names",y=colnames(matrix)[k]) + geom_bar(stat="identity") + theme(axis.text.x = element_blank(),legend.position="none") + xlab("") + ylab(paste("Signature",k))
  
  p = grid.arrange(plotlist[[1]],plotlist[[2]],plotlist[[3]],ncol=1)
  ggsave(filename=paste(address,model,"phi",Sys.Date(),".png",sep=""),plot=p)
}
