## Código que sirve para analizar textos, determinar relaciones y hacer clusters.

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggbiplot))
suppressPackageStartupMessages(library(directlabels))
suppressPackageStartupMessages(library(rgl))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tau))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(colorspace))

##---------------------------------------
## Leer mensajes
##---------------------------------------
setwd("C:/Users/cedn.ss1/Documents/Proyectos/sentiment text analysis gov")
messages <- read.csv("./data/comments.csv",stringsAsFactors=F)
messages <- messages[,1]

#Convertir html en texto.
toText<-function(messages){
  #for (i in 1:length(messages)){
#messages[i]<-html2txt(messages[i])
#}
messages<-removeNumbers(messages)
messages<-tolower(messages)
messages<-removePunctuation(messages)
messages<-str_replace_all(messages,"\t","")
for(i in 1:length(messages)){
  messages[i]<-iconv(messages[i],"UTF-8","ASCII","")
  messages[i]<-removeWords(messages[i],stopwords("spanish"))
}
messages
}
messages<-toText(messages)
messages<-str_sub(messages,str_locate(messages," ")[,2],str_length(messages))
messages<-str_trim(messages)
messages<-messages[str_length(messages)>2]
messages<-messages[!is.na(messages)]
#---------------------------------------------------------------------------------------------
#Estructura matriz de t?rminos
frex<-4
words<-tokenize(messages)
words<-words[str_length(words)>2]
words<-removeNumbers(words)
words<-count(words)
words<-words[!is.na(words$x),]
words<-words[words$freq>frex,]
term_matrix<-data.frame(matrix(0,nrow=length(unique(messages)),ncol=nrow(words)),
                        row.names=unique(messages))
colnames(term_matrix)<-words$x
#------------------------------------------------------------------------------------------------
#llenado matriz de t?rminos
for(i in 1:length(messages)){
  for(j in 1:ncol(term_matrix)){
  term_matrix[i,j]<-str_count(rownames(term_matrix)[i],colnames(term_matrix)[j])
  }
}
#futuras implementacions podemos hacer uso de t?cnicas de clustering para poner todos los t?rminos 
#que se asemejan bajo un mismo t?rmino.
#--------------------------------------------------------------------------------------------
#gr?ficas.
prc_fit<-prcomp(term_matrix,scale.=T)
ggbiplot(prc_fit,obs.scale=1, 
         var.scale=1,circle=T)+ggtitle("Gr?fico componentes principales")
#clustering
ncluster<-10
set.seed(13)
clus_fit<-kmeans(data.frame(prc_fit$x[,1],prc_fit$x[,2]),ncluster)
grupos=factor(clus_fit$cluster,levels=unique(clus_fit$cluster),labels=paste("grupo",unique(clus_fit$cluster),sep=": "))
centers<-as.data.frame(clus_fit$centers)
colnames(centers)<-c("V1","V2")
#gr?fica
clus_plot<-ggplot(data.frame(prc_fit$x[,1],prc_fit$x[,2]),aes(prc_fit$x[,1],prc_fit$x[,2],
             col=grupos))+geom_point()+geom_point(data=centers, aes(x=V1,y=V2,col='center')) +
  geom_point(data=centers, aes(x=V1,y=V2,col='center'), size=75, alpha=.3, show_guide=FALSE)+
  ggtitle("Agrupaci?n de tweets conforme a similaridad")+
  geom_text(aes(label=paste(str_sub(rownames(term_matrix),1,15),"...",sep="")),size=2.5)+
  xlab("Componente 1")+ylab("Componente 2")
clus_plot
#wordcloud
#crear un documento por cada cluster.
dir<-paste("./text_nclust",ncluster,sep="")
dir.create(dir)
msg_clust<-c()
for(i in 1:ncluster){
text<-paste(rownames(term_matrix)[clus_fit$cluster==i],collapse="\n")
msg_clust[i]<-text
dir_file<-paste("/clust",i,".txt",sep="")
write(text, paste(dir,dir_file,sep=""))
}
#crear matriz de terminos
corpus<-Corpus(DirSource(dir))
tdm_clust<-TermDocumentMatrix(corpus)
tm<-as.matrix(tdm_clust)
colnames(tm)<-rep("",ncluster)
comparison.cloud(tm,max.words=300,random.order=FALSE,
                 colors=rainbow_hcl(ncluster))
commonality.cloud(tm,random.order=FALSE,color="#F79646")

