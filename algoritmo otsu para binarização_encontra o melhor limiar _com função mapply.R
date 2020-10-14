#Otsu Thresholding

#Access the Height Matrix

image1<-read.table (file = "sample.txt")

z<-image1[3]

Height<-z[[1]]

y<-sort(Height)

Otsu<-function(x){
  p<-which(y<x)
  
  t0<-y[p] #second plan
  
  C0<-length(t0)
  
  q<-which(y>=x)
  
  t1<-y[q] #first plan
  
  C1<-length(t1)
  
  #Calculation of weights, class variances and class averages
  
  W0<-C0/(256*256)
  
  W1<-C1/(256*256)
  
  var0<-var(t0)
  
  var1<-var(t1)
  
  media0<-mean(t0)
  
  media1<-mean(t1)
  
  mediat<-(W0*media0)+(W1*media1)
  
  mediat
  
  #calculation of variance within classes
  
  Vw<-(W0*var0)+(W1*var1)
  
  #the best threshold will be the one that minimizes Vw
  
  Vw
  
  #calculation of variance between classes
  
  Vb<-(W0*W1)*((media0-media1)^2)
  
  #The best threshold will be the one that maximizes Vb
  
  Vb
  
  #calculation of total variance
  
  Vt<-Vw+Vb
  
  Vt
  
  #calculation of the separability parameter 
  
  m<-Vb/Vt
  
}

h<-mapply(Otsu,y)

plot(y,h, xlab="Threshold", ylab="η", main="OTSU THRESHOLD")

tabela<-data.frame(y,h)

write.table(tabela, "Otsu.txt", row.names = FALSE)
