
#GEREKLÝ OLAN KÜTÜPHANELER GERÇÝ BAZILARI HÝÇ KULLANILMADI
library(readxl)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(cluster)
library(fpc)
library(caret)
library("xlsx")
library("factoextra")
library("NbClust")
library(ggdendro)
library(ape)

#DATAYI OKUYORUM
my_data <- read_excel("data2.xlsx")



#BONUS PART  @@@@@@@@@@@@@@@@@@@@@@@@

a <- my_data$Countries_First_Author
b <- my_data$Countries_Unique_Count

k <- table(a)
k2 <- table(b)

print(tail(sort(k),5))
print(tail(sort(k2),5))

#BONUS PART BÝTÝÞÝ

#DATA CLEANING ÝÞLEMÝ BAÞLADI BURADA names(my_data)[nearZeroVar(my_data)]
#FONKSÝYONUNUN VERMÝÞ OLDUÐU ÇIKTIYA GÖRE SÝLECEKLERÝMÝZÝ BELÝRLEDÝK
#BU FONKSÝYON VARYANS 0'A YAKIN OLAN COLUMN'LARI PRINT EDIYO
my_data$Countries_Num <- NULL
my_data$Countries_Unique_Count <- NULL
my_data$Countries_First_Author <- NULL
my_data$Countries_Perc <- NULL
my_data$em_dash_mark <- NULL
my_data$comma_mark <- NULL
my_data$semicolon_mark <- NULL
my_data$underscore_mark <- NULL
my_data$curly_parenthesis_mark <- NULL
my_data$numFreqLexItems_reviews <- NULL
my_data$TTR_Title <- NULL
my_data$exclamation_mark <- NULL
my_data$square_parenthesis_mark <- NULL
my_data$and_mark <- NULL
my_data$equal_mark <- NULL
my_data$question_mark_loc <- NULL
my_data$presenceInitialPosition_the <- NULL
my_data$double_quote_mark <- NULL
my_data$apostrophe_mark <- NULL
my_data$numFreqLexItems_previews <- NULL
my_data$numPrepositionBeginning <- NULL
my_data$plus_mark <- NULL
my_data$backslash_mark <- NULL
my_data$question_mark_isExist <- NULL

 #J VEYA C YERÝNE EÐER J VARSA 1 C VARSA 0 ATADIM



#LAZIM OLABÝLÝR DÝYE BÖYLE BÝR ÞEY ATAMIÞTIM AMA PEK KULLANMADIM
#HÝÇ KULLANMAMIÞ DA OLABÝLÝRÝM
#col_names <- colnames(my_data)
#View(my_data)


my_data_w_jc <- my_data #J_OR_C COLUMN'UNUN OLMADIÐI BÝR TABLE OLUÞTURMAK ÝÇÝN
                        #MY_DATA'YI KOPYALADIM

my_data_with_jc <- my_data

my_data_w_q <- my_data #QUARTILE COLUMN'UNUN OLMADIÐI BÝR TABLE OLUÞTURMAK ÝÇÝN
                      #MY_DATA'YI KOPYALADIM

my_data_with_q <- my_data #QUARTILE COLUMN'UNUN OLDUÐU BÝR TABLE OLUÞTURMAK ÝÇÝN
                          #MY_DATA'YI KOPYALADIM

my_data_w_q$Quartile  <- NULL #QUARTILE COLUMN'UNU SÝLDÝM


my_data$J_or_C <- ifelse(my_data$J_or_C == "J" , 1 , 0)

my_data_w_q$J_or_C <- ifelse(my_data_w_q$J_or_C == "J" , 1 , 0) #J VEYA C YERÝNE EÐER J VARSA 1 C VARSA 0 ATADIM

my_data_with_q$J_or_C <- ifelse(my_data_with_q$J_or_C == "J" , 1 , 0) # YUKARIDAKI AYNI ÝÞLEM

my_data_with_jc$J_or_C <- ifelse(my_data_with_jc$J_or_C == "J" , 1 , 0)


#ADIM 3
my_data_w_q <- select(my_data_w_q , c(1,16,19,28)) #cor(my_data , my_data$Quartile) fonksiyonunun vermiþ olduðu çýktýya göre
                                                  #en related olan column'larý seçip onlar üstünde iþlem uygulamak adýna
                                                #my_data_w_q table'mi modify ettim.
 
my_data_with_jc <- select(my_data_with_jc , c(1,16,19,27,28)) #yukarýda olan açýklamanýn aynýsý fark cor(my_data , my_data$J_or_C)
my_data_with_q <- select(my_data_with_q , c(1,16,19,27,28)) #burada Quartile dahil olursa nasýl deðiþecek diye kontrol etmek adýna Quartile column'u dahil edildi

my_data_w_jc$J_or_C <- NULL #J_or_C Column'unu silerek J_or_C'siz clustering'e bakacaðým

#col_names2 <- colnames(my_data_w_jc)#kullanýlmýyor lazým olursa diye atamasý yapýldý


#NA CLEANING 
#AÞAÐIDA OLAN 4 FOR DÖNGÜSÜNDE DE DATA'DA HERHANGÝ BÝR MISSING VALUE VARSA
#O MISSING DATA'NIN PROCESS EDÝLÝRKEN HERHANGÝ BÝR ÞEKÝLDE HATA VERMEMESÝ ADINA
#BOÞ BULUNAN BÖLGE O COLUMN'UN MEAN'Ý ÝLE DOLDURULUYOR
for(i in 1:1000){
  
  
  for(j in 1:57){
    
    if(typeof(my_data_w_jc[i,j]) == "NA"){
      
      m_r <- mean(my_data_w_jc[[j]])
      
      my_data_w_jc[i,j] <- m_r
    }
    else{
      next
    }
  }
}


for(i in 1:1000){
  
  
  for(j in 1:5){
    
    if(typeof(my_data_with_jc[i,j]) == "NA"){
      
      m_r <- mean(my_data_with_jc[[j]])
      
      my_data_with_jc[i,j] <- m_r
    }
    else{
      next
    }
  }
}

for(i in 1:1000){
  
  
  for(j in 1:4){
    
    if(typeof(my_data_w_q[i,j]) == "NA"){
      
      m_r <- mean(my_data_w_q[[j]])
      
      my_data_w_q[i,j] <- m_r
    }
    else{
      next
    }
  }
}

for(i in 1:1000){
  
  
  for(j in 1:5){
    
    if(typeof(my_data_with_q[i,j]) == "NA"){
      
      m_r <- mean(my_data_with_q[[j]])
      
      my_data_with_q[i,j] <- m_r
    }
    else{
      next
    }
  }
}
#ADIM 3
cor(my_data , my_data$J_or_C)
cor(my_data , my_data$Quartile)



library(party)

set.seed(1234)

ind <- sample(2, nrow(my_data_with_q) , replace = TRUE , prob = c(0.7,0.3))
ind2 <- sample(2 , nrow(my_data_with_jc) , replace = TRUE, prob = c(0.7,0.3))


train.data <- my_data_with_q[ind==1,]
train.data2 <- my_data_with_jc[ind==1,]
test.data <- my_data_with_q[ind==2,]
test.data2 <- my_data_with_jc[ind==2,]

#View(my_data_with_q)
#View(my_data_with_jc)

myf <- Quartile ~ id+PaperAge+numPage+J_or_C
myf2 <- J_or_C ~ id+PaperAge+numPage+Quartile

wq_tree <- ctree(myf, data = train.data)
wq_tree2 <- ctree(myf2 , data = train.data2)

print(wq_tree)
print(wq_tree2)

table(predict(wq_tree) , train.data$Quartile)
table(predict(wq_tree2) , train.data2$J_or_C)



plot(wq_tree)
plot(wq_tree2)







#DATA SCALE EDÝLÝYOR ÝÞLEME TABÝ TUTULABÝLMEK ÝÇÝN
my_data_w_q <- scale(my_data_w_q)
my_data_with_q <- scale(my_data_with_q)

#gereksiz ama kullandým.
#kullanma amcým birden fazla plot yöntemi kullanarak kontrol etmekti
wssplot <- function(data , nc = 15 , seed =1234){
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i) $withinss)}
  plot(1:nc,wss,type="b",xlab="Number of clusters",ylab ="Within the groups sum of squares")
}


#ADIM 4                        
my_data_w_jc <- select(my_data_w_jc , c(1,16,19,27)) #Bu aslýnda adým 3 iþlemi geç yapýlmýþ sadece

my_data_w_jc <- scale(my_data_w_jc) #process edilebilmesi için datanýn scale iþlemi yapýlýyor

my_data_with_jc <- scale(my_data_with_jc)#yukarýdaki açýklamanýn aynýsý


wssplot(my_data_w_jc) #yukarýda gereksiz dediðim fonksiyonu kullanýyorum
wssplot(my_data_with_jc) #ayný açýklama

#BU AÞAÐIDA BULUNAN 2 SATIR'DA KMEANS CLUSTERING VE UNSUPERVISED CLASSIFICATION
#YAPILIYOR BÝRÝ CLUSTERING VE CLASSIFICATION YAPILIRKEN J_or_C COLUMN'UNUN KULLANILMASI
#DURUMUNDA OLACAK CLUSTERING'I VERÝYOR DÝÐERÝ KULLANILMAMASI DURUMNDA OLACAK CLUSTERING'I VERÝYOR
KM2 = kmeans(my_data_with_jc , 2)
KM = kmeans(my_data_w_jc , 2)

summary(my_data$J_or_C == KM$cluster)
str(KM) #CLUSTERING KONTROLÜ AMACI ÝLE GEREKSÝZ BÝR ADIM YORUM SATIRINA ALINABÝLÝR
str(KM2)
#summary(my_data$J_or_C == KM2$cluster)
 

print(cluster.stats(dist(my_data_w_jc) , KM$cluster))#BU ADIM J_or_C'li ve J_or_C'siz yapýlan clustering'i yorumlamak için yapýlmýþ bir adým
print(cluster.stats(dist(my_data_w_jc) , KM2$cluster))

#AÞAÐIDAKÝ 2 ADIMDA DA PLOT EDEREK CLUSTERÝNG VE CLASSIFICATION VISUALITON YAPILIYOR
autoplot(KM,my_data_w_jc,frame = TRUE) 
autoplot(KM2 , my_data_with_jc , frame = TRUE)

#BU FONKSÝYONU ÝNTERNETTE BULDUM ARKADA KENDÝ BÝR ANALÝZ YAPIP EN ÝYÝ CLUSTER'ING SAYISINA GÖRE
#PLOT EDIYOR WITH J_or_C'de 2 CLUSTER YAPIYOR , DÝÐERÝNDE 4 
fviz_nbclust(my_data_w_jc, kmeans, method = "gap_stat") # Best k value

fviz_nbclust(my_data_with_jc, kmeans, method = "gap_stat") # Best k value

#BU AÞAÐIDA BULUNAN 2 SATIR'DA HCLUST CLUSTERING VE UNSUPERVISED CLASSIFICATION
#YAPILIYOR BÝRÝ CLUSTERING VE CLASSIFICATION YAPILIRKEN J_or_C COLUMN'UNUN KULLANILMASI
#DURUMUNDA OLACAK CLUSTERING'I VERÝYOR DÝÐERÝ KULLANILMAMASI DURUMNDA OLACAK CLUSTERING'I VERÝYOR
Hcluster <- hclust(dist(my_data_w_jc), method = "ward.D")
Hcluster2 <- hclust(dist(my_data_with_jc) , method = "ward.D")

Hcluster$cluster <- cutree(Hcluster, 2)
Hcluster2$cluster <- cutree(Hcluster2, 2)
print(cluster.stats(dist(my_data_w_jc) , Hcluster$cluster))

print(cluster.stats(dist(my_data_with_jc) , Hcluster2$cluster))

summary(my_data$J_or_C == Hcluster$cluster)


#BURADA DA PLOT EDÝLMESÝ
ggdendrogram(Hcluster, rotate = FALSE, size = 2, theme_dendro = FALSE)
ggdendrogram(Hcluster2, rotate = FALSE, size = 2, theme_dendro = FALSE)

print(cluster.stats(dist(my_data_w_jc) , KM$cluster))#BURADA YÝNE ANALÝZ ÝÇÝN 

###############################################################################

#part 5
#BURADAKÝ BÜTÜN KODLAR PART 4 ÝLE AYNI TEK FARK YUKARIDAKÝLER J_or_C içindi
#BURADAKÝLER QUARTILE ÝÇÝN
wssplot(my_data_w_q)
wssplot(my_data_with_q)

KM4 = kmeans(my_data_with_q , 5)
KM3 = kmeans(my_data_w_q , 5)

print(cluster.stats(dist(my_data_w_q) , KM3$cluster))

autoplot(KM3,my_data_w_q,frame = TRUE)
autoplot(KM4 , my_data_with_q , frame = TRUE)

str(KM3) #CLUSTERING KONTROLÜ AMACI ÝLE GEREKSÝZ BÝR ADIM YORUM SATIRINA ALINABÝLÝR
str(KM4)

summary(my_data$Quartile == KM3$cluster)

fviz_nbclust(my_data_w_q, kmeans, method = "gap_stat") # Best k valuefviz_nbclust(my_data_with_q, kmeans, method = "gap_stat") # Best k value


Hcluster3 <- hclust(dist(my_data_w_q), method = "ward.D")
Hcluster4 <- hclust(dist(my_data_with_q) , method = "ward.D")

Hcluster3$cluster <- cutree(Hcluster3, 5)
Hcluster4$cluster <- cutree(Hcluster4, 5)
print(cluster.stats(dist(my_data_w_q) , Hcluster3$cluster))
print(cluster.stats(dist(my_data_with_q) , Hcluster4$cluster))

summary(my_data$Quartile == Hcluster3$cluster)



ggdendrogram(Hcluster3, rotate = FALSE, size = 2, theme_dendro = FALSE)
ggdendrogram(Hcluster4, rotate = FALSE, size = 2, theme_dendro = FALSE)


write.csv(my_data, "CENG464-ProjectGroup2-FinalProject-DataFile.csv", quote = TRUE , row.names = TRUE)



#variances<-apply(my_data , 2, var)
#write.csv(my_data, "modified.csv", quote = TRUE , row.names = TRUE)

#str(Hcluster)

#plot(Hcluster , hang = -1 , labels = NULL)

#print(table(cutree(Hcluster,4)))

#autoplot(my_data_w_jc , Hcluster$cluster)

 #a <- colnames(my_data)
#for(i in a){
#ifelse(my_data[i] == my_data["Authors_Num"]){
#print("Hello bro")
#}
#break
#}
#wssplot(my_data)
#write (my_data, "Modified_data2.csv", quote = FALSE, row.names = FALSE)
#my_data <- scale(my_data)
#for(i in col_names){
 # if(mean(my_data[[i]]) != my_data["Year"]){
  #  print("Naber knk")
  #}
#}
#ifelse(my_da)
#print(my_data)
#my_data.size()
#View(my_data)

