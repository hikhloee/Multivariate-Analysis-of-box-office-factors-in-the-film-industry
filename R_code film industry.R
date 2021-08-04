#<R code> 
library(lattice)
library(corrplot)
library(graphics)

#데이터 전처리--------------------------------------------------------------
MOVIES<-read.csv("C:/Users/ksc4you/Desktop/MOVIES.csv",header=T) #원본 데이터
summary(MOVIES) 
movies<-MOVIES[,3:11] #gross포함
head(movies)
colnames(movies)<- c("x1","x2","x3","x4","x5","x6","x7","x8","x9") #데이터 column명 수정
group1 <-factor(MOVIES$content_rating)
group2<-factor(MOVIES$genre) 
nogross<-movies[,c(1,2,4,5,6,7,8,9)] #gross제외
head(movies)
head(nogross)

#summary(group1)
#    G NC-17    PG PG-13     R 
#   52     3   390  1092  1167 

#summary(group2)
#     Action   Adventure   Animation   Biography      Comedy       Crime 
#        677         275          37         153         713         174 
#Documentary       Drama      Family     Fantasy      Horror     Mystery 
#         21         498           1          15         114          18 
#    Romance      Sci-Fi    Thriller     Western 
#          1           5           1           1 

#기초통계량--------------------------------------------------------------
apply(movies,2,sd) 
apply(movies,2,var)

#상관분석--------------------------------------------------------------
correlation<-cor(movies); correlation

#주성분 분석------------------------------------------------------------
p_cor<-princomp(nogross,cor=TRUE)
summary(p_cor)
p_cor$loadings # 장르로 나누지 않고 기본
screeplot(p_cor,type="lines",main="Scatter-plot of Movie Variables")
biplot(p_cor)
eigen(cor(nogross))$values

#인자 분석 according to 251 pg---------------------------------------------
#screeplot for factor analysis
plot(eigen(cor(movies))$values,type="l")
par(new=TRUE)
plot(eigen(cor(movies))$values,type="o") 

fact1<-factanal(nogross,factors=2,rotation="none")
fact2<-factanal(nogross,factors=2,rotation="varimax")
fact3<-factanal(nogross,factors=2,rotation="promax") #->best
fact1;fact2;fact3

namevar=names(fact3$loadings)=c("x1","x2","x4","x5","x6","x7","x8","x9")
plot(fact3$loadings[,1],fact3$loadings[,2],pch=16,xlab="factor1",ylab="factor2",main="factor pattern3")
text(x=fact3$loadings[,1],y=fact3$loadings[,2],labels=namevar,adj=0)
abline(v=0,h=0) #fact1, fact2, fact3 돌아가며 하기

#인자분석 기반 회귀분석  ---------------------------------------------------
fit1<-lm(x3~x4+x5)
summary(fit1);qqnorm(resid(fit1));qqline(resid(fit1))
fit2<-lm(x3~x7)
summary(fit2);qqnorm(resid(fit2));qqline(resid(fit2))
fit3<-lm(x3~x6)
summary(fit3);qqnorm(resid(fit3));qqline(resid(fit3))
fit4<-lm(x3~x8)
summary(fit4);qqnorm(resid(fit4));qqline(resid(fit4))

#군집분석 -----------------------------------------------------------------
#계층적
dist=dist(nogross, method="euclidean")

hc1 <- hclust(dist,method="single")   #최단연결법
hc1
plot(hc1,labels=1:2704,main="dandrogram:single")  #관람등급

rect.hclust(hc1,k=8,border="red")
plot(hc1,labels=movies[,2],main="dandrogram:single")  #장르
rect.hclust(hc1,k=3,border="red")
hc1.result=cutree(hc1,k=3)
plot(movies_n,pch=hc1.result,col=c("red","green3","blue"))
text(movies_n,adj=0,cex=0.5,main="single")

hc2<-hclust(sqrt(dist),method="complete")   #최장연결법
hc2
plot(hc2,labels=movies[,1],main="complete linkage")
rect.hclust(hc2,k=3,border="red")

rh <- rect.hclust(hc1, k = 3, border = "red")
beg_clus <- head(cumsum(c(1, lengths(rh))), -1)
y_clus <- weighted.mean(rev(hc1$height)[2:3], c(4, 1))
text(x=beg_clus, y=y_clus, col="red", labels=LETTERS[1:3], font=2)

plot(hc2,labels=movies[,2],main="complete linkage")
rect.hclust(hc2,k=3,border="red")
hc2.result=cutree(hc2,k=3)
plot(movies_n,pch=hc2.result,col=c("red","green3","blue"))
text(movies_n,adj=0,cex=0.5,main="complete")

hc3<-hclust(dist,method="ward.D")   #Ward방법
hc3
plot(hc3,labels=movies[,1],main="Ward Method")
rect.hclust(hc3,k=3,border="red")

rh <- rect.hclust(hc3, k = 3, border = "red")
beg_clus <- head(cumsum(c(1, lengths(rh))), -1)
y_clus <- weighted.mean(rev(hc3$height)[2:3], c(4, 1))
text(x=beg_clus, y=y_clus, col="red", labels=LETTERS[1:3], font=2)

plot(hc3,labels=movies[,2],main="Ward Method")
rect.hclust(hc3,k=3,border="red")
hc3.result=cutree(hc3,k=3)

plot(movies_n,pch=hc3.result,col=c("red","green3","blue"))
text(movies_n,adj=0,cex=0.5,,main="Ward")

hc4<-hclust(dist,method="average")   #평균연결법
hc4
plot(hc4,labels=movies[,1],main="Average linkage")
rect.hclust(hc4, k=6, border="red")
rh <- rect.hclust(hc4, k = 3, border = "red")
beg_clus <- head(cumsum(c(1, lengths(rh))), -1)
y_clus <- weighted.mean(rev(hc4$height)[2:3], c(4, 1))
text(x=beg_clus, y=y_clus, col="red", labels=LETTERS[1:3], font=2)

plot(hc4,labels=movies[,2],main="Average linkage")
rect.hclust(hc4, k=3, border="red")
hc4.result=cutree(hc4,k=3)
plot(movies_n,pch=hc4.result,col=c("red","green3","blue"))
text(moves_n,adj=0,cex=0.5,,main="Agerage Linkage")

#비계층적 --------------------------------------------------------------
movies_k=kmeans(scale(nogross),centers=3)
attributes(movies_k)
movies_k$cluster
sum(movies_k$cluster==1)
sum(movies_k$cluster==2)
sum(movies_k$cluster==3)

kc=table(movies_k$cluster)  #각 군집 별 데이터 개수
kc

ccent <- function(planets,cl) {
  f <- function(i) colMeans(planets[cl == i,])
  x <- sapply(sort(unique(cl)), f)
  colnames(x) <- sort(unique(cl))
  return(x)
}
ccent(nogross,movies_k$cluster)

#install.packages("factoextra")
library(factoextra)

df<-scale(nogross)
km.res<-kmeans(df,3)
fviz_cluster(km.res,data=df,pallette=c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot")
distance<-get_dist(df)
head(km.res)

#군집 각각 숫자 세기 ------------------------------------------------------

# R은 1168번까지
# PG-13은 1169-2260번까지
# PG는 2261-2650번까지
# NC-17은 2651-2653번까지
# G는 2654-2075까지 

# Action은 677까지
# Advencture는 678-951까지
# Animation은 952-989까지
# Biology는 990-1142까지
# Comedy는 1143-1855까지
# Crime은 1856-2029까지
# Document는 2030-2050
# Drama 2051-2548
# Family 2549
# Fantasy 2550-2564
# Horror 2565-2679
# Mystery 2680-2697
# Romance 2698
# Sci-Fi 2699-2703
# Thriller 2704
# Western 2705 

c<-movies_k$cluster

sumnum <- function(c) {
  
  result<-c(0,0,0)
  cw<-rep(0,100)
  for(n in 1:2700 ){
    n=n+100
    cw=c[n-100:n]
    x<-sum(cw==1)
    y<-sum(cw==2)
    z<-sum(cw==3)
    result<-rbind(result,c(x,y,z))
  }
  return(result)
}

#sumnum(c)
result<-sumnum(c)
result<-cbind(c(1:2701),result)
result #누적표 

#parallel plot --------------------------------------------------------------
install.packages("GGally")
library(GGally)
ggparcoord(MOVIES,columns=c(3,4,6,7,8,9,10,11),scale="center",groupColumn=1)#시청등급
ggparcoord(MOVIES,columns=c(3,4,6,7,8,9,10,11),scale="center",groupColumn=2)#장르
