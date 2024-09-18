

#Q1 normal(mu=10 , sigma_sq=1)
rm(list=ls())
Y=matrix(rnorm(1000*100,10,1),ncol=100)
m=matrix(rep(0,24),ncol=8)
n=c(15,30,60)
T1=rep(0,1000)
T2=rep(0,1000)
T3=rep(0,1000)
T4=rep(0,1000)
df1=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
df2=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
df3=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
df4=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
for(i in 1:3)
{ 
  T1=rowMeans(Y[,1:n[i]])
  df1[,i]=T1

  m[i,1]=mean(T1)
  m[i,2]=var(T1)

  T2=apply(Y[,1:n[i]],1,var)
  df2[,i]=T2
  m[i,3]=mean(T2)
  m[i,4]=var(T2)
  
  T3=apply(Y[,1:n[i]],1,median)
  df3[,i]=T3
  m[i,5]=mean(T3)
  m[i,6]=var(T3)
  
  T4=((n-1)/n)*apply(Y[,1:n[i]],1,var)
  df4[,i]=T4
  m[i,7]=mean(T4)
  m[i,8]=var(T4)
}

colnames(df1)=list("n=15","n=30","n=60");head(df1)

colnames(df2)=list("n=15","n=30","n=60");head(df2)
colnames(df3)=list("n=15","n=30","n=60");head(df3)
colnames(df4)=list("n=15","n=30","n=60");head(df4)

colnames(m)=list("mean","mean_var","var_mean","var_var","Median","median_var","t4=(1/n)*sum(xi-xbar)","var(t4)")
m

estimate=c(xbar=mean(m[,1]),xbar_var=mean(m[,2]),var_mean=mean(m[,3]),v_var=mean(m[,4]),median=mean(m[,5]),med_var=mean(m[,6]),t4=mean(m[,7]),var_t4=mean(m[,8]))
estimate

hist(df[,1],col="red")
hist(df[,2],col="yellow",add=TRUE)
hist(df[,3],col="orange",add=TRUE)
legend("topright",legend=c("n=15","n=30","n=60"),fill=c("red","yellow","orange"))


#Q2 poisson 
n <- c(5,15,30)
Mean_T1=Mean_T2=Var_T1=Var_T2=0
for (i in 1:length(n)){
  x <- matrix(rpois(1000*n[i],1.5),1000)
  T1 <- apply(x,1,mean)
  T2=apply(x,1,var)
  Mean_T1[i]=mean(T1)
  Mean_T2[i]=mean(T2)
  Var_T1[i]=var(T1)
  Var_T2[i]=var(T2)
}
cbind(n,Mean_T1,Mean_T2,Var_T1,Var_T2)
hist(T1,col=2)
hist(T2,col=1,add=TRUE)

#Q3 UNIFORM
rm(list=ls())
Y=matrix(runif(1000*30,0,10),ncol=30)
m1=matrix(rep(0,15),ncol=5)
n=c(5,15,30)
df1=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
df2=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
for(i in 1:3)
{ 
  T1=2*rowMeans(Y[,1:n[i]])
  df1[,i]=T1
  m1[i,1]=n[i]
  m1[i,2]=mean(T1)
  m1[i,3]=var(T1)
  
  T2=apply(Y[,1:n[i]],1,max)
  df2[,i]=T2
  m1[i,4]=mean(T2)
  m1[i,5]=var(T2)
  
}

colnames(m1)=list("n","2*Xbar","var(2*Xbar)","X(n)","var(X(n))")
m1
hist(df1[,1],col="red")
hist(df1[,2],col="yellow",add=TRUE)
hist(df1[,3],col="orange",add=TRUE)
legend("topright",legend=c("n=5","n=15","n=30"),fill=c("red","yellow","orange"))

hist(df2[,1],col="red")
hist(df2[,2],col="yellow",add=TRUE)
hist(df2[,3],col="orange",add=TRUE)
legend("topleft",legend=c("n=5","n=15","n=30"),fill=c("red","yellow","orange"))

hist(df1,col="lightgreen",main = "Overlayed histogram of distribution of 2*Xbar and X(n)")
hist(df2,col="lightblue",add=T)


#Q4 GAMMA

sum_xsq=function(x){
  n=length(x)
  sum_xsq=sum(x*x)
  return(sum_xsq/n)
}
alpha=0
beta=0
Y=matrix(rgamma(1000*30,8,1/10),ncol=30)
n=c(5,15,30)
for(i in 1:3)
{ 
  m1=rowMeans(Y[,1:n[i]])
  m2=apply(Y[,1:n[i]],1,sum_xsq)
  T1=(m1^2)/(m2-m1^2)
  beta[i]=mean(T1)
  T2=(m2-m1^2)/m1
  alpha[i]=mean(T2)
}
cbind(n,alpha,beta)



#
set.seed(420)
rm(list=ls())
theta <- 2.5
m1=matrix(rep(0,15),ncol=5)
n=c(5,15,30)

for(i in 1:3)
{ 
 
  Y=matrix(runif(1000*n[i],0,1),1000)
  X=theta - log(1-Y)
  T1=apply(X,1,min)
  T2=apply(X,1,mean)-1
  m1[i,1]=n[i]
  m1[i,2]=mean(T1)
  m1[i,3]=var(T1)
  m1[i,4]=mean(T2)
  m1[i,5]=var(T2)
}
colnames(m1)=list("n","X(1)","var(X(1))","X_bar - 1","V(X_bar - 1)")
m1
hist(T2,col="pink")
hist(T1,add=TRUE,col="lightblue")
boxplot(T1,add=TRUE)
boxplot(T2)


#exponential with locatuin mu and scale sigma
rm(list=ls())
mu <- 2.5
sigma <- 2
m1=matrix(rep(0,15),ncol=5)
n=c(5,15,30)

for(i in 1:3)
{ 
  
  Y=matrix(runif(1000*n[i],0,1),1000)
  X=mu-sigma*log(1-Y)
  T1=apply(X,1,min)
  T2=apply(X,1,mean)-1
  m1[i,1]=n[i]
  m1[i,2]=mean(T1)
  m1[i,3]=var(T1)
  m1[i,4]=mean(T2)
  m1[i,5]=var(T2)
}
colnames(m1)=list("n","X(1)","var(X(1))","X_bar - 1","V(X_bar - 1)")
m1
hist(T2,col="pink")
hist(T1,add=TRUE,col="lightblue")
boxplot(T1,add=TRUE)
boxplot(T2)


#Pareto
rm(list=ls())
theta <- 2.5
m1=matrix(rep(0,15),ncol=5)
n=c(5,15,30)

for(i in 1:3)
{ 
  
  Y=matrix(runif(1000*n[i],0,1),1000)
  X=(1-Y)^(-1/theta)
  T1_=apply(X,1,mean)
  T1= T1_ /(T1_ - 1)
  X1=log(X)
  T_=apply(X1,1,mean)
  T2=1/T_
  m1[i,1]=n[i]
  m1[i,2]=mean(T1)
  m1[i,3]=var(T1)
  m1[i,4]=mean(T2)
  m1[i,5]=var(T2)
}
colnames(m1)=list("n","T1","var(T1)","T2","V(T2)")
m1

hist(T2,col="pink")
hist(T1,col="lightblue",add=T)
#T2 is sufficient statistic





