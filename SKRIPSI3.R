library(readxl)
library(readr)
library(pastecs)
library(ggplot2)
library(gridExtra)
library(pracma)
library(stats)
library(MASS)

#Statistika Deskriptif
data2 <- read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
View(data2)
str(data2)
stat.desc(data2)
summary(data2)
var(data2)

plots <- list()
for (i in 1:4) 
{
  var_name <- paste0("X", i) 
  p <- ggplot(data2, aes_string(x = var_name, y = "Y")) +
    geom_point(color = "blue", alpha = 0.7) +
    labs(
         x = var_name,
         y = "Y") +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
  plots[[i]] <- p
}
grid.arrange(grobs = plots, ncol = 2)

#Scatterplottanpagaris
plots <- list()
for (i in 1:4) 
{
  var_name <- paste0("X", i) 
  p <- ggplot(data2, aes_string(x = var_name, y = "Y")) +
    geom_point(color = "blue", alpha = 0.7) +
    labs(
         x = var_name,
         y = "Y") +
    theme_minimal()
  plots[[i]] <- p
}
grid.arrange(grobs = plots, ncol = 2)

#Scatterplot
plots <- list()
for (i in 1:4) 
{
  var_name <- paste0("X", i) 
  p <- ggplot(data2, aes_string(x = var_name, y = "Y")) +
    geom_point(color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(title = paste("Scatterplot: Y vs", var_name),
         x = var_name,
         y = "Y") +
    theme_minimal()
  plots[[i]] <- p
}
grid.arrange(grobs = plots, ncol = 2)


#GCV Satu Knot
GCV1 = function(para)
{
  start_time <- Sys.time()
  data = read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
  data = as.matrix(data)
  p = length(data[,1])
  q = length(data[1,])
  m = ncol(data)-para-1
  dataA = data[,(para+2):q]
  F = matrix(0,nrow=p,ncol=p)
  diag(F)=1
  nk = length(seq(min(data[,2]),max(data[,2]),length.out=50))
  knot1 = matrix(ncol=m, nrow=nk)
  for (i in (1:m))
  {
    for(j in (1:nk))
    {
      a = seq(min(dataA[,i]),max(dataA[,i]),length.out=50)
      knot1[j,i] = a[j]
    }
  }
  a1=length(knot1[,1])
  knot1=knot1[2:(a1-1),]
  aa=rep(1,p)
  data1=matrix(ncol=m,nrow=p)
  data2=data[,2:q]
  a2=nrow(knot1)
  GCV=rep(NA,a2)
  Rsq=rep(NA,a2)
  for (i in 1:a2)
  {
    for (j in 1:m)
    {
      for (k in 1:p)
      {
        if (data[k,(j+para+1)]<knot1[i,j]) data1[k,j]=0 else data1[k,j]=data[k,(j+para+1)]-knot1[i,j]
      }
    }
    mx=cbind(aa,data2,data1)
    mx=as.matrix(mx)
    C=pinv(t(mx)%*%mx)
    B=C%*%(t(mx)%*%data[,1])
    yhat=mx%*%B
    SSE=0
    SSR=0
    for (r in (1:p))
    {
      sum=(data[r,1]-yhat[r,])^2
      sum1=(yhat[r,]-mean(data[,1]))^2
      SSE=SSE+sum
      SSR=SSR+sum1
    }
    Rsq[i]=(SSR/(SSE+SSR))*100
    MSE=SSE/p
    A=mx%*%C%*%t(mx)
    A1=(F-A)
    A2=(sum(diag(A1))/p)^2
    GCV[i]=MSE/A2
  }
  GCV=as.matrix(GCV)
  Rsq=as.matrix(Rsq)
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  cat("=========================================","\n")
  cat("Nilai Knot dengan Spline Linear 1 Knot", "\n")
  cat("=========================================","\n")
  print(knot1)
  cat("=========================================","\n")
  cat("Rsq dengan Spline Linear 1 Knot", "\n")
  cat("=========================================","\n")
  print(Rsq)
  cat("=========================================","\n")
  cat("HASIL GCV dengan Spline Linear 1 Knot", "\n")
  cat("=========================================","\n")
  print(GCV)
  s1=min(GCV)
  print(max(Rsq))
  cat("=========================================","\n")
  cat("HASIL GCV terkecil dengan spline linear 1 knot", "\n")
  cat("=========================================","\n")
  cat("GCV=",s1,"\n")
  cat("=========================================","\n")
  cat("MSE=", MSE, "\n")
  cat("=========================================","\n")
  cat("Waktu Eksekusi", "\n")
  cat("=========================================","\n")
  print(execution_time)
  cat("=========================================","\n")
  hasil=data.frame(GCV,Rsq,knot1)
  write.csv(hasil,file = "C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output knot1.csv")
}
GCV1(0)

# Load data
KNOT1V <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output knot1.csv")
View(KNOT1V)

#CGV Dua Knot
GCV2 = function(para)
{
  data = read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
  data = as.matrix(data)
  p = length(data[,1])
  q = length(data[1,])
  m = ncol(data)-1
  F = matrix(0,nrow=p,ncol=p)
  diag(F)=1
  nk = length(seq(min(data[,2]),max(data[,2]),length.out=50))
  knot = matrix(ncol=m, nrow=nk)
  for (i in (1:m))
  {
    for (j in (1:nk))
    {
      a=seq(min(data[,(i+1)]), max(data[,(i+1)]), length.out=50)
      knot[j,i]=a[j]
    }
  }
  z=(nk*(nk-1)/2)
  knot2=cbind(rep(NA,(z+1)))
  for (i in (1:m))
  {
    knot1=rbind(rep(NA,2))
    for (j in 1:(nk-1))
    {
      for (k in (j+1):nk)
      {
        xx=cbind(knot[j,i],knot[k,i])
        knot1=rbind(knot1,xx)
      }
    }
    knot2=cbind(knot2,knot1)
  }
  knot2=knot2[2:(z+1),2:(2*m+1)]
  aa=rep(1,p)
  data2=matrix(ncol=(2*m),nrow=p)
  data1=data[,2:q]
  a1=length(knot2[,1])
  GCV=rep(NA,a1)
  Rsq=rep(NA,a1)
  MSE=rep(NA,a1)
  for (i in 1:a1)
  {
    for (j in 1:(2*m))
    {
      if (mod(j,2)==1) b=floor(j/2)+1 else b=j/2
      for (k in 1:p)
      {
        if (data1[k,b]<knot2[i,j]) data2[k,j]=0 else data2[k,j]=data1[k,b] - knot2[i,j]
      }
    }
    mx=cbind(aa, data1, data2)
    mx=as.matrix(mx)
    C=pinv(t(mx)%*%mx)
    B=C%*%(t(mx)%*%data[,1])
    yhat=mx%*%B
    SSE=0
    SSR=0
    for (r in (1:p))
    {
      sum=(data[r,1]-yhat[r,])^2
      sum1=(yhat[r,]-mean(data[,1]))^2
      SSE=SSE+sum
      SSR=SSR+sum1
    }
    Rsq[i]=(SSR/(SSE+SSR))*100
    MSE=SSE/p
    A=mx%*%C%*%t(mx)
    A1=(F-A)
    A2=(sum(diag(A1))/p)^2
    GCV[i]=MSE/A2
  }
  GCV=as.matrix(GCV)
  Rsq=as.matrix(Rsq)
  cat("=========================================","\n")
  cat("Nilai Knot dengan Spline Linear 2 Knot", "\n")
  cat("=========================================","\n")
  print(knot2)
  cat("=========================================","\n")
  cat("Rsq dengan Spline Linear 2 Knot", "\n")
  cat("=========================================","\n")
  print(Rsq)
  cat("=========================================","\n")
  cat("HASIL GCV dengan Spline Linear 2 Knot", "\n")
  cat("=========================================","\n")
  print(GCV)
  s1=min(GCV)
  cat("=========================================","\n")
  cat("HASIL GCV terkecil dengan Spline Linear 2 Knot", "\n")
  cat("=========================================","\n")
  cat(" GCV =",s1,"\n")
  hasil=data.frame(GCV, Rsq, knot2)
  write.csv(hasil,file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output knot2.csv")
}
GCV2(0)

# Load data
KNOT2V <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output knot2.csv")
View(KNOT2V)

#GCV Tiga Knot
GCV3=function(para)
{
  data=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
  data=as.matrix(data)
  p=length(data[,1])
  q=length(data[1,])
  m=ncol(data)-para-1
  F=matrix(0,nrow = p,ncol=p)
  dataA=data[,(para+2):q]
  diag(F)=1
  nk=length(seq(min(data[,2]),max(data[,2]),length.out=50))
  knot=matrix(ncol=m,nrow=nk)
  for (i in (1:m))
  {
    for (j in (1:nk))
    {
      a=seq(min(dataA[,i]),max(dataA[,i]),length.out=50)
      knot[j,i]=a[j]
    }
  }
  knot=knot[2:(nk-1),]
  a2=nrow(knot)
  z=(a2*(a2-1)*(a2-2)/6)
  knot1=cbind(rep(NA,(z+1)))
  for (i in (1:m))
  {
    knot2=rbind(rep(NA,3))
    for (j in 1:(a2-2))
    {
      for (k in (j+1):(a2-1))
      {
        for (g in (k+1):a2)
        {
          xx=cbind(knot[j,i],knot[k,i],knot[g,i])
          knot2=rbind(knot2,xx)
        }
      }
    }
    knot1=cbind(knot1,knot2)
  }
  knot1=knot1[2:(z+1),2:(3*m+1)]
  aa=rep(1,p)
  data1=matrix(ncol=(3*m),nrow=p)
  data2=data[,(para+2):q]
  a1=length(knot1[,1])
  GCV=rep(NA,a1)
  Rsq=rep(NA,a1)
  MSE=rep(NA,a1)
  for (i in 1:a1)
  {
    for (j in 1:ncol(knot1))
    {
      b=ceiling(j/3)
      for (k in 1:p)
      {
        if (data2[k,b]<knot1[i,j]) data1[k,j]=0 else data1[k,j]=data2[k,b] - knot1[i,j]
      }
    }
    mx=cbind(aa,data[,2:q],data1)
    mx=as.matrix(mx)
    C=pinv(t(mx)%*%mx)
    B=C%*%(t(mx)%*%data[,1])
    yhat=mx%*%B
    SSE=0
    SSR=0
    for (r in (1:p))
    {
      sum=(data[r,1]-yhat[r,])^2
      sum1=(yhat[r,]-mean(data[,1]))^2
      SSE=SSE+sum
      SSR=SSR+sum1
    }
    Rsq[i]=(SSR/(SSE+SSR))*100
    MSE=SSE/p
    A=mx%*%C%*%t(mx)
    A1=(F-A)
    A2=(sum(diag(A1))/p)^2
    GCV[i]=MSE/A2
  }
  GCV=as.matrix(GCV)
  Rsq=as.matrix(Rsq)
  cat("=========================================","\n")
  cat("Nilai Knot dengan Spline Linear 3 Knot", "\n")
  cat("=========================================","\n")
  print(knot1)
  cat("=========================================","\n")
  cat("Rsq dengan Spline Linear 3 Knot", "\n")
  cat("=========================================","\n")
  print(Rsq)
  r=max(Rsq)
  print(r)
  cat("=========================================","\n")
  cat("HASIL GCV dengan Spline Linear 3 Knot", "\n")
  cat("=========================================","\n")
  print(GCV)
  s1=min(GCV)
  cat("=========================================","\n")
  cat("HASIL GCV terkecil dengan Spline Linear 3 Knot", "\n")
  cat("=========================================","\n")
  cat(" GCV =",s1,"\n")
  cat("=========================================","\n")
  hasil=data.frame(GCV,Rsq,knot1)
  write.csv(hasil, file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output knot3.csv")
}
GCV3(0)

# Load Data
KNOT3V <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output knot3.csv")
View(KNOT3V)

#GCV Kombinasi Knot Asli
GCVKK=function(para)
{
  data=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
  data=as.matrix(data)
  p1=length(data[,1])
  q1=length(data[1,])
  v=para+2
  F=matrix(0,nrow=p1,ncol=p1)
  diag(F)=1
  x1=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/x1.xlsx")
  x2=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/x2.xlsx")
  x3=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/x3.xlsx")
  x4=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/x4.xlsx")
  n2=nrow(x1)
  a=matrix(nrow = 4, ncol = 3^4)
  m=0
  for (i in 1:3)
    for (j in 1:3)
      for (k in 1:3)
        for (l in 1:3)
          {
            m=m+1
            a[,m]=c(i,j,k,l)
          }
  a=t(a)
  GCV=matrix(nrow=nrow(x1),ncol=3^4)
  for (i in 1:3^4)
  {
    for (h in 1:nrow(x1))
    {
      if (a[i,1]==1)
      {
        gab=as.matrix(x1[,1])
        gen=as.matrix(data[,v])
        aa=matrix(nrow=nrow(x1)*nrow(data),ncol=1)
        for (j in 1:1)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) aa[w,j]=0 else aa[w,j]=gen[w,j]-gab[h,j]
          }
      }
      else 
        if (a[i,j]==2)
        {
          gab=as.matrix(x1[,2:3])
          gen=as.matrix(cbind(data[,v],data[,v]))
          aa=matrix(nrow=nrow(x1)*nrow(data),ncol=2)
          for (j in 1:2)
            for (w in 1:nrow(data))
            {
              if (gen[w,j]<gab[h,j]) aa[w,j]=0 else aa[w,j]=gen[w,j]-gab[h,j]
            }
        }
      else
      {
        gab=as.matrix(x1[,4:6])
        gen=as.matrix(cbind(data[,v],data[,v],data[,v]))
        aa=matrix(nrow=nrow(x1)*nrow(data),ncol=3)
        for (j in 1:3)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) aa[w,j]=0 else aa[w,j]=gen[w,j]-gab[h,j]
          }
      }
      if (a[i,2]==1)
      {
        gab=as.matrix(x2[,1])
        gen=as.matrix(data[,(v+1)])
        bb=matrix(nrow=nrow(x1)*nrow(data),ncol=1)
        for (j in 1:1)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) bb[w,j]=0 else bb[w,j]=gen[w,j]-gab[h,j]
          }
      }
      else
        if (a[i,2]==2)
        {
          gab=as.matrix(x2[,2:3])
          gen=as.matrix(cbind(data[,(v+1)],data[,(v+1)]))
          bb=matrix(nrow=nrow(x1)*nrow(data),ncol=2)
          for (j in 1:2)
            for (w in 1:nrow(data))
            {
              if (gen[w,j]<gab[h,j]) bb[w,j]=0 else bb[w,j]=gen[w,j]-gab[h,j]
            }
        }
      else
      {
        gab=as.matrix(x2[,4:6])
        gen=as.matrix(cbind(data[,(v+1)],data[,(v+1)],data[,(v+1)]))
        bb=matrix(nrow=nrow(x1)*nrow(data),ncol=3)
        for (j in 1:3)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) bb[w,j]=0 else bb[w,j]=gen[w,j]-gab[h,j]
          }
      }
      if (a[i,3]==1)
      {
        gab=as.matrix(x3[,1])
        gen=as.matrix(data[,(v+2)])
        cc=matrix(nrow=nrow(x1)*nrow(data),ncol=1)
        for (j in 1:1)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) cc[w,j]=0 else cc[w,j]=gen[w,j]-gab[h,j]
          }
      }
      else
        if (a[i,3]==2)
        {
          gab=as.matrix(x3[,2:3])
          gen=as.matrix(cbind(data[,(v+2)],data[,(v+2)]))
          cc=matrix(nrow=nrow(x1)*nrow(data),ncol=2)
          for (j in 1:2)
            for (w in 1:nrow(data))
            {
              if (gen[w,j]<gab[h,j]) cc[w,j]=0 else cc[w,j]=gen[w,j]-gab[h,j]
            }
        }
      else
      {
        gab=as.matrix(x3[,4:6])
        gen=as.matrix(cbind(data[,(v+2)],data[,(v+2)],data[,(v+2)]))
        cc=matrix(nrow=nrow(x1)*nrow(data),ncol=3)
        for (j in 1:3)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) cc[w,j]=0 else cc[w,j]=gen[w,j]-gab[h,j]
          }
      }
      if(a[i,4]==1)
      {
        gab=as.matrix(x4[,1])
        gen=as.matrix(data[,(v+3)])
        dd=matrix(nrow=nrow(x1)*nrow(data),ncol=1)
        for (j in 1:1)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) dd[w,j]=0 else dd[w,j]=gen[w,j]-gab[h,j]
          }
      }
      else
        if (a[i,4]==2)
        {
          gab=as.matrix(x4[,2:3])
          gen=as.matrix(cbind(data[,(v+3)],data[,(v+3)]))
          dd=matrix(nrow=nrow(x1)*nrow(data),ncol=2)
          for (j in 1:2)
            for (w in 1:nrow(data))
            {
              if (gen[w,j]<gab[h,j]) dd[w,j]=0 else dd[w,j]=gen[w,j]-gab[h,j]
            }
        }
      else
      {
        gab=as.matrix(x4[,4:6])
        gen=as.matrix(cbind(data[,(v+3)],data[,(v+3)],data[,(v+3)]))
        dd=matrix(nrow=nrow(x1)*nrow(data),ncol=3)
        for (j in 1:3)
          for (w in 1:nrow(data))
          {
            if (gen[w,j]<gab[h,j]) dd[w,j]=0 else dd[w,j]=gen[w,j]-gab[h,j]
          }
      }
      ma=as.matrix(cbind(aa,bb,cc,dd))
      mx=cbind(rep(1,nrow(data)),data[,2:q1],na.omit(ma))
      mx=as.matrix(mx)
      C=pinv(t(mx)%*%mx)
      B=C%*%(t(mx)%*%data[,1])
      yhat=mx%*%B
      SSE=0
      SSR=0
      for (r in 1:nrow(data))
      {
        sum=(data[r,1]-yhat[r,])^2
        sum1=(yhat[r,]-mean(data[,1]))^2
        SSE=SSE+sum
        SSR=SSR+sum1
      }
      Rsq=(SSR/(SSE+SSR))*100
      MSE=SSE/p1
      A=mx%*%C%*%t(mx)
      A1=(F-A)
      A2=(sum(diag(A1))/p1)^2
      GCV[h,i]=MSE/A2
    }
    if (a[i,1]==1) sp=x1[,1] else
      if (a[i,1]==2) sp=x1[,2:3] else
        sp=x1[,4:6]
      if (a[i,2]==1) spl=x2[,1] else
        if (a[i,2]==2) spl=x2[,2:3] else
          spl=x2[,4:6]
        if (a[i,3]==1) splin=x3[,1] else
          if (a[i,3]==2) splin=x3[,2:3] else
            splin=x3[,4:6]
          if (a[i,4]==1) spline=x4[,1] else
            if (a[i,4]==2) spline=x4[,2:3] else
              spline=x4[,4:6]
              kkk=cbind(sp,spl,splin,spline)
              cat("=========================================","\n")
              print(i)
              print(kkk)
              print(Rsq)
  }
  write.csv(GCV, file="C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/output GCV kombinasi.csv")
  write.csv(Rsq, file="C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/output Rsq kombinasi.csv")
}
GCVKK(0)

#Load Kombinasi Knot
GCVKOM <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/output GCV kombinasi.csv")
View(GCVKOM)
RsqKOM <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/output Rsq kombinasi.csv")
View(RsqKOM)

#GCV Kombinasi
GCVKOM <- function(para) 
{
  # Read and prepare data
  data <- as.matrix(read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx"))
  p1 <- nrow(data)
  q1 <- ncol(data)
  v <- para + 2
  
  # Initialize F matrix
  F <- diag(p1)
  
  # Read X matrices
  x_files <- list(
    x1=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/x1.xlsx"),
    x2=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/x2.xlsx"),
    x3=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/x3.xlsx"),
    x4=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/x4.xlsx")
  )
  
  n2 <- nrow(x_files$x1)
  
  # Generate combinations matrix
  combinations <- expand.grid(1:3, 1:3, 1:3, 1:3)
  colnames(combinations) <- paste0("x", 1:4)
  
  # Initialize results matrices
  GCV <- matrix(nrow=n2, ncol=nrow(combinations))
  Rsq_results <- numeric(nrow(combinations))
  
  # Create a list to store knot information
  knot_info <- list()
  
  # Helper function to get knots
  get_knots <- function(x_value, x_data) {
    if (x_value == 1) {
      return(as.matrix(x_data[, 1, drop=FALSE]))
    } else if (x_value == 2) {
      return(as.matrix(x_data[, 2:3]))
    } else {
      return(as.matrix(x_data[, 4:6]))
    }
  }
  
  # Helper function to process X matrices
  process_x_matrix <- function(x_value, x_data, data_col, h) {
    if (x_value == 1) {
      gab <- as.matrix(x_data[, 1, drop=FALSE])
      gen <- as.matrix(data[, data_col])
      cols <- 1
    } else if (x_value == 2) {
      gab <- as.matrix(x_data[, 2:3])
      gen <- as.matrix(cbind(data[, data_col], data[, data_col]))
      cols <- 2
    } else {
      gab <- as.matrix(x_data[, 4:6])
      gen <- as.matrix(cbind(data[, data_col], data[, data_col], data[, data_col]))
      cols <- 3
    }
    
    result <- matrix(0, nrow=nrow(data), ncol=cols)
    for (j in 1:cols) {
      result[, j] <- ifelse(gen[, j] < gab[h, j], 0, gen[, j] - gab[h, j])
    }
    return(result)
  }
  
  # Main loop through combinations
  for (i in 1:nrow(combinations)) {
    current_combo <- as.numeric(combinations[i, ])
    current_knots <- list()
    
    # Store knot information for current iteration
    for (x_idx in 1:4) {
      knots <- get_knots(current_combo[x_idx], x_files[[x_idx]])
      current_knots[[x_idx]] <- knots
    }
    
    knot_info[[i]] <- list(
      iteration = i,
      combination = current_combo,
      knots = current_knots
    )
    
    for (h in 1:n2) {
      # Process each X matrix
      processed_matrices <- list()
      for (x_idx in 1:4) {
        processed_matrices[[x_idx]] <- process_x_matrix(
          current_combo[x_idx],
          x_files[[x_idx]],
          v + x_idx - 1,
          h
        )
      }
      
      # Combine processed matrices
      ma <- do.call(cbind, processed_matrices)
      mx <- cbind(1, data[, 2:q1], na.omit(ma))
      
      # Calculate coefficients and predictions
      C <- pinv(t(mx) %*% mx)
      B <- C %*% (t(mx) %*% data[, 1])
      yhat <- mx %*% B
      
      # Calculate SSE and SSR
      residuals <- data[, 1] - yhat
      SSE <- sum(residuals^2)
      predictions_centered <- yhat - mean(data[, 1])
      SSR <- sum(predictions_centered^2)
      
      # Calculate R-squared
      Rsq_results[i] <- (SSR/(SSE + SSR)) * 100
      
      # Calculate MSE and GCV
      MSE <- SSE/p1
      A <- mx %*% C %*% t(mx)
      A1 <- (F - A)
      A2 <- (sum(diag(A1))/p1)^2
      GCV[h, i] <- MSE/A2
    }
    
    # Print progress
    cat("Processing iteration", i, "of", nrow(combinations), "\n")
  }
  
  # Find minimum GCV and corresponding combination
  min_GCV <- min(GCV, na.rm = TRUE)
  min_GCV_indices <- which(GCV == min_GCV, arr.ind = TRUE)
  best_combination <- combinations[min_GCV_indices[1,2], ]
  
  # Add column names to GCV matrix
  colnames(GCV) <- paste0("Combination_", 1:ncol(GCV))
  rownames(GCV) <- paste0("Knot_", 1:nrow(GCV))
  
  # Create R-squared data frame
  Rsq_df <- data.frame(
    Combination = 1:length(Rsq_results),
    Rsq = Rsq_results
  )
  
  # Create knot output data frame
  knot_output <- data.frame(
    Iteration = integer(),
    Variable = character(),
    Knot_Number = integer(),
    Knot_Values = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(knot_info)) {
    for (j in 1:4) {
      knot_values <- knot_info[[i]]$knots[[j]]
      knot_output <- rbind(knot_output, data.frame(
        Iteration = i,
        Variable = paste0("X", j),
        Knot_Number = knot_info[[i]]$combination[j],
        Knot_Values = paste(format(round(as.numeric(knot_values), 4), nsmall = 4), collapse = ", "),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Print conclusion
  cat("\n========== CONCLUSION ==========\n")
  cat("Minimum GCV:", min_GCV, "\n")
  cat("Best combination (number of knots for each variable):", "\n")
  print(best_combination)
  cat("\nR-squared for best model:", Rsq_results[min_GCV_indices[1,2]], "%\n")
  
  # Write results to separate files
  write.csv(GCV, file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output_GCVfix.csv", row.names=TRUE)
  write.csv(Rsq_df, file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output_Rsqfix.csv", row.names=FALSE)
  write.csv(knot_output, file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output_knotsfix.csv", row.names=FALSE)
  
  # Return all results
  return(list(
    GCV = GCV,
    Rsq = Rsq_df,
    knots = knot_output,
    best_combination = best_combination,
    minimum_GCV = min_GCV
  ))
}
GCVKOM(0)

#Load Kombinasi Knot
kombinasifix <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output_GCVfix.csv")
View(kombinasifix)
rsqfix <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output_Rsqfix.csv")
View(rsqfix)
knotsfix <- read_csv("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/output_knotsfix.csv")
View(knotsfix)

#Uji Serentak dan Parsial
uji=function(para=0)
{
  data=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
  knot=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/knot3.xlsx")
  data=as.matrix(data)
  knot=as.matrix(knot)
  ybar=mean(data[,1])
  m=para+2
  p=nrow(data)
  q=ncol(data)
  
  dataA=cbind(data[,m],data[,m],data[,m],data[,m+1],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+2],data[,m+3],data[,m+3],data[,m+3])
  dataA=as.matrix(dataA)
  satu=rep(1,p)
  n1=ncol(knot)
  data.knot=matrix(ncol=n1,nrow=p)
  for (i in 1:n1)
  {
    for (j in 1:p)
    {
      if (dataA[j,i]<knot[1,i]) data.knot[j,i]=0 else data.knot[j,i]=dataA[j,i]-knot[1,i]
    }
  }
  mx=cbind(satu,data[,2],data.knot[,1:3],data[,3],data.knot[,4:6],data[,4],data.knot[,7:9],data[,5],data.knot[,10:12])
  mx=as.matrix(mx)
  B=(pinv(t(mx)%*%mx))%*%t(mx)%*%data[,1]
  cat("=========================================","\n")
  cat("Estimasi Parameter OLS","\n")
  cat("=========================================","\n")
  print(B)
  n1=nrow(B)
  yhat=mx%*%B
  res=data[,1]-yhat
  SSE=sum((data[,1]-yhat)^2)
  SSR=sum((yhat-ybar)^2)
  SST=SSR+SSE
  MSE=SSE/(p-n1)
  MSR=SSR/(n1-1)
  Rsq=(SSR/(SSR+SSE))*100
  alpha <- 0.05
  Fhit=MSR/MSE
  pvalue=pf(Fhit,(n1-1),(p-n1),lower.tail = FALSE)
  if (pvalue<=alpha)
  {
    cat("=========================================","\n")
    cat("Kesimpulan hasil uji serentak","\n")
    cat("=========================================","\n")
    cat("Tolak Ho yakni minimal terdapat 1 prediktor yang signifikan","\n")
    cat("","\n")
  }
  else
  {
    cat("=========================================","\n")
    cat("Kesimpulan hasil uji serentak","\n")
    cat("=========================================","\n")
    cat("gagal tolak Ho yakni semua prediktor tidak berpengaruh","\n")
    cat("","\n")
  }
  
  #uji t (uji individu)
  thit=rep(NA,n1)
  pval=rep(NA,n1)
  SE=sqrt(diag(MSE*(pinv(t(mx)%*%mx))))
  cat("=========================================","\n")
  cat("Kesimpulan hasil uji individu","\n")
  cat("=========================================","\n")
  thit=rep(NA,n1)
  pval=rep(NA,n1)
  for (i in 1:n1)
  {
    thit[i]=B[i,1]/SE[i]
    pval[i]=2*(pt(abs(thit[i]),(p-n1),lower.tail=FALSE))
    if (pval[i]<=alpha) cat("Tolak HO yakni prediktor signifikan dengan pvalue", pval[i],"\n") else cat("gagal tolak ho yakni prediktor tidak signifikan dengan pvalue", pval[i],"\n")
  }
  thit=as.matrix(thit)
  cat("=========================================","\n")
  cat("Nilai t hitung", "\n")
  cat("=========================================","\n")
  print(thit)
  cat("          Analysis of Variance", "\n")
  cat("=========================================","\n")
  cat("Sumber    df     SS        MS     Fhit", "\n")
  cat("Regresi ",(n1-1),"",SSR,"",MSR,"",Fhit,"\n")
  cat("Eror    ",p-n1,"",SSE,"",MSE,"\n")
  cat("Total   ",p-1,"",SST,"\n")
  cat("=========================================","\n")
  cat("s=",sqrt(MSE)," Rsq=",Rsq," Rsq Adj=",Rsq_adj,"\n")
  cat("pvalue(F)=",pvalue,"\n")
  write.csv(res,file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/uji residual.csv")
  write.csv(pval,file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/uji pvalue.csv")
  write.csv(mx,file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/uji mx knot.csv")
  write.csv(yhat,file="C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/uji yhat knot.csv")
  model <- list(
    coefficients = B,      # Koefisien regresi
    knot = knot,            # Posisi knot
    mx = mx,                # Matriks desain
    MSE = MSE,              # Mean Squared Error
    formula = "y ~ spline_truncated(X)",  # Deskripsi model (opsional)
    data_columns = colnames(data)[m:(m+3)]  # Kolom prediktor yang digunakan
  )
  return(model)
}
uji(0)
model_spline = uji(0)
save(model_spline, file = "C:/Users/acer/Downloads/Data/model_spline.RData")

resi=read.csv("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/uji residual.csv")
View(resi)

#Interval Konfidensi
interval=function(alpha,para)
{
  data=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
  knot=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/knot3.xlsx")
  data=as.matrix(data)
  knot=as.matrix(knot)
  ybar=mean(data[,1])
  m=para+2
  p=nrow(data)
  q=ncol(data)
  
  dataA=cbind(data[,m],data[,m],data[,m],data[,m+1],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+2],data[,m+3],data[,m+3],data[,m+3])
  dataA=as.matrix(dataA)
  satu=rep(1,p)
  n1=ncol(knot)
  data.knot=matrix(ncol=n1,nrow=p)
  for (i in 1:n1)
  {
    for (j in 1:p)
    {
      if (dataA[j,i]<knot[1,i]) data.knot[j,i]=0 else data.knot[j,i]=dataA[j,i]-knot[1,i]
    }
  }
  mx=cbind(satu,data[,2],data.knot[,1:3],data[,3],data.knot[,4:6],data[,4],data.knot[,7:9],data[,5],data.knot[,10:12])
  mx=as.matrix(mx)
  B=(pinv(t(mx)%*%mx))%*%t(mx)%*%data[,1]
  cat("=========================================","\n")
  cat("estimasi parameter","\n")
  cat("=========================================","\n")
  print(B)
  n1=nrow(B)
  yhat=mx%*%B
  res=data[,1]-yhat
  SSE=sum((data[,1]-yhat)^2)
  SSR=sum((yhat-ybar)^2)
  SST=SSR+SSE
  MSE=SSE/(p-n1)
  MSR=SSR/(n1-1)
  Rsq=(SSR/(SSR+SSE))*100
  
  #uji F
  alpha <- 0.05
  Fhit=MSR/MSE
  pvalue=pf(Fhit,(n1-1),(p-n1),lower.tail = FALSE)
  if (pvalue<=alpha)
  {
    cat("=========================================","\n")
    cat("kesimpulan hasil uji serentak","\n")
    cat("=========================================","\n")
    cat("Tolak Ho yakni minimal terdapat 1 prediktor yang signifikan","\n")
    cat("","\n")
  }
  else
  {
    cat("=========================================","\n")
    cat("Kesimpulan hasil uji serentak","\n")
    cat("=========================================","\n")
    cat("gagal tolak Ho yakni semua prediktor tidak berpengaruh signifikan","\n")
    cat("","\n")
  }
  
  #uji t (uji individu)
  thit=rep(NA,n1)
  pval=rep(NA,n1)
  SE=sqrt(diag(MSE*(pinv(t(mx)%*%mx))))
  cat("=========================================","\n")
  cat("Kesimpulan hasil uji individu","\n")
  cat("=========================================","\n")
  thit=rep(NA,n1)
  pval=rep(NA,n1)
  for (i in 1:n1)
  {
    thit[i]=B[i,1]/SE[i]
    pval[i]=2*(pt(abs(thit[i]),(p-n1),lower.tail=FALSE))
    if (pval[i]<=alpha) cat("Tolak HO yakni prediktor signifikan dengan pvalue", pval[i],"\n") else cat("gagal tolak ho yakni prediktor tidak signifikan dengan pvalue", pval[i],"\n")
  }
  thit=as.matrix(thit)
  cat("=========================================","\n")
  cat("nilai t hitung", "\n")
  cat("=========================================","\n")
  print(thit)
  cat("analysis of variance", "\n")
  cat("=========================================","\n")
  cat("Sumber    df     SS        MS     Fhit", "\n")
  cat("Regresi ",(n1-1),"",SSR,"",MSR,"",Fhit,"\n")
  cat("Eror    ",p-n1,"",SSE,"",MSE,"\n")
  cat("Total   ",p-1,"",SST,"\n")
  cat("=========================================","\n")
  cat("s=",sqrt(MSE)," Rsq=",Rsq,"\n")
  cat("pvalue(F)=",pvalue,"\n")
  
  #interval konfidensi
  ba=rep(NA,n1)
  bb=rep(NA,n1)
  SE=sqrt(diag(MSE*(pinv(t(mx)%*%mx))))
  for (i in 1:n1)
  {
    ba[i]=B[i,1]+(2.05*SE[i])
    bb[i]=B[i,1]-(2.05*SE[i])
  }
  ba=as.matrix(ba)
  bb=as.matrix(bb)
  ik=cbind(bb,ba,B)
  cat("============Confidence Interval============","\n")
  cat("=====batas bawah   batas atas   estimasi parameter=====", "\n")
  print(ik)
  write.csv(bb,file="C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/bb.csv")
  write.csv(ba,file="C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/ba.csv")
}
interval(0.05,0)

#Uji Glejser
glejser=function(data,knot,res,alpha,para=0)
{
  data=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/DATADATABARU.xlsx")
  knot=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/knot3.xlsx")
  res=read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/residual.xlsx")
  data=as.matrix(data)
  knot=as.matrix(knot)
  res=abs(res)
  res=as.matrix(res)
  rbar=mean(res)
  m=para+2
  p=nrow(data)
  q=ncol(data)
  dataA=cbind(data[,m],data[,m],data[,m],data[,m+1],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+2],data[,m+3],data[,m+3],data[,m+3])
  dataA=as.matrix(dataA)
  satu=rep(1,p)
  n1=ncol(knot)
  data.knot=matrix(ncol=n1,nrow=p)
  for (i in 1:n1)
  {
    for (j in 1:p)
    {
      if (dataA[j,i]<knot[1,i]) data.knot[j,i]=0 else data.knot[j,i]=dataA[j,i]-knot[1,i]
    }
  }
  mx=cbind(satu,data[,2],data.knot[,1:3],data[,3],data.knot[,4:6],data[,4],data.knot[,7:9],data[,5],data.knot[,10:12])
  mx=as.matrix(mx)
  B=(ginv(t(mx)%*%mx))%*%t(mx)%*%res
  n1=nrow(B)
  yhat=mx%*%B
  residual=res-yhat
  SSE=sum((res-yhat)^2)
  SSR=sum((yhat-rbar)^2)
  SST=SSR+SSE
  MSE=SSE/(p-n1)
  MSR=SSR/(n1-1)
  Rsq=(SSR/SST)*100
  
  #uji F (uji serentak)
  alpha <- 0.05
  Fhit=MSR/MSE
  pvalue=pf(Fhit,(n1-1),(p-n1),lower.tail = FALSE)
  if (pvalue<=alpha)
  {
    cat("=========================================","\n")
    cat("kesimpulan hasil uji serentak","\n")
    cat("=========================================","\n")
    cat("Tolak Ho yakni minimal terdapat 1 prediktor yang sign atau terjadi heteroskedastisitas","\n")
    cat("","\n")
  }
  else
  {
    cat("=========================================","\n")
    cat("Kesimpulan hasil uji serentak","\n")
    cat("=========================================","\n")
    cat("gagal tolak Ho yakni semua prediktor tidak berpengaruh atau tidak terjadi heteroskedastisitas","\n")
    cat("","\n")
  }
  cat("analysis of variance", "\n")
  cat("=========================================","\n")
  cat("Sumber   df     SS          MS      Fhit", "\n")
  cat("Regresi ",(n1-1),"",SSR,"",MSR,"",Fhit,"\n")
  cat("Eror    ",p-n1,"",SSE,"",MSE,"\n")
  cat("Total   ",p-1,"",SST,"\n")
  cat("=========================================","\n")
  cat("pvalue(F)=",pvalue,"\n")
}
glejser(0)

#Uji Durbin Watson
durbin_watson_test=function() 
{
  # Read data
  res <- read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI/SKRIPSI 2/SKRIPSI3/residual.xlsx")
  
  # Get residuals from first column
  residuals <- as.numeric(res[[1]])
  cat("\nResiduals Diagnostics:\n")
  cat("=====================\n")
  cat("Number of valid residuals:", length(residuals), "\n")
  cat("Mean of residuals:", mean(residuals), "\n")
  cat("Standard deviation:", sd(residuals), "\n")
  cat("Min value:", min(residuals), "\n")
  cat("Max value:", max(residuals), "\n")
  
  # Calculate Durbin-Watson statistic
  sum_squared_diff <- sum(diff(residuals)^2)
  sum_squared_res <- sum(residuals^2)
  dw_statistic <- sum_squared_diff / sum_squared_res
  
  # Set critical values based on sample size
  n <- length(residuals)
  if (n <= 15) {
    dl <- 1.08
    du <- 1.36
  } else if (n <= 25) {
    dl <- 1.20
    du <- 1.41
  } else if (n <= 35) {
    dl <- 1.30
    du <- 1.49
  } else {
    dl <- 1.2614
    du <- 1.7223
  }
  
  # Determine conclusion
  if (dw_statistic < dl) {
    conclusion <- "Tidak terdapat autokorelasi positif"
  } else if (dw_statistic > (4 - dl)) {
    conclusion <- "Tidak terdapat autokorelasi negatif"
  } else if (dl <= dw_statistic && dw_statistic <= (4 - du)) {
    conclusion <- "Tidak terdapat autokorelasi"
  } else {
    conclusion <- "Pengujian tidak dapat disimpulkan"
  }
  
  # Print results
  cat("\nDurbin-Watson Test Results:\n")
  cat("===========================\n")
  cat(sprintf("Durbin-Watson Statistic: %.4f\n", dw_statistic))
  cat(sprintf("Lower Critical Value: %.4f\n", dl))
  cat(sprintf("Upper Critical Value: %.4f\n", du))
  cat("Interpretasi:", conclusion, "\n")
}
durbin_watson_test()

#UJI DISTRIBUSI NORMAL KOLMOGOROV SMIRNOV
kolmogorovsmirnov=function(residuals)
{
  data <- read_excel("C:/Users/acer/OneDrive/Documents/SKRIPSI 2/SKRIPSI3/residual.xlsx")
  residuals <- data$res
  ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
  
  # Kolmogorov-Smirnov plot with colored points and line
  ks_plot <- ggplot(data.frame(residuals), aes(sample = residuals)) +
    stat_qq_line(color = "red", linewidth = 1) +
    stat_qq(color = "blue", size=3) +  # Blue points for residuals
    labs(title = "Kolmogorov-Smirnov Test",
         x = "Res",
         y = "Cumulative Probability",
         caption = paste("KS =", round(ks_test$statistic, 4), 
                         ", p-value =", round(ks_test$p.value, 4),
                         ", mean=", mean(residuals),
                         ", sd=", round(sd(residuals),4))) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0.5)  # Center the caption
    )
  
  print(ks_plot)
}
kolmogorovsmirnov()
