# libraries 
library("rminer")

# load data
load("sa_ssin_n2p.rda")

data <- sa_ssin_n2p

m <- fit(
  y~.,
  data = data,
  model = "ksvm",
  kernel = "rbfdot",
  scale = "none",
  sigma = 2^-8.25,
  C = 2^10
  )

I = Importance(
  m,
  data,
  method = "GSA",
  interactions = 1:4
)

cm = agg_matrix_imp(I)

print(round(cm$m1,digits=2))
print(round(cm$m2,digits=2))
fcm=cmatrixplot(cm,threshold=0.1) 


### ----- single input importance

Inps = length(I$inputs)
val = vector(length=Inps)

for (a in 1:Inps) {
  
  AT <- a
  x = I$sresponses[[1]]$x
  y = I$sresponses[[1]]$y
  X1 = unique(x[,AT[1]])
  
  if(length(AT)>1) { # not our case I SUPOSE
    
    # X2=unique(x[,AT[2]])
    # LX=length(X2)*length(X1)
    # 
    # if(is.matrix(y)) my = matrix(ncol=NCOL(y),nrow=LX)
    # 
    # Im=vector(length=LX)
    # k=1;
    # for(i in X1)
    #   for(j in X2)
    #   {
    #     W=which(x[,AT[1]]==i & x[,AT[2]]==j)
    #     Im[k]=W[1]
    #     if(is.matrix(y)) my[k,]=colMeans(y[W,])
    #     else my[k]=mean_resp(y[W])
    #     k=k+1
    #   }
    
  } else if (length(AT)==1) {
    
    LX=length(X1)
    
    if(is.matrix(y)) {
      my = matrix(
        ncol=NCOL(y),
        nrow=LX
      )
    } else if(is.factor(y)){
      my=factor(rep(levels(y)[1],LX),levels=levels(y))
    }  else {
      my=vector(length=LX)
    }
    
    Im = vector(length=LX); k=1;
    
    for(i in X1) {
      W=which(x[,AT[1]]==i)
      Im[k]=W[1]
      if(is.matrix(y)) my[k,]=colMeans(y[W,])
      else my[k]=mean_resp(y[W])
      k=k+1
    }
  }
  
  val[a] <- s_measure(my, "AAD")

}

ximportance = val/sum(val); ximportance # vector with the % overall importance values for each inputs, from att. 1 to 4.
