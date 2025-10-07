# libraries 
library("rminer")

# load data
load("train_data.RData")

tr=train_data[,c(2:5,1)]

# m model: SVM
m <- fit(
  ClassEfficiency~.,
  data = tr,
  model = "ksvm",
  kernel = "polydot",
  scale = "none",
  kpar = list(
    degree = 3,
    scale = 0.1
  ),
  C = 10
)

# m is a fitted model, svm.
# train_data is the data, class, 4 inputs.
tr = train_data[,c(2:5,1)]
m2 = fit(
  ClassEfficiency~.,
  tr,model="ksvm",
  kernel="polydot",
  scale="none",
  kpar=list(
    degree = 3,
    scale = 0.1),
  C = 10
  )

I2 = Importance(
  m2,
  tr,
  method = "GSA",
  interactions=1:4
)

cm2=agg_matrix_imp(I2)

print(round(cm2$m1,digits=2))
print(round(cm2$m2,digits=2))
fcm2=cmatrixplot(cm2,threshold=0.1) 

# # single input importance?
# Inps = 4
# val = vector(length=Inps)
# 
# for(a in 1:Inps){
#   
#   avg_imp(I2, 1 ,measure="AAD")
#   
#   val[i]= avg_imp(I2,i,measure="AAD")$value
#   ximportance=val/sum(val) # vector with the % overall importance values for each inputs, from att. 2 to 5.
#   
# } 





### 
Inps=4
val=vector(length=Inps)
for (a in 1:length(I2$inputs)) {
  AT <- a
  x=I2$sresponses[[1]]$x
  y=I2$sresponses[[1]]$y
  X1=unique(x[,AT[1]])
  print(X1)
  
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
  
  
  
  print(s_measure(my, "AAD"))
  val[a] <- s_measure(my, "AAD")
  
  
}

ximportance=val/sum(val) # vector with the % overall importance values for each inputs, from att. 2 to 5.
