library("rminer")

data(sa_psin)
# get 10-fold predictive performance results:
M1 = mining(
  y ~.,
  sa_psin,
  model = "ksvm",
  sigma = 2^-2.0,
  C = 2^6.87,
  epsilon = 2^-8,
  method = "k-fold"
)

e1 = mmetric(M1,metric="MAE")
cat("SVM predictive performance > MAE=",round(e1$MAE,digits=2),"\n")
# 0.01 something... (paper results were 0.14 : OK)

SVM = fit(
  y~.,
  sa_psin,
  model="ksvm",
  sigma=2^-2.0,
  C=2^6.87,
  epsilon=2^-8
)

SA_M = "1D-SA"
I1 = Importance(SVM,sa_psin,method=SA_M,measure="AAD")
cat(round(I1$imp,digits=2)) 
# 0.46 0.36 0.18 0 (paper results were 0.46, 0.36, 0.18, 0.00: OK)

SA_M = "DSA"
I2=Importance(SVM,sa_psin,method=SA_M,measure="AAD",LRandom=10)
cat(round(I2$imp,digits=2))
# 0.51 0.35 0.13 0.01 (paper results were 0.52, 0.33, 0.15, 0.01: OK)
cm2=agg_matrix_imp(I2) # aggregate matrix
print(round(cm2$m1,digits=2))
# [,1] [,2] [,3] [,4]
# [1,] 0.00 0.29 0.33 0.34
# [2,] 0.18 0.00 0.20 0.20
# [3,] 0.08 0.09 0.00 0.09
# [4,] 0.01 0.00 0.01 0.00
print(round(cm2$m2,digits=2))
# [,1] [,2] [,3] [,4]
# [1,] 0.00 0.21 0.40 0.31
# [2,] 0.12 0.00 0.14 0.24
# [3,] 0.40 0.17 0.00 0.32
# [4,] 0.44 0.21 0.44 0.00
# similar to Table 8 of the paper: OK

SA_M = "GSA"
I3 = Importance(SVM,sa_psin,method=SA_M,measure="AAD", interactions = 1:4)
cm3 = agg_matrix_imp(I3) # aggregate matrix
print(round(cm3$m1,digits=2))
# [,1] [,2] [,3] [,4]
# [1,] 0.00 0.32 0.32 0.32
# [2,] 0.19 0.00 0.19 0.20
# [3,] 0.09 0.09 0.00 0.09
# [4,] 0.01 0.01 0.01 0.00
print(round(cm3$m2,digits=2))
# [,1] [,2] [,3] [,4]
# [1,] 0.00 0.19 0.09 0.01
# [2,] 0.32 0.00 0.09 0.01
# [3,] 0.32 0.19 0.00 0.01
# [4,] 0.32 0.20 0.09 0.00
# similar to Table 8 of the paper: OK

# now how to compute input importances?
if(FALSE){
# specific code for "DSA":
ys1=I2$sresponses[[1]]$y
# I2$sresponses[[1]]$yy
sm=vector(length=3)
for(k in 1:3) sm[k]=s_measure(ys1[k,],"AAD")
mean(sm) # 0.3220933
#fcm=cmatrixplot(cm,threshold=0.1) # OK, DONE!
}


# I is importance object
# method is a sensitivity method, works only for "GSA"
# measure is a sensitivity measure, say "AAD", works with all measures
# Aggregation = 1 for classification, 3 for regression
# L the number of sensitivity levels

# Ricardo: I need ti define yaggregate to use it
# yaggregate
yaggregate = function(y, N = 1) {
  
  if (N == 1){
    
    return(mean(y))
    
  } else if (N == 3){
    
      r = range(y)
      
      return(c(r[1],mean(y),r[2]))
      
  } else {
    
    y = quantile(y,seq(0,1,length.out = N))
    attr(y,"names") = NULL
    
    return(y)
    
  }
  
}

imp_gsa = function(I, method = "GSA", measure = "AAD", Aggregation = 1, L = 7) {
  
 # only 1 object for all inputs.
 xsa = I$sresponses[[1]]$x # I$sresponses[[1]]$x
 ysa = I$sresponses[[1]]$y # 2401 elems. due to GSA with D=4.
 #AI=aggregate_imp(I3,AT=1:4,measure="AAD",Aggregation=3,method="GSA",L=7)

 KA = ncol(xsa) # number of inputs
 N = Aggregation

 sm = matrix(ncol = L, nrow = N)
 val = vector(length = N)
 value = vector(length = KA)
 
 for(k in 1:KA) {
   
   Lx = unique(xsa[,k]) # x_k levels
  
   for(l in 1:L) {
     
     Is = which(xsa[,k] == Lx[l])
     
     sm[1:N,l] = yaggregate(ysa[Is], N = N)
   
   }
   
   for(n in 1:N) {
       
    val[n] = s_measure(sm[n,], measure = measure)
    
   }
   
  value[k] = mean(val)
 
 }
 
 imp = value/sum(value)
 I$value = value
 I$imp = imp
 
 return(I)
 
}

I3 = imp_gsa(I3,method="GSA",measure="AAD",Aggregation=3,L=7)
cat("I3 imp. (GSA)=",round(I3$imp,digits=2),"\n")
# 0.53 0.32 0.15 0.01 (paper results 0.53, 0.32, 0.14, 0.00: OK)

#If(FALSE) # another example
#{
data(sa_ssin_n2p)

# get 10-fold predictive performance results:
M=mining(y~.,sa_ssin_n2p,model="ksvm",sigma=2^-8.25,C=2^10,method="k-fold")
auc=mmetric(M,metric="AUC")
cat("SVM predictive performance > AUC=",round(auc$AUC,digits=2),"\n")
# 0.99 something... (closer to the paper results)

# Input importance methods:

# GSA Importance method: interactions only work if starting at index 1!
# fit SVM to all data:
#data=sa_ssin_n2p[,c(5,2:4)] # test if this order works? it does not work!
data = sa_ssin_n2p

# fit SVM to all data:
SVM2 = fit(
  y~.,
  data,
  model = "ksvm",
  sigma = 2^-8.25,
  C = 2^10
)

SA_M = "1D-SA"
I1 = Importance(
  SVM2,
  data,
  method = SA_M,
  measure = "AAD"
)

cat("I1 imp (1D-SA)=",round(I1$imp,digits=2),"\n") 
# 0.98 0.02 0 0 0 (very close to the INS 2013 paper results)

SA_M="DSA"
I2=Importance(SVM2,data,method=SA_M,measure="AAD",LRandom=10)
cat("I2 imp. (DSA)=",round(I2$imp,digits=2),"\n")
# 0.62 0.24 0.13 0.01 0 (very close to the INS 2013 paper results)
#cm2=agg_matrix_imp(I2) # aggregate matrix
#print(round(cm2$m1,digits=2))
#print(round(cm2$m2,digits=2))
#fcm=cmatrixplot(cm2,threshold=0.1) 

# compute Importance object:
SA_M="GSA"
I3=Importance(SVM2,data,method=SA_M,measure="AAD",interactions=1:4)
I3=imp_gsa(I3,method="GSA",measure="AAD",Aggregation=1,L=7)
cat("I3 imp. (GSA)=",round(I3$imp,digits=2),"\n")
# 0.67 0.18 0.12 0.03 (paper results : 0.78, 0.12, 0.08, 0.02 OK?)

#cm=agg_matrix_imp(I) # aggregate matrix
#print(round(cm$m1,digits=2))
#print(round(cm$m2,digits=2))
#fcm=cmatrixplot(cm,threshold=0.1) 





