library(randomForest)
###### optimize parameters  ########
parm=vector('list')
OUT=vector('list')
dataset  # dataset in training model
Yield    # corresponding
for (i in 1:6) {   # for six time-intervals
  
  data=dataset[[i]]
  y=as.numeric(Yield[,i])

 
  for (j in 1:6) {  
    
    NT=c(200,400,600,800,1000,1200) # set different values for ntree
    
    A=tuneRF(data, a,mtryStart=2,step=0.8,ntreeTry = NT[j],trace = TRUE,improve=0)
    
    parm[[j]]=A
    
  }
  
  OUT[[i]]=parm
}

# select the optimum parameter

NT=c(200,400,600,800,1000,1200)  # different ntree

An=matrix(nrow=6,ncol=6)
Am=matrix(nrow=6,ncol=6)
for (i in 1:6) {
  for (j in 1:6) {
    U=OUT[[i]][[j]]
    row=which(U[,2]==min(U[,2]))
    Am[j,i]=U[row,1]
    An[j,i]=U[row,2]
  }
}
A=matrix(nrow=2,ncol=6)
for (i in 1:6) {
  row=which(An[,i]==min(An[,i]))
  A[1,i]=Am[row,i]
  A[2,i]=NT[row]
}

#  A is the optimal ntree and mtry
#  there are total 6 mtry and ntree for 6 time intervals


######## yield forecast  #######
data.set  # training dataset including different predictors and years (during 2013-2019)
Yield     # crop yield
R=matrix(nrow=7,ncol=6)
nRMSE=matrix(nrow=7,ncol=6)
for (k in 1:6) {

for ( j in 1:7) {
  
  AGdata=data.set[[k]]
  y=as.numeric(Yield[,k])

  row=which(AGdata[,1]==j+2012) # start from 2013 
  
  data_test=AGdata[row,]
  Test=y[row]               # crop yield for test set
  
  data_test=data_test[,2:ncol(data_test)]   # exclude the year number column
  
  data=AGdata[-row,2:ncol(AGdata)]        # traning data, exclude the year for the test set. 
  
  y=y[-row]            # exclude the year for the test yield 

  RFmodel <- randomForest(y ~ ., data=data,xtest=data_test,importance=TRUE,
                          proximity=TRUE, mtry=A[1,k],na.action = na.omit,ntree=A[2,k])
  
  Pre=RFmodel[["test"]][["predicted"]]
  
  R[j,k]=cor(Pre,Test)   #  Pearson's correlation coefficient of different year and time interval.
  
  nRMSE[j,k]=sqrt(mean((Pre-Test)^2))/mean(Test) # nRMSE of different year and time interval.
  

  }

}
