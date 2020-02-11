
# Created:    Jul 10, 2017
# @author     Rouzbeh Razavi, PhD (rrazavi@kent.edu)
# @version:   1.0
# File:       River_Crossing1.r
# Comment:    Solves the Farmer-Wolf-Chicken-Spider-Caterpillar-Lettuce
#             River Crossing Problem
################################################################

rm(list = ls())
checkValidity <- function(S){
  result=1;
  S_numeric=as.numeric(S)
  N=length(S_numeric)

  if (!all(head(S_numeric,N/2)+tail(S_numeric,N/2)==rep(1,N/2))){result=0}
  
  if (sum(S[1],S[2])==2 & S[6]==0) {result=0}
  if (sum(S[2],S[3])==2 & S[6]==0) {result=0}
  if (sum(S[3],S[4])==2 & S[6]==0) {result=0}
  if (sum(S[4],S[5])==2 & S[6]==0) {result=0}
  
  if (sum(S[7],S[8])==2 & S[12]==0) {result=0}
  if (sum(S[8],S[9])==2 & S[12]==0) {result=0}
  if (sum(S[9],S[10])==2 & S[12]==0) {result=0}
  if (sum(S[10],S[11])==2 & S[12]==0) {result=0}
  
  return(result)
}
#########################################3
Get_Valid_Moves<- function(S){
  
  N=ncol(S)/2-1;
  Boat_Capacity=4;
  
  if (S[N+1]==1){
    M_Left <- as.data.frame (expand.grid(rep(list(0:1),N)))
    for (m in 1:ncol(M_Left)){
      M_Left[,m]= M_Left[,m]*as.numeric(S[,m])
    }
    M_Left=unique(M_Left)  
    M_Left<-cbind(M_Left,0)
    M_Right<-M_Left;
    for (m in 1:ncol(M_Left)){
      M_Right[,m]= -1*(M_Left[,m]-1)
    }
    
  }else { 
    M_Right <- as.data.frame (expand.grid(rep(list(0:1),N)))
    for (m in 1:ncol(M_Right)){
      M_Right[,m]= M_Right[,m]*as.numeric(S[,m+4])
    }
    M_Right=unique(M_Right)  
    M_Right<-cbind(M_Right,0)
    M_Left<-M_Right;
    for (m in 1:ncol(M_Right)){
      M_Left[,m]= -1*(M_Right[,m]-1)
    }
  }
  M=cbind(M_Left,M_Right)
  colnames(M)<-colnames(S)
  
  Delta=rep(0,nrow(M))
  for (m in 1:nrow(M)){
    Delta[m]=sum(abs(as.numeric(M[m,])-as.numeric(S)))
  }
  M=M[Delta<=2*Boat_Capacity,]  
  Validity_flag=rep(0,nrow(M))
  

  if (nrow(M)>0){
    for (j in 1:nrow(M)){
      Validity_flag[j]=checkValidity(M[j,])
    }
  }
  
  M=M[Validity_flag==1,]
  return(M)
}

###################################################

Start_State=data.frame(W_L=1,CH_L=1,S_L=1,C_L=1,L_L=1,F_L=1,W_R=0,CH_R=0,S_R=0,C_R=0,L_R=0,F_R=0)
Success_State= data.frame(W_L=0,CH_L=0,S_L=0,C_L=0,L_L=0,F_L=0,W_R=1,CH_R=1,S_R=1,C_R=1,L_R=1,F_R=1)
Acceptable_states<-c()
Acceptable_states[1]=list(Start_State);
success=0
impossible=0

while (success==0 & impossible==0){
  Number_of_States=length(Acceptable_states)
  for (j in 1:Number_of_States){
    State_Records=Acceptable_states[[j]]
    Latest_State=State_Records[nrow(State_Records),]
    New_States=Get_Valid_Moves(Latest_State)
    if (nrow(New_States)>0){
      for (m in 1:nrow(New_States)){
        
        T=rbind(Success_State,New_States[m,])
        if (nrow(unique(T))<nrow(T)&success==0){
          success=1
          print('####Solution Found######')
          Solution=rbind(State_Records,New_States[m,])
          rownames(Solution) <- NULL               
          print(Solution)
          break
        }
        
        T=rbind(State_Records,New_States[m,])
        if (nrow(unique(T))==nrow(T)){
          Acceptable_states[[length(Acceptable_states)+1]]=T
        }
      }
    }
  }
  for (m in seq(Number_of_States,1, by=-1)){
    Acceptable_states[[m]]<-NULL
  }
  if (length(Acceptable_states)==0){
    print('#### No Solution Can Be Found ######')
    impossible=1
  }
}
