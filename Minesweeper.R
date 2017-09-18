library(ggvis)

Board_size<-c(50,50)
Nmines<-200

GetEnoughPositions<-function(pos,mines,Board_size)
{
  p<-unique(pos)
  if (nrow(p)>=mines)
    p[1:mines,]
  else
  {
    pos<-rbind(p,unique(t(sapply(1:500,function(x){c(unique(floor(runif(1,1,Board_size[[1]])))
                                                ,unique(floor(runif(1,1,Board_size[[2]]))))}))))
    GetEnoughPositions(pos,mines)
  }
}

findNeighbours<-function(mt,pos,direct=FALSE,connected=FALSE)
{
  if(direct)
  {
    neigbour_matrix<-list(c(-1,0),c(0,-1),c(0,1),c(1,0))
  }
  else
  {
    neigbour_matrix<-list(c(-1,-1),c(-1,0),c(-1,1),c(0,-1),c(0,1),c(1,-1),c(1,0),c(1,1))
  }
  neigbour_matrix<-lapply(neigbour_matrix,function(x) x+pos)
  neigbour_matrix<-lapply(neigbour_matrix,function(x) if((x[[1]]<1 | x[[1]]>nrow(mt) | x[[2]]<1 | x[[2]]>ncol(mt))) NULL else x)
  neigbour_matrix[sapply(neigbour_matrix,is.null)]<-NULL
  neigbour_matrix<-lapply(neigbour_matrix,function(x) if(connected&(mt[x[[1]],x[[2]]]!=0)) NULL else x)
  neigbour_matrix[sapply(neigbour_matrix,is.null)]<-NULL
  neigbour_matrix
}


GenerateBoard<-function(Size,mines)
{
  Board<-matrix(rep(0,Size[[1]]*Size[[2]]),nrow = Size[[1]],ncol=Size[[2]])
  
  positions<-sapply(1:mines*10,function(x){c(unique(floor(runif(1,1,Size[[1]])))
                                      ,unique(floor(runif(1,1,Size[[2]]))))})
  positions<-t(positions)
  positions<-unique(positions)
  
  positions<-GetEnoughPositions(positions,Nmines,Size)
  
  for(i in 1:Nmines)
  {
    Board[positions[[i,1]],positions[[i,2]]]<-Size[[1]]*Size[[2]]
    neigbours<-findNeighbours(Board,positions[i,])
    for(j in neigbours) Board[j[[1]],j[[2]]]<-Board[j[[1]],j[[2]]]+1
  }
  plot(x=0,y=0,xlim=c(0,Board_size[[2]]),ylim=c(0,Board_size[[1]]))
  invisible(lapply(0:Board_size[[1]],function(x) abline(h=x)))
  invisible(lapply(0:Board_size[[2]],function(x) abline(v=x)))
  invisible(lapply(0:(Board_size[[1]]-1),function(x) lapply(0:(Board_size[[2]]-1),function(y) rect(y,x,y+1,x+1,col="grey"))))
  Board
}

Board<-GenerateBoard(Board_size,Nmines)


invisible(lapply(1:(Board_size[[1]]),function(x) lapply(1:(Board_size[[2]]),function(y) if(Board[[x,y]]<=8) text(y-0.5,x-0.5,Board[[x,y]],col=Board[x,y]))))

Find_cont<-function(Board)
{
  ind0<-which(Board==0,arr.ind = T)
  ind0<-cbind(ind0,rep(-1,nrow(ind0)))
  nComp<-1
  
}
