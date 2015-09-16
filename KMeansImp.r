KMeansImp <- function(data, k) {
  
  #Validation
  if(k<1){
    return('Please Enter K greater than 1')
  }
  
  #Validation for dimensionality
  if(ncol(data)>2){
    return('Please enter data in 2 dimension only.');
    }
  
  
  #Calculate the range of dataset
  min1 = min(data[,1])
  max1 = max(data[,1])
  min2 = min(data[,2])
  max2 = max(data[,2])
  
  #calculating the random data points
  
  randX = runif(k,min1,max1)
  randY = runif(k,min2,max2) 
  
  
  #Matrix for centroid information
  cenData <- data.frame(matrix(ncol = 2, nrow = k))
  
  for(m in 1:k)
  {
    cenData[m,1]= randX[m]
    cenData[m,2]= randY[m]
  }
  
  flag = TRUE
  
  #Matrix creation for storing cluster number in resultset
  clusteredData <- data.frame(matrix(ncol = ncol(data)+1, nrow = nrow(data)))
  
  while(flag== TRUE){


    #Making Initial matrix for centroid and data distance
    for(i in 1: nrow(data))
    {
     clusterNum = 0
     minimum = -1
  
     #Calculating the Euclidean Distance between centroid and data point
  
      for(j in 1: k)
      {
        d = sqrt((data[i,1]- cenData[j,1])^2 + (data[i,2]- cenData[j,2])^2)
    
       if(minimum < 0 || minimum > d)
          {
          minimum = d
          clusterNum = j
          }
      }
     
     #Populating the result Matrix
     clusteredData[i,1] = data[i,1]
     clusteredData[i,2] = data[i,2]
     clusteredData[i,3] = clusterNum
    }

#current centroid data is copied to another matrix
  cenData1 = cenData
  flag= FALSE;

#Updating the centroid by taking mean of dataset
  for(n in 1:k)
    {
      clus = subset(clusteredData,clusteredData[,3]  == n)
  
      if(nrow(clus)>0)
      {
         cenData[n,1]= mean(clus[,1],na.rm=TRUE)
         cenData[n,2]= mean(clus[,2],na.rm=TRUE)
      }
  
      if(cenData[n,1]!=cenData1[n,1] || cenData[n,2]!=cenData1[n,2])
      {
         flag = TRUE;
      }
    }  
  }	

return(clusteredData)
}


# Bisecting K-Means Algorithm
#Assumptions: Maximum SSE cluster is split until K is reached
BKMeansImp<- function(data, n) {
  
  result = KMeansImp(data, 2)
  curr=2;
  while(curr < n || curr==n){
    
    cnum = unique(result[,3])
    SSE = data.frame(matrix(ncol = 3, nrow = length(cnum)))
    maxClus = -1
    max= -1
    
    for(i in 1:length(cnum))
    {

      clusIndex= subset(result,result[,3] == cnum[i])
      SSE[cnum[i],1]= mean(clusIndex[,1])
      SSE[cnum[i],2]= mean(clusIndex[,2])
      SSE[cnum[i],3]=0
      
      for(num in 1:nrow(clusIndex))
      { 
          SSE[cnum[i],3]= SSE[cnum[i],3] + (clusIndex[num,1]-SSE[cnum[i],1])^2 + (clusIndex[num,2]-SSE[cnum[i],2])^2
      }
      
      
      #root logic for SSE can be implemented here
      if(max < 0 || max < SSE[cnum[i],3])
      {
        max = SSE[cnum[i],3]
        maxClus = cnum[i]
      }
    }
    if(maxClus > 0)
    { 
      data1=subset(result,result[,3]  == maxClus, select=1:2)
      res = KMeansImp(data1, 2)
      clus1 = subset(res,res[,3] == 2)
      cnum1 = max(cnum)+1
      for(r1 in 1: nrow(clus1))
      {
        for(d1 in 1: nrow(result))
        {
          flag = TRUE
          
          for(dims in 1: (ncol(result)-1))
          {
            if(result[d1,dims]!=clus1[r1,dims])
            {
              flag=FALSE
            }
          }
          
          if(flag)
          {
            
            result[d1,3] = cnum1
            
          }
        }
      }			
    }
    print(max)
    curr = curr + 1
  }
 
  return(result)
}
