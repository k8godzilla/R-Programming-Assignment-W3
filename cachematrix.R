## The combination of the two functions below, can evaluate the inverse matrix
##of the matrix inputed and cache the result to save the calculation in the next
##time the matrix is inputed.

##cache and provide the result of cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                   ## Set an empty value
  set<-function(y){
    x<<-y
    m<<-NULL
  }                        ## Set a matrix
  get<-function() x        ## get matrix
  setinverse<-function(inverse){
    m<<-inverse
  }                       ## set inverse
  getinverse<-function(){
    m
  }                       ## get inverse
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}



## If the result is cached in the function of makeCacheMatrix,returned the result directly.
##If not, evaluate the inverse matrix.

cacheSolve<-function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  starmatrix<-matrix(nrow = nrow(data),ncol = ncol(data))     ##Establish a NA matrix with same number of rows and cols with data
  ddata<-det(data)         ##Determinant of data
  if(ncol(data)==2){
    m<-solve(data)
  }
  else{
    for (i in 1:nrow(data)) {
      for (j in 1:ncol(data)) {
        dataexliandj<-data[-i,-j]
        starmatrix[j,i]<-(-1)^(i+j)*det(dataexliandj)
        
      }                 ## Set the value for starmatrix
      m<-(starmatrix/ddata)
      
    }
  }
  x$setinverse(m)
  m
}
