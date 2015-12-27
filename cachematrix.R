##These two functions calculate the inverse mean of a matrix and cache it so it does
## need to be calculated repeateldly.


##function that holds the inverse matrix.
makeCacheMatrix <- function(x=Matrix()){
  
  #variable to hold inverse matrix
  i <- NULL
  
  #function to store the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
    
  }
  
  #return the matrix
  get <- function() x
  
  #set the inverse matrix
  setinv <-function(inv) i <<- inv
  
  #get the inverse matrix
  getinv <- function() i
  
  
  list(set=set,get=get,setinv = setinv,getinv=getinv)
  
  
}

#function the computes the inverse matrix.
cacheSolve <- function(x,...){
  #get inverse matrix
  i <- x$getinv()
  #check to see if inverse has been calculated
  if(!is.null(i)){
    message("geting cached data")
    return(i)
  }
  
  #calculate inverse matrix if not already done and assign to i
  data <-x$get()
  i <- solve(data,...)
  x$setinv(i)
  
  #return inverse matrix
  return (i)
  
}