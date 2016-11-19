## Put comments here that give an overall description of what your
## functions do

#Set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function (y){
    x<<-y
    inv<<- NULL
  }
  #Get the value of the matrix
  get <- function() x
  
  #Set the value of the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  #Get the value of the inverse matrix
  getinverse <- function () inv
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  #Checks if inverse is already calculated
  inv <- x$getinverse()
  
  #If yes, get the inverse from the cache and skip the computation
  if (!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
  
  #If not, calculate the inverse
  data <- x$get()
  inv <- solve (data)
  
  #Set the inverse in the cache
  x$setinverse(inv)
  inv
}
