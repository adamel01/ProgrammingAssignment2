## There are 2 functions in the file:
## These functions will create a matrix and then calculate its inverse.  If the inverse
## has already been calculated at some point, then it will recall those values from
## the cache. 


## This function creates a special "matrix" object that can cache its inverse
##It will perform the following:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {       
  
  m <- NULL   
  set <- function(y) {                       
    x <<- y                                  #set the value of the matrix in the function environment
    m <<- NULL                               #set the value of m to NULL in the makeCacheMatrix environment
  }
  get <- function() x                        #get the value of the matrix in the evaluating environment
  setInverse <- function(solve) m <<- solve  #set the inverse matrix
  getInverse <- function() m                 #get the inverse matrix
  list(set = set, get = get,                
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed),then the cachesolve should retrieve
##the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
     m <- x$getInverse()                #get the inverse matrix
    if(!is.null(m)) {                   #if the inverse matrix already exists, it will be pulled form the cache
      message("getting cached data")
      return(m)                         #returns the cached, inverse matrix that was pulled in prior lines
    }
    data <- x$get()                     #if the inverse didn't already exist, this line executes and we get a matarix
    m <- solve(data, ...)               #solve for the inverse of the matrix created in the prior line
    x$setInverse(m)                     #store the newly created inverse in the cache
    m
  }
