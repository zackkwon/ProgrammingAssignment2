## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # define inverse variable
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } #cahcing
  get <- function() x # apply function 
  setInv <- function(inverse) inv <<- inverse #set inverse
  getInv <- function() inv #get inverse
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv) #print list of makeCacheMatrix
}



## Write a short comment describing this function
x = makeCacheMatrix(matrix(1:4, 2, 2))

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv() #get inverse matrix
  if (!is.null(inv)) { #is cached value?
    message("getting cached data")
    return(inv) #print cached inverse matrix
  }
  mat <- x$get()  
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv #print ivnverse matrix
}