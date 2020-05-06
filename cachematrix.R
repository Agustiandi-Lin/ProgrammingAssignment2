## Function "makeCacheMatrix" can be use to create special matrix
## and save it's inverse value 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Function "cacheSolve" use the special matrix of "makeCacheMatrix" function 
## as an input to calculate the matrix's inverse value.
## Beside of that, this function will check first if that matrix's inverse have
## been calculated. If so, the matrix's inverse value will call out. 
## else, the function will calculate the matrix's inverse value

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
