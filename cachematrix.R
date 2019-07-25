## This assignment is caching the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
InvMat <- NULL
      set <- function (y) {
 		x <<- y
            InvMat <<- NULL
       }
	get <- function() x
      setMatInv <- function(inverse) InvMat <<- inverse
      getMatInv <- function() InvMat
      list(set = set,get = get,
           setMatInv = setMatInv,
           getMatInv = getMatInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 InvMat <- x$getMatInv()
        if(!is.null(InvMat)) {
                message("getting cached data")
                return(InvMat)
        }
data <- x$get()
InvMat <- solve(data, ...)
x$setMatInv(InvMat)
InvMat
}
