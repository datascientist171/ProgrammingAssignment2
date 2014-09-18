## cachematrix.R
## Caching the Inverse of a Matrix
## The functions compute and cache the inverse of the given matrix 
## so that cached value is returned when needed, avoiding costly recomputation
## Command to run: cacheSolve(makeCacheMatrix(x))

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse,
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix 


makeCacheMatrix <- function(x = matrix()) {
    xinverse = matrix() 
    xinverse<- NULL
    set <- function(y) {
      ## store in the global environment
      x <<- y     
      xinverse <<- NULL
    }
    get <- function() x
    
    ## store/cache inverse in global env
    setinverse <- function(inv) xinverse <<- inv
    getinverse <- function() xinverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache. 
## Otherwise it computes the inverse and sets the value in the cache using setinverse function.
## Matrix supplied is assumed to be always invertible and the inverse of a square matrix is done with the solve function in R. 


cacheSolve <- function(x, ...) {
      xinverse <- x$getinverse()  ## get cached value
      if(!is.null(xinverse)) {
          ## If cached inverse is non-null, return it
          message("Getting cached inverse matrix")
          return(xinverse)
      }
      matrix <- x$get()
      xinverse <- solve(matrix) ## compute inverse
      x$setinverse(xinverse)    ## cache the computed inverse matrix
      xinverse
}
