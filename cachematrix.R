
##  functions that cache the inverse of a matrix rather than compute it repeatedly 


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(matrix) m <<- matrix
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}

# Test case 1
x <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow = 2))
# First time computation
cacheSolve(x)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# Second time getting cached data
cacheSolve(x)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4


