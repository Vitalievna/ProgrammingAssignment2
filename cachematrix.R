## 
# makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## This function takes a matrix, inversible, creates a list of four objects to store
#the input matix and reverse matrix in diferent environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function return the inverse of x. if it has been already calculated for the same
#matrix, it reports that the output retrieved without new time-consuming calculation
#from the cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
#Usage:
# > mtx2<-matrix(c(2,0,0,2), 2, 2)
# > mtx_l2<-makeCacheMatrix(mtx2)
# > xtm2<-cacheSolve(mtx_l2)
# > xtm2
# [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5
# > xtm2<-cacheSolve(mtx_l2)
# getting cached data
# > xtm2
# [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5
# id the matrix is not inversible, the error is generated:
# > mtx3<-matrix(1:16, 4, 4)
# > mtx3_l<-makeCacheMatrix(mtx3)
# > xtm3<-cacheSolve(mtx3_l)
# Rerun with Debug
# Error in solve.default(data, ...) : 
#   Lapack routine dgesv: system is exactly singular: U[3,3] = 0 