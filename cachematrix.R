## Functions create a special matrix object that can cache its inverse
## then returns the inverse of the matrix from cache if previously 
## stored.

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  
  ## makeCacheMatrix: This function creates a special 
  ## "matrix" object that can cache its inverse.
  
  ## Computing the inverse of a square matrix can be done 
  ## with the solve function in R. 
  
  ## For example, if X is a square invertible matrix, 
  ## then solve(X) returns its inverse.
  
  ## For this assignment, assume that the matrix supplied is always invertible.
  
  ## Not all square matrices have inverses.  If a matrix has 
  ## an inverse, we call it nonsingular or invertible.  
  ## Otherwise it is called singular.
  
  m <- NULL
  
  set <- function(cachedData) {
    ## The <<- operator is used to assign a value to an object in an 
    ## environment that is different from the current environment
    
    ## x = function parameter of type numeric()
    x <<- cachedData
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inv){ 
    m <<- inv
  }
  
  getInverse <- function() {
    m
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##  This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x,...)
{
  ##  This function computes the inverse of the special 
  ## "matrix" returned by makeCacheMatrix. If the inverse 
  ## has already been calculated (and the matrix has not 
  ## changed), then cacheSolve should retrieve the inverse 
  ## from the cache.
  
  ## Computing the inverse of a square matrix can be done 
  ## with the solve function in R. 
  
  ## For this assignment, assume that the matrix supplied 
  ## is always invertible.
  
  ## Not all square matrices have inverses.  If a matrix has 
  ## an inverse, we call it nonsingular or invertible.  
  ## Otherwise it is called singular.
  
  ## TEST
  ## source('makeCacheMatrix.R')
  ## source('cacheSolve.R')
  
  ## Test - Invertible / Nonsingular
  ## 1. matrix <- matrix(data = c(1,1,2,3,5,8,13,21,34,55,89,144), nrow = 2, ncol = 2)
  ## 2. cachedMatrix <- makeCacheMatrix(matrix)
  ## 3. cacheSolve(cachedMatrix)
  
  ## Result: NOT previously cached
  ##  > cacheSolve(cachedMatrix)
  ##  [,1] [,2]
  ##  [1,]    3   -2
  ##  [2,]   -1    1
  
  ## Result: Previously cached
  ##  > cacheSolve(cachedMatrix)
  ##  getting cached data ...
  ##  [,1] [,2]
  ##  [1,]    3   -2
  ##  [2,]   -1    1
  
  ## Test - Matrix is singular and cannot be inverted
  ## 1. matrix <- matrix(data = c(1,1,2,3,5,8,13,21,34,55,89,144), nrow = 2, ncol = 3)
  ## 2. cachedMatrix <- makeCacheMatrix(matrix)
  ## 3. cacheSolve(cachedMatrix)
  
  ## Result:
  ## supplied matrix is not invertible! 
  ##     [,1] [,2] [,3]
  ## [1,]    1    2    5
  ## [2,]    1    3    8
  
  m <- x$getInverse()
  
  ## Test for previously cached
  if(!is.null(m)) {
    message("getting cached data ...")
    return(m)
  }
  
  data <- x$get()
  
  invertible <- TRUE
  
  ## Test is supplied matrix is invertible
  invertible <- tryCatch(
      {
        m <- solve(data, ...)
        invertible == TRUE
      },
      error = function(cond) {
        ##message(cond)
        invertible == FALSE
      }
      )

  if(invertible)
  {
    x$setInverse(m)
    return(m)
  }
  else
  {
    cat("\n","ERROR: supplied matrix is not invertible!","\n\n")
    return(data)
  }
}
  
