## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix, which is really a list containing a function to set matrix, get the matrix, set the inverse of matrix and get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ##Defining the matrix mat_inv for caching
  mat_inv <- NULL
  
  ##Caching the matrix
  set_matrix <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  
  ##Getting the matrix from cache
  get_matrix <- function() x
  
  ##Caching the inverse of the matrix
  setMatrixInverse <- function(inv) mat_inv <<- inv
  
  ##Getting the inverse from cache
  getMatrixInverse <- function() mat_inv
  
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


##The following function generates the inverse of the special matrix created with the above function. 
cacheSolve <- function(x, ...) {
  
  mat_inv <- x$getMatrixInverse()
  
  ##Checking if inverse is already cached
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  
  ##Computing the inverse of the matrix since its not in cache
  data <- x$get_matrix()
  mat_inv <- solve(data, ...)
  
  ##Caching the inverse
  x$setMatrixInverse(mat_inv)
  mat_inv
}
