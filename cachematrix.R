## Functions makeCacheMatrix and cacheSolve 1) create a matrix that is able to cache its inverse and 2) solves (inverts) the matrix

## This function creates a matrix that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  set_minv <- function(m_inverse) m_inv <<- m_inverse
  get_minv <- function() m_inv
  list(set = set, get = get, set_minv = set_minv, get_minv = get_minv)
}


## This function solves (inverts) the matrix created by the makeCacheMatrix function 

cacheSolve <- function(x, ...) {
  m_inv <- x$get_minv()
  if(!is.null(m_inv)) {
    message("getting cached data (matrix)")
    return(m_inv)
  }
  my_matrix <- x$get()
  m_inv <- solve(my_matrix)
  x$set_minv(m_inv)
  m_inv
}

