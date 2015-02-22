makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) inv <<- inversa
  getinversa <- function() inv
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}