
## makeCacheMatrix function will return list of functions(get, set, getInverse, setInverse) defined in the current environment and 
## those functions will be used by cacheSolve function to get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) 
    inv <<- solve
  print(inv)
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function will calculate inverse of a matrix and  on first call it will not have cached data 
## On second call it will return cached data.

cacheSolve <- function(a) {
  ## Return a matrix that is the inverse of 'x'
  inv <- a$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$setinverse(inv)
  inv
}
