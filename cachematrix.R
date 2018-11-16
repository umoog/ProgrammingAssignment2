## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)m <<- solve  #calculates inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse =getinverse)
  }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

  cacheSolve <- function(x, ...) {
    m <- x$getinverse()               #getting the cahed data  
    if(!is.null(m)) {
    message("getting cached matricx inversion")
    return(m)
    }
    data <- x$get()
    m <- solve(data)                ##calculating the inverse
    x$setinverse(m)                 ##storing the inverse
    return(m)                       ## Return a matrix that is the inverse of 'x'
	}