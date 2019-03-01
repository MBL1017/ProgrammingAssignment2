## makeCachematrix creates an R object that stores a matrix and its inverse

a<-makeCacheMatrix (rbind(c(1,-1/4),c(-1/4,1)))
b<- makeCacheMatrix (rbind(c(1,-1/4),c(-1/4,1)))

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
         setinverse= setinverse,
         getinverse = getinverse)
  }


## cachesolve populates and/or retrieves the inverse from makeCacheMatrix()

cacheSolve <- function(x, ...) {
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

a<-makeCacheMatrix (rbind(c(1,-1/4),c(-1/4,1)))
cacheSolve(a)
#show pulling cache value by displaying message "getting cached data"
cacheSolve(a)
