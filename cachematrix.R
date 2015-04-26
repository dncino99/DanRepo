## makeCacheMatrix creates a matrix and sets x and m essentially to a default value
## so that if the value provided by user doesn't change there will be no need to rerun mean
## NOTE:  For anyone who also struggled with this assignment, this code does not work
## makeCacheMatrix(x) simply returns the code
## cachemean(x) returns "Error in makeCacheMatrix$getmean : 
## object of type 'closure' is not subsettable
## cacheSolve seems to work (let's be greatful for little things)

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

cachemean <- function(x, ...) {
     m <- makeCacheMatrix$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- makeCacheMatrix$get()
     m <- mean(data, ...)
     makeCacheMatrix$setmean(m)
     m
}


## cachemean searches cache for previously calculated data mean 
## returns the mean if it has been calculated; if not, it calculates it and sets the mean

cacheSolve <- function(data, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverseofx <- solve(x)
     inverseofx
     
}
