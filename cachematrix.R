## Creates a matrix "class" which stores the matrix and caches its inverse if it has 
## already been computed and gives a function to compute or retrieve the inverse

## Defines the creation of a cache matrix and the functions which allow one to get
## set both the matrix and the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function checks if inverse is cached. If so it is returned, if not it is computed and
## then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <-solve(data,...)
    x$setinv(i)
    i
}
