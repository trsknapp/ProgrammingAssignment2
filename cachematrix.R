##The makeCacheMatrix monitors the input matrix and the output "inverse" 
##matrix. The CacheSolve overwrites the output "inverse" matrix. 

##The "makeCacheMatrix" returns a list data object to the parent environment.
##The list data object contains 4 elements
##      1.  A "set" function: redefines the input Matrix used when the 
##      makeCacheMatrix function was originally called. 
##      2.  A "get" function: retrieves the input Matrix used when the 
##      makeCacheMatrix function was called. 
##      3.  A "setInverse" function: calculates the cached inverse when
##      the makeCacheMatrix is called inside of the CacheSolve function 
##      4.  A "getInverse" function: retrieves the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## CacheSolve calculates the inverse of the input from makeCacheSolve, and 
## overwrites the inverse in the makeCacheSolve function. If it's already
## been calculated, it uses the cached data. 

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m

}
