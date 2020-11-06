## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
    InverseOut <- NULL
    Set <- function(y) {
        x <<- y
        InverseOut <<- NULL
    }
    Get <- function() x
    SetInverse <- function(x) {InverseOut <<- x}
    GetInverse <- function() {return(InverseOut)}
    list(Set = Set, Get = Get,
         SetInverse = SetInverse,
         GetInverse = GetInverse)
}

cacheSolve <- function(x, ...) {
    Inverse <- x$GetInverse()
    if(!is.null(Inverse)) {
        message("Getting cached data ...")
        return(Inverse)
    }
    data <- x$Get()
    m <- solve(data, ...)
    x$SetInverse(m)
    return(m)
}
