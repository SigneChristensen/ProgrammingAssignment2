## This script contains a  apir of functions that will compute the inverse of a matrix


## The first function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x=matrix()) {
        s<-NULL
        set<- function(y) {
                x<<-y
                s<<-NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<-solve
        getsolve <- function() s
        list(set = set , get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
