# This two functions are used to cache the inverse of a matrix.


# makeCacheMatrix contains 4 functions:
# 1. set: stores the value of a matrix
# 2. get: returns the value of the matrix
# 3. setinverse: stores the value of inverse of the matrix (but it doesn't calculate it)
# 4. getinverse: returns the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


# cacheSolve returns the inverse of the matrix. It first checks if the inverse 
# has already been calculated, if so, it gets the inverse from the cache. Otherwise,
# it calculates the inverse and sets its value in the cache via the setinverse
# function. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
