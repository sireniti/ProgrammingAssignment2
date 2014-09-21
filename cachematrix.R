# R Programming 9/2014
# Project 2
# mlau-20140921

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {    
        x <<- y      #set matrix
        i <<- NULL   #set inverse matrix i to null
    }
    get <- function() x     #return cached matrix
    setinv <- function(solve) i <<- solve  #calculate inv. matrix, store in i
    getinv <- function() i     #return cached inverse matrix
    list(set = set, get = get,    #returns data as a list for external access
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    i <- x$getinv()     #sets inverse matrix to i from makeCacheMatrix list
    if(!is.null(i)) {   #if there is cached inv matrix, get it
        message("getting cached data")
        return(i)
    }
    else message("solving new inverse matrix")   #if no cached inv matrix
    data <- x$get()    #load original matrix
    i <- solve(data, ...)    #calculate inv matrix
    x$setinv(i)    #cache new inv matrix
    i    #return new inv matrix
}