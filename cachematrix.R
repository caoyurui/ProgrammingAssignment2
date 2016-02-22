## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  #set the initial inverse matrix, which is null
        set <- function(y) {
                x <<- y  #set the input matrix, assign the value to x
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,  
             setinverse = setinverse,
             getinverse = getinverse)   #returns a list of four functions
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)   #return the inverse matrix if it has been calculated
        }
        data <- x$get()
        i <- solve(data, ...)  #if not, compute the inverse
        x$setinverse(i)  #cache the inverse
        i
}
