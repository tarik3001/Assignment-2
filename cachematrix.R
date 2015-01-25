## Put comments here that give an overall description of what your
## A function (makeCacheMatrix) will create a matrix object that can cache its inverse
## A function (cacheSolve) will compute the inverse of the matrix returned by makeCacheMatrix above.  

## This function creates matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<-y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will compute the inverse of the matrix returned by the makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
