## Because matrix inversion is usually a costly computation, 
## there's a benefit in cacheing the inverse of the matrix
## rather than compute it repeatedly. The following two functions below
## are used to cache the inverse of a matrix.

## The fist function makeCashMatrix, creates a list that contains another function(z) that:
# a) set the value of the matrix:

makeCacheMatrix <- function(x = matrix())
{
## b) get the value of the matrix
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
                }
## c. set the value of inverse of the matrix        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse

## d) get the value of inverse of the matrix
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## e) If the inverse of the matrix has not being computed, 
##   the cacheSolve function computes it, sets the value in the cache via
##   setinverse function.
##   This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) 
{
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

