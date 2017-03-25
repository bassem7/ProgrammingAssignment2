## Put comments here that give an overall description of what your
## functions do


# Background : 
#     Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
#       rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#       Your assignment is to write a pair of functions that cache the inverse of a matrix.
# Computing the inverse of a square matrix can be done with the solve function in R. 
#       For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# Assumptions:  
#       matrix supplied is always invertible.


## Write a short comment describing this function
# makeCacheMatrix: 
#       Input:
#       Process:        creates a special "matrix" object that can cache its inverse
#       Output:
#
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setinverse <-function(inv1) m<<- inv1
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# cacheSolve: This function
#       Input: matrix x
#       Process:       
#               If the inverse has already been calculated (and the matrix has not changed), 
#                       then cacheSolve should retrieve the inverse from the cache.
#               Else:                
#                     computes the inverse of the special "matrix" returned by makeCacheMatrix  
#       Output: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data)
                x$setinverse(m)
                m
}
