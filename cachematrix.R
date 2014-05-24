## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
#1.The first function, makeVector creates a special "vector", which is really a list containing a function to
#2.set the value of the matrix
#3.get the value of the matrix
#4.set the value of the matrix
#5.get the value of the matrix
# setwd("C:/Users/pavan keely/ProgrammingAssignment2")
makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setInv <- function(inv) invMat <<- inv  #invoke the inverse function
        getInv <- function() invMat
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInv()
        if(!is.null(invMat)) {
                message("getting cached matrix inverse")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data)
        x$setInv(invMat)
        invMat
}
