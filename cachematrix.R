## cachematrix.R  
## MATRIX INVERSION 5/12/2016
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## This assignment is to write a pair of functions that cache the inverse of a matrix.
## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse:
##       1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##       2. cacheSolve:      This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##                           If the inverse has already been calculated (and the matrix has not changed), then the 
##                           cachesolve should retrieve the inverse from the cache.


# Function-1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 		1. set the value of the matrix
# 		2. get the value of the matrix
# 		3. set the value of inverse of the matrix
# 		4. get the value of inverse of the matrix
# Note: <<- operator which can be used to assign a value to an object in an environment that is different 
# from the current environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


# Function: cacheSolve: The following function calculates the inverse of matrix created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
# cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets its value in the 
# cache via the setinverse function.
# Assumption: The matrix is invertible.
# Function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv   
}

######UNIT TEST RESULTS ######

## > x <- matrix(c(3,1,-10,4),2,2)
## > x
##      [,1] [,2]
## [1,]    3  -10
## [2,]    1    4
## > m = makeCacheMatrix(x)
## > m
#$set
#function (y) 
#{
#    x <<- y
#    inv <<- NULL
#}
#<environment: 0x0000000012d32f28>
#
#$get
#function () 
#x
#<environment: 0x0000000012d32f28>
#
#$setinverse
#function (inverse) 
#inv <<- inverse
#<environment: 0x0000000012d32f28>
#
#$getinverse
#function () 
#inv
#<environment: 0x0000000012d32f28>
#
#> m$get()
#     [,1] [,2]
#[1,]    3  -10
#[2,]    1    4
#> m$getinverse()
#NULL
#> #not calc'ed before
#> cacheSolve(m)
#            [,1]      [,2]
#[1,]  0.18181818 0.4545455
#[2,] -0.04545455 0.1363636
#> cacheSolve(m)
#Getting cached data...
#            [,1]      [,2]
#[1,]  0.18181818 0.4545455
#[2,] -0.04545455 0.1363636
