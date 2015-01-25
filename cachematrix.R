## Matrix inversion is usually a time-consuming computation.
## So there may be some benefit to cache the inverse of a matrix rather than compute it repeatedly.
## The following shows a pair of functions that can compute and cache the inverse of a matrix.

## The function makeCacheMatrix creates a list containing 2 values (a matrix and its inverse) and 4 functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}

## Test Case

## tm is a 3x3 Vandermonde matrix, thus invertible
## > tm <- matrix(c(1,1,1,2,3,4,4,9,16), nrow = 3, ncol = 3)

## Create a list which contains tm and 4 functions
## > x <- makeCacheMatrix(tm)

## Show the matrix tm
## > x$get()
##      [,1] [,2] [,3]
## [1,]    1    2    4
## [2,]    1    3    9
## [3,]    1    4   16

## Compute the inverse
## > cacheSolve(x)

## Retrieve the inverse from the cache
## itm <- cacheSolve(x)

## Verify that the product of tm and its inverse itm equals identity matrix
## tm %*% itm