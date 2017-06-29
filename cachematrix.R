## These functions creates a "special" matrix that caches the value of the 
## matrix (x) and it's inverse (inv).  
# Author: 	William Karasz
# Date:		6/28/2017

# makeCacheMatrix creates a special matrix that is actually a list of 4 
# functions (get, set, getinverse, setinverse)
# Example Usage:
# > A<-makeCacheMatrix()
#> A<-set(diag(1:3))
#> A$set(diag(1:3))
#> A$get()
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    2    0
#[3,]    0    0    3
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
			x
		}
        setinverse <- function(inverse) {
			inv <<- inverse
		}
        getinverse <- function() {
			inv
		}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## CacheSolve accesses the special matrix created by makeCacheMatrix and
## computes the inverse of x if the inverse value has not already been 
## computed.  The inverse value is stored with the special matrix.
# Example Usage:
#> cacheSolve(A)
#     [,1] [,2]      [,3]
#[1,]    1  0.0 0.0000000
#[2,]    0  0.5 0.0000000
#[3,]    0  0.0 0.3333333
#> z$getinverse()
#     [,1] [,2]      [,3]
#[1,]    1  0.0 0.0000000
#[2,]    0  0.5 0.0000000
#[3,]    0  0.0 0.3333333
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}