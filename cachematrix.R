## 2nd Programming Assigment of Course Programming R 
## by JValentin77, aka, Joel Vanhalakka

## Matrix inversions use a lot of computational power. Therefore, in a case where
## a lot of matrix inversions are made, it's possible to use lexical scoping,
## i.e. saving objects into memory, to reduce the required computations.

## The two functions presented here do exactly that. "makeCacheMatrix" creates
## special matrises that can cache information into memory and "cacheSolve"
## either solves the inverse calculation or gets it from memory, therefore,
## saving computational power in repeated calculations.

## "makeCacheMatrix" creates a special matrix that:
## 1) sets the value of the matrix
## 2) gets the value of the matrix
## 3) sets the value of the inverse
## 4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # set inverse to NULL at beginning
        i <- NULL
        # set matrix value
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # get matrix value
        get <- function() x
        ## set inverse
        setinverse <- function(solve) i <<- solve
        ## get inverse value
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


## "cacheSolve" computes the inverse of the special "matrix
## returned by makeCacheMatrix. If the inverse has already
## been calculated, it will be retrieved from memory

cacheSolve <- function(x, ...) {
        ## get inverse value
        i <- x$getinverse()
        ## if inverse value != NULL, get it from memory
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if inverse value == NULL, count it and save it
        else {
                data <- x$get()
                i <- solve(data)
                x$setinverse(i)
                i
        }
}

# Example code run
# > x <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)
# > test <- makeCacheMatrix(x)
# > test$get()
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > cacheSolve(test)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > cacheSolve(test)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
