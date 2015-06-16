## ==========================================================
##     _    _         ____            _ 
##    / \  | |__  ___|  _ \ _ __   __| |
##   / _ \ | '_ \/ __| |_) | '_ \ / _` |
##  / ___ \| |_) \__ \  _ <| | | | (_| |
## /_/   \_\_.__/|___/_| \_\_| |_|\__,_|
##
## Title: Coursera R-Programming (rprog-015) Assignment 2 
## Author: Stuart Malcolm
## Created: 2015-06-16
## Version: 1.0
##
## Functions to implement memoized matrix inverse. 
## ==========================================================

## Create a cache matrix that memorizes its inverse.
## 
## INPUT:
## x = matrix to create, or empty matrix if missing
## 
## OUTPUT: a list containing four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## 2. get the value of the matrix
    get <- function() x
    ## 3. set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    ## 4. get the value of the inverse
    getinverse <- function() i
    ## return list of 4 functions defined above
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

## ----------------------------------------------------------
## calculate the inverse of a cache vector that has been
## created with the makeCacheMatrix defined above.
## If the inverse has already been calculated and cached
## then this value is returned without re-calcuation.
## 
## INPUT:
## x = cache matrix (created using makeCahceMatrix fn)
## ... any option parameters valid for Solve()
## 
## OUTPUT:
## inverse of the matrix derived using Solve()
## 
cacheSolve <- function(x, ...) {
    ## get the cached inverse value..
    i <- x$getinverse()
    ## .. if it is not NULL then return it as result
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## ..cached value is NULL so calculate inverse
    matrix <- x$get()
    i <- solve(matrix,...)
    x$setinverse(i)
    ## return the inverse as the result
    i
}
