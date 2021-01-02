#################
# cachematrix.R #
#################

# This file contains two functions for a matrix inversion calculation that can
# be cached.  It is a solution for the Programming Assignment 2 of Roger D.
# Peng's "R Programming" course. It will only work for matrices that can be
# inverted. Such a matrix can be created e.g as follows:
#
#     n <- 4
#     x <- matrix(rnorm(n^2), n)


## First function: makeCacheMatrix

# This function will create an environment that can cache a matrix `matr` and its
# inversion `matr_inv`.  It returns a list of four functions for setting or
# getting `matr` or `matr_inv`.
#
# There are two ways for setting `matr` to a matrix `x` of your choice:
#
# 1. At the instant of creating the environment by calling this function with
#    an argument, e.g. as follows:
#
#         cacheMatrix <- makeCacheMatrix(x)
#
# 2. After the environment has already been called by calling the `set`
#    function, e.g. as follows:
#
#         cacheMatrix <- makeCacheMatrix()
#         cacheMatrix$set(x)

makeCacheMatrix <- function(matr = matrix()) {
    matr_inv <- NULL
    set <- function(x) {
        matr <<- x
        matr_inv <<- NULL
    }
    get <- function() matr
    setinverted <- function(x) matr_inv <<- x
    getinverted <- function() matr_inv
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}

## Second function: cacheSolve

# This function returns the inverted matrix `matr_inv` of the matrix `matr`
# cached in the cache matrix list created by the first function:
#
#     cacheSolve(cacheMatrix)
#
# If `matr_inv` has been cached previously, then the cached version will be
# returned. If `matr_inv` has not yet been cached, it will be calculated anew
# and then cached.

cacheSolve <- function(cach, ...) {
    matr_inv <- cach$getinverted()
    if(!is.null(matr_inv)) {
        message("getting cached data")
        return(matr_inv)
    }
    matr <- cach$get()
    matr_inv <- solve(matr, ...)
    cach$setinverted(matr_inv)
    matr_inv
}
