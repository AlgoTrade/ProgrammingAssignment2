## makeCacheMatrix function creates/store a matrix object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    ## set the value of the matrix (set function)
    set <- function(y){
        x <<- y
        mInv <<- NULL
    }
    ## get the value of the matrix (get function)
    get <- function() x
    ## set the value of the inverse (setMInv(inv) function)
    setMInv <- function(inv) mInv <<- inv
    ## get the value of the inverse (getMInv function)
    getMInv <- function() mInv
    list(set = set, get = get, setMInv = setMInv, getMInv = getMInv )
}
## This function gets the inverse of a matrix returned by `makeCacheMatrix` above.
## If the inverse has already been calculated, then the function return the inverse of the matrix from the cache.
## This function assume that the matrix is always invertible
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getMInv()
    if(!is.null(mInv)){
        ##getting inverse matrix cached
        message("getting cached data...")
        return(mInv)
    }
    ##get the matrix and use 'solve' to get the inverse,
    ##then cache it with setMInv function
    mtx <- x$get()
    mInv <- solve(mtx)
    x$setMInv(mInv)
    mInv
}
## Test run
##> source('F:/RProgramming/ProgrammingAssignment2/cachematrix.R')
##> x <- matrix(c(2,1,4,3), nrow=2,ncol=2)
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,] 2 4
##[2,] 1 3
##> cacheSolve(m)
##[,1] [,2]
##[1,] 1.5 -2
##[2,] -0.5 1
##> cacheSolve(m)
##getting cached data...
##[,1] [,2]
##[1,] 1.5 -2
##[2,] -0.5 1