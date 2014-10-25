##  makeCacheMatrix function creates/store a matrix object 
##  that can cache its inverse.
#1. set the value of the matrix (set function)
#2. get the value of the matrix (get function)
#3. set the value of the inverse (setMInv(inv) function)
#4. get the value of the inverse (getMInv function)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        mInv <<- NULL      
    }
    get function() x
    setMInv <- function(inv) mInv <<- inv
    getMInv <- function() mInv
    list(set = set, get = get, setMInv = setMInv, getMinv = getMInv )
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
