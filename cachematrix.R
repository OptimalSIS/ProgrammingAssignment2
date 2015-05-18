## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
##  makeCacheMatrix creates a special "matrix" which is a list containing
##  a function to 
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(Inv) inv <<- Inv
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
##  cacheSolve calculates the inverse of the special "matrix" created with 
##  the above function. However, it first checks to see if the inverse has 
##  already been calculate. If so, it gets the inverse from the cache and 
##  skips the computation. Otherwise, it calculates the inverse of the matrix
##  and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
