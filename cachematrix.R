## The following makeCacheMatrix and cacheSolve methods create a special
## "matrix" object and they cache the calculated inverse matrix.
##
## Operations go as follow.
##
## Firstly, makeCacheMatrix creates a custom matrix object, which
## will record the inverse matrix once the created object has been set by 
## its method setinv().
## 
## Statements may goes as follow.
## 
## > m <- makeCacheMatrix(matrix(1:9, ncol=3, nrow=3))
##
##
## Secondly, cacheSolve check whether the given makeCacheMatrix has been
## calculated its inverse matrix. If the answer is yes, it will returns the
## given object's cached inverse matrix. Otherwise, it will calculate the
## inverse matrix.
## 
## Statements may goes as follow.
## 
## > cacheSolve(m)
##


## makeCacheMatrix(matrix) will have the following operations
## that could be used.
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix
##
## It will record its inverse matrix.
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(new.m) {
        x <<- new.m
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv.m) inv <<- inv.m
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve(makeCacheMatrix.obj) caches the calculated value
## which has been called before.
##
## Its operations go as follow.
##
## 1. check whether the given matrix has a calculated inverse matrix.
## 2. If yes, it return the warning message "getting cached data" and the cached
##    value
## 3. If no, it calculates the inverse matrix, save it into its inverse
##    matrix attribute, and returns the calculated result.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
