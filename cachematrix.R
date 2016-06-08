
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the inverse of the matrix
#4.get the inverse of the matrix

# This will be used as input to the cache solve function

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
    x<<-y
    inv<<-NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}
# the function returns the inverse of matrix created using the above function

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    # checking if the inverse is already calculated
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #stoting matrix data in data1
    data1 <- x$get()
    #finding inverse using the solve function
    inv <- solve(data1, ...)
    #setting the inverse value in cache using setinv funciton
    x$setinv(inv)
    inv
       
}
