## There are two functions that are used to create a "matrix" 
## object that stores a invertible matrix and caches its inverse 

## makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        iv<- NULL
        set <- function(y){
                x<<-y
                iv<<- NULL
        }
        get <- function() x
        setiv <- function(inverse) iv<<-inverse
        getiv <- function() iv
        list (set=set, get=get,
              setiv=setiv, getiv=getiv)

}


## The cacheSolve calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix. However, it first checks to 
## see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setiv function. 
## (assume that the matrix supplied is always invertible.)

cacheSolve <- function(x, ...) {
        iv<- x$getiv()
        if (!is.null(iv)){
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data,...)
        x$setiv(iv)
        iv
        ## Return a matrix that is the inverse of 'x'
}
