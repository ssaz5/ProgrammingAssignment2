## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatix 
## creates a matrix which can save its inverse in memory

makeCacheMatrix <- function(M = matrix()){
    # Give a warning in case the dimensions are wrong
    if(dim(M)[1] != dim(M)[2]){
        print("Warning: Matrix is not a Square Matrix")
    }
    # initialize INV as NULL
    INV <-NULL
    
    
    set <- function(x){ # set value of M
        if(dim(x)[1] != dim(x)[2]){
            print("Warning: Matrix is not a Square Matrix")
        }
        M <<-x
        INV <<- NULL
        
    }
    get <- function() M # read value of M
    setINV <- function(inverse) INV <<- inverse # set the cached INV variable
    getINV <- function() INV # read the cached inverse variable
    
    list(set = set, get = get, setINV = setINV, getINV = getINV)
    
    
} 

## Write a short comment describing this function

## cacheSolve
## calculates inverse of a cacheMatrix when the value of cached Inverse is NULL
## returns the cached inverse otherwise

cacheSolve <- function(M, ...){
    INV <- M$getINV() # read cached Inverse value of the cacheMatrix
    if (!is.null(INV)) {
        message("getting cached data")
        return(INV) # returns the read value of cached inverse if it is not NULL
    }
    data <- M$get()
    INV <- solve(data,...) # calculates inverse of cacheMatrix
    M$setINV(INV) # sets cached Inverse value
    INV # returns calculated inverse
    
}
