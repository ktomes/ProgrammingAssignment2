## First, a specail thanks to Fu Sheng Wang in the discussion forum. Without his
## description on how the examples 'makeVector' and 'cachemean' work I would most certainly
## have submitted this assignment late :)

## The following functions "makeCacheMatrix" and "cacheSolve " work together 
## to take a matrix as an arguement, create the inverse of the matrix and cache it

## a special "matrix" object that can cache its inverse
## The makeCacheMatrix function takes an argument x of type matrix and defines 
## 4 functions that are returned as a list. You can then call any functions in the list.
## Ex: a <- makeCacheMatrix(c(1,2,3)) then a$set(matrix(5:8,2)), a$get(), a$setInverse(), a$getInverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { # set function will take matrix as arg and write to cache
                x <<- y
                m <<- NULL
        }
        get <- function() x  # get function returns a matrix from cache
        setInverse <- function(solve) m <<- solve # set function uses solve to return the inverse, then write to cache
        getInverse <- function() m # getInverse returns invers of Matrix
        
        ## returns a list that of functions that can be called via an object created by calling "makeCachMatrix
        ## or by "cacheSolve" the client function.
        list(set = set, 
             get = get,
             getInverse = getInverse,
             setInverse = setInverse)
        
}

## cacheSolve is a client function that uses the makeCacheMatrix function in its implementation
## It expects a "special matrix" returned by makeCacheMatrix. The output is the inverse which 
## comes from either cache or, if not in cache, is computed using the 'solve' function, written 
## to cache and then returned 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()           # call getInverse() so see if inverse is in cache         
        if(!is.null(m)) {             # if there is a cache
                message("getting cached matrix") 
                return(m)             # return the cached matrix, no computation needed
        }
        data <- x$get()               # if there's no cached matrix
        m <- solve(data, ...)         # we create the inverse
        x$setInverse(m)               # and save the result back to cache
        m                             # and return the matrix inverse        
        
}
