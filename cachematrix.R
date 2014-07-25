## A set of functions that create and cache the inverse of a matrix

## Create a special matrix to be inversed
makeCacheMatrix <- function(m = matrix()) {   ## Although stylistically different than the assignment sample code, I prefer to use curly brackets for all functions.      
        ## First we intialize an object for the inverse matrix  
        im <- NULL
        
        ## A nested (internal) function to set the matrix  
        set <- function(matrix) {
                m <<- matrix
                im <<- NULL
        }
        
        ## A nested (internal) function to get the matrix
        get <- function() {
                
                ## Return the resulting matrix 
                m
        }
        
        ## A nested (internal) function to set the inverse of the matrix
        setinverse <- function(solve){
                im <<- solve
        }
        
        ## A nested (internal) function to get the inverse of the matrix
        getinverse <- function(){
                
                ## Return the inverse matrix
                im
        }
        
        ## Return a list of the the nested (internal) functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## A function that calculates the inverse of the special matrix calculated above with "makeCacheMatrix". 
## The function "cacheSolve" first checks if the inverse matrix has already been calculated and if so, 
## the inverse matrix is retreived from memory, otherwise the inverse matrix is caluclated and returned
cacheSolve <- function(m, ...) {
        
        ## Get the inverse matrix        
        im <- m$getinverse()
        
        ## Test to see if inverse matrix already exists, if it exists return it and inform the use it was cached  
        if(!is.null(im)) {
                message("getting cached matrix")
                return(im)
        }
        
        ## If the inverse matrix does not exist, get the initial matrix and calculate the inverse 
        data <- m$get()
        im <- solve(data, ...)
        
        ## Set the inverse matrix
        m$setinverse(im)
        
        ## Return the inverse matrix of "m'
        im
        
}
