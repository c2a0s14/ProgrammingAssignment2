
## Below are two functions (makeCacheMatrix and cacheSolve)
## that are used to create a special object which allows
## storing (caching) a data matrix and its inverse.
## These functions assume that the matrix supplied is always a 
## square invertible matrix.





##################################################################
#################       makeCacheMatrix       ####################
##################################################################
## makeCacheMatrix creates a new special object (a list) for
## working with a square invertible matrix and its cached inverse.
## This creates the functions to:
##     see if the input data is identical to the previously cached data
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse of the matrix 
##     get the value of the inverse of the matrix
## This returns a list to allow use of all functions created.
##################################################################

makeCacheMatrix <- function(x = matrix()) {
        ## Argument 'x' must be a square invertible matrix
        
        ## initialize environment variables.
        
        inv <- NULL
        cached_matrix <- NULL
        same <- FALSE
        
        ## 'see_If_Same' ... determines if the input matrix 'y' is already cached.
        
        see_If_Same <- function(y) {
                if (!identical(cached_matrix, y)) {
                        same <<- FALSE
                } 
                else {
                        same <<- TRUE
                }
        }
        
        ## 'set' ... Sets the value of 'x' after determining if 'y' is new.
        ##      If 'y' is identical to the previously cached matrix, then 'set' does nothing.
        ##      If 'y' is a new matrix, then 'set':
        ##              preserves the previous 'x' into 'cached_matrix', 
        ##              resets x to the value of the new matrix 'y', and 
        ##              resets 'inv' to NULL to signal that calculation of the inverse is required.
        ## The argument 'y' is the input matrix containing the matrix to be inverted        

        set <- function(y) { 
                same <- see_If_Same(y)
                if (!same) {
                        message("..This is a new input matrix... Resetting the inverse to NULL")
                        cached_matrix <<- x
                        x <<- y         
                        inv <<- NULL    
                }
        }
        
        ## 'get' ... Returns the matrix 'x' from the special object
        
        get <- function() x
        
        ## 'setInverse' ... assigns the value 'inverse' to the 'inv' environment variable.
        ## The argument 'inverse' is of type 'matrix' and holds the calculated inverse to store.
        
        setInverse <- function(inverse) inv <<- inverse
        
        ## 'getInverse' ...  returns the current value of the 'inv' environment variable 
        
        getInverse <- function() inv
        
        
        ## Create and return a list of all 5 functions defined above.
        
        matrix_object <- list(see_If_Same = see_If_Same,
                              set = set, 
                              get = get,
                              setInverse = setInverse, 
                              getInverse = getInverse)
        
        return(matrix_object)
        
}


##################################################################
#################          cacheSolve         ####################
##################################################################
## This function computes the inverse of the special object that  
## was created by makeCacheMatrix above.  It first calls
## 'cm$see_If_Same' to ensure this matrix is not a duplicate of 
## the previously cached data.  If the data has not changed,
## and if the inverse has already been calculated, then cacheSolve
## retrieves the cached inverse and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse matrix in the cm argument.
##################################################################

cacheSolve <- function(cm, ...) {
        ## The 'cm' argument is the special object obtained from 'MakeCacheMatrix'.
        
        ## Get the data matrix that is currently stored.
        data <- cm$get()
        
        ## See if the input data in cm is the same as the previously-cached-data.  If not
        ## identical, then call 'cm$set' to set the data and reset inv to NULL
        same <- cm$see_If_Same(data)
        if (!same){
                cm$set(data)    ## will set the inverse to NULL if this is new input data
        }         
        
        ## See if the inverse has already been cached
        inverse <- cm$getInverse()
        if(!is.null(inverse)) {
                message("..getting cached inverse matrix")
                return(inverse)
        }
        
        message("..calculating inverse matrix from new input data")
        inverse <- solve(data)
        cm$setInverse(inverse)
        return(inverse)     
        
        
}
