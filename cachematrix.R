##***********************************************************************************************
##***********************************************************************************************
##*                                                                                             *
##*            Filename: cachematrix.R                                                          *
##*  Included Functions: makeCacheMatrix                                                        *
##*                      cacheSolve                                                             *       
##*        Date Written: Saturday, June 21, 2014                                                *
##*              Author: Brian Farish                                                           *
##*         Description: This file implements two functions that demonstrate the following:     *
##*                      1) calculating the inverse of a matrix using "solve"                   *
##*                      2) storing the results of a calculation in "cache" for reuse           *
##*                      3) retrieving the cached value and displaying it                       *
##*                      4) detecting that the calculation has already been performed on        *
##*                         subsequent attempts to run the calculation and returning the        *
##*                         cached value instead of re-running the calculation.                 *
##*                                                                                             *
##***********************************************************************************************
##***********************************************************************************************

##***********************************************************************************************
##*                                                                                             *
##* makeCacheMatrix takes supplied parameters and builds a matrix which is stored in cache.     *
##* It also defines methods that can be used to access the results and manipulate it.           *
##*                                                                                             *
##***********************************************************************************************

makeCacheMatrix <- function(x = matrix()) {
        
        ##***************************************************************************************
        ##*  Initialize the m variable                                                          *
        ##***************************************************************************************
        m <- NULL        
        ##***************************************************************************************
        ##*   Define the "set" function to cache calculation results.                           *
        ##***************************************************************************************
        set <- function(y) {
                x <<- y
                m <<- NULL
        }        
        ##***************************************************************************************
        ##*   Define the "get" function to retrieve cached calculation results.                 *
        ##***************************************************************************************
        get <- function()x        
        ##***************************************************************************************
        ##*   Define the "setCacheMatrix" function to save off a matrix in cache.               *
        ##*   This cached matrix will also have had the inverse of the matrix derived.          *
        ##***************************************************************************************
        setCacheMatrix <- function(solve) m <<- solve        
        ##***************************************************************************************
        ##*   Define the "getCacheMatrix" function to retrieve the cached matrix.               *
        ##***************************************************************************************
        getCacheMatrix <- function() m        
        ##***************************************************************************************
        ##*   Return the functions as part of a list.                                           *
        ##***************************************************************************************
        list(set = set, get = get,
             setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix)
}

##***********************************************************************************************
##*                                                                                             *
##* cacheSolve checks to see if a result has been previously cached and if so, returns that     *
##* value.  If not, it does the matrix inversion and returns the value and caches it.           *
##*                                                                                             *
##***********************************************************************************************

cacheSolve <- function(x = matrix(), ...) {
        ##         
        ##***************************************************************************************
        ##*   Return a matrix that is the inverse of 'x'                                        *
        ##*   Refers to a method previously defined to retrieve the cached value                *
        ##***************************************************************************************
        ## 
              m <- x$getCacheMatrix()        
        ##***************************************************************************************
        ##*   Check to see if m has been defined and if so, retrieve that value and return it.  *
        ##***************************************************************************************
        ## 
              if(!is.null(m)) {
                      message("getting cached data")
                      return(solve(m))
              }
        ##         
        ##***************************************************************************************
        ##*   If no value was detected for m, do the calculation and return the inverted matrix.*
        ##***************************************************************************************
              data <- x$get()        
        ##***************************************************************************************
        ##*   Calculate the inverse of the provided matrix.                                     *
        ##***************************************************************************************
              m <- solve(data, ...)        
        ##***************************************************************************************
        ##*   Store off the calculated inverse matrix in cache.                                 *
        ##***************************************************************************************
              x$setCacheMatrix(m)        
        ##***************************************************************************************
        ##*   Return the calculated inverse matrix                                              *
        ##***************************************************************************************
              m
}
