## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#What this function does is to create a list which contains a function
#to set/get the value of a matrix and to set/get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    get<-function() x
    setInverseMatrix <-function(solve) m<<-solve
    getInverseMatrix<-function() m
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
#This function calculates the inverse of a matrix of the matrix created 
#with the makeCacheMatrix function. It doesn't check if the matrix is square.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
}
