## Coursera: R Programming
## October 2020
## author: Arnold2811

## The makeCacheMatrix function creates as special matrix which is a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of hte inverse of the matrix
# 4) get the value of hte inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { #function argument requires a matrix
  i <- NULL             #initializing two objects: x and i
  set <- function(y) {  #takes argument named as y  
    x <<- y             #assigns input argument to the x object in the parent environment
    i <<- NULL          #assigns the value of NULL to i; clears any value for i if previously cached
  }
  get <- function() x                            #defines the getter for the matrix x 
  setinverse <- function(inverse) i <<- inverse  #defintes the setter for inverse i
  getinverse <- function() i                     #defintes the getter of inverse i

  #assigns each of these functions as an element within a list
  list(set = set,    #gives the name 'set' to the set() function defined above
       get = get,    #gives the name 'get' to the get() function defined above
       setinverse = setinverse,  #gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse)  #gives the name 'getinverse' to the getinverse() function defined above
}



#This function is required to populate and/or retrieve the inverse from a matrix of type in makeCacheMatrix
#The cacheSolve function calculates the inverse of the input matrix created with the makeCacheMatrix
#It first checks if the inverse have previously been calculated. If so, it retrieves it from the cache and skips computation
#Otherwise, it computes the inverse of the matrix and sets the value of inverse

cacheSolve <- function(x, ...) {    #starts with a single argument x and ellipsis which allows passing of additional arguments
  i <- x$getinverse()               #attempts to retrieve an inverse from the object passed as argument
  if(!is.null(i)) {                 #checks whether result is NULL
    message("getting cached data")  #if value is not equal to NULL, it shows a message and then
    return(i)                       #we have a valid, cached inverse and return it to the parent environment
  }
  data <- x$get()                   #assigns the matrix
  i <- solve(data, ...)             #runs the function 'solve' to calculate the inverse of the matrix
  x$setinverse(i)                   #sets the value of inverse in the cache via the setinverse function
  i
}
