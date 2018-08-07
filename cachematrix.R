## This function creates a special "matrix" object that can cache its inverse.

##to set the value of the vector
#to get the value of the vector
#to set the value of the inverse matrix
#to get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) cachematrix<- function(x=matrix()){
    m<-NULL
#create a null variable
    set function(y)
    {
        x<<-y
        m<<-NULL
    }
    get<- function()x
#set inverse function
    setinverse<-function(inverse)m<<-inverse
    getinverse<-function()m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data=x$get()
    m=solve(data,...)
    x$setinverse(m)
    m
}
