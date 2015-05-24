## makeCacheMatrix creates a copy of a matrix x that can have its inverse easily found using the solve function
##cacheSolve computes the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        setM<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)}
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
