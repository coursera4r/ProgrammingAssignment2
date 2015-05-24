## makeCacheMatrix sets up a copy of a matrix that can have its inverse computed with the solve function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        setM<-function(y){
        x<<-y
        m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve #this is where the actual computation takes place
        getinverse<-function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #a list that holds the inverse of the matrix
}


## cacheSolve actually returns the inverse of the matrix x
cacheSolve <- function(x, ...) {
      
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)}
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
