## In This Assignment makeCacheMatrix first create an object of a matrix that cached its solve
# And The Second Function cacheSolve Check the object , if its cache exsist return it else return solve(matrix) if the matrix is square

## This Function Create An Object Of A Matrix That Carry Its Value And Cache And Fucntions For Setting And Getting Value
 

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function()x
  
  setinv<-function(inv) s<<-inv
  getinv<-function() s
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This Function Check The Getinv of the Object That Created By Prev Function If Its Solve Cache Exist Then Return It , Else Compute The Invert Of The Matrix By Solve(matrix) if The Matrix is Square.

cacheSolve <- function(x, ...) {
  s<-x$getinv()
  if (!is.null(s)){
    message("Getting Cached Data")
    return(s)
    
  }
  data<-x$get()
  if (!(dim(data)[1]==dim(data)[2])) {
    message("Matrix Must Be Square")
    return (NULL)
  }
  s<-solve(data,...)
  x$setinv(s)
  s
}
