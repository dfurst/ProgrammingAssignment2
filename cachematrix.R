makeCacheMatrix <- function(InputMatrix=matrix()) {   #This function creates a special "matrix" object that can cache its inverse
  m <- NULL                                           #initializes m 
  set <- function(y) {                                #sets InputMatrix to cache
    InputMatrix <<- y
    m <<- NULL                                        #once a new input matrix is set, m is reset to NULL
  }
  get<-function() x                                   #pulls the InputMatrix from the cache if it exists
  setInv <- function(solved) m <<- solved             #puts matrix inversion into cache as m
  getInv <- function() m                              #retreives matrix inversion from cache into m
  list(set = set, get = get,                          #lists out the 4 functions established
       setInv = setInv,
       getInv = getInv) }
#--------------------------------------
cacheSolve <- function(InputMatrix, ...) { #This function computes the inverse of the matrix returned by makeCacheMatrix 
  m <- InputMatrix$getInv()                 #retrieves the Inverse matrix
  if(!is.null(m)) {                         #determines if inverse matrix exists, if yes, it retrieves the inverse
    message("getting cached data")          
    return(m)                               #returns the saved matrix
  }
  message("calculating Inverse")        
  data <- InputMatrix$get()                 #gets the input matrix from cache
  m <- solve(data, ...)                     #saves the "solved" or calculated inverse of the input matrix
  InputMatrix$setInv(m)                     #saves the inverse to the cache
  return(m) 
  }
# ---------------------------------
x=matrix(c(1,2,2.9,2,0,2,3,2,1),3,3)        #creates the input matrix
xx<-makeCacheMatrix(x)                      #initializes the 4 functions, saves the input matrix to cache, sets m to NULL
xx                                          #shows the cached matrix
cacheSolve(xx)                             #returns the 1st iteration where the inversion is calculated
cacheSolve(xx)                             #returns the 2nd iteration where the cache is returned
