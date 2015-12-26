## CacheSolve will take an input and test for previous saved results
## in global vars, incliding the base matrix.  If no results are saved,
## test for invertibility, solve using the makeCacheMatrix to ensure
## future presence of result in global var

## Invert matrix, store original and invert in global vars

makeCacheMatrix <- function(x = matrix()) {
  mat_orig <<- x
  matsq_scoped <<- solve(x)
}


## invoke this to invert a matrix.  Will call makeCacheMatrix
## if the global vars do not exit.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matdif <- "FALSE"
  if (exists("matsq_scoped"))
  {
    orig <- mat_orig
    if (identical(x, orig))
    {
      message("Getting cached matrix")
      return(matsq_scoped)
      
    }
    else
    {
      message("Matrix has changed, recomputing.")
      matdif <- "TRUE"
    }
  }
  
  if (matdif <- "FALSE")
  {
    message("Nothing cached, recomputing...")
  }
  
  #cache results if the matrix is invertible
    out <- tryCatch(
      determinant(x), error = function(e)
        e
    )
    any(class(out) == "error")
    if (is.null(out$modulus))
    {
      return(print("Please input a sqaure matrix."))
    }
    else
    {
      makeCacheMatrix(x)
      return(matsq_scoped)
      
    }
}

