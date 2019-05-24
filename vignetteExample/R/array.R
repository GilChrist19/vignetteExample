#' Make an array
#'
#' this is a minimal reproducible example!
#'
#' @param names a character vector
#'
#' @export
makeArray <- function(names = c("AA", "Aa", "aa")){


  size <- length(names)
  tMatrix <- array(data=0, dim=c(size, size, size), dimnames=list(names, names, names))
  testVec <- setNames(object = numeric(size), nm = names)

  # fill up the array
  for (i in names)
  {
    for (j in names)
    {

      j_name <- strsplit(j, split='')[[1]]
      i_name <- strsplit(i, split='')[[1]]
      ij_prod <- as.vector( outer(j_name, i_name, paste0, sep='') )

      # sort
      ij_prod <- vapply( strsplit(ij_prod, split=''),
                           function(x) {paste0(sort(x, decreasing=TRUE), collapse='')},
                           FUN.VALUE = character(1))

      for (k in ij_prod)
      {
        testVec[k] <- testVec[k]+5
      }

      # testVec[] <- testVec/sum(testVec)

      tMatrix[i,j, ] <- testVec

      testVec[] <- 0
    }
  }


  return(tMatrix)
}
