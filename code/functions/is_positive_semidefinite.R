#' @title Helper function: Check positive definiteness of a matrix.
#'
#' @description Checks positive definiteness of a matrix, given a tolerance.
#'
#' @param mat a matrix.
#' @param message character string that specifies the message that should be displayed if `mat` is not PSD.
#'
#' @return Return the warning message specified in ` message` if `mat` is not PSD.
#' 


is_positive_semidefinite <- function(mat, message){

  # Calculate the eigenvalues of the matrix
  eigenvalues <- eigen(mat)$values
  
  # # Check if all eigenvalues are non-negative
  # is_positive_semidefinite <- all(eigenvalues >= 0)
  
  # With tolerance
  tol <- 1e-12
  is_positive_semidefinite <- all(eigenvalues > -tol)
  
  if(is_positive_semidefinite == FALSE){
    warning(message)
  }
  
}