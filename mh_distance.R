mh_distance <- function(x, y){
  if (typeof(x) != typeof(y)){
    warning("x and y different vector types")
    return(-1)
  }
  if(is.na(x) == TRUE || is.na(y) == TRUE || 
     is.nan(x) == TRUE || is.nan(y) == TRUE){
    warning("x or y is NA or NaN")
    return(-1)
  }
  if(is.infinite(x) == TRUE || is.infinite(y) == TRUE){
    warning("x or y is an infinite value")
    return(-1)
  }
  if ((nchar(x) != nchar(y)) 
      & (typeof(x) != "logical")){
    warning("x and y do not have the same number of digits or letters")
    return(-1)}
  if ((is.numeric(x) == TRUE) & (is.numeric(y) == TRUE)){
    if ((x %% 1 != 0) || (y %% 1 != 0)){
      warning("x or y contains a decimal")
      return(-1)
    }
  }
  if (typeof(x) == "character"){
    x_list = strsplit(x, "")
    y_list = strsplit(y, "")
    x_vector = unlist(x_list)
    y_vector = unlist(y_list)
    number = x_vector != y_vector
    return(sum(number, na.rm = TRUE))
  }
  if (typeof(x) == "double"){
    xchar = toString(x)
    ychar = toString(y)
    x_list = strsplit(xchar, "")
    y_list = strsplit(ychar, "")
    x_vector = unlist(x_list)
    y_vector = unlist(y_list)
    number = x_vector != y_vector
    return(sum(number, na.rm = TRUE))
  }
  if (typeof(x) == "logical"){
    if (((x == TRUE) & (y == TRUE)) || ((x == FALSE) & (y == FALSE))){
      return (0)
    }
    else{
      return(1)
    }
  }
}