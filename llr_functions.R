# llr function that computes the fits at a point zi and apply it to each element of z:
llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

# Compute f_hat function with optimized calculations:
compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_vector(z, x, omega)  # Now returns a vector
  X = make_predictor_matrix(x)
  
  # Replace Wz %*% X and Wz %*% y with optimized calculations
  WX = apply(X, 2, function(col) Wz * col)
  Wy = Wz * y
  
  # Compute the components
  XtWX = t(X) %*% WX
  XtWy = t(X) %*% Wy
  
  # Calculate f_hat
  f_hat = c(1, z) %*% solve(XtWX) %*% XtWy
  return(f_hat)
}

# Modify make_weight_matrix to return a vector instead:
make_weight_vector = function(z, x, omega) {
  W = function(r) {
    ifelse(abs(r) < 1, (1 - abs(r)^3)^3, 0)
  }
  distances = abs(x - z) / omega
  weights = W(distances)
  return(weights)
}

# Below is our task to write predictor matrix function:
make_predictor_matrix = function(x) {
  # Create the predictor matrix X with the first column as 1's and the second as x
  n = length(x)
  X = cbind(rep(1, n), x)
  return(X)
}

#for git test