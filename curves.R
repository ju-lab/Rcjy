# Simple function to draw sine curve between two points 
# Use linear transformation to shift sine curve. 
transform<-function(x, y, scale, theta, h, v){
  x_t = scale* ( x*cos(theta) - y * sin(theta)) + h
  y_t = scale * (x*sin(theta) + y * cos(theta)) + v

  return(list(x_t, y_t))
}


sine_curve <- function(x1, y1, x2, y2, color='black'){
  # Y=sin(X)
  X<- seq(0, pi, length.out=100)
  Y<- sin(X)
  
  # length scale
  scale = sqrt((y2-y1)**2 + (x2-x1)**2) / (pi)
  # Rotation angle
  theta = atan((y2-y1)/(x2-x1))
  
  # transformed vectors
  transformed_sin = transform(X, Y, scale, theta, min(x1,x2), min(y1,y2))
  lines(transformed_sin[1][[1]],transformed_sin[2][[1]], col = color)
  return(transformed_sin)
}


plot(-10:10, -10:10)
sin_curve(1, 10, 10, 10)
sin_curve(1, 1, 2, 2)
sin_curve(2, 2, 1, 1,color="red")
sin_curve(1, 1, 3, 3)
sin_curve(3, 3, 1, 1)
