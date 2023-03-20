source("hall.R")

# The Fundamental Theorem of Projective Geometry: Given any projective plane,
# a coordinate system can be introduced so that there are four points whose
# coordinates are in some order (1, 0, 0), (0, 1, 0), (0, 0, 1), and (1, 1, 1).

# As such, (0, 0, 1) is the origin of coordinates in the affine plane.
# (0, 1, 0) and (0, 0, 1) are the points at infinity along the direction of the 
# x-axis and the direction of the y-axis, respectively. (1, 1, 1) is the point
# in the affine plane where x and y are both equal to 1.
x_coords <- numeric(length = 91)
y_coords <- numeric(length = 91)
w <- 0
x <- 0
y <- 0
z <- 0

# The point at infinity reached via the line with slope 1
pt_at_infty <- 0

# abstract_coords is a vector of length 9 containing the abstract coordinates
# names(hall_coords) sets the names of the vector elements to correspond 
# to the coordinates in the projective plane.
abstract_coords <- c("0", "1", "2", "a", "b", "c", "d", "e", "f")
hall_coords <- c("0", "1", "2", "a", "a+1", "a+2", "2a", "2a+1", "2a+2")
af_coords <- character(9)