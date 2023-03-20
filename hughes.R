source("hall.R")

# Start with the miniquaternion geometry. Rather than using i, j, and k, we can
# use a, b, and c. Then call their negatives 2a, 2b, and 2c, respectively.

miniquats <- c("0", "1", "2", "a", "c", "b", "2a", "2b", "2c")
addition_tbl <- matrix(rbind(c("0", "1", "2", "a", "c", "b", "2a", "2b", "2c"),
                             c("1", "2", "0", "c", "b", "a", "2b", "2c", "2a"),
                             c("2", "0", "1", "b", "a", "c", "2c", "2a", "2b"),
                             c("a", "c", "b", "2a", "2b", "2c", "0", "1", "2"),
                             c("c", "b", "a", "2b", "2c", "2a", "1", "2", "0"),
                             c("b", "a", "c", "2c", "2a", "2b", "2", "0", "1"),
                             c("2a", "2b", "2c", "0", "1", "2", "a", "c", "b"),
                             c("2b", "2c", "2a", "1", "2", "0", "c", "b", "a"),
                             c("2c", "2a", "2b", "2", "0", "1", "b", "a", "c")),
                       9)

mult_tbl <- matrix(rbind(c("0", "0", "0", "0", "0", "0", "0", "0", "0"),
                         c("0", "1", "2", "a", "c", "b", "2a", "2b", "2c"),
                         c("0", "2", "1", "2a", "2c", "2b", "a", "b", "c"),
                         c("0", "a", "2a", "2", "2b", "c", "1", "2c", "b"),
                         c("0", "c", "2c", "b", "2", "2a", "2b", "a", "1"),
                         c("0", "b", "2b", "2c", "a", "2", "c", "1", "2a"),
                         c("0", "2a", "a", "1", "b", "2c", "2", "c", "2b"),
                         c("0", "2b", "b", "c", "2a", "1", "2c", "2", "a"),
                         c("0", "2c", "c", "2b", "1", "a", "b", "2a", "2")),
                   9)

# Adding elements
add <- function(a, b) {
  n <- match(a, miniquats)
  m <- match(b, miniquats)
  addition_tbl[n, m]
}

# Multiplying elements
multiply <- function(a, b) {
  n <- match(a, miniquats)
  m <- match(b, miniquats)
  mult_tbl[n, m]
}

# The inverse of 0 is infinity.
# 1 is its own inverse. 2 is its own inverse.
# The inverses of a, b, and c are 2a, 2b, and 2c, respectively.
# The inverses of 2a, 2b, and 2c are a, b, and c, respectively.
inverses_vec <- c("infinity", "1", "2", "2a", "2c", "2b", "a", "b", "c")

# Finding inverses
invert <- function(a) {
  index <- match(a, miniquats)
  inverses_vec[index]
}

# -1 = 1, -2 = 2, -a = 2a, -b = 2b, -c = 2c, -2a = a, -2b = b, -2c = c
# We have used Boolean algebra to rename negatives.
negatives_vec <- c("0", "2", "1", "2a", "2c", "2b", "a", "b", "c")

# Finding negatives
negative <- function(a) {
  index <- match(a, miniquats)
  negatives_vec[index]
}

# a - b
subtract <- function(a, b) add(a, negative(b))

# Multiplicaton facts for the miniquaternion near field.
# Reference: Miniquaternion Geometry by Room and Kirkpatrick
# Addition test cases
print(add("a", "2b") == "1")
print(add("b", "2c") == "1")
print(add("c", "2a") == "1")
print(add("a", add("b", "c")) == "0")
print(add("c", add("b", "a")) == "0")

# Subtraction test cases
print(subtract("a", "b") == "1")
print(subtract("b", "c") == "1")
print(subtract("c", "a") == "1")
print(subtract("2", "1") == "1")
print(subtract("1", "2") == "2")
print(subtract("0", "2") == "1")

# Multiplication test cases
print(multiply("a", "b") == "c")
print(multiply("b", "c") == "a")
print(multiply("c", "a") == "b")
print(multiply("a", "c") == "2b")
print(multiply("b", "a") == "2c")
print(multiply("c", "b") == "2a")
print(multiply("a", multiply("b", "c")) == "2")
print(multiply("b", multiply("b", "a")) == "2a")

# Inverse test cases
print(invert("2") == "2")
print(invert("a") == "2a")
print(invert("2a") == "a")
print(invert("b") == "2b")
print(invert("0") == "infinity")

# Every point has three coordinates, c_1, c_2, and c_3. If we multiply all of
# the coordinates by the same constant, then the new coordinates still represent
# the same point. In other words, c_1, c_2, and c_3 are specifying a subspace 
# rather than a point. This function determines which point corresponds to the
# three inputted coordinates.
coords_to_pt <- function(c1, c2, c3) {
  if (c3 == "0") {
    if (c2 == "0") return(83)
    if (c1 == "0") return(82)
    prod <- multiply(c1, invert(c2))
    return(unname(which.max(prod == miniquats)) + 82)
  }
  c1 <- multiply(c1, invert(c3))
  c2 <- multiply(c2, invert(c3))
  n <- unname(which.max(c1 == miniquats))
  m <- unname(which.max(c2 == miniquats))
  return(9 * (m - 1) + n)
}

# All of these coordinate truples correspond to point 78
coords_to_pt("2", "a", "b")
coords_to_pt("1", "2a", "2b")
coords_to_pt("a", "1", "c")

# Expand coords_to_pt such that it accepts vectors as inputs too
ctp_vectorized <- Vectorize(coords_to_pt, c("c1", "c2"))

# Tests for points given vector coordinates
outer(miniquats, miniquats, ctp_vectorized, 1)
outer(miniquats, miniquats, ctp_vectorized, 2)
ctp_vectorized("2", "a", "b")
ctp_vectorized(miniquats, "2a", "2b")
ctp_vectorized(miniquats, "1", "c")

# Now set up a dataframe with 91 rows (one for each line) and 10 columns (for
# the 10 points on each line).
hughes_tbl <- matrix(data = NA, 91, 10)

# Start by making the lines that are parallel to the x-axis
# rows 1-9
for (line in 1:9) {
  vec <- c(ctp_vectorized(miniquats, miniquats[line], "1"),
         ctp_vectorized("0", "1", "0"))
  hughes_tbl[line, ] <- vec
}

# Then make the lines that are parallel to the y-axis
# rows 10-18
for (line in 1:9){
  vec <- c(ctp_vectorized(miniquats[line], miniquats, "1"),
         ctp_vectorized("1", "0", "0"))
  hughes_tbl[line + 9, ] <- vec
}

# Make the lines with slope 1, which are well-behaved
# rows 19-27
for (line in 1:9) {
  vec <- vector("numeric", 10)
  for (i in 1:9) {
    x <- miniquats[i]
    y <- add(x, miniquats[line])
    vec[i] <- ctp_vectorized(x, y, "1")
  }
  vec[10] <- ctp_vectorized("1", "1", "0")
  hughes_tbl[line + 18, ] <- sort(vec)
}

# Make the lines with slope 2, which are also well-behaved
# rows 28-36
for (line in 1:9) {
  vec <- vector("numeric", 10)
  for (i in 1:9) {
    x <- miniquats[i]
    y <- add(multiply("2", x), miniquats[line])
    vec[i] <- ctp_vectorized(x, y, "1")
  }
  vec[10] <- ctp_vectorized("2", "1", "0")
  hughes_tbl[27 + line, ] <- sort(vec)
}

# We still have to account for the other six elements, a, b, c, 2a, 2b, and 2c.
# There is a rule that is spelled out in detail in the book by Room and
# Kirkpatrick where the slope k is one of the other 6 algebraic elements. This
# function uses their rule to generate all the lines with slope k.
rs <- c("0", "1", "2")
for(k in 4:9) {
  K <- miniquats[k]
  v <- numeric(10)
  for (t in (1:3)){
    for(u in (1:3)){
      r <- rs[t]
      s <- rs[u]
      for (i in 1:9){
        x <- miniquats[i]
        y <- add(s, multiply(K,subtract(x,r)))
        cat(r,s,x,y,ctp_vectorized(x,y,"1"),"\n")
        v[i] <- ctp_vectorized(x,y,"1")
      }
      v[10] <- ctp_vectorized(K,"1","0")
      hughes_tbl[9*k+3*(t-1)+u,] <- sort(v)
    }
  }
}
subtract("c", "0")
subtract("2c", "0")
K
multiply(K, "c")
multiply(K, "2c")
hughes_tbl[91,] <- 82:91
hughes_tbl[19,]
hughes_tbl[28,]
hughes_tbl[84,]
pts_to_line_test(hughes_tbl)
hughes_tbld <- create_dual(hughes_tbl)
pts_to_line_test(hughes_tbld)

collinear <- function(p1,p2,p3,p4){
  return(shared_line(p1,p2) == shared_line(p3,p4) )
}

collinear3 <- function(p1,p2,p3){
  return(shared_line(p1,p2) == shared_line(p1,p3) )
}

