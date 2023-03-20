source("read_hall.R")

# Exploring Hall Planes Via Dr. Paul Bamberg's Instruction

# Matrix of numbers that specifies which points are on each line
# The 91 rows correlate to the 91 lines
# The 10 columns correlate to the 10 points on each line
hall_tbl <- matrix(data = NA, 91, 10)

# Dual of hall_tbl
# Matrix of numbers that specifies which 10 lines each point lies on
dual_tbl <- matrix(data = NA, 91, 10)

# Moorhouse's website gives the points and lines
# https://ericmoorhouse.org/handouts/
# The planes were specified in Hall's article
# https://www.ams.org/journals/tran/1943-054-02/S0002-9947-1943-0008892-4/S0002-9947-1943-0008892-4.pdf


# Addition table - elementary abelian group of order 9; addition table for
# polynomials of the form ax + b with a, b in Z3
# The additive group in all systems
# 1 ~ 0
# 2 ~ 1
# 3 ~ 2
# 4 ~ a
# 5 ~ a+1
# 6 ~ a+2
# 7 ~ 2a
# 8 ~ 2a+1
# 9 ~ 2a + 2
addition_tbl <- matrix(rbind(c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                             c(2, 3, 1, 5, 6, 4, 8, 9, 7),
                             c(3, 1, 2, 6, 4, 5, 9, 7, 8),
                             c(4, 5, 6, 7, 8, 9, 1, 2, 3),
                             c(5, 6, 4, 8, 9, 7, 2, 3, 1),
                             c(6, 4, 5, 9, 7, 8, 3, 1, 2),
                             c(7, 8, 9, 1, 2, 3, 4, 5, 6),
                             c(8, 9, 7, 2, 3, 1, 5, 6, 4),
                             c(9, 7, 8, 3, 1, 2, 6, 4, 5)),
                       9,
                       dimnames = list(1:9, 1:9))

# Case 1: Field constructed using the irreducible polynomial for which the 
# replacement rule is z^2 = 2
mult_tbl_2 <- matrix(rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                           c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                           c(1, 3, 2, 7, 9, 8, 4, 6, 5),
                           c(1, 4, 7, 3, 6, 9, 2, 5, 8),
                           c(1, 5, 9, 6, 7, 2, 8, 3, 4),
                           c(1, 6, 8, 9, 2, 4, 5, 7, 3),
                           c(1, 7, 4, 2, 8, 5, 3, 9, 6),
                           c(1, 8, 6, 5, 3, 7, 9, 4, 2),
                           c(1, 9, 5, 8, 4, 3, 6, 2, 7)),
                     9)

# Case 2: Field constructed using the irreducible polynomial for which the 
# replacement rule is z^2 = z + 1
mult_tbl_z_1 <- matrix(rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                             c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                             c(1, 3, 2, 7, 9, 8, 4, 6, 5),
                             c(1, 4, 7, 5, 8, 2, 9, 3, 6),
                             c(1, 5, 9, 8, 3, 4, 6, 7, 2),
                             c(1, 6, 8, 2, 4, 9, 3, 5, 7),
                             c(1, 7, 4, 9, 6, 3, 5, 2, 8),
                             c(1, 8, 6, 3, 7, 5, 2, 9, 4),
                             c(1, 9, 5, 6, 2, 7, 8, 4, 3)),
                       9)

# Case 3: Field constructed using the irreducible polynomial for which the 
# replacement rule is z^2 = 2z + 1
mult_tbl_2z_1 <- matrix(rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                              c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                              c(1, 3, 2, 7, 9, 8, 4, 6, 5),
                              c(1, 4, 7, 8, 2, 5, 6, 9, 3),
                              c(1, 5, 9, 2, 6, 7, 3, 4, 8),
                              c(1, 6, 8, 5, 7, 3, 9, 2, 4),
                              c(1, 7, 4, 6, 3, 9, 8, 5, 2),
                              c(1, 8, 6, 9, 4, 2, 5, 3, 7),
                              c(1, 9, 5, 3, 8, 4, 2, 7, 6)),
                        9)

# Four other non-isomorphic systems specified by Veblen-Wedderburn.

# Hall's System R. The Group System
# Found from doubly transitive group on 9 letters where the identity fixes two
# of the letters
mult_tbl_HR <- matrix(rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                            c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                            c(1, 3, 2, 7, 9, 8, 4, 6, 5),
                            c(1, 4, 7, 3, 8, 5, 2, 9, 6),
                            c(1, 5, 9, 6, 3, 7, 8, 4, 2),
                            c(1, 6, 8, 9, 4, 3, 5, 2, 7),
                            c(1, 7, 4, 2, 6, 9, 3, 5, 8),
                            c(1, 8, 6, 5, 7, 2, 9, 3, 4),
                            c(1, 9, 5, 8, 2, 4, 6, 7, 3)),
                      9)

# Hall's System S. z^2 = z + 1
# 0, 1, and 2 lie in the center. Every other element z satisfies z^2 = z + 1
mult_tbl_HS <- matrix(rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                            c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                            c(1, 3, 2, 7, 9, 8, 4, 6, 5),
                            c(1, 4, 7, 5, 2, 9, 6, 3, 8),
                            c(1, 5, 9, 8, 6, 2, 3, 7, 4),
                            c(1, 6, 8, 2, 7, 4, 9, 5, 3),
                            c(1, 7, 4, 9, 3, 5, 8, 2, 6),
                            c(1, 8, 6, 3, 4, 7, 5, 9, 2),
                            c(1, 9, 5, 6, 8, 3, 2, 4, 7)),
                      9)

# Hall's System T. z^2 = 2z + 1
# 0, 1, and 2 lie in the center. Every other element z satisfies z^2 = 2z + 1
mult_tbl_HT <- matrix(rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                            c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                            c(1, 3, 2, 7, 9, 8, 4, 6, 5),
                            c(1, 4, 7, 8, 6, 2, 9, 5, 3),
                            c(1, 5, 9, 2, 7, 4, 6, 3, 8),
                            c(1, 6, 8, 5, 2, 9, 3, 7, 4),
                            c(1, 7, 4, 6, 8, 3, 5, 9, 2),
                            c(1, 8, 6, 9, 3, 5, 2, 4, 7),
                            c(1, 9, 5, 3, 4, 7, 8, 2, 6)),
                      9)

# Hall's System U. 2 not in center
# Consists of 0 and 1 only
mult_tbl_HU <- matrix(rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                            c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                            c(1, 3, 2, 7, 9, 8, 4, 6, 5),
                            c(1, 4, 8, 6, 2, 7, 5, 9, 3),
                            c(1, 5, 7, 9, 6, 3, 2, 4, 8),
                            c(1, 6, 9, 3, 7, 5, 8, 2, 4),
                            c(1, 7, 6, 8, 3, 4, 9, 5, 2),
                            c(1, 8, 5, 2, 4, 9, 6, 3, 7),
                            c(1, 9, 4, 5, 8, 2, 3, 7, 6)),
                      9)


# Function that checks whether the addition or multiplication tables are
# commutative
abelian_test <- function(table) {
  for (i in 1:8) {
    for (j in (1 + i):9) {
      if (table[i, j] != table[j, i]) return(FALSE)
    }
  }
  TRUE
}

# The addition table is always abelian
abelian_test(addition_tbl)

# If using a finite field table, the multiplication table is always abelian
abelian_test(mult_tbl_2)
abelian_test(mult_tbl_z_1)
abelian_test(mult_tbl_2z_1)

# If using one of Hall's systems, the multiplication table is not abelian.
# All four of these systems are merely near fields.
abelian_test(mult_tbl_HR)
abelian_test(mult_tbl_HS)
abelian_test(mult_tbl_HT)
abelian_test(mult_tbl_HU)


# Vectorized addition table
vect_addition_tbl <- Vectorize(function(i, j) {
  as.numeric(addition_tbl[i, j])
})

# Function that checks whether the left distributive law holds for an addition
# or multiplication table
left_distr_test <- function(table) {
  expected_results <- list(
    row9 <- vect_addition_tbl(table[, 6], table[, 4]),
    row8 <- vect_addition_tbl(table[, 5], table[, 4]),
    row7 <- vect_addition_tbl(table[, 4], table[, 4]),
    row6 <- vect_addition_tbl(table[, 4], table[, 3]),
    row5 <- vect_addition_tbl(table[, 4], table[, 2]),
    row3 <- vect_addition_tbl(table[, 2], table[, 2])
  )
  actual_results <- list(
    row9 <- as.numeric(table[9, ]),
    row8 <- as.numeric(table[8, ]),
    row7 <- as.numeric(table[7, ]),
    row6 <- as.numeric(table[6, ]),
    row5 <- as.numeric(table[5, ]),
    row3 <- as.numeric(table[3, ])
  )
  return(identical(expected_results, actual_results))
}

# The left distributive law holds for the finite fields
left_distr_test(mult_tbl_2)
left_distr_test(mult_tbl_z_1)
left_distr_test(mult_tbl_2z_1)

# The left distributive law does not hold for Hall's systems
left_distr_test(mult_tbl_HR)
left_distr_test(mult_tbl_HS)
left_distr_test(mult_tbl_HT)
left_distr_test(mult_tbl_HU)

# Function that checks whether the right distributive law holds for an addition
# or multiplication table
right_distr_test <- function(table) {
  expected_results <- list(
    row9 <- vect_addition_tbl(table[6, ], table[4, ]),
    row8 <- vect_addition_tbl(table[5, ], table[4, ]),
    row7 <- vect_addition_tbl(table[4, ], table[4, ]),
    row6 <- vect_addition_tbl(table[4, ], table[3, ]),
    row5 <- vect_addition_tbl(table[4, ], table[2, ]),
    row3 <- vect_addition_tbl(table[2, ], table[2, ])
  )
  actual_results <- list(
    row9 <- as.numeric(table[9, ]),
    row8 <- as.numeric(table[8, ]),
    row7 <- as.numeric(table[7, ]),
    row6 <- as.numeric(table[6, ]),
    row5 <- as.numeric(table[5, ]),
    row3 <- as.numeric(table[3, ])
  )
  return(identical(expected_results, actual_results))
}

# The right distributive law holds for the finite fields
right_distr_test(mult_tbl_2)
right_distr_test(mult_tbl_z_1)
right_distr_test(mult_tbl_2z_1)

# The right distributive law also holds for Hall's systems
right_distr_test(mult_tbl_HR)
right_distr_test(mult_tbl_HS)
right_distr_test(mult_tbl_HT)
right_distr_test(mult_tbl_HU)

# We can also take the transpose of each multiplication table to simulate what
# happens if we take a standard near field and reverse the roles of rows and
# columns. The result is a different projective plane that is isomorphic to the
# Hall plane, but it is different because the roles of points and lines are
# interchanged. These represent our dual systems. In projective geometry, it is 
# not possible to distinguish the roles of point and lines without written 
# context because of this strict duality where one can interchange points and 
# lines.

# For the near field, it is also true that for every row and every column,
# each of the symbols shows up. This property can still hold even if we do not
# have a field. This property exists if there is a loop.
loop_test <- function(table) {
  for (i in 2:9) {
    if (!all.equal(2:9, unname(sort(table[i, 2:9])))) return(FALSE)
  }
  TRUE
}

# There is looping in the finite fields
loop_test(mult_tbl_2)
loop_test(mult_tbl_z_1)
loop_test(mult_tbl_2z_1)

# There is also looping in Hall's systems
loop_test(mult_tbl_HR)
loop_test(mult_tbl_HS)
loop_test(mult_tbl_HT)
loop_test(mult_tbl_HU)

# In a projective plane, any pair of lines determines a point and any pair of
# points determines a line.

# The following function generates an incident table for a projective plane. It
# works for field planes and Hall planes. First, it creates all the lines in 
# slope-intercept form. Since multiplication is not commutative, it is very
# important to ensure that the slope is multiplied on the right of the
# coordinate x. If that convention is reversed, then the dual plane is
# generated.

# Once we have the multiplication table from Hall's 1943 article and the 
# standard elementary abelian addition table, we can create the entire
# projective plane. Basically, we say, for each committee we start with 
# equation satisfied by the committee and crank it out.

# rows 1-9: y = c for some constant c
# rows 10-18: x = c for some constant c
# rows 19-82: y = slope(x) + intercept
incidence_tbl <- function(add_tbl, mult_tbl) {
  i_tbl <- matrix(data = NA, 91, 10)
  for (y in 1:9) i_tbl[y, 1:9] <- (9 * y - 8):(9 * y)
  for (x in 1:9) i_tbl[9 + x, 1:9] <- x + 9 * (0:8)
  for (slope in 2:9) {
    for (intercept in 1:9) {
      vec <- c()
      for (x in 1:9) {
        y <- add_tbl[mult_tbl[x, slope], intercept]
        p <- c(vec, as.numeric(9 * (y - 1) + x))
      }
      i_tbl[9 * slope + intercept, 1:9] <- sort(vec)
    }
  }
  for (i in 0:9) {
    for (j in 1:9) i_tbl[9 * i + j, 10] <- i + 82
  }
  i_tbl[91, ] <- 82:91
  i_tbl
}

# We can also create the dual table
create_dual <- function(table) {
  slope <- vector("list", 91)
  for (i in (1:91)) {
    for(j in (1:10)) {
      k <- table[i, j]
      slope[[k]] <- c(slope[[k]], i)
    }
  }
  dual <- matrix(data = NA, 91, 10)
  for (i in (1:91)) dual[i, ] <- slope[[i]]
  dual
}

# Then, after creating the incidence table, this function checks that the result
# really is a projective plane by seeing if a line can always be generated from
# any two given points.
pts_to_line_test <- function(table) {
  ans <- TRUE
  for (i in (1:90)) {
    for (j in ((i + 1):91)) {
      if (!(length(intersect(table[i, ], table[j, ])) == 1)) {
        !ans
        cat(i, j, length(intersect(table[i, ], table[j, ])), "\n")
      }
    }
  }
  ans
}

# shared_pt gives the point where two lines intersect
shared_pt <- function(a, b) {
  paste("The point that lines ", a, " and ", b, " have in common is ",
        intersect(table[a, ], table[b, ]))
}

# shared_line gives the line where two points intersect by using the dual plane
shared_line <- function(a, b) {
  paste("The line where points ", a, " and ", b, " intersect is ",
        intersect(dual_tbl[a, ], dual_tbl[b, ]))
}

# It is sometimes useful to check when points are collinear, so this function
# checks if the inputted points are collinear.
collinear_test <- function(a, b, c, d = 0) {
  if (d == 0) return(shared_line(b, c) == (shared_line(a, b)))
  shared_line(c, d) == (shared_line(a, b))
}

# Reference: Chapters 5 and 6 of Projective Geometry by Rey Casse

