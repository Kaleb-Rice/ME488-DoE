# Problem 3.26
alcohol_data <- data.frame(Chemist = factor(rep(1:4, each = 3)),Methyl_Alcohol = c(
    84.99, 84.04, 84.38,  # Chemist 1
    85.15, 85.13, 84.88,  # Chemist 2
    84.72, 84.48, 85.16,  # Chemist 3
    84.20, 84.10, 84.55   # Chemist 4
  )
)

# Problem 3.27S
battery_data <- data.frame(
  Brand = factor(rep(c("Brand 1", "Brand 2", "Brand 3"), each = 5)),
  Weeks = c(
    100, 96, 92, 96, 92,    # Brand 1
    76, 80, 75, 84, 82,     # Brand 2
    108, 100, 96, 98, 100   # Brand 3
  )
)