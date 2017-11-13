
# ChiSquare distribution arises when random cross-categories are put 
# through the observed-expected formula:

# here, a and b are independent
a <- sample(1:3, size=30, replace=T)
b <- sample(1:3, size=30, replace=T)
a
b
t <- table(a,b)  # observed
t

# what is "Expected?"
# Well, if the rows and cols are separate, we can use the row and col proportions
#  to see what "should" be in each cell if they are uncorrelated.

# Be sure you follow what's happening in the next four lines!
row_proportions <- rowSums(t)/sum(t)
col_proportions <- colSums(t)/sum(t)
expected_proportions <- col_proportions %*% t(row_proportions)
expected_counts <- expected_proportions * sum(t)

# let's simulate drawing these things a bunch, and see 
# what the distribution of the chi-sq statistic is
chi_sq_values <- NULL  # we'll store our chi-squared values here
tables <- list()  # and each table here for later inspection
for(i in 1:1000){  # 1000 simulations
  a <- sample(1:3, size=50, replace=T)  # much like above
  b <- sample(1:3, size=50, replace=T)
  t <- table(a,b)  # observed
  tables[[i]] <- t  # put table into list of tables for later
  row_proportions <- rowSums(t)/sum(t)  # like above
  col_proportions <- colSums(t)/sum(t)
  expected_proportions <- col_proportions %*% t(row_proportions)
  expected_counts <- expected_proportions * sum(t)
  cs <- sum((t - expected_counts)^2 / expected_counts)  # sum of observed-expected squared, divided by expected
  chi_sq_values <- c(chi_sq_values, cs)  # add that value to our vector of values
}

# This is the distribution of the chi-squared values we got:  
hist(chi_sq_values)
# It is a random variable that follows a Chi-Squared Distribution.

# Note that because of randomness, it's totally normal to get some odd ones!
# let's look at a weird one:
biggest_cs <- which(chi_sq_values == max(chi_sq_values))
tables[[biggest_cs]]
# yep, it looks like those are correlated, even though we know they aren't!

# So, in the Chi-Squared test, we assume variables aren't correlated
# Then calculate the Chi-Squared value (observed - expected, etc etc)
# Then, see how large that is.
# If it's very large compared to the normal values expected by chance...
# ... we say the variables are unlikely to be uncorrelated
# (e.g. we infer they are related)

# Now, play with the sample sizes (e.g. how many points in each table) and see
# how that affects the shape of the chi-sq distribution.
# also toy with how many categories are in each variable...
# ... because that affects the degrees of freedom

