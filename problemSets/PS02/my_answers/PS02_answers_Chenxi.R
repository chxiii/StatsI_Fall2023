
# Import package
# install.packages("reshape2")
# install.package("ggplot2") # If not install, install the package

library(reshape2)
library(ggplot2)

############## Question 1 ##############

####### (a) #######

# Formulate the row of cross table
class_type <- c(rep("Upper class", 27), rep("Lower class", 15))
class_type <- factor(class_type)
# Formulate the columns of cross table
bribe_type <- c(rep("Not stopped", 14), rep("Bribe requested", 6), 
                rep("Stopped warning", 7),
                rep("Not stopped", 7), rep("Bribe requested", 7),
                rep("Stopped warning", 1))
bribe_type <- as.factor(bribe_type)
# Check cross table
table(class_type, bribe_type)
# Calculate row and columns counts
row_1 <- 15 ; row_2 <- 27
col_1 <- 13 ; col_2 <- 21; col_3 <- 8
n <- 42
# Calculate expected frequencies
e11 <- (row_1 * col_1)/n; e12 <- (row_1 * col_2)/n; e13 <- (row_1 * col_3)/n
e21 <- (row_2 * col_1)/n; e22 <- (row_2 * col_2)/n; e23 <- (row_2 * col_3)/n
# Calculate Chi-squared Value
x11 <- (7 - e11)^2/e11; x12 <- (7 - e12)^2/e12; x13 <- (1 - e13)^2/e13
x21 <- (6 - e21)^2/e21; x22 <- (14 - e22)^2/e22; x23 <- (7 - e23)^2/e23
# Calculate chi-square value and print
x_square_value <- x11 + x12 + x13 + x21 + x22 + x23
print(x_square_value)

####### (b) #######

# Calculate degree of freedom
df <- (3 - 1) * (2 - 1)
# Calculate p value
p_value <- 1 - pchisq(x_square_value, df)
print(p_value)
# Set alpha
alpha <- 0.1
# Check chi-square value with 0.1 alpha
critical_chisq <- qchisq(1 - alpha, df)
print(critical_chisq)
# Compared with p value and alpha 
if (p_value > alpha) {
  print("Reject H1, Accept H0")
} else {
  print("Reject H0, Accept H1")
}
# Another way: compared with critical level and chi-square results
if (x_square_value < critical_chisq) {
  print("Reject H1, Accept H0")
} else {
  print("Reject H0, Accept H1")
}

####### (c) #######

# According to (a), we have calculated expected frequencies
# Now we can calcualte residual by (frequencies - expected values)
resi11 <- 7 - e11; resi12 <- 7 - e12; resi13 <- 1 - e13;
resi21 <- 6 - e21; resi22 <- 14 - e22; resi23 <- 7 - e23;
# Calculate standard residual by [residual / sqrt(expected value)]
s_resi11 <- resi11/sqrt(e11); s_resi12 <- resi12/sqrt(e12); 
s_resi13 <- resi13/sqrt(e13)
s_resi21 <- resi21/sqrt(e21); s_resi22 <- resi22/sqrt(e22); 
s_resi23 <- resi23/sqrt(e23)
# Print and check the results
cat(s_resi11, s_resi12, s_resi13,"\n", s_resi21, s_resi22, s_resi23)

####### (d) #######
# Draw a heat map to visualize the differences between standard residual
# Make a matrix about standard residual
s_resi_mat <- matrix(
  c(s_resi11, s_resi12, s_resi13, s_resi21, s_resi22, s_resi23),
  nrow = 2, ncol = 3, byrow = TRUE)
# Give colnames and rownames in matrix
colnames(s_resi_mat) <- c("Bribe requested", "Not stopped", "Stopped warning")
rownames(s_resi_mat) <- c("Lower class", "Upper class")
# Transfer matrix into long data table to draw ggplot picture
s_resi_long <- melt(s_resi_mat)
print(s_resi_long)
# Use ggplot to draw heatmap
s_resi_heatmap <- ggplot(s_resi_long, aes(x=Var2, y=Var1)) + 
  geom_tile(aes(fill=value), color="black", size=0.3) +
  scale_fill_gradient(low="#D0E7D2", high="#618264") + 
  labs(title="Heatmap: Standard Residual",
       x="Bribe Type", y="Class Type") +
  theme_bw()
print(s_resi_heatmap)

############## Question 2 ##############

####### (a) #######

####### (b) #######

# Import data from website link
df <- readr::read_csv(
  "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
# Set regression's dependent variable and independent variable
reg_y <- df$water; reg_x <- df$reserved
# Because the independent variable is categorical, 
# so draw a bar plot instad of scatter
# Draw a scatter to see differences between these two variables

# Create a data frame about mean and upper, lower ci
reg_mean_ci <- aggregate(reg_y ~ reg_x, data=df, FUN=function(x){
  mea_y <- mean(x)
  ci <- t.test(x)$conf.int
  return <- (c(mea_y, ci[1], ci[2]))
})
# Create x and y for point plot
sca_x <- c("0", "1"); sca_y <- c(14.74, 23.99)
# Create lowerci and upperci for point plot
lower_ci <- reg_mean_ci$reg_y[,2]; upper_ci <- reg_mean_ci$reg_y[,3]
reg_sca <- ggplot(reg_mean_ci, aes(x=sca_x, y=sca_y)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=0.2) +
  labs(x="Reversed", y="Mean of Water",
       title="New Repaired Drinking-water Facilities Group by Reserved") +
  scale_x_discrete(labels=c("No", "Yes")) +
  ylim(0,35) +
  theme_bw()
reg_sca
# Create a regression model
reg_mod <- lm(reg_y ~ reg_x, data=df)
summary(reg_mod)


