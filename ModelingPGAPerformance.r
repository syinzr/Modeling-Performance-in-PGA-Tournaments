golf.data<- read.csv(file="BinaryData.csv", header=TRUE, sep=",")

golf.data$Outcome_re <- relevel(as.factor(golf.data$Outcome),ref="0")

golf.data[, c("Latitude_re","Longitude_re","Altitude_re","CoursePar_re",
"AvgTemp_re","Precip_re","AvgWindspeed_re","Height_re","Weight_re", "Age_re","YearsPro_re")] <- 
scale(golf.data[, c("Latitude","Longitude","Altitude","CoursePar","AvgTemp","Precip",
"AvgWindspeed","Height","Weight","Age","YearsPro")])

#fitting hierarchical model for binary outcome
library(glmmTMB)

log.fit <- glmmTMB(Outcome_re ~ Latitude_re + Longitude_re + Altitude_re + CoursePar_re 
+ AvgTemp_re + Precip_re + AvgWindspeed_re + Height_re + Weight_re + Age_re + YearsPro_re 
+ Round + (1 | Course) + (1 + Round | Player:Course), data = golf.data, 
family = binomial(link="logit"))

summary(log.fit)

#####################################################
#conducting z-tests for equality to zero of sigmas  #
#####################################################
sdr <- log.fit$sdr
sdr_sum <- summary(sdr, "fixed")
theta_idx <- grep("^theta", rownames(sdr_sum))

log_sd_est <- sdr_sum[theta_idx, "Estimate"]
log_sd_se  <- sdr_sum[theta_idx, "Std. Error"]

sd_est <- exp(log_sd_est)
sd_se  <- sd_est * log_sd_se   # delta method

z_vals <- sd_est / sd_se
p_vals <- 2 * (1 - pnorm(abs(z_vals)))

# Assign proper names
theta_names <- c("Course: Intercept SD", "Player:Course: Intercept SD",
"Player:Course: Slope SD (Round)", "Player:Course: Intercept-Slope Cov/Cor")

result <- data.frame(Random_Effect = theta_names, Sigma = sd_est, SD_SE = sd_se,
Z = z_vals, P_value = p_vals)

print(result)

#################################################################

golf2.data<- read.csv(file="RawData.csv", header=TRUE, sep=",")

golf2.data[, c("Latitude_re","Longitude_re","Altitude_re","CoursePar_re",
"AvgTemp_re","Precip_re","AvgWindspeed_re","Height_re","Weight_re", "Age_re","YearsPro_re")] <- 
scale(golf2.data[, c("Latitude","Longitude","Altitude","CoursePar","AvgTemp","Precip",
"AvgWindspeed","Height","Weight","Age","YearsPro")])


#fitting hierarchical model for normal outcome
library(lme4)
norm.fit<- lmer(rawdiff ~ Latitude_re + Longitude_re + Altitude_re 
+ CoursePar_re + AvgTemp_re + Precip_re + AvgWindspeed_re + Height_re + Weight_re 
+ Age_re + YearsPro_re + Round + (1 | Course) + (1 | Player:Course), 
data=golf2.data)

summary(norm.fit)
#computing p-values based on normal approximation
coefs<- summary(norm.fit)$coefficients
pvals<- 2*(1-pnorm(abs(coefs[,"t value"])))

cbind(coefs, "p value (normal approx.)"=pvals)

#####################################################
#conducting z-tests for equality to zero of sigmas  #
#####################################################
vc <- VarCorr(norm.fit)
sd_values <- c(
  attr(vc$Course, "stddev")[1],         # Course intercept SD
  attr(vc$`Player:Course`, "stddev")[1] # Player:Course intercept SD
)

# Assign proper names
theta_names <- c("Course: Intercept SD", "Player:Course: Intercept SD")

# Compute approximate SEs using profile confidence intervals
ci <- confint(norm.fit, parm="theta_", method="profile", oldNames=FALSE)

# Extract only SD rows
sd_rows <- grep("^sd_", rownames(ci))

# Approximate SE: (upper-lower)/(2*1.96)
sd_se <- (ci[sd_rows,2] - ci[sd_rows,1]) / (2*1.96)

# Compute Wald z and p-values
z_vals <- sd_values / sd_se
p_vals <- 2 * (1 - pnorm(abs(z_vals)))

# Build results table
result <- data.frame(Random_Effect = theta_names, Sigma = sd_values, SD_SE = sd_se,
Z = z_vals, P_value = p_vals)

print(result)
