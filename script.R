library(dplyr)
library(tidyverse)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggcorrplot)

#reading 1 of 50 files just to understand the structure
x<-read_csv("Data/SchoolingEarnings.csv")
glimpse(x)

#running regression 
M1 <- lm(earnings ~ schooling, data = x)
texreg::screenreg(M1)
#results : 1 additional year of schooling is associated with 1.16 times increase in earnings
# with no years of schooling the earnings are 8.80

#now i will plot the fitted values and actual values
ggplot(data=x, aes(x=schooling,  y=earnings))+
         geom_point(color="blue") +
         geom_smooth(data=x, method="lm",se=FALSE,color="green",linewidth=1,size=0.1)


#different regression (2nd,3rd etc)
M1 <- lm(earnings ~ schooling, data = x)

x<-x%>%
  mutate (
    sq_schooling=schooling^2,
    tr_schooling=schooling^3,
    qu_schooling=schooling^4
  )
M2 <- lm(earnings ~ schooling + sq_schooling, data = x)
M3 <- lm(earnings ~ schooling + sq_schooling+tr_schooling, data = x)
M4 <- lm(earnings ~ schooling + sq_schooling+tr_schooling+qu_schooling, data = x)
texreg::screenreg(list(M1,M2,M3,M4))



x <- x %>%
  mutate(
    fitted_M1 = fitted(M1),
    fitted_M2 = fitted(M2),
    fitted_M3 = fitted(M3),
    fitted_M4 = fitted(M4),
    delta_M1 = fitted(M1)-earnings,
    delta_M2 = fitted(M2)-earnings,
    delta_M3 = fitted(M3)-earnings,
    delta_M4 = fitted(M4)-earnings
  )

x %>%
  summarize(
    M1_MAE = mean(abs(delta_M1)),  # Mean Absolute Error for M1
    M2_MAE = mean(abs(delta_M2)),
    M3_MAE = mean(abs(delta_M3)),
    M4_MAE = mean(abs(delta_M4)),
    M1_RMSE = sqrt(mean(delta_M1^2)),  # Root Mean Squared Error for M1
    M2_RMSE = sqrt(mean(delta_M2^2)),
    M3_RMSE = sqrt(mean(delta_M3^2)),
    M4_RMSE = sqrt(mean(delta_M4^2))
  )

# Create a long-format dataset for visualization
x_long <- x %>%
  select(earnings, fitted_M1, fitted_M2, fitted_M3, fitted_M4) %>%
  pivot_longer(cols = starts_with("fitted"), 
               names_to = "Model", 
               values_to = "Fitted_Value")

# Plot actual vs fitted values for all models
ggplot(x_long, aes(x = earnings, y = Fitted_Value, color = Model)) +
  geom_point(alpha = 0.7) +  # Points for actual vs fitted values
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Line y=x
  labs(
    title = "Actual vs Fitted Values for All Models",
    x = "Actual Earnings",
    y = "Fitted Values",
    color = "Model"
  ) +
  theme_minimal()