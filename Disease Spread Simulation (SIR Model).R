library(deSolve)
library(ggplot2)
library(tidyr)

# Define the SIR model function
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Calculate derivatives
    dS <- -beta * S * I / N         # Susceptible
    dI <- beta * S * I / N - gamma * I  # Infected
    dR <- gamma * I                 # Recovered
    
    # Return the rates of change
    return(list(c(dS, dI, dR)))
  })
}

# Simulation parameters
N <- 1000           # Total population
init <- c(S = 999,  # Initial susceptible (99.9% of population)
          I = 1,    # Initial infected (1 case)
          R = 0)    # Initial recovered (0 cases)

params <- c(beta = 0.3,   # Infection rate (contacts per day)
            gamma = 0.1)  # Recovery rate (1/days infectious)

times <- seq(0, 100, by = 1)  # Time frame (100 days in daily steps)

# Run the simulation
out <- ode(y = init, 
           times = times, 
           func = sir_model, 
           parms = params)

# Convert to data frame for plotting
out_df <- as.data.frame(out)

# Reshape data for ggplot (long format)
out_long <- pivot_longer(out_df, 
                         cols = c("S", "I", "R"), 
                         names_to = "Compartment", 
                         values_to = "Count")

# Create the plot with improved formatting
ggplot(out_long, aes(x = time, y = Count, color = Compartment)) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_color_manual(values = c("S" = "blue", "I" = "red", "R" = "green")) +
  labs(title = "SIR Model Simulation of Disease Spread",
       subtitle = paste("β =", params["beta"], ", γ =", params["gamma"], 
                        ", R₀ =", round(params["beta"]/params["gamma"], 2)),
       x = "Time (days)", 
       y = "Number of Individuals",
       color = "Compartment") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right",
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, N)) +
  geom_vline(xintercept = which.max(out_df$I), 
             linetype = "dashed", color = "gray") +
  annotate("text", x = which.max(out_df$I)+10, y = N*0.9,
           label = paste("Peak at day", which.max(out_df$I)),
           color = "black")

