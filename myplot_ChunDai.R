myplot <- function(data, title = "Time Series Plot", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "skyblue", alpha = 0.5) +
    geom_line(aes(y = Median), color = "grey", linetype = "solid") +
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(legend.position = "top", legend.box = "horizontal")
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + 
      geom_line(aes(y = Average), color = color) +
      geom_point(aes(y = Average), color = color, shape = 1, fill = "white", size = 3) +  # shape=1 for hollow circle
      labs(color = "Average")
  }
  
  # Display the plot
  print(plot)
}










##################################################
myplot <- function(data, title = "Time Series Plot", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "skyblue", alpha = 0.5) +
    geom_line(aes(y = Median), color = "grey", linetype = "solid") +
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(legend.position = "right", legend.box = "horizontal")  # Move the legend outside the plot area
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + 
      geom_line(aes(y = Average), color = color) +  # No need for aes(color = "Average") here
      geom_point(aes(y = Average), color = color, shape = 1, fill = "white", size = 3) +
      labs(color = "Average")
  }
  
  # Display the plot
  print(plot)
}



##############################################################################################
myplot <- function(data, title = "Time Series Plot", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    # Q1 & Q3 range shaded ribbon
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.5) +
    # Median line
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    # Add the plot title and labels
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(
      legend.position = "right",  # Place the legend to the right outside the plot
      legend.box = "vertical",  # Ensure the legend box is vertical
      legend.justification = "top",  # Align the legend to the top
      legend.title = element_blank()  # Optional: remove legend title if not needed
    )
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + 
      geom_line(aes(y = Average, color = "Average"), size = 1) +  # Add color mapping for legend
      geom_point(aes(y = Average, color = "Average"), shape = 1, fill = "white", size = 3) +  # Hollow circles
      scale_color_manual(values = c("Average" = color, "Median" = "grey", "Q1 & Q3" = "skyblue"))  # Define colors
  } else {
    # Without the mean line, just set the colors
    plot <- plot + scale_color_manual(values = c("Median" = "grey", "Q1 & Q3" = "skyblue"))
  }
  
  # Display the plot
  print(plot)
}


############################################
myplot <- function(data, title = "Time Series Plot", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    # Q1 & Q3 range shaded ribbon
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.5) +
    # Median line
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    # Add the plot title and labels
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(
      legend.position = "right",  # Place the legend to the right outside the plot
      legend.box = "vertical",  # Ensure the legend box is vertical
      legend.justification = "top",  # Align the legend to the top
      legend.title = element_blank()  # Optional: remove legend title if not needed
    ) +
    scale_fill_manual(values = c("Q1 & Q3" = "skyblue"))  # Set fill color for Q1 & Q3 range
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + 
      geom_line(aes(y = Average, color = "Average"), size = 1) +  # Add color mapping for legend
      geom_point(aes(y = Average, color = "Average"), shape = 1, fill = "white", size = 3) +  # Hollow circles
      scale_color_manual(values = c("Average" = color, "Median" = "grey", "Q1 & Q3" = "skyblue"))  # Define colors
  } else {
    # Without the mean line, just set the colors
    plot <- plot + scale_color_manual(values = c("Median" = "grey", "Q1 & Q3" = "skyblue"))
  }
  
  # Display the plot
  print(plot)
}




################################################################
myplot <- function(data, title = "Time Series Plot", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    # Q1 & Q3 range shaded ribbon
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.5) +
    # Median line
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    # Add the plot title and labels
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(
      legend.position = "right",  # Place the legend on the right outside the plot
      legend.box = "vertical",  # Set the legend box to be vertical
      legend.justification = "top",  # Align the legend to the top
      legend.title = element_blank()  # Optional: remove legend title if not needed
    ) +
    scale_fill_manual(values = c("Q1 & Q3" = "skyblue"))  # Set fill color for Q1 & Q3 range
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + 
      geom_line(aes(y = Average, color = "Average"), size = 1) +  # Add color mapping for legend
      geom_point(aes(y = Average, color = "Average"), shape = 1, fill = "white", size = 3) +  # Hollow circles
      scale_color_manual(values = c("Average" = color, "Median" = "grey", "Q1 & Q3" = "skyblue"))  # Define colors
  } else {
    # Without the mean line, just set the colors
    plot <- plot + scale_color_manual(values = c("Median" = "grey", "Q1 & Q3" = "skyblue"))
  }
  
  # Set the legend order to have Average on top, followed by Median, and then Q1 & Q3
  plot <- plot + guides(
    fill = guide_legend(order = 3),
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )
  
  # Display the plot
  print(plot)
}


#######################################################
myplot <- function(data, title = " ", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    # Q1 & Q3 range shaded ribbon
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.5) +
    # Median line
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    # Add the plot title and labels
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(
      legend.position = "left",  # Place the legend on the right outside the plot
      legend.box = "vertical",  # Set the legend box to be vertical
      legend.justification = "bottom",  # Align the legend to the top
      legend.title = element_blank(),  # Optional: remove legend title if not needed
      axis.line = element_line(color = "black", size = 1),  # Add black axis lines
      axis.ticks = element_line(color = "black", size = 0.8),  # Add black ticks
      axis.ticks.length = unit(0.3, "cm"),  # Make ticks short and outward
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      #panel.grid.major = element_line(color = "grey80")  # Light major grid lines
    ) +
    scale_fill_manual(values = c("Q1 & Q3" = "skyblue"))  # Set fill color for Q1 & Q3 range
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + 
      geom_line(aes(y = Average, color = "Average"), size = 1) +  # Add color mapping for legend
      geom_point(aes(y = Average, color = "Average"), shape = 1, fill = "white", size = 4) +  # Hollow circles
      scale_color_manual(values = c("Average" = color, "Median" = "grey", "Q1 & Q3" = "skyblue"))  # Define colors
  } else {
    # Without the mean line, just set the colors
    plot <- plot + scale_color_manual(values = c("Median" = "grey", "Q1 & Q3" = "skyblue"))
  }
  
  # Set the legend order to have Average on top, followed by Median, and then Q1 & Q3
  plot <- plot + guides(
    fill = guide_legend(order = 3),
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )
  
  # Display the plot
  print(plot)
}


###############################################################################

myplot <- function(data, title = " ", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    # Q1 & Q3 range shaded ribbon
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.5) +
    # Median line
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    # Add the plot title and labels
    labs(title = title, y = "Value", x = "Time") +
    theme_minimal() +
    theme(
      legend.position = "right",  # Place the legend on the right outside the plot
      legend.box = "vertical",  # Set the legend box to be vertical
      #legend.justification = "bottom",  # Align the legend to the top
      legend.title = element_blank(),  # Optional: remove legend title if not needed
      axis.line = element_line(color = "black", size = 1),  # Keep axis lines
      axis.ticks = element_line(color = "black", size = 0.8),  # Keep axis ticks
      axis.ticks.length = unit(0.3, "cm"),  # Make ticks short and outward
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.grid.major.x = element_blank(),  # Remove vertical grid lines
      panel.grid.major.y = element_line(color = "grey80"),  # Keep horizontal grid lines
      panel.border = element_blank()  # Remove the border around the plot
    ) +
    scale_fill_manual(values = c("Q1 & Q3" = "skyblue"))  # Set fill color for Q1 & Q3 range
  
  # Add average line if requested
  if (plotmean) {
    plot <- plot + 
      geom_line(aes(y = Average, color = "Average"), size = 1) +  # Add color mapping for legend
      geom_point(aes(y = Average, color = "Average"), shape = 1, fill = "white", size = 4) +  # Hollow circles
      scale_color_manual(values = c("Average" = color, "Median" = "grey", "Q1 & Q3" = "skyblue"))  # Define colors
  } else {
    # Without the mean line, just set the colors
    plot <- plot + scale_color_manual(values = c("Median" = "grey", "Q1 & Q3" = "skyblue"))
  }
  
  # Set the legend order to have Average on top, followed by Median, and then Q1 & Q3
  plot <- plot + guides(
    fill = guide_legend(order = 3),
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )
  
  # Display the plot
  print(plot)
}

########################################################################
myplot <- function(data, title = " Time series plot ", color = "black", plotmean = TRUE) {
  # Check if the input is a data frame, matrix, or ts object
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  # Convert ts object to a data frame for consistent handling
  if (is.ts(data)) {
    # Get the time index for the ts object
    time_index <- time(data)
    # Convert the ts object into a data frame and add the time information
    data <- as.data.frame(data)
    data$Time <- time_index
    # Reshape data into long format
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    # If data is a data frame or matrix, ensure it's in long format
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  # Check for missing data and warn the user
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  # Compute summary statistics
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    # Q1 & Q3 range shaded ribbon
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.5) +
    # Median line
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    # Average line
    geom_line(aes(y = Average, color = "Average"), size = 1) +
    # Hollow circles for Average data points with matching stroke width
    geom_point(aes(y = Average, color = "Average"), shape = 1, fill = "white", size = 2, stroke = 1.5) +
    # Customize the theme to match the uploaded plot
    labs(
      title = title,
      y = "Attainment deficit (%)",
      x = "Year",
      color = NULL, fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Centered bold title
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "right", # Legend on the right
      legend.text = element_text(size = 12),
      legend.key.size = unit(1, "lines"),
      axis.line = element_line(color = "black", size = 1),
      axis.ticks = element_line(color = "black", size = 0.8),
      axis.ticks.length = unit(0.3, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey80"),
      panel.border = element_blank()
    ) +
    scale_fill_manual(values = c("Q1 & Q3" = "skyblue")) + # Set fill color for Q1 & Q3 range
    scale_color_manual(values = c("Average" = "black", "Median" = "darkgrey")) + # Define colors
    guides(
      fill = guide_legend(order = 3),
      color = guide_legend(order = 2),
      shape = guide_legend(order = 1)
    )
  
  # Display the plot
  print(plot)
}



#########################################################
myplot <- function(data, title = " Time series plot ", color = "black", plotmean = TRUE) {
  if (is.numeric(data) && !is.ts(data)) {
    stop("Error: Input must be a data frame, matrix, or 'ts' object, not a numeric vector.")
  } else if (!is.data.frame(data) && !is.matrix(data) && !is.ts(data)) {
    stop("Error: Unsupported input type. Must be a data frame, matrix, or 'ts' object.")
  }
  
  if (is.ts(data)) {
    time_index <- time(data)
    data <- as.data.frame(data)
    data$Time <- time_index
    data <- data %>% pivot_longer(-Time, names_to = "Series", values_to = "Value")
  } else {
    if (!("Time" %in% colnames(data))) {
      data <- data.frame(Time = seq_len(nrow(data)), data)
    }
    data <- pivot_longer(data, -Time, names_to = "Series", values_to = "Value")
  }
  
  if (any(is.na(data$Value))) {
    warning("Warning: Missing values found. Proceeding with NA handling.")
  }
  
  data_summary <- data %>%
    group_by(Time) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE)
    )
  
  # Plotting
  plot <- ggplot(data_summary, aes(x = Time)) +
    geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Q1 & Q3"), alpha = 0.5) +
    geom_line(aes(y = Median, color = "Median"), size = 1) +
    geom_line(aes(y = Average, color = "Average"), size = 1) +
    geom_point(aes(y = Average, color = "Average"), shape = 1, fill = "white", size = 2, stroke = 1.5) +
    labs(
      title = title,
      y = "Attainment deficit (%)",
      x = "Year",
      color = NULL, fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Centered bold title
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "right", 
      legend.text = element_text(size = 12),
      legend.key.size = unit(1, "lines"),
      axis.line = element_line(color = "black", size = 1),
      axis.ticks = element_line(color = "black", size = 0.8),
      axis.ticks.length = unit(0.3, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey80"),
      panel.border = element_blank()
    ) +
    scale_fill_manual(values = c("Q1 & Q3" = "skyblue")) + 
    scale_color_manual(values = c("Average" = "black", "Median" = "darkgrey")) + 
    guides(
      fill = guide_legend(order = 3),
      color = guide_legend(order = 2),
      shape = guide_legend(order = 1)
    )
  
  print(plot)
}


# Example usage with a sample dataset
library(dplyr)
library(ggplot2)
library(tidyr)  # Load tidyr for pivot_longer

# Dataset for the source figure
ad <- read.csv("dataraw/AD_cluster_3.csv")
rownames(ad) <- ad[, 1]
ad <- ad[, -1]

# Dataset in ts format
EuStockMarkets

# Dataset with missing values
ad_na <- ad
ad_na$CHOTF[ad$CHOTF == 0] <- NA

# Dataset of 1 time series (should give an error)
ts01 <- rnorm(100)
ts02 <- ts(ts01)
ts03 <- ad[, 1, drop = FALSE]

# Check the function
## 60 points: Should work in each case
myplot(ad)
myplot(EuStockMarkets)
myplot(ad_na)

## 25 points: Consider changing the color and add/remove the line for the average
## (the arguments plotmean and color can be named differently by the student)
myplot(ad, plotmean = TRUE, color = "red")

## 15 points: Should not work (give an informative error in each case)
myplot(ts01)
myplot(ts02)
myplot(ts03)
