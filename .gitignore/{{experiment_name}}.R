---
title: "NA"
sample: 25
output:
  flexdashboard::flex_dashboard:
    source_code: embed
    vertical_layout: fill
    orientation: columns
    theme: 
      version: 4
      bootswatch: cosmo
runtime: shiny
---

```{r user_defined_parameters, message=FALSE}
# The user defines different variables and parameters that will be specific to the study.

inj_date <- as.POSIXct(as.Date("na", format = "%m/%d/%Y"), tz = "GMT")

trt_date <- as.POSIXct(as.Date("na", format = "%m/%d/%Y"), tz = "GMT")

eos_date <- as.POSIXct(as.Date("na", format = "%m/%d/%Y"), tz = "GMT")

engraftment_imaging_date <- as.Date("na", format = "%m/%d/%Y")

dates <- as.POSIXct(as.Date(c("na", "na", "na", "na", "na", "na"), format = "%m/%d/%Y"), tz = "GMT")

number_of_groups <- 4
mice_per_group <- 5
filtered_mice <- c(1:25)
labels <- c("0" = "Non-study", "1" = "na", "2" = "na", "3" = "na", "4" = "na")
pal <- c("Non-study" = "#000000", "na" = "#f8766d", "na" = "#7cae00", "na" = "#00bfc4", "na" = "#C3B1E1")

# Link to organization logo
src <- "na"

# Save descriptives and anova stat outputs as a .txt file? y or n
# Default is n to stop the .txt from constantly being overwritten when the .Rmd file is ran
save_output <- "n"
```

```{r Load Libraries, message=FALSE}
# Set root.dir to the current file's directory
knitr::opts_knit$set(root.dir = dirname(rstudioapi::getSourceEditorContext()$path))
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
library(flexdashboard) # dashboard view for multiple panes
library(tidyverse) # tidyverse
library(shiny) # shiny app functionality
library(DT) # for rendering tables
library(scales) # scale functions for customizing visualizations
library(plotly) # interactive cursor friendly plots
library(survival) # for survival curves
library(survminer) # for survival curves
library(jmv) # performing statistical analyses
```

```{r Data Pre-Processing, message=FALSE, warning=FALSE}
# Create 'processed_data' which will be one data frame that contains:
# 1. all of the imaging data across weeks by funneling all of the weeks' imaging data into one data frame.
# 2. all of the mass data across weeks.

# Read the 'raw_data.csv' file into a data frame
raw_data <- read.csv("data/raw/raw_data.csv")

# Set the file path
file_path <- "data/imaging/processed/"

# Get a list of all weeks' imaging data .csv files in the directory
csv_files <- list.files(file_path, pattern = "\\.csv$", full.names = TRUE)

# Function to read, process, and save each csv file as a data frame with the same name
read_process_save_csv <- function(file) {
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the csv file into a data frame with check.names = FALSE
  df <- read.csv(file, check.names = FALSE)
  
# Rename the last column header indirectly because otherwise, the exponent cannot be handled
  colnames(df)[4] <- "avg_radiance"

# Remove specified columns
columns_to_remove <- c("Image Number", "ROI", "Image Layer", "Stdev Radiance", "Min Radiance", "Max Radiance")
df <- df[, !colnames(df) %in% columns_to_remove]
  
# Rename columns
colnames(df)[colnames(df) == "ROI Label"] <- "id"
colnames(df)[colnames(df) == "Experiment"] <- "imaging_date"
colnames(df)[colnames(df) == "Total Flux [p/s]"] <- "total_flux"
  
  # Assign the processed data frame to a variable with the same name as the csv file
  assign(file_name, df, envir = .GlobalEnv)
}

# Use lapply() to read, process, and save each csv file
invisible(lapply(csv_files, read_process_save_csv))

# Combine all weeks' imaging data using do.call() and bind_rows()
invisible(imaging_data <- do.call(bind_rows, mget(tools::file_path_sans_ext(basename(csv_files)), envir = .GlobalEnv)))

# Convert imaging_date to Date class with the appropriate format
imaging_data$imaging_date <- as.Date(imaging_data$imaging_date, format = "%m/%d/%Y")

# Arrange the imaging_data data frame by imaging_date and then id
imaging_data <- imaging_data %>%
  arrange(imaging_date, id)

# Combine 'imaging_data' with 'raw_data' to make processed_data
processed_data <- left_join(raw_data, imaging_data, by = "id")

# Write the 'processed_data' data frame to a CSV file
write_csv(processed_data, file = "data/processed/processed_data.csv")
```

```{r Treatment Assignment, message=FALSE, warning=FALSE}
# Using the already user-defined filtered_mice, randomly assign mice to treatment conditions so that each treatment condition has equivalent total_flux between treatments. All of the filtered/unassigned remaining mice are assigned to na which gets updated to trt_factor = 0 aka the non-study/expansion group.

# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

filtered_data <- processed_data %>%
  filter(imaging_date == engraftment_imaging_date, id %in% filtered_mice) %>%
  distinct(id, .keep_all = TRUE) %>%
  arrange(total_flux)

# Define the assign_groups function
assign_groups <- function(data, num_groups, num_per_group) {
  # Create an empty vector to store group assignments
  trt <- integer(nrow(data))
  
  # Create a data frame to store mean total_flux for each group
  mean_total_flux <- data.frame(Group = 1:num_groups, Mean_Total_Flux = numeric(num_groups))
  
  # Shuffle the data to randomize assignments
  data <- data[sample(nrow(data)), ]
  for (i in 1:num_groups) {
    # Select a subset of data for each group
    group_data <- data[((i - 1) * num_per_group + 1):(i * num_per_group), ]
    
    # Calculate mean total_flux for the group
    mean_flux <- mean(group_data$total_flux)
    
    # Assign the group number to the trt column
    trt[((i - 1) * num_per_group + 1):(i * num_per_group)] <- i
    
    # Store the mean total_flux in the mean_total_flux data frame
    mean_total_flux[i, 2] <- mean_flux
  }
  
  # Add the trt column to the data frame
  data$trt <- trt
  
  # Return the data frame with group assignments
  return(list(Data = data, Mean_Total_Flux = mean_total_flux))
}

# Initialize variables to store the best seed and its corresponding variability
best_seed <- NULL
best_variability <- Inf

# Loop through the first 5000 seeds
for (seed in 1:5000) {
  set.seed(seed)
  
  # Call the function to assign groups using filtered_data
  result <- assign_groups(filtered_data, number_of_groups, mice_per_group)
  
  # Calculate the standard deviation of mean total_flux across groups
  variability <- sd(result$Mean_Total_Flux$Mean_Total_Flux)
  
  # Check if this seed has lower variability than the current best
  if (variability < best_variability) {
    best_variability <- variability
    best_seed <- seed
  }
}

# Print the seed with the least variability
cat("Seed with the least variability:", best_seed, "\n")

set.seed(best_seed)

# Function to assign mice to groups
assign_groups <- function(data, num_groups, num_per_group) {
  # Create an empty vector to store group assignments
  trt_factor <- integer(nrow(data))
  
  # Create a data frame to store mean total_flux for each group
  mean_total_flux <- data.frame(Group = 1:num_groups, Mean_Total_Flux = numeric(num_groups))
  
  # Shuffle the data to randomize assignments
  data <- data[sample(nrow(data)), ]
  for (i in 1:num_groups) {
    # Select a subset of data for each group
    group_data <- data[((i - 1) * num_per_group + 1):(i * num_per_group), ]
    
    # Calculate mean total_flux for the group
    mean_flux <- mean(group_data$total_flux)
    
    # Assign the group number to the trt_factor column
    trt_factor[((i - 1) * num_per_group + 1):(i * num_per_group)] <- i
    
    # Store the mean total_flux in the mean_total_flux data frame
    mean_total_flux[i, 2] <- mean_flux
  }
  
  # Add the trt_factor column to the data frame
  data$trt_factor <- trt_factor
  
  # Return the data frame with group assignments
  return(list(Data = data, Mean_Total_Flux = mean_total_flux))
}

# Call the function to assign groups using filtered_data
result <- assign_groups(filtered_data, number_of_groups, mice_per_group)

# Add trt_factor and trt to processed_data data frame.
# NA trt_factor values become 0 and trt becomes Non-study
# Add trt_factor and trt to processed_data data frame
processed_data <- processed_data %>%
  left_join(result$Data %>% select(id, trt_factor), by = "id") %>%
  mutate(trt_factor = as.factor(ifelse(is.na(trt_factor), "0", trt_factor))) %>%
  select(id, trt_factor, everything()) %>%
  mutate(trt = factor(trt_factor, levels = names(labels), labels = labels)) %>%
  relocate(trt_factor, .before = trt_injection_vial) %>% 
  relocate(trt, .before = trt_injection_vial) %>%
  mutate(imaging_date = format(as.POSIXct(imaging_date, tz = "GMT"), "%Y/%m/%d"))
  
# EXAMPLE CODE FOR REPLACING id = 6 WITH ID = 23
#processed_data <- processed_data %>%
  #mutate(imaging_date = as.Date(imaging_date, format = "%Y/%m/%d")) %>%
  #mutate(
    #trt_factor = if_else(id == 23 & imaging_date > as.Date("2023-12-14"), first(trt_factor[id == 6]), trt_factor),
    #trt = if_else(id == 23 & imaging_date > as.Date("2023-12-14"), first(trt[id == 6]), trt)
  #)
  
# Write the 'processed_data' data frame to a CSV file
write_csv(processed_data, file = "data/processed/processed_data.csv")
```

# Data

Column {.tabset data-width=600}
-----------------------------------------------------------------------

### Treatment Plot

```{r Treatment Plot, message=FALSE, warning=FALSE, fig.height=40, fig.width=11}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  mutate(imaging_date = as.POSIXct(imaging_date, tz = "GMT")) %>%
  mutate(days_from_trt = as.numeric(difftime(imaging_date, trt_date, units = "days"))) %>% 
  relocate(days_from_trt, .before = imaging_date)

# Define UI
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for PDX (tumor_injection) selection
      selectInput("pdx", "PDX:",
                  choices = unique(processed_data$tumor_injection),
                  selected = unique(processed_data$tumor_injection)[1]),
      
      # Dropdown for Y-axis selection
      selectInput("y_axis", "Y-axis:",
                  choices = c("Flux" = "total_flux", "Radiance" = "avg_radiance"),
                  selected = "total_flux"),
      
      # Checkbox for including non-study mice
      checkboxInput("include_non_study", "Include non-study mice", value = FALSE),
      
      # Dropdown for Calculation Type
      selectInput("calculation_type", "Calculation Type:",
                  choices = c("Mean", "Median", "Individual"),
                  selected = "Mean")
    ),
    
    mainPanel(
      # Plot output
      plotlyOutput("scatter_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load data
  processed_data <- read.csv("data/processed/processed_data.csv") %>%
  mutate(imaging_date = as.POSIXct(imaging_date, tz = "GMT")) %>%
  mutate(days_from_trt = as.numeric(difftime(imaging_date, trt_date, units = "days"))) %>% 
  relocate(days_from_trt, .before = imaging_date)
  
  # Create reactive dataset based on PDX and toggle selection
  selected_data <- reactive({
    data <- processed_data[processed_data$tumor_injection == input$pdx, ]
    if (!input$include_non_study) {
      data <- data[data$trt != "Non-study", ]
    }
    data
  })
  
  # Create scatter plot without facet wrap
output$scatter_plot <- renderPlotly({
  y_axis_title <- if (input$y_axis == "total_flux") "Flux [p/s]" else "Radiance [p/s/cm^2/sr]"
  y_axis_label <- if (input$y_axis == "total_flux") "Flux" else "Radiance"

  # Calculate values based on the selected calculation type
  if (input$calculation_type == "Individual") {
    p <- selected_data() %>%
      mutate(trt = reorder(trt, trt_factor)) %>%
      group_by(trt, id) %>%
      plot_ly(x = ~days_from_trt, y = ~get(input$y_axis),
              type = 'scatter', mode = 'lines+markers',
              color = ~trt, colors = pal,
              marker = list(size = 10),
              legendgroup = ~trt,
              name = ~trt,
              hoverinfo = "text",
              text = ~paste("Imaging Date: ", imaging_date, "<br>ID: ", id, "<br>", y_axis_label, ": ", sprintf("%.2e", get(input$y_axis)))) %>%
      layout(
        yaxis = list(
          type = "log",
          title = y_axis_title
        ),
        xaxis = list(
          type = "linear",
          title = "Days from Treatment",
          tickmode = "array",
          tickvals = c(unique(selected_data()$days_from_trt), as.numeric(inj_date - trt_date)),
          ticktext = c(as.character(unique(selected_data()$days_from_trt)),
                       paste(as.numeric(inj_date - trt_date)))
        ),
        showlegend = TRUE,
        legend = list(x = 1, y = 0.5, title = "Treatment"),
        images = list(
          source = src,
          xref = "paper",
          yref = "paper",
          x = 0,
          y = 1,
          sizex = 0.2,
          sizey = 0.2,
          opacity = 0.8
        ),
        shapes = list(
          list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = as.numeric(inj_date - trt_date), x1 = as.numeric(inj_date - trt_date), line = list(color = "black", dash = "dot")),
          list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = as.numeric(eos_date - trt_date), x1 = as.numeric(eos_date - trt_date), line = list(color = "black", dash = "dot"))
        )
      ) %>%
      config(scrollZoom = TRUE, modeBarButtonsToAdd = list('drawopenpath'))
  } else {
    data_summary <- selected_data() %>%
      mutate(trt = reorder(trt, trt_factor)) %>%
      group_by(days_from_trt, trt, imaging_date) %>%
      summarise(y_value = ifelse(input$calculation_type == "Mean", mean(get(input$y_axis)),
                                 median(get(input$y_axis))))

    p <- data_summary %>%
      group_by(trt) %>%
      plot_ly(x = ~days_from_trt, y = ~y_value,
              type = 'scatter', mode = 'lines+markers',
              color = ~trt, colors = pal,
              legendgroup = ~trt,
              name = ~trt,
              marker = list(size = 10),
              hoverinfo = "text",
              text = ~paste("Imaging Date: ", imaging_date, "<br>", y_axis_label, ": ", sprintf("%.2e", y_value))) %>%
      layout(
        yaxis = list(
          type = "log",
          title = y_axis_title
        ),
        xaxis = list(
          type = "linear",
          title = "Days from Treatment",
          tickmode = "array",
          tickvals = c(unique(data_summary$days_from_trt), as.numeric(inj_date - trt_date)),
          ticktext = c(as.character(unique(data_summary$days_from_trt)),
                       paste(as.numeric(inj_date - trt_date)))
        ),
        showlegend = TRUE,
        legend = list(x = 1, y = 0.5, title = "Treatment"),
        images = list(
          source = src,
          xref = "paper",
          yref = "paper",
          x = 0,
          y = 1,
          sizex = 0.2,
          sizey = 0.2,
          opacity = 0.8
        ),
        shapes = list(
          list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = as.numeric(inj_date - trt_date), x1 = as.numeric(inj_date - trt_date), line = list(color = "black", dash = "dot")),
          list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = as.numeric(eos_date - trt_date), x1 = as.numeric(eos_date - trt_date), line = list(color = "black", dash = "dot"))
        )
      ) %>%
      config(scrollZoom = TRUE, modeBarButtonsToAdd = list('drawopenpath'))
  }

  p
})
}

# Run the application
shinyApp(ui, server)
```

### Kaplan-Meier Curve

```{r Kaplan-Meier Curve, eval=FALSE, message=FALSE, warning=FALSE, fig.height=10, fig.width=11}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  distinct(id, trt, death_date, trt_injection_date) %>% 
    mutate(
      death_date = as.Date(death_date, format = "%m/%d/%Y"),
      trt_injection_date = as.Date(trt_injection_date, format = "%m/%d/%Y"),
      event = ifelse(!is.na(death_date), 1, 0),
      time = ifelse(!is.na(death_date), as.numeric(difftime(death_date, trt_injection_date, units = "days")), as.numeric(difftime(Sys.Date(), trt_injection_date, units = "days")))
    )

# Specify the data that have been used to fit the survival curves
# This line is needed otherwise object "processed_data" is not found
data(processed_data)

kmcurve <- survfit(Surv(time, event)~trt, data = processed_data)

ggsurvplot(kmcurve, data = processed_data, xlim = c(0, 100), break.x.by = 7, ylab = "", xlab = "",
		   pval = TRUE,
		   risk.table = TRUE,
		   risk.table.title = "",
		   legend.labs = labels,
		   legend.title = "",
		   surv.scale = "percent",
		   palette = pal,
		   title = "",
		   risk.table.height = .20)

# Create a new plot area for the summary
# This prevents the survival curve plot from occluding the printed summary_table
par(mar = c(0, 0, 0, 0))
plot.new()

# Get the summary table
summary_table <- summary(kmcurve, times = seq(0, 100, 7))

# Print the formatted summary
print(summary_table, digits = 3)
```

### Spleen and Marrow Counts 

```{r Spleen and Marrow Counts, message=FALSE, warning=FALSE}
plotly::renderPlotly({
  
  # Load the processed data
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  distinct(id, trt, spleen_total_live_cell_count, number_of_spleen_cryovials, cryovial_spleen_cell_total, spleen_cell_viability, manner_of_death, death_date)
  
plot <- plot_ly(
  data = processed_data,
  x = ~id,
  y = ~spleen_total_live_cell_count,
  type = "bar",
  color = ~trt, colors = pal,
  marker = list(line = list(width = 2)),  # Adjust the width of the bars
  hoverinfo = "text",
              text = ~paste(
              "ID: ", id,
              "<br>Total Live Spleen Count:", sprintf("%.2e", spleen_total_live_cell_count),
              "<br>Viability:", spleen_cell_viability,
              "<br>", number_of_spleen_cryovials, "cryovial(s) at", sprintf("%.2e", cryovial_spleen_cell_total), " cells each",
              "<br>Death date:", death_date,
              "<br>Manner of death:", manner_of_death
                                                        )
  ) %>%
  layout(
    xaxis = list(title = "Mouse ID"),
    yaxis = list(title = "Spleen Total Live Cell Count"),
    images = list(
      source = src,
      xref = "paper",
      yref = "paper",
      x = 0,
      y = 1,
      sizex = 0.2,
      sizey = 0.2,
      opacity = 0.8
    )
  ) %>%
  config(scrollZoom = TRUE, modeBarButtonsToAdd = list('drawopenpath'))

# Display the Plotly plot
print(plot)
})
```

### Flow Cytometry

```{r Flow Cytometry, results='asis'}
# Set root.dir to the current file's directory
current_file_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
knitr::opts_knit$set(root.dir = current_file_dir)

# Specify the relative directory containing PNG files
png_directory <- "data/flow"

# Get a list of .PNG files within the specified directory
png_files <- list.files(file.path(current_file_dir, png_directory), pattern = "\\.PNG$", full.names = TRUE)

# Create a container div for the images
cat("<div style='text-align: center;'>")

# Loop through the .PNG files and generate HTML content with centered labels above each image
for (png_file in png_files) {
  # Get the base name of the PNG file (without the path)
  file_name <- basename(png_file)

  # Add a label div above the image with increased font size
  cat(sprintf("<div style='font-size: 18px;'>%s</div>\n", file_name))

  # Create HTML content with an image tag
  cat(sprintf("<img src='%s' width='700' />\n", png_file))
}

# Close the container div
cat("</div>\n")
```

Column {.tabset data-width=400}
-----------------------------------------------------------------------

### IVIS Images

```{r IVIS Images, results='asis'}
# Set root.dir to the current file's directory
current_file_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
knitr::opts_knit$set(root.dir = current_file_dir)

# Specify the relative file path
file_path <- "data/imaging/processed/"

# Get a list of all subdirectories one level deep within the specified directory
subdirectories <- list.dirs(file.path(current_file_dir, file_path), full.names = TRUE, recursive = FALSE)

# Sort the subdirectories in descending order
subdirectories <- sort(subdirectories, decreasing = TRUE)

# Loop through each subdirectory and generate HTML content
for (subdirectory in subdirectories) {
  # Get the name of the current subdirectory
  folder_name <- basename(subdirectory)
  
  # Get a list of .PNG files within the subdirectory
  png_files <- list.files(subdirectory, pattern = "\\.PNG$", full.names = TRUE, recursive = FALSE)
  
  # Sort the PNG files in descending order
  png_files <- sort(png_files, decreasing = TRUE)
  
  # Create a container div for each subdirectory
  cat("<div style='text-align: center;'>")
  
  # Loop through the .PNG files and generate HTML content with centered labels above each image
  for (png_file in png_files) {
    # Add a label div above the image with increased font size
    cat(sprintf("<div style='font-size: 18px;'>%s</div>\n", folder_name))
    
    # Create HTML content with an image tag
    cat(sprintf("<img src='%s' width='700' />\n", png_file))
  }
  
  # Close the container div
  cat("</div>\n")
}
```

### Data Table

```{r Data Table, message=FALSE, warning=FALSE}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

# Select the desired columns and format trt
most_recent_data <- processed_data %>%
  filter(id %in% filtered_mice) %>%
  select(cage_number, id, ear_punch, trt, imaging_date, total_flux, avg_radiance, death_date, manner_of_death)

# Create a .csv file of current mice data with the most recent imaging_date
processed_data %>%
  select(cage_number, id, ear_punch, trt, imaging_date, total_flux, avg_radiance, death_date, manner_of_death) %>% 
  group_by(id) %>%
  arrange(desc(imaging_date)) %>%
  slice(1) %>%  # Keep only the rows with the most recent imaging_date for each id
  select(cage_number, id, ear_punch, trt, imaging_date, total_flux, avg_radiance, death_date, manner_of_death) %>%
  write.csv(file = "data/processed/current_mice_data.csv", row.names = FALSE)

# Create an interactive DataTable with initial sorting and exclude the ear_punch column
datatable(most_recent_data, 
          options = list(pageLength = 20, scrollX = TRUE, order = list(list(5, 'asc'), list(2, 'asc')),
          columnDefs = list(list(targets = 3, visible = FALSE))))
```

# Subjects

Column {.tabset data-width=500}
-----------------------------------------------------------------------

### Mass Over Time

```{r Mass, message=FALSE, warning=FALSE}
# Read the files into data frames
raw_mass <- read.csv("data/raw/raw_mass.csv")
processed_data <- read.csv("data/processed/processed_data.csv")

# Assuming you've handled duplicates in combined_data
combined_data <- raw_mass %>%
  inner_join(processed_data, by = "id")

# Convert mass_date to the Date class
combined_data$mass_date <- as.Date(combined_data$mass_date, format = "%m/%d/%Y")

# Sort data by mass_date from earliest to latest
combined_data <- combined_data %>%
  arrange(mass_date)

# Create a ggplot object with facetting by trt and grouping by id
p <- ggplot(combined_data, aes(x = mass_date, y = mass, color = factor(id), group = id)) +
  geom_line() +
  geom_point() +
  facet_wrap(~trt, scales = "free", ncol = 2)  # Adjust ncol as needed

# Convert ggplot to plotly
p <- ggplotly(p)

# Display the interactive plot
p
```

### Engraftment Status Report

#### Engraftment Status

```{r Engraftment Status, message=FALSE, warning=FALSE, , fig.height=6, fig.width=9.5}
processed_data <- read.csv("data/processed/processed_data.csv")

# Precompute the order of 'id' based on 'total_flux' for the engraftment_imaging_date
order_by_flux <- processed_data %>%
  filter(imaging_date == engraftment_imaging_date) %>%
  arrange(desc(total_flux)) %>%
  pull(id)

# Filter the data for the plot
filtered_data <- processed_data %>%
  filter(imaging_date <= engraftment_imaging_date)

# Use the precomputed order in ggplot
filtered_data %>%
  ggplot(aes(x = factor(id, levels = order_by_flux), y = total_flux, color = imaging_date)) +
  geom_point() +
  scale_color_manual(values = setNames(rainbow(length(unique(filtered_data$imaging_date))), 
                                      unique(filtered_data$imaging_date))) +
  scale_y_log10() +
  labs(x = "Mouse Number", y = "Total Flux", color = "Imaging Date") +
  theme_minimal()

# Count the number of mice with total_flux > 1e6 and total_flux <= 1e6
mice_greater_than_1e6 <- processed_data %>%
  filter(imaging_date == engraftment_imaging_date) %>% 
  distinct(id, .keep_all = TRUE) %>%
  filter(total_flux > 1e6) %>%
  nrow()

mice_less_than_1e6 <- processed_data %>%
  filter(imaging_date == engraftment_imaging_date) %>% 
  distinct(id, .keep_all = TRUE) %>%
  filter(total_flux <= 1e6) %>%
  nrow()

# Get the specific mouse numbers for mice with total_flux <= 1e6
specific_mice_numbers <- processed_data %>% filter(total_flux <= 1e6) %>% select(id)

# Create a table
table_data <- data.frame(
  "Engrafted" = mice_greater_than_1e6,
  "Not Engrafted" = mice_less_than_1e6
)

# Print the table
table_data

# Find the mouse numbers that are not engrafted, check if there are non-engrafted mice, and return the numbers
processed_data %>%
  filter(total_flux <= 1e6, imaging_date == engraftment_imaging_date) %>%
  pull(id) %>%
  unique() %>%
  { if (length(.) > 0) {
      cat("These mice are not engrafted because their total flux is less than 1e6:", paste(., collapse = ", "))
    } else {
      cat("All mice are engrafted because their total flux is greater than 1e6.")
    }
  }
```

#### Treatment Assignment Plot From Engraftment Imaging Date

```{r Treatment Assignment Plot From Engraftment Imaging Date, message=FALSE, warning=FALSE, fig.height=6, fig.width=9.5}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

# Convert trt to factor with levels sorted by trt_factor
processed_data$trt <- factor(processed_data$trt, levels = unique(processed_data$trt[order(processed_data$trt_factor)]))

# Create the ggplot
plot <- ggplot(data = processed_data %>% filter(imaging_date == engraftment_imaging_date),
       aes(x = trt, y = total_flux, color = trt, label = id)) +
  geom_jitter(position = position_dodge2(width = 0.7), size = 3) +  # Decrease the width for less separation
  geom_text(position = position_dodge2(width = 0.7), vjust = -1, size = 3) +  # Adjust vjust to move the label up
  scale_color_manual(values = pal, breaks = unique(processed_data$trt)) +  # Define colors and arrange the order
  labs(x = "Treatment", y = "Flux [p/s]", color = "Treatment") +  # Rename the legend title
  theme_minimal()

# Display the plot
print(plot)
```

### Thawing Cells

#### Tumor

##### na

tbd

#### Treatment

##### na

tbd

##### na

tbd

##### na

tbd

Column {.tabset data-width=500}
-----------------------------------------------------------------------

### Subjects

#### Mouse Current Status Plot

```{r Current Status by Cage and Treatment, message=FALSE, warning=FALSE, fig.height=6, fig.width=10}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

# Create a scatterplot for cage_number, trt, and status without overlap
scatterplot <- processed_data %>%
  arrange(manner_of_death) %>%  # Arrange data to ensure NA is at the end
  distinct(trt, id, .keep_all = TRUE) %>% 
  ggplot(aes(x = factor(cage_number), y = reorder(trt, -id))) +  # Use reorder to order trt by id
  geom_point(
    aes(color = factor(ifelse(is.na(manner_of_death) | manner_of_death == "", "Alive", "Dead"))),
    size = 3,
    position = position_dodge2(width = 0.5),  # Adjust width as needed
    alpha = 0.7
  ) +
  geom_text(
    aes(label = as.character(id), color = factor(ifelse(is.na(manner_of_death) | manner_of_death == "", "Alive", "Dead"))),
    position = position_dodge2(width = 0.5),  # Align with the jittered points
    vjust = -1,  # Adjust vertical position to add space
    size = 3
  ) +
  scale_color_manual(
    values = c("Dead" = "red", "Alive" = "green"),
    guide = guide_legend(title = "Status")
  ) +
  labs(x = "Cage Number", y = "Treatment", color = "Status") +
  theme_minimal() +
  theme(legend.position = "top", 
        axis.text.x = element_text(size = 12, margin = margin(0, 40, 0, 40))) +  # Remove angle and adjust size
  guides(color = "none") +  # Remove the legend for "Status"
  scale_x_discrete(breaks = unique(processed_data$cage_number))  # Set breaks for x-axis

print(scatterplot)
```

#### Euthanasia Criteria Check

```{r Euthanasia Criteria Check, message=FALSE, warning=FALSE}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

# Read the 'raw_mass.csv' file into a data frame
raw_mass <- read.csv("data/raw/raw_mass.csv")

# Combine 'raw_mass' with 'processed_data'
processed_data <- left_join(processed_data, raw_mass, by = "id")

# Convert date columns to date format
processed_data$imaging_date <- as.Date(processed_data$imaging_date, format="%m/%d/%Y")
processed_data$mass_date <- as.Date(processed_data$mass_date, format="%m/%d/%Y")
processed_data$death_date <- as.Date(processed_data$death_date, format="%m/%d/%Y")

# Check if all mice are dead based on the 'manner_of_death' column
any_alive <- any(is.na(processed_data$death_date))

# Find the most recent imaging_date
most_recent_imaging_date <- max(processed_data$imaging_date, na.rm = TRUE)

if (any_alive) {
  # Find the oldest and newest mass_date for each mouse, excluding mice with missing mass_date
  mass_date_summary <- processed_data %>%
    filter(!is.na(mass_date)) %>%
    group_by(id) %>%
    summarize(
      oldest_mass_date = ifelse(all(is.na(mass_date)), NA, min(mass_date, na.rm = TRUE)),
      newest_mass_date = ifelse(all(is.na(mass_date)), NA, max(mass_date, na.rm = TRUE))
    )

  # Calculate the mass loss percentage for each mouse, excluding mice with missing mass_date
  mass_loss <- mass_date_summary %>%
    mutate(
      id = as.character(id),  # Convert id to character
      mass_loss_percentage = (1 - newest_mass_date / oldest_mass_date) * 100
    )

  # Find mice with total_flux >= 2e10 from the most recent imaging_date
  high_flux_mice <- processed_data %>%
    filter(imaging_date == most_recent_imaging_date,
           total_flux >= 2e10,
           is.na(death_date))

  # Find mice meeting the mass loss criterion (20% or more loss)
  mass_loss_mice <- mass_loss %>%
    filter(!is.na(oldest_mass_date), !is.na(newest_mass_date),  # Exclude mice with missing mass dates
           mass_loss_percentage >= 20)

  # Combine the two sets of mice that meet either criteria
  mice_to_euthanize <- union(high_flux_mice$id, mass_loss_mice$id)

  # Check if there are any mice to euthanize based on either criterion
  if (length(mice_to_euthanize) > 0) {
    if (length(high_flux_mice$id) > 0) {
      cat("With respect to the IACUC protocol, you need to euthanize mouse number(s) meeting the following criteria:\n")
      cat("- Total flux greater than or equal to 2x10^10:", paste(high_flux_mice$id, collapse = ", "), "\n")
    }

    if (length(mass_loss_mice$id) > 0) {
      cat("- Mass loss of 20% or more:", paste(mass_loss_mice$id, collapse = ", "), "\n")
    }
  } else {
    cat("No mice meet the criteria for euthanasia.")
  }
} else {
  cat("All mice are dead. No further action is required.")
}
```

#### Comments

na

### Cage Cards

```{r Cage Cards, results='asis'}
# Set root.dir to the current file's directory
current_file_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
knitr::opts_knit$set(root.dir = current_file_dir)

# Specify the relative directory containing PNG files
png_directory <- "data/cage_cards"

# Get a list of .PNG files within the specified directory
png_files <- list.files(file.path(current_file_dir, png_directory), pattern = "\\.PNG$", full.names = TRUE)

# Create a container div for the images
cat("<div style='text-align: center;'>")

# Loop through the .PNG files and generate HTML content with centered labels above each image
for (png_file in png_files) {
  # Get the base name of the PNG file (without the path)
  file_name <- basename(png_file)

  # Add a label div above the image with increased font size
  cat(sprintf("<div style='font-size: 18px;'>%s</div>\n", file_name))

  # Create HTML content with an image tag
  cat(sprintf("<img src='%s' width='700' />\n", png_file))
}

# Close the container div
cat("</div>\n")
```

# Statistical Analyses

Column
-----------------------------------------------------------------------

</verbatim>
</div>

### Descriptives

<div class="column-scrollable">
<verbatim>

```{r Descriptives, message=FALSE, warning=FALSE, comment=NA}
processed_data <- read.csv("data/processed/processed_data.csv")

descriptives <- jmv::descriptives(
    formula = total_flux ~ imaging_date:trt,
    data = processed_data,
    desc = "rows",
    se = TRUE,
    ci = TRUE)

descriptives

# Check if save_output is equal to "y"
if (save_output == "y") {
  # Save descriptives as a .txt file
  capture.output(descriptives, file = "data/processed/descriptives.txt", append = TRUE)
}
```

### ANOVA

```{r ANOVA, message=FALSE, warning=FALSE, comment=NA}
processed_data <- read.csv("data/processed/processed_data.csv")

tryCatch(
  {
    anova <- jmv::ANOVA(
      formula = total_flux ~ imaging_date + trt + imaging_date:trt,
      data = processed_data,
      effectSize = "eta",
      homo = TRUE,
      norm = TRUE,
      postHoc = ~ trt + imaging_date,
      postHocCorr = c("tukey", "bonf"),
      postHocES = "d",
      postHocEsCi = TRUE,
      emmPlots = FALSE
    )
    # Check if save_output is equal to "y"
if (save_output == "y") {
  # Save ANOVA results as a .txt file
  capture.output(anova, file = "data/processed/anova.txt", append = TRUE)
}

    # Display ANOVA results
    anova
  },
  error = function(e) {
    if (grepl("incorrect number of dimensions", e$message)) {
      cat("Error: The data has incorrect dimensions for ANOVA analysis.\n")
    } else {
      cat("Error occurred during ANOVA analysis:\n")
      cat(e$message, "\n")
    }
  }
)
```
