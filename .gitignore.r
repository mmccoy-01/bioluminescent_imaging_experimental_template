---
title: "{{name}}"
status: {{status}}
sample: {{num}}
output:
  flexdashboard::flex_dashboard:
    vertical_layout: fill
    orientation: columns
runtime: shiny
---

```{r User-Defined Experimental Parameters, message=FALSE}
# The user defines different variables and parameters that will be specific to the study.

inj_date <- as.Date("%m/%d/%Y", format = "%m/%d/%Y")

trt_date <- as.Date("%m/%d/%Y", format = "%m/%d/%Y")

sac_date <- as.Date("%m/%d/%Y", format = "%m/%d/%Y")

surv_date <- as.Date("na", format = "%m/%d/%Y")

engraftment_imaging_date <- as.Date("%m/%d/%Y", format = "%m/%d/%Y")

dates <- as.Date(c("%m/%d/%Y", "%m/%d/%Y", "%m/%d/%Y", "%m/%d/%Y3"), format = "%m/%d/%Y")

number_of_expansion_mice <- num
number_of_groups <- num
mice_per_group <- num
filtered_mice <- c(num:num)
labels <- c("1" = "AAA", "2" = "BBB", "3" = "CCC AAA", "4" = "DDD")
```

```{r Load Libraries, message=FALSE}
# Load packages
library(tidyverse) # tidyverse
library(shiny) # shiny app functionality
library(kableExtra) # for knitting .rmd tables to .html tables
library(DT) # for rendering tables for cases when kable does not work
library(scales) # scale functions for customizing visualizations
library(plotly) # interactive cursor friendly plots
library(survival) # for survival curves
library(survminer) # for survival curves
library(jmv) # performing statistical analyses

# Set the working directory to the current script's directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
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
# Randomly assign mice to treatment conditions so that each treatment condition has equivalent total_flux between treatments. All of the unassigned remaining mice have the lowest total_flux and are assigned to trt = na aka the expansion group/DDD group.

# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

filtered_data <- processed_data %>%
  filter(imaging_date == engraftment_imaging_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  arrange(total_flux) %>%
  filter(if (number_of_expansion_mice == 0) TRUE else row_number() <= number_of_expansion_mice)

set.seed(18902)

# Function to assign mice to groups
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

# Call the function to assign groups using filtered_data
result <- assign_groups(filtered_data, number_of_groups, mice_per_group)

# Rename the Group column in the result$Mean_Total_Flux data frame
result$Mean_Total_Flux$Group <- labels

processed_data <- processed_data %>%
  left_join(result$Data %>% select(id, trt), by = "id") %>%
  mutate(trt = coalesce(trt.x, trt.y)) %>%
  select(-trt.x, -trt.y) %>%
  select(id, trt, everything()) %>%
  relocate(trt, .before = trt_injection_vial)
  
# Write the 'processed_data' data frame to a CSV file
write_csv(processed_data, file = "data/processed/processed_data.csv")
```

# Data

Column {.tabset data-width=600}
-----------------------------------------------------------------------

### Treatment Plot

```{r Treatment Plot, message=FALSE, warning=FALSE, fig.height=40, fig.width=11}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

ui <- fluidPage(
  titlePanel("Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tumor_injection_select", "Tumor Injection", choices = unique(processed_data$tumor_injection)),
      checkboxInput("facet_wrap", "Facet Wrap", value = TRUE),
      selectInput("y_variable", "Y-Axis Variable", choices = c("total_flux", "avg_radiance"), selected = "total_flux"),
      selectInput("calculation_type", "Calculation Type", choices = c("Mean", "Median", "Individual"), selected = "Mean"),
      checkboxInput("group_points", "Group Points", value = TRUE),
      selectInput("plot_type", "Plot Type", choices = c("Line Graph", "Bar Graph"), selected = "Line Graph")
    ),
    mainPanel(
      plotlyOutput("total_flux_plot"),
      tableOutput("count_table")
    )
  )
)

server <- function(input, output) {
  # Read the data
  processed_data <- read.csv("data/processed/processed_data.csv")

# Define a custom theme for the plot (without vertical grid lines)
my_theme <- theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 12, margin = margin(r = 15)),  # Adjust the margin
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank()   # Remove vertical minor grid lines
  )

  # Group by trt and imaging_date, and calculate the mean and median for each group
  summary_data <- processed_data %>%
    group_by(trt, imaging_date) %>%
    summarise(mean_total_flux = mean(total_flux, na.rm = TRUE),
              mean_avg_radiance = mean(avg_radiance, na.rm = TRUE),
              median_total_flux = median(total_flux, na.rm = TRUE),
              median_avg_radiance = median(avg_radiance, na.rm = TRUE)) %>%
    mutate(imaging_date = as.Date(imaging_date))

output$total_flux_plot <- renderPlotly({
    y_var <- ifelse(input$y_variable == "total_flux", "Total Flux", "Average Radiance")
    calculation_type <- input$calculation_type
    facet_enabled <- input$facet_wrap
    group_points_enabled <- input$group_points

    plot_type <- input$plot_type
    
    # Filter data based on selected tumor injection
    processed_data_filtered <- processed_data
    if (!is.null(input$tumor_injection_select)) {
      processed_data_filtered <- processed_data_filtered %>%
        filter(tumor_injection == input$tumor_injection_select)
    }

    # Create the plot
    p <- ggplot(data = processed_data, 
                aes(x = as.Date(imaging_date), y = !!sym(input$y_variable), color = factor(trt))) +
      labs(x = "Imaging Date", y = y_var, color = "Treatment") +
      scale_color_manual(
        name = "Treatment",
        values = c(
          "1" = alpha("#f8766d", 1),
          "2" = alpha("#7cae00", 1),
          "3" = alpha("#00bfc4", 1),
          "4" = alpha("#C3B1E1", 1)
        ),
        labels = labels
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      my_theme

    # Conditional y-axis scaling
    if (input$y_variable == "total_flux") {
      p <- p + scale_y_log10(labels = scientific_format())
    } else {
      p <- p + scale_y_continuous()
    }

    if (calculation_type == "Mean") {
      if (plot_type == "Line Graph") {
        p <- p +
          geom_line(data = summary_data, aes(x = imaging_date, y = !!sym(paste0("mean_", input$y_variable)), group = trt), linewidth = 0.9)
      } else if (plot_type == "Bar Graph") {
        if (facet_enabled) {
          p <- p +
            geom_bar(data = summary_data, aes(x = imaging_date, y = !!sym(paste0("mean_", input$y_variable)), fill = factor(trt)), stat = "identity")
        } else {
          p <- p +
            geom_bar(data = summary_data, aes(x = imaging_date, y = !!sym(paste0("mean_", input$y_variable)), fill = factor(trt)), stat = "identity", position = "dodge")
        }
      }
      grouped_points <- summary_data
      point_variable <- paste0("mean_", input$y_variable)
    } else if (calculation_type == "Median") {
      if (plot_type == "Line Graph") {
        p <- p +
          geom_line(data = summary_data, aes(x = imaging_date, y = !!sym(paste0("median_", input$y_variable)), group = trt), linewidth = 0.9)
      } else if (plot_type == "Bar Graph") {
        if (facet_enabled) {
          p <- p +
            geom_bar(data = summary_data, aes(x = imaging_date, y = !!sym(paste0("median_", input$y_variable)), fill = factor(trt)), stat = "identity")
        } else {
          p <- p +
            geom_bar(data = summary_data, aes(x = imaging_date, y = !!sym(paste0("median_", input$y_variable)), fill = factor(trt)), stat = "identity", position = "dodge")
        }
      }
      grouped_points <- summary_data
      point_variable <- paste0("median_", input$y_variable)
    } else if (calculation_type == "Individual") {
      if (plot_type == "Line Graph") {
        p <- p +
          geom_line(aes(group = id), alpha = 0.5, size = 0.5)
      } else if (plot_type == "Bar Graph") {
        # You can choose an appropriate way to represent individual data as bars
        # For example, you can use geom_bar with fill based on trt
      }
      grouped_points <- processed_data %>%
        group_by(trt, imaging_date) %>%
        summarise(x = mean(as.Date(imaging_date)), y = mean(!!sym(input$y_variable))) %>%
        ungroup()
      point_variable <- paste0("mean_", input$y_variable)
    }

    # Toggle for grouping points
    if (group_points_enabled) {
      if (plot_type == "Line Graph") {
        p <- p +
          geom_point(data = grouped_points, size = 2, aes(x = imaging_date, y = !!sym(point_variable), color = factor(trt)))
      } else if (plot_type == "Bar Graph") {
        # You can choose an appropriate way to represent grouped data as bars
        # For example, you can use geom_bar with fill based on trt
      }
    } else {
      # Show individual points
      if (plot_type == "Line Graph") {
        p <- p +
          geom_point(size = 2, shape = 16, alpha = 0.5, aes(text = id))
      } else if (plot_type == "Bar Graph") {
        # You can choose an appropriate way to represent individual data as bars
        # For example, you can use geom_bar with fill based on trt
      }
    }

    # Add facet_wrap if enabled
    if (facet_enabled) {
      p <- p + facet_wrap(~ trt)
    }

    # Convert ggplot to Plotly
    ggplotly(p) %>%
      layout(legend = list(orientation = "v", x = 1.05, y = 0.5), hovermode = "x+y")
  })
output$count_table <- renderTable({
  # Group the data by trt and imaging_date and calculate the count of id
  count_data <- processed_data %>%
    group_by(trt, imaging_date) %>%
    summarise(id_count = n()) %>%
    spread(key = trt, value = id_count, fill = 0)

  # Ensure that the count values are whole numbers with no decimal places
  count_data <- count_data %>%
    mutate(across(-imaging_date, ~ as.integer(.)))

  count_data
})
}

shinyApp(ui, server)
```

### Kaplan-Meier Curve

```{r Kaplan-Meier Curve, message=FALSE, warning=FALSE, fig.height=10, fig.width=11}
processed_data <- read.csv("data/processed/processed_data.csv")

# Check if processed_data has any rows before proceeding
if (nrow(processed_data) > 0) {
  # Filter the data by id
  processed_data <- processed_data %>%
    filter(id %in% filtered_mice)
  
  # Convert the columns to Date format with the format 'month/day/year'
  processed_data <- processed_data %>%
    mutate(
      death_date = as.Date(death_date, format = "%m/%d/%Y"),
      trt_injection_date = as.Date(trt_injection_date, format = "%m/%d/%Y"),
      tumor_injection_date = as.Date(tumor_injection_date, format = "%m/%d/%Y")
    )
  
  # Calculate the days_from_trt_to_death using both trt_injection_date and tumor_injection_date
  processed_data <- processed_data %>%
    mutate(
      days_from_trt_to_death = as.numeric(difftime(death_date, trt_injection_date, units = "days")),
      # Replace NA values in days_from_trt_to_death with the corresponding difference using tumor_injection_date
      days_from_trt_to_death = ifelse(is.na(days_from_trt_to_death), as.numeric(difftime(death_date, tumor_injection_date, units = "days")), days_from_trt_to_death)
    )
  
  # Create a survival object
  survival_data <- with(processed_data, Surv(days_from_trt_to_death, event = rep(1, length(trt))))
  
  # Check if there are any non-missing observations in survival_data
  if (any(!is.na(survival_data))) {
    # Fit Kaplan-Meier survival curves for each treatment group
    surv_fit <- survfit(survival_data ~ trt, data = processed_data)
    
    # Plot Kaplan-Meier curves
    ggsurvplot(surv_fit, data = processed_data, risk.table = TRUE, pval = TRUE)
    
    # Check if the days_from_trt_to_death column exists before selecting it
    if ("days_from_trt_to_death" %in% colnames(processed_data)) {
      # Create a table that shows the rounded average days of survival by trt
      average_days_by_trt <- processed_data %>%
        group_by(trt) %>%
        summarize(average_survival_in_days = round(mean(days_from_trt_to_death, na.rm = TRUE)))
      
      # Create and print the table
      average_days_by_trt %>%
        kable("html") %>%
        kable_styling(bootstrap_options = "striped", full_width = FALSE)
    } else {
      cat("Column 'days_from_trt_to_death' not found in processed_data.")
    }
  } else {
    cat("No non-missing observations for survival data found.")
  }
} else {
  cat("No data available for analysis.")
}
```

Column {.tabset data-width=400}
-----------------------------------------------------------------------

### IVIS Images

```{r IVIS Images, results='asis'}
# Set the file path
file_path <- "data/imaging/processed/"

# Get a list of all subdirectories one level deep within the specified directory
subdirectories <- list.dirs(file_path, full.names = TRUE, recursive = FALSE)

# Loop through each subdirectory and generate HTML content
for (subdirectory in subdirectories) {
  # Get the name of the current subdirectory
  folder_name <- basename(subdirectory)
  
  # Get a list of .PNG files within the subdirectory
  png_files <- list.files(subdirectory, pattern = "\\.PNG$", full.names = TRUE, recursive = FALSE)
  
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

```{r Mouse Current Status Table, message=FALSE, warning=FALSE}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

# Select the desired columns and format trt
most_recent_data <- processed_data %>%
  filter(id %in% filtered_mice) %>%
  select(cage_number, id, ear_punch, trt, imaging_date, total_flux, avg_radiance, death_date, manner_of_death) %>%
  mutate(trt = labels[as.character(trt)])

# Create a .csv file of current mice data with the most recent imaging_date
most_recent_data %>%
  group_by(id) %>%
  arrange(desc(imaging_date)) %>%
  slice(1) %>%  # Keep only the rows with the most recent imaging_date for each id
  select(cage_number, id, trt, imaging_date, total_flux, avg_radiance, death_date, manner_of_death) %>%
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

# Define colors for trt values
trt_colors <- c(
  "1" = alpha("#f8766d", 1),
  "2" = alpha("#7cae00", 1),
  "3" = alpha("#00bfc4", 1),
  "4" = alpha("#C3B1E1", 1)
)

# Create a ggplot object with facetting by trt and grouping by id
p <- ggplot(combined_data, aes(x = mass_date, y = mass, color = factor(trt), group = id)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = trt_colors) +
  facet_wrap(~trt, scales = "free", ncol = 2) +  # Adjust ncol as needed
  theme_minimal()

# Convert ggplot to plotly
p <- ggplotly(p)

# Display the interactive plot
p
```

### Engraftment Status Report

#### Engraftment Status

```{r Engraftment Status, message=FALSE, warning=FALSE, fig.height=6, fig.width=11}
processed_data <- read.csv("data/processed/processed_data.csv")

# Precompute the order of 'id' based on 'total_flux' for the engraftment_imaging_date
order_by_flux <- processed_data %>%
  filter(imaging_date == engraftment_imaging_date) %>%
  arrange(desc(total_flux)) %>%
  pull(id)

# Filter the data for the plot
filtered_data <- processed_data %>%
  filter(id %in% filtered_mice, imaging_date <= engraftment_imaging_date)

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
kable(table_data, format = "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

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

```{r Treatment Assignment Plot, message=FALSE, warning=FALSE}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

trt_means <- processed_data %>%
  mutate(trt = factor(trt, levels = names(labels), labels = labels)) %>%
  filter(imaging_date == engraftment_imaging_date) %>%
  group_by(trt) %>%
  summarize(mean_total_flux = mean(total_flux))

trt_means

# Define colors for trt values
trt_colors <- c(
  "1" = alpha("#f8766d", 1),
  "2" = alpha("#7cae00", 1),
  "3" = alpha("#00bfc4", 1),
  "4" = alpha("#C3B1E1", 1)
)

# Create the ggplot object with data filtering using subset and add jitter
p <- ggplot() +
  geom_point(data = trt_means, aes(x = factor(trt), y = mean_total_flux), 
             size = 3, color = "blue") +
  geom_point(data = subset(processed_data, imaging_date == engraftment_imaging_date), 
             aes(x = factor(trt, levels = 1:number_of_groups, labels = labels), y = total_flux, color = factor(trt), label = id), 
             size = 2, position = position_jitter(width = 0.2, height = 0)) +
  scale_color_manual(values = trt_colors) +
  labs(x = "trt", y = "Total Flux") +
  theme_minimal()

ggplotly(p)
```

### Thawing Cells

#### Tumor:

tumor vial info

#### Treatment

treatment vial info

Column {.tabset data-width=500}
-----------------------------------------------------------------------

### Subjects

#### Mouse Current Status Plot

```{r Current Status by Count and Treatment, message=FALSE, warning=FALSE, fig.height=6, fig.width=10}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

# Your summary table
summary_table <- processed_data %>%
  filter(!is.na(trt)) %>%
  distinct(trt, id, .keep_all = TRUE) %>%
  group_by(trt) %>%
  summarize(
    Dead = sum(!is.na(manner_of_death) & manner_of_death != ""),  # Count non-blank manner_of_death as dead
    Alive = sum(ifelse(is.na(manner_of_death) | manner_of_death == "", 1, 0)),  # Count blank or NA manner_of_death as alive
    Dead_ids = toString(id[!is.na(manner_of_death) & manner_of_death != ""]),
    Alive_ids = toString(id[is.na(manner_of_death) | manner_of_death == ""])
  )

# Calculate total unique mice dead and alive
total_dead <- sum(summary_table$Dead)
total_alive <- sum(summary_table$Alive)

# Create a grouped bar plot
p <- ggplot(summary_table, aes(x = factor(trt, labels = labels))) +
  geom_bar(aes(y = Alive, fill = "Alive"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Dead, fill = "Dead"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Dead" = "red", "Alive" = "green")) +  # Adjust fill colors
  labs(x = "Treatment", y = "Count", fill = "Status") +
  ggtitle("Dead and Alive Counts of Study Mice by Treatment") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())  # Remove horizontal grid lines between y-axis tick marks

# Set x-axis breaks and limits using scale_x_discrete
p <- p + scale_x_discrete(breaks = labels, limits = labels)

# Set y-axis breaks and limits
p <- p +
  scale_y_continuous(
    breaks = seq(0, max(summary_table$Dead, summary_table$Alive) + 2, by = 1),  # Set breaks dynamically
    limits = c(0, max(summary_table$Dead, summary_table$Alive) + 4)  # Adjust the limits dynamically
  )

# Add annotations for alive and dead mouse numbers with color
p <- p +
  geom_text(data = summary_table, aes(x = factor(trt, labels = labels), 
                y = Dead + Alive + 0.5, label = Alive_ids, color = "green"), 
            size = 5, hjust = 0.5) +  
  geom_text(data = summary_table, aes(x = factor(trt, labels = labels), 
                y = Dead + Alive - 0.5, label = Dead_ids, color = "red"), 
            size = 5, hjust = 0.5)  

# Set legend labels including total counts
p <- p +
  labs(fill = paste("Status (Alive:", total_alive, ", Dead:", total_dead, ")"))

# Specify the color scale for text labels
p <- p +
  scale_color_manual(values = c("red" = "red", "green" = "green"))

# Remove the color legend for "Status"
p <- p +
  guides(color = "none")

print(p)
```

```{r Current Status by Cage and Treatment, message=FALSE, warning=FALSE, fig.height=6, fig.width=10}
# Load data
processed_data <- read.csv("data/processed/processed_data.csv")

# Create a scatterplot for cage_number, trt, and status without overlap
scatterplot <- processed_data %>%
  select(cage_number, id, trt, manner_of_death) %>% 
  distinct(trt, id, .keep_all = TRUE) %>% 
  ggplot(aes(x = factor(cage_number), y = factor(trt, labels = labels))) +  # Use labels here
  geom_jitter(
    aes(color = ifelse(is.na(manner_of_death) | manner_of_death == "", "Alive", "Dead")),
    size = 3,
    position = position_dodge2(width = 0.5),  # Adjust width as needed
    alpha = 0.7
  ) +
  geom_text(
    aes(label = id), 
    position = position_dodge2(width = 0.5),  # Match the jitter width
    vjust = -1,  # Adjust vertical position to add space
    size = 3
  ) +
  scale_color_manual(
    values = c("Dead" = "red", "Alive" = "green")
  ) +
  labs(x = "Cage Number", y = "Treatment", color = "Status") +
  ggtitle("Mouse Status by Cage and Treatment") +
  theme_minimal() +
  theme(legend.position = "top", 
        axis.text.x = element_text(size = 12, margin = margin(0, 40, 0, 40))) +  # Remove angle and adjust size
  guides(color = "none") +  # Remove the legend for "Status"
  scale_x_discrete(breaks = unique(raw_data$cage_number))  # Set breaks for x-axis

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
processed_data$imaging_date <- as.Date(processed_data$imaging_date)
processed_data$mass_date <- as.Date(processed_data$mass_date)
processed_data$death_date <- as.Date(processed_data$death_date)

# Find the most recent imaging_date
most_recent_imaging_date <- max(processed_data$imaging_date, na.rm = TRUE)

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
```

#### Comments



### Cage Cards

```{r Cage Cards, results='asis'}
# Set the directory containing PNG files
png_directory <- "cage_cards"

# Get a list of .PNG files within the specified directory
png_files <- list.files(png_directory, pattern = "\\.PNG$", full.names = TRUE)

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

### Spleen and Marrow Counts 

```{r Spleen and Marrow Counts, message=FALSE, warning=FALSE, fig.height=10, fig.width=15}
processed_data <- read.csv("data/processed/processed_data.csv")

bar_plot <- processed_data %>% 
  filter(id %in% filtered_mice) %>%
  mutate(
    cryovial_spleen_cell_total = coalesce(cryovial_spleen_cell_total, 0),
    cryovial_marrow_cell_total = coalesce(cryovial_marrow_cell_total, 0)
  ) %>%
  ggplot() +
  geom_bar(aes(x = id - 0.2, y = cryovial_spleen_cell_total, fill = "Spleen"), stat = "identity", position = "identity", width = 0.4) +
  geom_bar(aes(x = id + 0.2, y = cryovial_marrow_cell_total, fill = "Marrow"), stat = "identity", position = "identity", width = 0.4) +
  scale_fill_manual(values = c("Spleen" = "blue", "Marrow" = "red")) +
  labs(x = "Mouse Number", y = "Cell Total", fill = "Tissue") +
  ggtitle("Cryovial Spleen and Marrow Cell Total by Mouse Number") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = unique(processed_data$id), labels = unique(processed_data$id)) +
  coord_cartesian(ylim = c(0, max(processed_data$cryovial_spleen_cell_total, processed_data$cryovial_marrow_cell_total) + 10))

# Print the bar plot
print(bar_plot)
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
processed_data <- read.csv("data/processed/processed_data.csv") %>%
    filter(id %in% filtered_mice)

# Create a factor variable using the predefined labels
processed_data$trt <- factor(processed_data$trt, levels = names(labels), labels = labels)

descriptives <- jmv::descriptives(
    formula = total_flux ~ imaging_date:trt,
    data = processed_data,
    desc = "rows",
    se = TRUE,
    ci = TRUE)

descriptives

# save descriptives as a .txt file
capture.output(descriptives, file = "data/processed/descriptives.txt", append = TRUE)
```

### ANOVA

```{r ANOVA, message=FALSE, warning=FALSE, comment=NA}
processed_data <- read.csv("data/processed/processed_data.csv") %>%
    filter(id %in% filtered_mice)

# Create a factor variable using the predefined labels
processed_data$trt <- factor(processed_data$trt, levels = names(labels), labels = labels)

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

    # Save ANOVA results as a .txt file
    capture.output(anova, file = "data/processed/anova.txt", append = TRUE)
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
# Create and print the table
kable(table_data, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
```
