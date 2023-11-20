# Load the .Rmd file into R, download the necessary packages, then run document to see the experiment.

This is a template I made for making reproducable, replicable, templateable, modular, and accurate experimental documentation for studies involving bioluminescent imaging.

My goal is to neatly package all experimental aspects together to provide an exhaustive yet succinctly navigable format.

The top of the .Rmd file must be modified with respect to specific experimental parameters. Then, the .Rmd file is run using R through flexdashboard and shiny for an interactive and user-friendly report.

Just to reiterate: **YOU NEED TO RUN THE .Rmd FILE IN RStudio IN ORDER TO SEE THE DATA IN A NICE USER-FRIENDLY FORMAT!!!**

*raw_data.csv* and *raw_mass.csv* must be manually edited as the experiment progresses.

The output files from Living Image® are manually added to *data/imaging/raw/week_#/*. After processing, these raw files can be deleted so as to conserve storage space. It's okay, all of the data is still retained in the processed files.

The processed output files from Living Image® are manually added to *data/imaging/processed/week_#/*. For each week, one file is a .csv and one is a folder containing processed Living Image® data

Here is the folder hierarchy:

- **experiment_name/**
  - *experiment_name.Rmd*: R Markdown file for knitting experimental documentation into a flexdashboard with shiny app and plotly configuration
  - *README.md*: Dcoumentation and support file for understanding folder hierarchy
  - **data/**
    - **imaging/**
      - **processed/**
        - *%Y-%m-%d/*: Contains processed Living Image data for %Y-%m-%d
        - *%Y-%m-%d.csv*: Contains imaging data for %Y-%m-%d
        - ...
      - **raw/**: (Since processed imaging data combines raw imaging data, you can delete this so as to conserve storage space)
        - *%Y-%m-%d/*: Contains unprocessed Living Image data for %Y-%m-%d
        - ...
    - **cage_cards/**
      - *000001.PNG* (PNG must be capitalized)
      - *000002.PNG* (PNG must be capitalized)
      - ...
    - **flow/**
      - *pdx_name.wsp*
      - *pdx_name.PNG* (PNG must be capitalized)
      - ...
    - **processed/**
      - *processed_data.csv*: Contains all experimental data
      - *mouse_current_data.csv*: Contains currently relevant mouse data for tracking (used for physical printing)
      - *anova.txt*: ANOVA output
      - *descriptives.txt*: Descriptives output
    - **raw/**
      - *raw_data.csv*: Contains all experimental subjects' data except imaging_data data and mass data
      - *raw_mass.csv*: Contains mass of all mice
     

```mermaid
graph LR
experiment_name --> experiment_name.Rmd
experiment_name --> README.md
experiment_name --> data

data --> imaging

imaging --> G(processed)

G(processed) --> A(%Y-%m-%d/)
G(processed) --> L(%Y-%m-%d.csv)
G(processed) --> I(...)

imaging --> E(raw)

E(raw) --> C(%Y-%m-%d/)
E(raw) --> J(...)

data --> cage_cards

cage_cards --> 000001.PNG
cage_cards --> 000002.PNG
cage_cards --> K(...)

data --> flow

flow --> pdx_name.wsp
flow --> pdx_name.PNG

data --> H(processed)

H(processed) --> processed_data.csv
H(processed) --> mouse_current_data.csv
H(processed) --> anova.txt
H(processed) --> descriptives.txt
data --> F(raw)

F(raw) --> raw_data.csv
F(raw) --> raw_mass.csv
```
