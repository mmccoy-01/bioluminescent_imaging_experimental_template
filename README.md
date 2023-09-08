Click on experiment_name.html for viewing the experiment.

Here is the folder hierarchy:

- **experiment_name/**
  - *experiment_name.md*: Markdown file for all experimental documentation
  - *experiment_name.Rmd*: R Markdown file for knitting experimental documentation into HTML
  - *experiment_name.html*: HTML file for user-friendly rendering of experimental documentation

  - **cage_cards/**
    - *000001.png*
    - *000002.png*
    - ...

  - **data/**
    - **imaging/**
      - **processed/**
        - *week_1/*: Contains processed Living Image data for Week 1
        - *week_2/*: Contains processed Living Image data for Week 2
        - *week_1.csv*: Contains imaging data for Week 1
        - *week_2.csv*: Contains imaging data for Week 2
        - ...
      - **raw/**
        - *week_1/*: Contains unprocessed Living Image data for Week 1
        - *week_2/*: Contains unprocessed Living Image data for Week 2
        - ...

    - **processed/**
      - *imaging_data.csv*: Contains imaging data across all weeks
      - *processed_data.csv*: Contains all experimental data
      - *mouse_current_data.csv*: Contains currently relevant mouse data for tracking

    - **raw/**
      - *raw_data.csv*: Contains all experimental data except imaging_data.csv
      - *raw_mass.csv*: Contains mass of all mice
