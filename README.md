This is a template I made for making reproducable, replicable, templateable, modular, and accurate experimental documentation for studies involving bioluminescent imaging.

My goal is to neatly package all experimental aspects together to provide an exhaustive yet succinctly navigable format.

The top of the .Rmd file must be modified with respect to specific experimental parameters. Then, the .Rmd file is knitted to .html for a user-friendly report.

*raw_data.csv* and *raw_mass.csv* must be manually edited. The output .csv files from Living Image® are manually added to *data/imaging/raw/week_#/*

Here is the folder hierarchy:

- **experiment_name/**
  - *experiment_name.md*: Markdown file for all experimental documentation
  - *experiment_name.Rmd*: R Markdown file for knitting experimental documentation into HTML
  - *experiment_name.html*: HTML file for user-friendly rendering of experimental documentation

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

    - **cage_cards/**
      - *000001.png*
      - *000002.png*
      - ...

    - **processed/**
      - *processed_data.csv*: Contains all experimental data
      - *mouse_current_data.csv*: Contains currently relevant mouse data for tracking

    - **raw/**
      - *raw_data.csv*: Contains all experimental data except imaging_data.csv
      - *raw_mass.csv*: Contains mass of all mice
     

```mermaid
graph TD
  subgraph experiment_name
    experiment_name.md --> experiment_name.Rmd
    experiment_name.Rmd --> experiment_name.html
  end

  subgraph data
    subgraph imaging
      subgraph processed
        week_1 --> week_1.csv
        week_2 --> week_2.csv
        week_1 --> week_1
        week_2 --> week_2
      end
      subgraph raw
        week_1 --> week_1
        week_2 --> week_2
      end
    end

    subgraph cage_cards
      000001.png
      000002.png
      ...
    end

    subgraph processed
      processed_data.csv
      mouse_current_data.csv
    end

    subgraph raw
      raw_data.csv
      raw_mass.csv
    end
  end
```
