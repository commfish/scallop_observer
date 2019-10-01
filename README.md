# Organization of observer data summary

Ben Williams  
8-2018  
ben.williams@alaska.gov

Data ----

The catch, bycatch, catch_comp, crab_size, logbook, and shell_height data files are downloaded annually from the Kodiak wiki  
http://kodweb.fishgame.state.ak.us/index/Data_Access:Scallop_Observer

General questions about the data should be directed to the scallop observer coordinator: Ryan Burt (ryan.burt@alaska.gov)

 - All functions begin with a `f_`
 - snake_case with underscores is used for all naming conventions
 - a capital words are factors 
 
 Process
 
  - The raw data are cleaned up, the fishing year is added, names are changed etc. This has to occur at the beginning by running the scripts in `0_data_cleaning.R`
  - All functions are stored in the `functions.R` file
  - Libraries and map data are stored in the `helper.R` file
  - the functions and helper files are sourced in the analysis scripts.
  
The complete folder setup is:

 - code
 - data
    - age
    - bycatch
      - old_bycatch
    - catch
    - catch_comp
    - crab_size
    - log
    - meat_weight
    - old_catch
    - shell_height
  - figs
    - each year
  - models
    - each year
  - notes
  - output
    - each year
  - safe_figs
    - each year
  - tables
    - each year
  - tables
    - each year
  - text
    - each year
  



