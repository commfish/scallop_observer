# Organization of observer data repository

Tyler Jackson  
Feb 2020
tyler.jackson@alaska.gov

Data ----

The catch, bycatch, catch_comp, crab_size, logbook, and shell_height data files are downloaded as needed from the Kodiak wiki  
http://kodweb.fishgame.state.ak.us/index/Data_Access:Scallop_Observer. Contact Tyler Jackson for .csv files.


General questions about the data should be directed to the scallop observer program: Ryan Burt (ryan.burt@alaska.gov); Alyssa Hopkins (alyssa.hopkins@alaska.gov)

 - All custom functions begin with a `f_` and are sourced within a separate script
 - snake_case with underscores is used for all naming conventions
 - a capital words are factors 
  
The primary folder setup is:

 - code
   - age_estimation
   - cpue_standardization
   - discard_estimation
   - maps
   - misc
   - safe
 - data (see README file therein)
 - figures
   - age_estimation
   - cpue_standardization
   - discard_estimation
   - observer_data_report
   - safe
 - output
   - safe
 - text
   - age_estimation







