# SiRiUS
 Simulate Rice Ubiquitous System (SiRiUS) is an R Package for running ORYZA rice crop model simulations with a spatial configuration. ORYZA originally was created to mimic crop growth on individual plots but has proven to be a potent tool for providing reasonable production and yield estimates over regions; a reliable piece of information for different purposes, i.e. estimating supply, damage and losses, mitigating environmental stresses, targetting farming solutions, etc. It is capable of leveraging on freely available soil and weather data to estimate yield and other parameters considered in the ORYZA model. (In the near future, user-provided inputs can also be ingested by using tools to format them in ORYZA-readable files.)
 
 This also manages ORYZA configuration and output files to be able to easily review simulation results. 
 
## Components
 
 
### Schema Builder
  The Schema Builder allows a user to design a schema for running ORYZA for different purposes. A schema is a set of configurations needed to run ORYZA. In addition to the usual ORYZA Parameters that needs to be specified, the Area of Interest (AOI), is a major component of a schema which allows Sirius to automatically provide weather and soil inputs for the simulations. It also provides a convenient way to ingest real-world farm management practices in any run of ORYZA.
  
  The Schema Builder organizes schemas so that files are easier to retrieve for review. It is best to create a UI for this to avoid changing the modules for every run.     

### Controller - Worker
  The Controller manages the execution of a schema and delegates simulation runs to workers. A worker is basically a clone of ORYZA, copied in a different folder. The ORYZA executable, originally, can only be called/run once; but if you create a copy of the executable file in a different folder together with input files, a user will be able to run another instance at the same time and essentially double the efficiency. For powerful computers, the controller will be able to run more than 2 parallel instances of ORYZA by setting NWORKERS parameter to at least 3. 
  
  Progress monitoring may be possible in the controller, but it may not be necessary for small to medium-scale runs. 
 
### Plotter
  The Plotter turns tables created by ORYZA into maps. Some basic options will be available to visualize the results right away.   
 
### Input Formatters
  The Input Formatters rewrite input files, such as farm management, soil and weather, to a format that ORYZA can recognize and use. It is independent from the other components so that formatting can be done in advance if needed.
