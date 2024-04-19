# SiRiUS
 Simulate Rice Ubiquitous System (SiRiUS) is an application for running small to large-scale ORYZA rice crop model simulations. It is capable of leveraging on freely available soil and weather data to estimate yield and greenhouse gas emissions. (In the near future, user-provided inputs can also be ingested by using tools to format them in ORYZA-readable files.)
 
 This also manages ORYZA configuration and output files to be able to easily review simulation results. 
 
## Components
 
 
### Schema Builder
  The Schema Builder allows a user to design a schema for running ORYZA for different purposes. A schema is a set of configurations needed to run ORYZA. This includes the Area of Interest (AOI), and the target period covered. It organizes schemas so that files are easier to retrieve. It is best to create a UI for this to avoid changing the modules.     

### Controller - Worker
  The Controller executes the schema created by the schema builder. For powerful computers, the controller will be able to run parallel instances of ORYZA via workers. Progress monitoring may be possible in the controller, but it may not be necessary for small to medium-scale runs. 
 
### Plotter
  The Plotter turns the tables created by ORYZA into maps. Some basic options will be available to visualize the results right away.   
 
### Input Formatter
  The Input Formatter rewrites input files, such as soil and weather, to a format that ORYZA can recognize and use. It is somewhat independent from the other components, so that formatting can be done in advance if needed.