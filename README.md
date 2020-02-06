# NeuralNetCast
Forecasting biodiversity using neural networks

## The basic idea
 - Building/testing short-term forecasts of biodiversity data following the method described in Grasso et al. (2019) (for toxin data)
 - Uses collections of survey data that measure species abundance for many species at many sites, at regular time steps
 - Input data looks something like this:
```   
   | Time Step | Location | Species 1 | Species 2 |...|
   |-----------|----------|-----------|-----------|---|
   |   1997    |   XYZ1   |     1234  |       37  |...|
   |   1997    |   XYZ2   |       31  |      987  |...|
   |   1998    |   XYZ1   |        0  |       23  |...|
   |    ...    |    ...   |      ...  |      ...  |...|
```
 - The data are rearranged into "postage stamp" images, representing the precursor to an event, at each site and each time step, that we are forecasting:
```
              -----------------------------
   Species 1  |  45  |  31  | 234  |   0  |  
   Species 2  |   1  |   8  |  99  | 123  |     ...associated with
   ...        | ...  | ...  | ...  | ...  |    ------------------------->  SUBSEQUENT EVENT
   Species n  |   4  |  13  | 444  |  22  |
              -----------------------------
                 4      3      2      1
                 Time steps prior to event
```
 - The neural network then trains on (some subset of) these data, and can be tested in forecasting mode
 
## This repo
 - The `Code` directory has:
   - `NeuralNetCast*.R` - The functions written for all the steps in the process
   - `NeuralNetCastExamples*.R` - Scripted examples of using the code
 - The `Data` directory has data used in the examples
 
   
