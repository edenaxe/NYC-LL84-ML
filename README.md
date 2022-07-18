# NYC-LL84-ML

Machine learning exercise using New York City's Local Law 84 benchmarking data. Completed as part of Harvard's Professional Certifacte in Data Science. 

### Data sources
- Local Law 84 data from NYC Open Data - [Link](https://opendata.cityofnewyork.us/)
- Weather station data from NOAA's National Weather Service - [Link](https://www.weather.gov/wrh/Climate?wfo=okx)

### Contents
- R script for retrieving and cleaning the data
- R script to run the analysis, stripped version
- Rmarkdown which includes the full report
- A knit PDF version of the Rmarkdown

### Results

The final test set RMSE was 25.11, a 46.8% improvement over the naive model. The final validation set (2019 RY) RMSE was 23.57, even lower than the test set. Results are presented by method in the summary table below. 

![Final Table](https://github.com/edenaxe/NYC-LL84-ML/blob/main/Images/Final%20RMSE%20Table.png)

![Final Plots](https://github.com/edenaxe/NYC-LL84-ML/blob/main/Images/Actual%20vs%20Predicted.png)
