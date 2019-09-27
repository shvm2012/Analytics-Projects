## Time Series forecasting model to predict next day electricity bid price of the Indian Energy Exchange (power trading exchange)

Model is created in R and uses R shiny for its minimilistic UI. 
ARIMA (AutoRegressive Integrated Moving Average) models are selected for this bid price prediction model.

The input data is last 6 days prices (96 data points for 1 day; 1 data point for 15 minute slot) and the predicted output will be next day slot prices (96 data points).

The algorithm internally builds 36 ARIMA models and chooses the optimal model (on the basis of Akaike information criterion) for the prediction task.

The total run time of this model is ~45 minutes.
The model out put looks like this : (image)

![alt text](https://github.com/shvm2012/IEX-bid-price-prediction-model/blob/master/Capture.JPG)

#### Guidelines for running the model:
* open the R shiny application in browser mode
* select the sample data (sample_data_electricity_price.xlsx) and hit the "Run Model" button. The model starts to run in the backend R program. 
* once the model execution is completed, the results can be downloaded in the csv format.
