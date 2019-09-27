## Time Series forecasting model to predict next day electricity bid price of the Indian Energy Exchange (power trading exchange)

Model is created in R and uses R shiny for its minimilistic UI. 
ARIMA(AutoRegressive Integrated Moving Average) models are selected for this Bidding price prediction model.

The input data is last 6 days prices (96 data points for 1 day; 1 data point for 15 minute slot) and the predicted output will be next day slot prices (96 data points).

The algorithm internally builds 36 ARIMA models and chooses the optimal model (on the basis of Akaike information criterion) for the prediction task.

The total run time of this model is ~45 minutes.
