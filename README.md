Background
----------
In the starting period of a business, it must be decided upon which KPIs should be tracked to ensure sustainable success. Especially in the online retail area, the application of analytics, forecasting, and price optimization gains importance in order to compete with rapid marketplaces. The added value of these can be substantial especially with access to the right data.

Goal
----
Our goal is to apply the technical foundations, with special focus on machine learning, optimization and making interactive visualizations with a dashboard, while using Git for version control. The rationale behind it is to build an interactive front-end that also appears native to “non-coder” business end users. Additionally, those foundations will be combined with the methodological elements, e.g. customer segmentation, n-month-ahead forecasting, and revenue optimization. 
What makes this project unique in addition to adding optimization on top of machine learning is the access to price data: usually, modern data sets from online retailers that are only available for confidential use include more predictors, especially on marketing, sales campaigns, inventory levels etc. and the success rates of the latter. In this project, the goal is to fulfill the listed guidelines based on the predictor limitation but still creating accurate customer profiles, demand forecasts, and revenue optimization potentials. 

Data
----
Usually, business data with any price information is almost impossible to find. The UCI Machine Learning Repository offers a 2-year (timeseries) multivariate dataset that was already used in some research articles especially for data mining research purposes. The data is from an internationally exporting British online retailer of unique all-occasion giftware. To our knowledge, price optimization has not been done on this data set before, at least publicly. The structure of the dataset is as follows: 

- Invoice: Invoice level, indicating the basket of a customer
- StockCode: Unique product identifier, with 4,631 unique products
- Description: Product description in text form 
- Quantity: Sales quantity, with returns as negative values
- InvoiceDate: Datetime in the minute level, from 2009-12-01 10:06 to 2011-12-09 9:57
- Price: Unit price in pounds
- Customer ID: Unique ID for each customer, with 23% have a missing value
- Country: Country of the customer, 43 unique values

Total amount of rows is 1,067,371 before cleaning and aggregating.

Detailed Plan 
-------------
1.	Data Cleaning and Exploration
    1.	Data cleaning, aggregation to monthly level
    2.	Exploratory Data Analysis
    3.	Outlier analysis & selecting products to make forecasts on
    4.	Calculating of further KPIs (revenue, revenue per product/invoice/client/country)
  
2.	Customer Segmentation (RFM)
    1.	Classification based on Recency, Frequency, and Monetary (RFM)
    2.	B2B and B2C classification clusters
    3.	K-means on top of RFM to cluster the clients
  
3.	Demand Forecasting (ARIMA)
    1.	Choosing of a cross validation method
    2.	Derivation of prediction window and product choice
    3.	N-month-ahead forecasts (inventory and finance-oriented)
    4.	Accuracy tests (R-squared, RMSE, MAE, MAPE)
  
4.	Optimal Price Optimization to Maximize Revenue
    1.	Product and customer segment choice (based on ABC/XYZ analysis)
    2.	Mathematical baseline (sets, parameters, decision variables, constraints, objective function)
    3.	Optimization based on linear programming, Rglpk/lpsolve, and price elasticities from the ARIMA model coefficients
    
5.	Analytics Dashboards (shiny + shinydashboard)
    1.	ABC/XYZ analysis
    2.	Customer profiles and segmentation results
    3.	Order flow and demand forecast 

Tags
----
retailer, optimization, segmentation, forecasting, visualization, interactive, git, shiny, dashboard


