**Live version available here:**  [https://karoronty.shinyapps.io/SalesIntelligence/](https://karoronty.shinyapps.io/SalesIntelligence/)

![ui](https://github.com/aleksejhoffaerber/SalesIntelligence/blob/master/ui_screenshot.png)

## Goal
The goal of “Sales Intelligence” is to apply product classification, forecasting, and revenue optimization for the next month ahead to maximize revenue. The end goal is to build interactive visualizations in a dashboard that appear natural to non-technical business end users.

What makes this project unique in addition to adding optimization is the access to price data: usually, modern data sets from online retailers that are only available for confidential use include more predictors, especially on marketing, sales campaigns, inventory levels etc. In this project, the goal is to fulfill the listed guidelines based on the predictor limitation but still creating accurate product profiles to identify revenue optimization potential.

## Usage
In the dashboard, the user selects a single product where the price needs to be optimized. The user can filter the products by a segment created by the product level RFM analysis if desired. Initially, there are two visible tabs for the user to analyze the entire set of products with. The main tab, RFM analysis, contains a heat map based on the products available for price optimization. The second tab, Segments, contains the monetary values of the different segments the user can filter the products on. After selecting a product and running the optimization, results of the optimization are presented. The top bar contains the optimal price, the forecasted revenue with that price and the revenue increasement. The plots show the effect on sales in three different ways, the effect on revenue over time, the effect on quantity over time and the resulting revenues from different prices. The blue values indicate the sales without optimization, while the white dot indicates the optimized sales with the optimal price. It is important to keep in mind that the forecasted sales with the optimized prices are only indicative and may not reflect the actual future sales in the most accurate way.

## Data
The UCI Machine Learning Repository offers [a 2-year (time series) multivariate dataset](https://archive.ics.uci.edu/ml/datasets/Online+Retail+II) that was already used in some research articles especially for data mining research purposes. The data is from an internationally exporting British online retailer of unique all-occasion giftware. To our knowledge, revenue optimization has not been done on this data set before, at least publicly. The structure of the dataset is as follows:

- **Invoice**: Invoice level, indicating the basket of a customer
- **StockCode**: Unique product identifier, with 4,631 unique products
- **Description**: Product description in text form 
- **Quantity**: Sales quantity, with returns as negative values
- **InvoiceDate**: Datetime in the minute level, from 2009-12-01 10:06 to 2011-12-09 9:57
- **Price**: Unit price in pounds
- **Customer ID**: Unique ID for each customer, with 23% have a missing value
- **Country**: Country of the customer, 43 unique values

## Code Architecture 
In order to facilitate deployment, code readability, and further maintenance the code base is segmented into four different scripts. The data flows and code interdependencies between the scripts can be depicted as follows:

### 1. EDA.ipynb
Containst the exploratory data analysis that acts as the basis for several decisions made in dashboard. 

### 2. modeling.R
modeling.R includes the data loading, cleansing, and modeling of the application. Most of the basic data cleansing refers to product name harmonization, deletion of invalid products and invoice positions. Orders with a negative price or quantity or products with less than 24 months of order history were deleted as well. Additionally, empirical fluctuation process analysis was applied in order to delete products with significant breaks in their data. This is essential for the demand forecasts based on ARIMA.

Then, a product classification is implemented based on the Recency, Frequency, Monetary (RFM) analysis. The resulting product level RFM segments were calculated using sensible values.

Lastly and because of 701 resulting products, model training is carried out using parallelization. The ARIMA models use the monthly historic mean of the price in order to predict quantity one month ahead, with the parameters for the models being chosen automatically using AICc.

The resulting data, segments, and models are saved as R objects for usage in app.R. This script is not run inside the Shiny app to save time but should instead be run periodically to obtain latest models with the latest data. In this application the exchange of data happens via .RDS files, unlike in the optimal scenario where the data would be stored in databases and read from there by the Shiny app.

### 3. functions.R
functions.R is the functional backbone of the application. It includes functions for demand forecasts, revenue optimization, necessary plots, and data translations to allow for an easy understandable user interface. Unlike in a typical R program, all the functions also include the data and variables needed inside the functions for the deployment as required by Shiny.

##### translate_input()
Translates the product name used in the input to a product id.

##### get_forecasts()
Creates product prices, based on the app input, that are allowed to vary by ±30% percent, using £0.01 steps from the original price. Based on the ARIMA demand prediction model, new demand quantities will be predicted for the different, varying prices. Original and predicted prices will be joined into the resulting tibble.

##### get_optimal_forecast()
Identifies the best price point based on the best revenue result by performing a simple sorting on the forecasted revenue and saves it into a tibble.

##### plot_revenue_forecasts()
Takes the results from get_optimal_forecast() and plots the revenue time-series for the given product. Also compares the forecasted revenue without an optimized price (based on the ARIMA quantity forecast and the historical mean price (t+1)), with the optimized revenue (based on the forecast and optimization).

##### plot_quantity_forecasts() 
Similar to plot_revenue_forecasts(), but just for the quantity. It compares forecasted quantity without price optimization with quantity based on the optimization.

##### plot_revenue_price()
Compares the price and revenue between the forecasted and optimized scenario. The plot is computed based on the complete product price variation from the resulting tibble of get_forecasts(). 

##### create_segments()
Computes the RFM segments

### 4. app.R
app.R is the application backbone and includes the necessary libraries, connects to modeling.R and functions.R, imports the aforementioned data and models, includes the product segmentation plots and the UI and server functions for Shiny. 

##### ui()
The shiny front-end is based on a simple dashboard page. This page consists of:
- Two dynamic input fields for product segment (RFM) and product name
- A button to initiate the optimization for a single selected product
- A menu consisting of RFM, Segments, and Results for the optimization results
- For RFM and Segments the respective classification graphs are shown in the corresponding menu tabs
- The graphs for revenue, quantity and revenue-price comparisons using a 2x1 (patchwork) grid that are shown after optimization computation 
- InfoBoxes summarizing relevant information for optimized price, and absolute and relative revenue increase 
- A notification to notify the user of the ongoing optimization process

##### server()
The back end mainly takes care of the dynamic event triggers, plot renders, info-box messages and notifications:
- The initial menu bar only includes RFM and Segments. The final item “Results” only appears after the final optimization has finished computing
- Input that can be filtered in the product name depends on the selected input from the previous RFM segment filter
- InfoBox outputs are calculated based on the data from get_forecasts()
- Demand and optimization related plots are rendered as a single plot using a patchwork grid 
- The RFM and segment plots are static and therefore rendered only when the app is launched, and updated only when the underlying data is updated, and the models are run again

## Tags
r, retailer, optimization, segmentation, forecasting, ARIMA, time series, visualization, interactive, git, shiny, dashboard
