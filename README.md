# Telemarketing Calls

### Concept
The data relates to telemarketing phone calls to sell long-term deposits. Within a campaign, the agents make phone calls to a list of clients to sell the product (outbound) or, if meanwhile the client calls the contact-center for any other reason, he is asked to subscribe the product (inbound). Thus, the contact is either unsuccessful or successful.
This study considers real data collected from one of the retail bank, from May 2008 to June 2010, in a total of 39883 phone contacts. More than one contact with the same client was frequently required in order to determine whether the product (bank term deposit) would be subscribed ('yes') or not ('no').

### Purpose
Investigate which factors contribute to a successful contact (i.e. the client subscribes to the product).


### Analysis Overview:
* Data preparation (handle missing values, group variables, normalization) 
* Bivariate Analysis (correlation coefficients, boxplots, contingency tables)
* Logistic Regression Model Selection:
  * Remove problematic values (coefficients with “NA” values, coefficients whose value and standard error is very “high”)
  * Hypothesis testing (Wald test, Goodness of fit test)
  * Variable Selection (Stepwise procedures, Multi-collinearity testing)
  * Model Interpretation
  * Assumptions of logistic regression
  * Conclusions and Discussions


# Retail Bank Data

### Output Variable
Variable | Content
--- | ---
SUBSCRIBED | Has the client subscribed a term deposit? (binary: 'yes', 'no')

### Input variables

:arrow_right: **Bank client data**
Variable | Content
--- | ---
age | age of client (numeric)
job | job type (categorical: 'admin.', 'blue-collar', 'entrepreneur', 'housemaid', 'management', 'retired','self-employed', 'services', 'student', 'technician', 'unemployed', 'unknown')
marital | marital status (categorical: 'divorced', 'married', 'single', 'unknown'; note: 'divorced' means divorced or widowed)
education | education level (categorical: basic.4y', 'basic.6y', 'basic.9y', 'high.school', 'illiterate', 'professional.course', 'university.degree', 'unknown')
default | has credit in default? (categorical: 'no', 'yes', 'unknown')
housing | has housing loan? (categorical: 'no', 'yes', 'unknown')
loan | has personal loan? (categorical: 'no', 'yes', 'unknown')


:arrow_right: **Related with the last contact of the current campaign**
Variable | Content
--- | ---
contact | contact communication type (categorical: 'cellular', 'telephone') 
month | last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
day_of_week | last contact day of the week (categorical: 'mon', 'tue', 'wed', 'thu', 'fri')
duration | last contact duration, in seconds (numeric)

:arrow_right: **Other attributes**
Variable | Content
--- | ---
campaign | number of contacts performed during this campaign and for this client (numeric, includes last contact)
pdays | number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
previous | number of contacts performed before this campaign and for this client (numeric)
poutcome | outcome of the previous marketing campaign (categorical: 'failure', 'nonexistent', 'success')

:arrow_right: **Social and economic context attributes**
Variable | Content
--- | ---
emp.var.rate | employment variation rate - quarterly indicator (numeric)
cons.price.idx | consumer price index - monthly indicator (numeric) 
cons.conf.idx | consumer confidence index - monthly indicator (numeric) 
euribor3m | euribor 3 month rate - daily indicator (numeric)
nr.employed | number of employees - quarterly indicator (numeric)
