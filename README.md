# Telemarketing Calls

### Scenario
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

## Output variable | Content
--- | ---
SUBSCRIBED | Has the client subscribed a term deposit? (binary: 'yes','no')

## Input variables

### Bank client data
1 - age (numeric)
2 - job : type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)
4 - education (categorical: basic.4y','basic.6y','basic.9y', 'high.school','illiterate','professional.course', 'university.degree','unknown')
5 - default: has credit in default? (categorical: 'no','yes','unknown')
6 - housing: has housing loan? (categorical: 'no','yes','unknown')
7 - loan: has personal loan? (categorical: 'no','yes','unknown')

### related with the last contact of the current campaign:
8 - contact: contact communication type (categorical: 'cellular','telephone') 
9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
11 - duration: last contact duration, in seconds (numeric).

### other attributes:
12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
14 - previous: number of contacts performed before this campaign and for this client (numeric)
15 - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')

### social and economic context attributes
16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
17 - cons.price.idx: consumer price index - monthly indicator (numeric) 
18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
20 - nr.employed: number of employees - quarterly indicator (numeric)

