# Data Analysis and Parametric Hypothesis
This project is a report of insights from a dataset of a textile company
<br />
The final report can be visualized on https://diegodlima.github.io/Parametric_Hypothesis_with_R/
<h3>Files Description</h3>
- <b>.csv file:</b> contain the datasets<br />
- <b>failures_analysis.R:</b> contain the script of the project<br />
- <b>failures_analysis.Rmd:</b> contain the markdown script<br />
- <b>index.html:</b> this file is the compiled report
<hr />
<h2>Description</h2>
The dataset contains registers of production and failures of a textile company.<br />
The company has introduced changes in it's process trying to reduce the frequency of failures, and desires a detailed report about how these changes have reproduced results.<br />
It's demanded to collect as much insights as possible in a trying bring out potential tendences, strategical variables, and evidences of the possible changes with it's implementation.
<br />
<h2>Sheets</h2>
The dataset contains three sheets:<br />
1 - <b>FAILURES:</b> register of failures<br />
2 - <b>ORDERS:</b> relation of orders and products<br />
3 - <b>PRODUCTION:</b> register of production<br />

<b>Additional information: </b> the sheets FAILURES and PRODUCTION can be used together to describre and analyse the behavior of the process, regarding to quantities of production and failures.
The only usage of the sheet ORDERS is in case of using the sheet FAILURES alone. So this first can be joined to select the product by ORDER.

<h2>Data fields</h2>
Sheet FAILURE
<br />
<b>ORDER:</b> number of the order of production<br />
<b>ID:</b> identification of the product<br />
<b>LENGHT:</b> main measurement of the piece of the product<br />
<b>MACHINE:</b> equipment where the product was made<br />
<b>DATE:</b> date of production<br />
<b>1 to 18:</b> the numbers correspond to a code for specific failures, and these variables regard to the meters that the failures occurr<br />
<b>19_NOTES:</b> description of new type of failures<br />
<b>19_LENGHT:</b> meters that the failures occurr<br />
<b>1_ to 19_:</b> number of occurrences of the failure<br />
<b>TOTAL:</b> total of failures per register<br />

Sheet PRODUCTION
<br />
<b>ORDER, ID, MACHINE and DATE:</b> the same mean of FAILURES sheet<br />
<b>PRODUCT:</b> type of product<br />
<b>LENGTH:</b> the size of the product before the splitting<br />
<b>VERSION:</b> type of proccess (0 for BEFORE the changes, 1 for AFTER the changes)<br />
