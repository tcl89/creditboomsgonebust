This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor). 

In the paper “Credit booms gone bust: Monetary Policy, Leverage Cycles, and Financial Crises, 1870
– 2008” by M. Schularick and A. M. Taylor, the authors take the financial crisis of 2007 – 2009 as
a cause to investigate the role of money and credit fluctuations and in particular credit booms in
generating financial instabilities and crashs.
In this problem set you will be guided through Schularick and Taylor’s main findings. Besides, you
will learn how to deal with advanced panel data in R.

The paper can be found under the link https://www.aeaweb.org/articles.php?doi=10.1257/aer.102.2.1029

## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
if (!require(devtools))
  install.packages("devtools")
source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)
  
install_github("tcl89/creditboomsgonebust")
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(creditboomsgonebust)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/creditboomsgonebust")
# Adapt your user name
run.ps(user.name="Jon Doe", package="creditboomsgonebust",
       load.sav=TRUE, sample.solution=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
