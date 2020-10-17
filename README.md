# gd-scrape-eff
How to scrape company reviews from Glassdoor website efficiently using R script.

Glassdoor is a popular website which provides information based on employee's reviews on various companies across the world.

When we need to analyse individual reviews of a company, it will be overwhelming if we go through them one by one on the webpage.

So, Glassdoor provides their own APIs, using which we can extract the required information and then do our analysis offline.

However, in general these APIs are restricted and provide access only to general details.

Another alternative is that, we can build our own web scrapper to extract the required details from the webpages. There are popular python libraries like BeautifulSoup which do this job amazingly.

Here, we will see an R script, which extract the company reviews on Glassdoor very efficiently.

It tries to read all reviews of a company one after the other, and extracts only the required details or fields.

The R script is provided in a separate file.

