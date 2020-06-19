<h2> What is NewsApp? </h2>
NewsApp is a tool for searching and extracting of news articles from online versions of newspapers based on certain keyword(s) and time period. The resulting .csv table will contain Links, Publication Dates, Headlines, Newspaper Name and (if available) full articles.
 
<h2> What online versions of newspapers are available? </h2>
Clicking on “Choose Source” displays the list of available newspapers. These include titles from the UK (BBC, Financial Times, Guardian, and the Independent), the US (New York Times, Washington Post, Wall Street Journal), and Hong Kong/China (Hong Kong Free Press, Headline Daily,  Xinhuanet).
 
<h2> Why do some newspapers require API keys to access and download the newspaper articles?</h2>
The API key is necessary because it provides a channel for the NewsApp to connect to the data provided by the online newspaper(s). To obtain the API key for the specific newspaper you will need to go to that newspaper’s registration page to first obtain a key. An API key is required for the following newspapers:

<b>New York Times</b>
https://developer.nytimes.com/docs/articlesearch-product/1/overview

<b>Guardian</b>
https://open-platform.theguardian.com/documentation/

Several other newspapers in NewsApp require and API key from NewsAPI. You can obtain the API key from the link below:

<b>News API</b>
https://newsapi.org/docs
<i>It should be noted that the free version of NewsAPI will only provide access to a maximum of 5 pages of articles published within the last 30 days with only the first 30-40 words of the article displayed. The paid version should provide full access to complete texts.</i>

 
<h2> How does the NewsApp work? </h2>

1. Select the news source and enter the search term.

2. On most newspapers you can also select a Start and End date to indicate the period in which to extract the relevant articles. Newspapers that have a "cut-off date" will collect all articles between the "cut-off" and the present day. 
3. If prompted, enter the required API key. 

4. Finally, click Build Dataset and the extracting will begin and the results will be displayed on screen. Once a table is displayed on the right of the page, you can download the file by clicking the "Download Data" Button. 

5. If you want to search for and download another dataset, click the reset button. 
 
<h2> Who created NewsApp and how can I find out more about it? </h2>
NewsApp was created by Dmitry Kuznetsov. You can find the source code of the app by following this link: https://github.com/Gulfon/R_Experimentation.

If you have any questions or suggestions, you can post issues to the APPs github page or can contact Dmitry at dmtkuznetsov[@]outlook.com
 
