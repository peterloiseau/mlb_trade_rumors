# mlb_trade_rumors
This repository has code and visualization from my exploration of the MLB media ecosystem and the rumors which sustain it. Note the code scripts require the installation of many 
packages. MLB trade rumors script specifically uses Selenium to press the button to extend the page for each team. This requires the installation of a webdriver, in my case
a gecko driver for Firefox. Unfortunately, the pagination is unreliable, so I ran the script multiple times and joined the output data sets untill I was able to reach a 
total of 2000 unique posts for each team.
Article can be read at the Fangraphs Community Research Blog: https://community.fangraphs.com/quantifying-rumor-mongering-in-the-baseball-media-ecosystem/
