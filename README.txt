Following our discussion, here are all the data sets:
(1) Survey data (Responses_Processed): https://docs.google.com/spreadsheets/d/1DUsE2XXAj5SXbA4ASeWFs1TBCs_BWYFNiQ63WS9Vybo/edit#gid=0

(2) Courses data (Courses Processed): https://docs.google.com/spreadsheets/d/1rYIhYHl-Wtb1fMOLAeN8Im6jkXXGJNeZxFRI58FO1MA/edit#gid=161997847

(3) Attendees data (Attendees_Processed): https://docs.google.com/spreadsheets/d/1tCj_HwKKQVZ8slSylJHj_1H-ncSjcgzlWV3CH1jrUwM/edit#gid=0
- Not used; same as (2)

These three datasets are the Training Data
Training impact survey data (Responses) https://docs.google.com/spreadsheets/d/1TgPPXalzJFAhuxS73-0dd-RgE_M7rupkqsXqRJvumsE/edit#gid=450226309

Regarding joining the Training Data
Courses data (Unique_ID) - Survey data (Unique_ID)
Courses data (EventbriteID) - Attendees data (eventbrite_ID)

Questions to address

Analyses
1) Within the course evaluation survey
   Correlation with NPS:
   Month
   Weekday
   Time during day
   University
   Tool
   9 quantitative metrics
   
2) Does NPS correlate with:
   Disciplines x tools (or technology)
   Faculty x tools/tech
   Roles x tools/tech
  
3) Does NPS correlate with statements from the qualitative metrics (x5)
   Sentiment analysis on tools for this data - Charlotte has R scripts.

Further analyses
Turn into a ML project by reading into Python using linear regression
Training data - 80% total, or 2022
Test data     - 20%,       or 2021 - previous
Add to sentiment analysis, join phrases with quantitative measures (NPS/scores)

Visualising in dashboards
Difficulty is highest with Looker, then PwerBI then Tableau.


