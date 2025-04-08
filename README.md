# 133_final_shiny

# Data Acquisition and Wrangling
Refer to prepare_data.R. We use three utility R script to complete the whole progress. They are

GPT_classifier.R : Use Deepseek to analyze the advisory content and generate a class label for each advisory

Extract_data_from_web.R: Generate formatted URL

utility_tables.R: Get information for FAA centers and airportts.

# Data Processing
Refer to process_data.R. All summary tables are saved to RDS file, and will be used in app.R

# Shiny Source Code
Refer to app.R.

# Shiny App
The shiny app can be viewed by running app.R in Posit Cloud. I also publish it to Posit Connect Cloud: https://0196175a-8665-fbf3-3dae-ba890ff73576.share.connect.posit.cloud/.

## Instruction
This Shiny App includes five panels. The first four display graphs and figures summarizing advisory statistics over time. The final panel features an interactive output powered by DeepSeek, which helps make advisory texts more accessible. Since the raw advisory content can be difficult for non-experts to understand, DeepSeek is used to "translate" it into plain, readable language.

# Github Repo
https://github.com/yniuJASON/133_final_shiny

----------

Deepseek API Key: sk-4c49f4e440724c41906d8d1ebe02c1ed