
library(DiagrammeR)

DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = TB, label = '   Spring Waterfowl Survey\n\n', labelloc = t, fontname = arial] # LR
node [shape = rectangle, style = filled, fillcolor = Grey, fontname = arial]

survey_data [label = 'Current year survey data', shape = rectangle, fillcolor = Grey]
sql [label = 'Update SQLite database', shape = rectangle, fillcolor = Grey]
summary [label =  'Summarize/analyze data']
annual_outputs [label = 'Annual model outputs']
dashboard [label= 'Data dashboard']
rmarkdown [label= 'RMarkdown reports']

survey_data -> sql -> summary
summary -> annual_outputs
summary -> dashboard
summary -> rmarkdown

}")
