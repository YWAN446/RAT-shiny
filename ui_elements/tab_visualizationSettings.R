tab_visualizationSettings <- tabPanel('Visualization Settings',
                                      helpText("General settings for how the data are displayed."),
                                      h3('Pie Chart Size'),
                                      sliderInput('pw',"Pie Width:", min=200, max=500, value=350, step=25),
                                      sliderInput('ph',"Pie Height:", min=200, max=500, value=400, step=25),
                                      h3("Layout Settings"),
                                      sliderInput('num_columns', 'Number of Columns', 1, 6, value=2),
                                      helpText("Sets the number of columns displayed for each set of visualizations on a tab.  As this increases,
                                    the number of rows on the page will shrink, but the plots will shrink as well. ")
                                      
)