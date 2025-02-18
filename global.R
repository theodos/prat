library(shinythemes)
library(vroom)
options(shiny.maxRequestSize = 700 * 1024^2)
library(excelR)
library(shinyWidgets)
#library(bslib)
library(shinyBS)
library(DT)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(pivottabler)
library(ggplot2)
library(ggrepel)
library(colourpicker)
library(periscope)
library(shinybusy)

themeelector <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
                  c("default", shinythemes:::allThemes()),
                  selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'flatly';
        curThemePath = 'shinythemes/css/flatly.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}
