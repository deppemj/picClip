#' Paste Box Input
#'
#' Create a paste box input control for images.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control.
#' @param width The width of the paste box, e.g., '100px'.
#' @param height The height of the paste box, e.g., '100px'.
#'
#' @return A Shiny tag list that creates a UI element for pasting images.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(base64enc)
#'
#'   ui <- fluidPage(
#'     pasteBoxInput("testInput", "Paste Image Here", "300px", "150px"),
#'     uiOutput("image")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$testInput, {
#'       if (!is.null(input$testInput) && input$testInput != "") {
#'         output$image <- renderUI({
#'           tags$img(src = input$testInput, style = "max-width: 100%; height: auto;")
#'         })
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export

pasteBoxInput <- function(inputId, label, width = "100px", height = "100px") {
  # Validate the input parameters
  shiny::validateCssUnit(width)
  shiny::validateCssUnit(height)

  # Create the HTML for the paste box
  pasteBoxHtml <- shiny::tags$div(
    id = inputId,
    class = "paste-box",
    style = sprintf("width: %s; height: %s; line-height: %s;", width, height, height), # Set the width, height, and line height
    label # Label text inside the box
  )

  # Create the CSS for the paste box
  pasteBoxCss <- HTML(sprintf("
    #%s {
      border: 2px dashed #000000;
      padding: 10px;
      text-align: center;
      color: #BBBBBB;
      font-size: 16px;
      overflow: hidden;
      position: relative;
      background-color: #f0f0f0;
      box-shadow: 5px 5px 15px rgba(0, 0, 0, 0.4);
      border-radius: 5px;
      transition: transform 0.2s, box-shadow 0.2s;
    }
    #%s:hover {
      transform: translateY(-5px);
      box-shadow: 10px 10px 20px rgba(0, 0, 0, 0.5);
    }", inputId, inputId))

  # Create the JavaScript
  pasteBoxJs <- HTML(sprintf("
  document.addEventListener('DOMContentLoaded', function() {
    var pasteBox = document.getElementById('%s');
    pasteBox.addEventListener('click', function() {
      this.focus();
    });
    pasteBox.addEventListener('paste', function(e) {
      var items = (e.clipboardData || e.originalEvent.clipboardData).items;
      for (var index in items) {
        var item = items[index];
        if (item.kind === 'file') {
          var blob = item.getAsFile();
          var reader = new FileReader();
          reader.onload = function(event) {
            Shiny.setInputValue('%s', event.target.result);
          };
          reader.readAsDataURL(blob);
        }
      }
      e.preventDefault();
    });
  });", inputId, inputId))


  # Combine and return the HTML, CSS, and JavaScript
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(pasteBoxCss),
      shiny::tags$script(pasteBoxJs)
    ),
    pasteBoxHtml
  )
}
