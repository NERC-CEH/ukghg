#' busyIndicator
#' 
#' An indicator to show that calculation is in progress
#' 
#' @param text The text to show
#' @param img An animated gif
#' @param wait The amount of time to wait before showing the busy indicator. Default is 1000 ms.
#' @keywords internal progress
#' @export
busyIndicator <- function(text = "Calculation in progress..",img = "busyIndicator/ajaxloaderq.gif", wait=1000) {
	tagList(
  		singleton(tags$head(
    		tags$link(rel="stylesheet", type="text/css",href="busyIndicator/busyIndicator.css")
  			))
  		,div(class="shinysky-busy-indicator",p(text),img(src=img))
  		,tags$script(sprintf(
  		"	setInterval(function(){
  		 	 if ($('html').hasClass('shiny-busy')) {
  		    setTimeout(function() {
  		      if ($('html').hasClass('shiny-busy')) {
  		        $('div.shinysky-busy-indicator').show()
  		      }
  		    }, %d)  		    
  		  } else {
  		    $('div.shinysky-busy-indicator').hide()
  		  }
  		},100)
  		",wait)
  			)
	)	
}
