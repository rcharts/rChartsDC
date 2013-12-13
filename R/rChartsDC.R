Dcjs <- setRefClass('Dcjs', contains = 'rCharts', methods = list(
  initialize = function(){
    callSuper()
    params <<- c(params, list(charts = list()))
  },
  getPayload = function(chartId){
    rows = setNames(makeDiv(params$layout, params$charts), NULL)
    list(chartParams = toJSON(params), chartId = chartId, 
         lib = basename(lib), liburl = LIB$url, rows = rows
    )
  },
  addChart = function(name, ...){
    params$charts[[name]] <<- list(name = name, ...)
  },
  layout = function(...){
    params$layout <<- list(...)
  }
))


makeDiv <- function(layout, charts){
  lapply(layout, function(row){
    chart_names = names(row)
    charts = lapply(chart_names, function(chart_name){
      charts = list(
        title = charts[[chart_name]]$title,
        class = row[[chart_name]],
        id = chart_name
      )
      return(charts)
    })
    list(charts = charts)
  })
}