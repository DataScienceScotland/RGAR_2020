# Globally format thousands separator
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


# 2 COVID-19 --------------------------------------------------------------

# Location deaths ---------------------------------------------------------

plots[["COVID_location_deaths"]] <-
  hchart(
    datasets[["COVID_location_deaths"]],
    "line",
    hcaes(x = date,
          y = deaths,
          group = location),
    color = c(col_neut_silver,
              col_neut_grey,
              col_neut_tundora,
              col_nrs_purple),
    dashStyle = c("Dash",
                  "Solid",
                  "Dash",
                  "Solid")
  ) %>%
  hc_plotOptions(line = list(
    dataLabels = list(enabled = F)
  )) %>%
  hc_yAxis(
    min = 0,
    max = 500,
    labels = list(format = "{value:,.0f}"),
    title = list(text = "COVID-19 deaths")
  ) %>%
  hc_xAxis(
    tickPositions = c(datetime_to_timestamp(min(datasets[["COVID_location_deaths"]]$date)),
                      datetime_to_timestamp(as.Date("2020/07/01")),
                      datetime_to_timestamp(as.Date("2020/11/01")),
                      datetime_to_timestamp(as.Date("2021/03/01")),
                      datetime_to_timestamp(max(datasets[["COVID_location_deaths"]]$date))),
    type = "datetime",
    labels = list(
      allowOverlap = F,
      style = list(textOverflow = 'none'),
      formatter = JS(
        "function() {
          return Highcharts.dateFormat('%b %Y', this.value);}"
      )
    ),
     title = F,
    plotLines = list(
      list(
        label = list(text = "1st Lockdown",
                     rotation = 0,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px"
                     )),
        color = col_neut_silver,
        width = 1,
        value = datetime_to_timestamp(as.Date("2020-03-23")),
        dashStyle = "shortdash"
      ),
      list(
        label = list(text = "2nd Lockdown",
                     rotation = 0,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px"
                     )),
        color = col_neut_silver,
        width = 1,
        value = datetime_to_timestamp(as.Date("2021-01-04")),
        dashStyle = "shortdash"
      ),
      list(
        label = list(text = "Vaccinations begin",
                     rotation = 0,
                     align = "right",
                     x = -5,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px"
                     )),
        color = col_neut_silver,
        width = 1,
        value = datetime_to_timestamp(as.Date("2020-12-08")),
        dashStyle = "shortdash"
      )
    )
    ) %>%
  hc_add_theme(hc_theme) %>%
  hc_tooltip(
    formatter = JS(
      "function () { return '<b>'
      + this.series.name
      + '</b><br /> '
      + 'Deaths: '
      + Highcharts.numberFormat(this.point.y, -1)
      + ' <br />'
      + Highcharts.dateFormat('%e %b %Y',
      new Date(this.x));}"
    )
  ) %>%
  hc_legend(reversed = T,
            itemStyle = list(fontSize = "18px",
                             fontStyle = "normal",
                             fontWeight = "light")) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "covid_19_deaths_by_location",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS'))))

plots[["COVID_location_deaths"]]



# Excess deaths by age ----------------------------------------------------

plots[["COVID_deaths_age"]] <-
  map(unique(datasets[["COVID_deaths_age"]]$age),
      function(x) {
        data <- datasets[["COVID_deaths_age"]] %>%
          filter(age == x)
        
        hchart(data,
               "arearange",
               hcaes(x = date,
                     low = min,
                     high = max),
               name = "5 year average range",
               color = col_neut_silver) %>%
          hc_add_series(
            data,
            "line",
            hcaes(x = date,
                  y = all_deaths),
            name = x,
            color = col_nrs_purple,
            showInLegend = F,
            lineWidth = 3
          ) %>%
          hc_plotOptions(line = list(
            marker = list(enabled = F),
            dataLabels = list(enabled = F)
          )
          ) %>%
          hc_yAxis(
            tickPositions = c(0, 200, 400, 600, 800),
            labels = list(format = "{value:,.0f}"),
            title = list(text = "Deaths")
          ) %>%
          hc_chart(marginLeft = 70) %>% 
          hc_title(x = 20) %>% 
          hc_xAxis(
            tickPositions = c(datetime_to_timestamp(min(datasets[["COVID_deaths_age"]]$date)),
                              datetime_to_timestamp(as.Date("2020-11-23")),
                              datetime_to_timestamp(max(datasets[["COVID_deaths_age"]]$date))),
            type = "datetime",
            title = list(enabled = F),
            labels = list(
              allowOverlap = F,
              style = list(textOverflow = 'none'),
              formatter = JS(
                "function() {
          return Highcharts.dateFormat('%b <br /> %Y', this.value);}"
              )
            )
          ) %>%
          hc_title(text = x) %>%
          hc_add_theme(hc_theme) %>%
          hc_tooltip(
            formatter = JS(
              "function () { return '<b>'
                            + this.series.name
                            + '</b><br /> '
                            + 'Deaths: '
                            + Highcharts.numberFormat(this.point.y, -1)
                            + ' <br />'
                            + Highcharts.dateFormat('%e %b %Y',
                                          new Date(this.x));}"
            )
          ) 
          
      })

#Remove y axis from all but the first chart
plots[["COVID_deaths_age"]][c(2, 3)] <-
  map(plots[["COVID_deaths_age"]][c(2, 3)], function(x) {
    hc_yAxis(x, labels = list(enabled = FALSE),
             title = list(enabled = F))
  })

# plots[["COVID_deaths_age"]][c(1)] <-
#   map(plots[["COVID_deaths_age"]][c(1)], function(x) {
#     hc_chart(x, marginLeft = 50)
#   })

# Center the 1st chart title (shifted due to y axis)
# plots[["COVID_deaths_age"]][c(1:6)] <-
#   map(plots[["COVID_deaths_age"]][c(1:6)], function(x) {
#     hc_title(x, x = 25)
#   })


plots[["COVID_deaths_age"]] <- 
  hw_grid(plots[["COVID_deaths_age"]], 
            rowheight = 500, ncol = 3) %>%
  htmltools::browsable() 
plots[["COVID_deaths_age"]]


# Excess deaths by type (Dementia, Respiratory, Circulatory, Cancers) ----------------------------------------------------

plots[["COVID_cause_of_death_1"]] <-
  map(unique(datasets[["COVID_cause_of_death_1"]]$type),
      function(x) {
        data <- datasets[["COVID_cause_of_death_1"]] %>%
          filter(type == x)
        
        hchart(data,
               "arearange",
               hcaes(x = date,
                     low = min,
                     high = max),
               name = "5 year average range",
               color = col_neut_silver) %>%
          hc_add_series(
            data,
            "line",
            hcaes(x = date,
                  y = average),
            name = x,
            dashStyle = "Dash",
            color = col_neut_grey,
            showInLegend = F
          ) %>%
          hc_add_series(
            data,
            "line",
            hcaes(x = date,
                  y = deaths),
            name = x,
            color = col_nrs_purple,
            showInLegend = F
          ) %>%
          hc_yAxis(
            tickPositions = c(0, 500, 1000, 1500, 2000),
            labels = list(format = "{value:,.0f}",
                          reserveSpace = F,
                          x = -5),
            title = list(text = "Deaths",
                         x = -37.5)
          ) %>%
          hc_chart(marginLeft = 65) %>% 
          hc_title(x = 30) %>% 
          hc_xAxis(
            tickPositions = c(datetime_to_timestamp(min(datasets[["COVID_cause_of_death_1"]]$date)), 
                              datetime_to_timestamp(max(datasets[["COVID_cause_of_death_1"]]$date))),
            type = "datetime",
            title = list(enabled = F),
            labels = list(
              allowOverlap = F,
              style = list(textOverflow = 'none'),
              formatter = JS(
                "function() {
          return Highcharts.dateFormat('%b <br> %Y', this.value);}"
              )
            )
          ) %>%
          hc_title(text = x) %>%
          hc_add_theme(hc_theme) 
          
      })

#Remove y axis from all but the first chart
plots[["COVID_cause_of_death_1"]][c(2, 3, 4)] <-
  map(plots[["COVID_cause_of_death_1"]][c(2, 3, 4)], function(x) {
    hc_yAxis(x, labels = list(enabled = FALSE),
             title = F)
  })

plots[["COVID_cause_of_death_1"]] <- plots[["COVID_cause_of_death_1"]] %>%
  hw_grid(rowheight = 500, ncol = 4) %>%
  htmltools::browsable() 
plots[["COVID_cause_of_death_1"]]

# Excess deaths by type (Suicide, Alcohol, Drugs) ----------------------------------------------------

plots[["COVID_cause_of_death_2"]] <-
  map(unique(datasets[["COVID_cause_of_death_2"]]$type),
      function(x) {
        data <- datasets[["COVID_cause_of_death_2"]] %>%
          filter(type == x)
        
        hchart(data,
               "arearange",
               hcaes(x = date,
                     low = min,
                     high = max),
               name = "5 year average range",
               color = col_neut_silver) %>%
          hc_add_series(
            data,
            "line",
            hcaes(x = date,
                  y = average),
            name = x,
            dashStyle = "Dash",
            color = col_neut_grey,
            showInLegend = F
          ) %>%
          hc_add_series(
            data,
            "line",
            hcaes(x = date,
                  y = deaths),
            name = x,
            color = col_nrs_purple,
            showInLegend = F
          ) %>%
          hc_yAxis(
            tickPositions = c(0, 50, 100, 150, 200),
            labels = list(format = "{value:,.0f}",
                          reserveSpace = F,
                          x = -5),
            title = list(text = "Deaths",
                         x = -30)
          ) %>%
          hc_chart(marginLeft = 60) %>% 
          hc_title(x = 30) %>% 
          hc_xAxis(
            tickPositions = c(datetime_to_timestamp(min(datasets[["COVID_cause_of_death_2"]]$date)), 
                              datetime_to_timestamp(max(datasets[["COVID_cause_of_death_2"]]$date))),
            type = "datetime",
            title = list(enabled = F),
            labels = list(
              allowOverlap = F,
              style = list(textOverflow = 'none'),
              formatter = JS(
                "function() {
          return Highcharts.dateFormat('%b <br> %Y', this.value);}"
              )
            )
          ) %>%
          hc_title(text = x) %>%
          hc_add_theme(hc_theme) 
        
      })

#Remove y axis from all but the first chart
plots[["COVID_cause_of_death_2"]][c(2, 3)] <-
  map(plots[["COVID_cause_of_death_2"]][c(2, 3)], function(x) {
    hc_yAxis(x, labels = list(enabled = FALSE),
             title = F)
  })


plots[["COVID_cause_of_death_2"]] <- plots[["COVID_cause_of_death_2"]] %>%
  hw_grid(rowheight = 500, ncol = 4) %>%
  htmltools::browsable() 
plots[["COVID_cause_of_death_2"]]


# 3 MIGRATION ------------------------------------------------------------------

# Net migration Natural change --------------------------------------------

hide_lines <- datasets[["MIGRATION_migration"]] %>% 
  filter(year %in% c(1960:1987),
         type == "Natural change")
hide_lines_2 <- datasets[["MIGRATION_migration"]] %>% 
  filter(year %in% c(2003:2017),
         type == "Net migration")

plots[["MIGRATION_migration"]] <- hchart(
  datasets[["MIGRATION_migration"]],
  "line",
  hcaes(x = year,
        y = total * 1000,
        group = type),
  color = c(col_neut_silver, col_nrs_purple),
  showInLegend = F,
  zIndex = 7
) %>%
  hc_yAxis(
    min = -60000,
    max = 60000,
    tickPositions = c(-60000, -40000, -20000, 0, 20000, 40000, 60000),
    title = list(text = "People",
                 reserveSpace = F,
                 x = 10),
    labels = list(format = "{value:,.0f}"),
    plotLines = list(
      list(
        color = col_neut_tundora,
        width = 1,
        value = 0,
        dashStyle = "dash",
        zIndex = 6
      )
    )
  ) %>%
  hc_xAxis(
    title = list(enabled = F),
    tickPositions = c(1961, 1975, 2004, 2016),
    plotLines = list(
      list(
        label = list(text = "Contraceptive <br/>pill
                             first available<br/>
                             on the NHS",
                     rotation = 0,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px")),
        color = col_neut_silver,
        width = 1,
        value = 1961,
        dashStyle = "dash"
      ),
      list(
        label = list(
          text = "Equal Pay Act <br/>and Sex<br/>
                  Discrimination <br/>Act",
          rotation = 0,
          xIndex = 6,
          style = list(
            fontFamily = windowsFont("Segoe UI"),
            fontSize = "18px")
        ),
        color = col_neut_silver,
        width = 1,
        value = 1975,
        dashStyle = "dash",
        zIndex = 3
      ),
      list(
        label = list(text = "EU <br/>expansion",
                     rotation = 0,
                     align = "right",
                     x = -5,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px")),
        color = col_neut_silver,
        width = 1,
        value = 2004,
        dashStyle = "dash"
      ),
      # list(
      #   label = list(text = "UK joined the EU",
      #                rotation = 0,
      #                align = "right",
      #                x = -5,
      #                style = list(
      #                  fontFamily = windowsFont("Segoe UI"),
      #                  fontSize = "18px")),
      #   color = col_neut_silver,
      #   width = 1,
      #   value = 1973,
      #   dashStyle = "dash"
      # ),
      list(
        label = list(text = "EU <br/>referendum",
                     rotation = 0,
                     align = "right",
                     x = -5,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px")),
        color = col_neut_silver,
        width = 1,
        value = 2016,
        dashStyle = "dash"
      )
    )
  ) %>%
hc_add_annotation(
  draggable = '',
  labelOptions = list(
    borderColor = "transparent",
    shape = 'connector',
    align = 'right',
    justify = F,
    style = list(
      fontFamily = windowsFont("Segoe UI"),
      fontSize = "18px"
    )
  ),
  labels = list(
list(
  point = list(
    xAxis = 0,
    yAxis = 0,
    x = min(datasets[["MIGRATION_migration"]]$year),
    y = 18912
  ),
  text = "<b>Natural change</b><br>
   (births minus <br>deaths)",
  y = 60,
  style = list(
    color = col_neut_grey
  )
),
list(
  point = list(
    xAxis = 0,
    yAxis = 0,
    x = min(datasets[["MIGRATION_migration"]]$year),
    y = -24000
  ),
  text = "<b>Net migration</b><br>
  (migration in minus <br> migration out)",
  style = list(
    color = col_nrs_purple
  )
))
) %>% 
  hc_add_series(hide_lines,
                "arearange",
                hcaes(
                  x = year,
                  low = -60000,
                  high = total*1000),
                zIndex = 4,
                color = "white") %>%
  hc_add_series(hide_lines_2,
                "arearange",
                hcaes(
                  x = year,
                  low = -60000,
                  high = total*1000),
                zIndex = 2,
                color = "white") %>%
  hc_plotOptions(arearange = list(
    fillOpacity = 3,
    animation = F,
    enableMouseTracking = F
  )) %>% 
  hc_add_theme(hc_theme) %>% 
  hc_legend(enabled = F) %>%
  hc_add_annotation(
    draggable = '',
    type = "verticalLine",
    labelOptions = list(
      style = list(
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "18px"
      )
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1952,
          y = 28912
        ),
        text = "test",
        y = 30,
        style = list(
          color = col_neut_silver
        )
      ))
    ) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "natural_change_and_net_migration",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )
  
plots[["MIGRATION_migration"]]
  

# Non-British nationals ---------------------------------------------------

regions <- datasets[["MIGRATION_non_british"]] %>% 
  group_by(region) %>% 
  summarise(population = sum(population)) %>% 
  arrange(desc(region))

plots[["MIGRATION_non_british"]] <- hchart(regions,
  "pie",
  hcaes(
    name = region,
    y = population,
    color = c(col_neut_silver,
              col_nrs_purple)
  ),
  size = 1,
  showInLegend = T,
  dataLabels = list(enabled = F),
  allowPointSelect = F,

  point = list(
    events = list(
      legendItemClick = JS("function(e){
        e.preventDefault();
      }"))),
  tooltip = list(shadow = F),
  states = list(
    hover = list(
      enabled = F
    )
  )) %>%
  hc_add_series(
    datasets[["MIGRATION_non_british"]],
                "pie",
                innerSize = '50%',
                hcaes(
                  name = nationality,
                  y = population, 
                  color = c(col_nrs_purple,
                            col_nrs_purple,
                            col_nrs_purple,
                            col_nrs_purple,
                            col_neut_silver,
                            col_neut_silver,
                            col_neut_silver)
                  )) %>% 
  hc_add_theme(hc_theme) %>% 
  hc_plotOptions(pie = list(
    dataLabels = list(
     # distance = -50,
      style = list(
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "18px",
        fontWeight = "light"
      )))) %>%
  hc_tooltip(
        formatter = JS(
          "function () { return '<b>'
          + this.point.name
          + '</b>' 
          + '<br /> '
          + 'Population: ' 
          + Highcharts.numberFormat(this.point.y, -1);}"
        )
      ) %>%
  hc_legend(
    enableMouseTracking = F,
    itemStyle = list(fontSize = "18px",
                     fontStyle = "normal",
                     fontWeight = "light")) %>% 
  hc_size(height=500,width=500) %>%
  hc_exporting(
    enabled = TRUE,
    filename = "non_british_nationals",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )


plots[["MIGRATION_non_british"]]



# Population projections --------------------------------------------------


plots[["MIGRATION_projections"]] <- hchart(datasets[["MIGRATION_projections"]],
                                     "line",
                                     hcaes(
                                       x = year,
                                       y = population,
                                       group = type
                                     ),
                                     color = c(col_neut_tundora,
                                               col_neut_grey,
                                               col_nrs_purple
                                               ),
                                     dashStyle = c("Dash",
                                                   "Dash",
                                                   "solid"),
                                     marker = list(enabled = F)) %>%
  hc_plotOptions(
    line = list(
      dataLabels = list(
        enabled = F))) %>% 
  hc_legend(enabled = F) %>% 
  hc_yAxis(tickPositions = c(5300000, 5400000, 5500000, 5600000),
           gridLineWidth = 0,
           labels = list(format = "{value:,.0f}"),
           title = list(text = "Population")) %>%
  hc_xAxis(tickLength = 0,
           lineColor = "transparent",
           title = list(enabled = F),
           tickPositions = c(2019, 2030, 2043)) %>%
  hc_add_theme(hc_theme) %>%
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      borderColor = "transparent",
      shape = 'connector',
      align = 'right',
      justify = 'right',
      style = list(
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "18px"
      )
    ),
    labels = list(
      list(
        point = list(
          x = 2043,
          y = 5574819,
          xAxis = 0,
          yAxis = 0
        ),
        y = 30,
        style = list(
          color = col_nrs_purple
        ),
        text = "Principal"
      ),
      list(
        point = list(
          x = 2043,
          y = 5493101,
          xAxis = 0,
          yAxis = 0
        ),
        style = list(
          color = col_neut_tundora
        ),
        text = "Half projected <br> EU migration",
        y = 50
      ),
      list(
        point = list(
          x = 2043,
          y = 5411300,
          xAxis = 0,
          yAxis = 0
        ),
        style = list(
          color = col_neut_grey
        ),
        text = "No EU migration",
        y = 30
      )
    )
  ) %>%
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      backgroundColor = col_nrs_light,
      borderColor = "transparent",
      align = "left",
      justify = 'left',
      style = list(
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "16px",
        color = col_neut_tundora
      )
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 2022,
          y = 5330000
        ),
        y = 0,
        useHTML =  T,
        text = list(c("We made these projections before Brexit <br>
                and the COVID-19 pandemic. We will publish <br>
                updated projections later in 2021.<br> <br>
                We base these projections on past trends and <br>
                assumptions about future births, deaths, and <br>
                migration. So they get less certain as <br>
                they go further into the future."))
      ))) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "population_projections",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )

plots[["MIGRATION_projections"]] 

# 4 AGEING POPULATION -------------------------------------------------------

# Age profiles ------------------------------------------------------------



plots[["AGEING_age_profile"]] <- map(c(1931, 2019, 2043), function(x) {
  data <- datasets[["AGEING_age_profile"]] %>%
    filter(year == x)
  
  highchart() %>%
    hc_add_series(data$male,
                  type = "bar",
                  name = "Male",
                  color = col_nrs_purple) %>%
    hc_add_series(data$female,
                  type = "bar",
                  name = "Female",
                  color = col_neut_silver) %>%
    hc_xAxis(categories = data$age,
             tickPositions = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)
    ) %>%
    hc_plotOptions(series = list(stacking = "normal"),
                   bar = list(pointWidth = 15)) %>%
    hc_yAxis(
      labels = list(formatter = JS("function() {
                       return Math.abs(this.value);}"),
                    #format = "{value:,.0f}",
                    rotation = 90#,
                    #reserveSpace = F,
                   # x = -5
      ),
      tickPositions = c(-300000, 0, 300000)
    ) %>%
    #hc_chart(marginLeft = 100) %>% 
    hc_tooltip(
      shared = FALSE,
      formatter = JS(
        "function () {
            return this.point.category + '<br/>' +
            '<b>' + this.series.name + '</b> ' +
            Highcharts.numberFormat(Math.abs(this.point.y), 0);}"
      )
    ) %>%
    hc_title(text = x,
             align = "center",
             x = 25) %>%
    hc_legend(enabled = F) %>%
    hc_add_theme(hc_theme)
}) 

# plots[["AGEING_age_profile"]][c(2, 3)] <-
#   map(plots[["AGEING_age_profile"]][c(2, 3)], function(x) {
#     hc_xAxis(x, labels = list(enabled = FALSE)) #%>%
#    # hc_title(x = 15)
#   })

plots[["AGEING_age_profile"]][c(2)] <-
  map(plots[["AGEING_age_profile"]][c(2)], function(x) {
    hc_add_annotation(x, 
                      draggable = '',
                      labelOptions = list(
                        borderColor = "transparent",
                        shape = "connector",
                        align = 'left',
                        justify = "left",
                        style = list(color = col_nrs_purple,
                                     fontFamily = windowsFont("Segoe UI"),
                                     fontSize = "18px"
                        )
                      ),
                      labels = list(
                        list(
                          point = list(
                            xAxis = 0,
                            yAxis = 0,
                            x = 1,
                            y = 290000
                          ),
                          style = list(color = col_neut_grey),
                          text = "Female"
                        ),
                        list(
                          point = list(
                            xAxis = 0,
                            yAxis = 0,
                            x =  1,
                            y = -250000
                          ),
                          text = "Male"
                        )))
  })


plots[["AGEING_age_profile"]] <- plots[["AGEING_age_profile"]] %>% 
  hw_grid() %>%
  htmltools::browsable()
plots[["AGEING_age_profile"]]

# Changing causes of death ------------------------------------------------

y_values <- datasets[["AGEING_causes_of_death"]] %>% filter(year == min(year))

plots[["AGEING_causes_of_death"]] <- hchart(
  datasets[["AGEING_causes_of_death"]],
  "line",
  hcaes(x = year,
        y = count,
        group = type),
  color = c(
    col_neut_silver,
    col_nrs_purple,
    col_neut_silver
  ),
  showInLegend = F,
  marker = list(enabled = F)
) %>%
  hc_yAxis(labels = list(format = "{value:,.0f}"),
           title = list(text = "Deaths")) %>%
  hc_xAxis(title = list(enabled = F),
           tickPositions = c(seq(min(datasets[["AGEING_causes_of_death"]]$year), 
                                 max(datasets[["AGEING_causes_of_death"]]$year),
                                 by = 2))) %>%
  hc_add_theme(hc_theme) %>%
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      borderColor = "transparent",
      backgroundColor = "transparent",
      shape = 'rect',
      align = 'left',
      justify = "left",
      style = list(color = col_neut_grey,
                   fontFamily = windowsFont("Segoe UI"),
                   fontSize = "18px"
      )
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = min(datasets[["AGEING_causes_of_death"]]$year),
          y = 12412
        ),
        y = -8,
        x = 0,
        text = "Ischaemic heart disease"
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = min(datasets[["AGEING_causes_of_death"]]$year),
          y = 6803
        ),
        y = 0,
        x = 0,
        text = "Cerebrovascular disease"
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = min(datasets[["AGEING_causes_of_death"]]$year),
          y = 2031
        ),
        y = 35,
        x = 0,
        text = "Dementia and Alzheimer's disease",
        style = list(color = col_nrs_purple)
        
      )
    )
  ) %>%
  hc_exporting(
    enabled = TRUE,
    filename = "top_causes_of_death",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )

plots[["AGEING_causes_of_death"]]




# 5 HEALTH INEQUALITY --------------------------------------------------------------

# Life Expectancy ---------------------------------------------------------


# Create a separate plot for each sex

data <- datasets[["INEQUALITY_life_expectancy"]] %>%
  filter(sex == "Females")

plots[["life_expectancy_a"]] <-
    hchart(
      data %>% 
        filter(country == "Eastern European Countries"),
      "arearange",
      name = "Eastern European Countries",
      hcaes(x = year, low = min,
            high = max),
      color = col_neut_silver
    ) %>%
      hc_add_series(
        data %>%
          filter(country == "Western European Countries"),
        "arearange",
        name = "Western European Countries",
        hcaes(x = year,
              low = min,
              high = max),
        color = col_nrs_purple
      ) %>%
      hc_add_series(
        data %>%
          filter(country == "Scotland"),
        "line",
        name = "Scotland",
        hcaes(x = year,
              y = life_expectancy),
        color = col_neut_tundora
      ) %>%
      
      hc_add_series(
        data %>%
          filter(country == "UK"),
        "line",
        name = "UK",
        hcaes(x = year,
              y = life_expectancy),
        color = col_neut_tundora,
        dashStyle = "Dash"
      ) %>%
      hc_yAxis(min = 60,
               max = 90,
               gridLineWidth = 0,
               title = list(text = "Life expectancy")) %>%
      hc_xAxis(tickPositions = c(0, 37),
               title = F) %>%
      hc_title(text = unique(data$sex)) %>%
      hc_add_theme(hc_theme) %>%
      hc_add_annotation(
        draggable = '',
        labelOptions = list(
          borderColor = "transparent",
          shape = 'connector',
          align = "right",
          rotation = 90,
          justify = 'right',
          style = list(fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px")
        ),
        labels = list(
          list(
            point = list(
              xAxis = 0,
              yAxis = 0,
              x = 0,
              y = 66
            ),
            text = "<b>Eastern</b> European <br/>Countries",
            style = list(color = col_neut_grey)
          ),
          list(
            point = list(
              xAxis = 0,
              yAxis = 0,
              x = 0,
              y = 85
            ),
            text = "<b>Western</b> European <br/>Countries",
            style = list(color = col_nrs_purple)
          )
        )
      ) %>%
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      shape = 'connector',
      align = "right",
      rotation = 90,
      justify = 'right',
      style = list(fontFamily = windowsFont("Segoe UI"),
                   fontSize = "18px")
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 37,
          y = 81.13
        ),
        y = 40,
        text = "<b>Scotland"
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 37,
          y = 83.06
        ),
        y = -16,
        text = "<b>UK"
      )
    )
  )
    
    
data <- datasets[["INEQUALITY_life_expectancy"]] %>%
      filter(sex == "Males")

plots[["life_expectancy_b"]] <-
  hchart(
    data %>% 
    filter(country == "Eastern European Countries"),
    "arearange",
    name = "Eastern European Countries",
    hcaes(x = year, low = min,
          high = max),
    color = col_neut_silver
  ) %>%
  
  hc_add_series(
    data %>%
      filter(country == "Western European Countries"),
    "arearange",
    name = "Western European Countries",
    hcaes(x = year,
          low = min,
          high = max),
    color = col_nrs_purple
  ) %>%
  
  hc_add_series(
    data %>%
      filter(country == "Scotland"),
    "line",
    name = "Scotland",
    hcaes(x = year,
          y = life_expectancy),
    color = col_neut_tundora
  ) %>%
  
  hc_add_series(
    data %>%
      filter(country == "UK"),
    "line",
    name = "UK",
    hcaes(x = year,
          y = life_expectancy),
    color = col_neut_tundora,
    dashStyle = "Dash"
  ) %>%
  hc_yAxis(min = 60,
           max = 90,
           gridLineWidth = 0,
           title = list(text = "Life expectancy")) %>%
  hc_xAxis(tickPositions = c(0, 37),
           title = F) %>%
  hc_title(text = unique(data$sex)) %>%
  hc_add_theme(hc_theme) %>% 
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      shape = 'connector',
      align = "right",
      rotation = 90,
      justify = 'right',
      style = list(fontFamily = windowsFont("Segoe UI"),
                   fontSize = "18px")
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 37,
          y = 77.13
        ),
        y = 50,
        text = "<b>Scotland"
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 37,
          y = 79.37
        ),
        y = -20,
        text = "<b>UK"
      )
    )
  ) 

plots[["INEQUALITY_life_expectancy"]] <-  hw_grid(plots[["life_expectancy_a"]], plots[["life_expectancy_b"]]) %>%
  htmltools::browsable()

plots[["INEQUALITY_life_expectancy"]]



# Drug-related deaths by age ------------------------------------------------------------

plots[["INEQUALITY_drug_deaths"]] <-
  map(unique(datasets[["INEQUALITY_drug_deaths"]]$age),
      function(x) {
        data <- datasets[["INEQUALITY_drug_deaths"]] %>%
          filter(age == x)
        
plots[["INEQUALITY_drug_deaths"]] <-
  hchart(data, 
         "line",
         name = "Death rate per <br> 100,000",
         hcaes(x = year,
               y = round(count, digits = 2)),
         color = c(col_nrs_purple)
  ) %>%
  hc_yAxis(
    min = 0,
   # tickPositions = c(0.0, 0.2, 0.4, 0.6, 0.8),
    tickPositions = c(0, 20, 40, 60, 80),
    title = list(text = "Death rate per 100,000",
                 x = -47.5),
    labels = list(
                  reserveSpace = F,
                  #align = "left",
                  x = -30)
  ) %>%
  hc_chart(marginLeft = 75) %>%
  hc_title(text = x) %>%
  hc_xAxis(
    tickPositions = c(min(data$year), 
                      max(data$year)),
    title = F
  ) %>% 
  hc_legend(enabled = F) %>% 
  hc_add_theme(hc_theme)
})

plots[["INEQUALITY_drug_deaths"]][c(2, 3, 4, 5)] <-
  map(plots[["INEQUALITY_drug_deaths"]][c(2, 3, 4, 5)], function(x) {
    hc_yAxis(x, labels = list(enabled = FALSE),
             title = F)
  })

plots[["INEQUALITY_drug_deaths"]] <- plots[["INEQUALITY_drug_deaths"]] %>%
  hw_grid(rowheight = 500, ncol = 5) %>%
  htmltools::browsable() 



plots[["INEQUALITY_drug_deaths"]]


# 6 MARRIAGE --------------------------------------------------------------

# Same sex marriage -------------------------------------------------------

plots[["SAME_SEX_MARRIAGE_marriages"]] <- hchart(
  datasets[["SAME_SEX_MARRIAGE_marriages"]],
  "line",
  hcaes(x = year,
        y = count,
        group = type),
  color = c(col_neut_silver,
            col_nrs_purple),
  showInLegend = F
) %>%
  hc_plotOptions(line = list(
    marker = list(enabled = T,
                  radius = 5),
    dataLabels = list(enabled = F)
  )) %>%
  hc_yAxis(
    min = 0,
    title = list(text = "Number of marriages and partnerships"),
    labels = list(format = "{value:,.0f}")
  ) %>%
  hc_xAxis(tickLength = 0,
           title = list(enabled = F),
           lineColor = "transparent",
           plotLines = list(
             list(
               label = list(text = "Same-sex couples allowed <br/> to form civil partnerships",
                            rotation = 0,
                            style = list(fontFamily = windowsFont("Segoe UI"),
                                         fontSize = "18px"
                            )),
               color = col_neut_silver,
               width = 1,
               value = datasets[["SAME_SEX_MARRIAGE_marriages"]]$year[1],
               dashStyle = "dash"
             ),
             list(
               label = list(text = "Same-sex couples <br/>allowed to marry",
                            rotation = 0,
                            style = list(fontFamily = windowsFont("Segoe UI"),
                                         fontSize = "18px"
                            )),
               color = col_neut_silver,
               width = 1,
               value = datasets[["SAME_SEX_MARRIAGE_marriages"]]$year[10],
               dashStyle = "dash"
             ))) %>%
  hc_add_theme(hc_theme) %>%
  hc_tooltip(
    formatter = JS(
      "function () { return '<b>'
                            + this.series.name
                            + '<br /> '
                            + '<b>'
                            + this.point.x
                            + ': '
                            + '</b> '
                            + Highcharts.numberFormat(this.point.y, -1)
                            + ' <br />';}"
    )
  ) %>% 
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      borderColor = "transparent",
      shape = 'connector',
      align = "right",
      justify = 'right',
      style = list(
        color = col_neut_silver,
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "18px"
      )
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 2020,
          y = 83
        ),
        y = -5,
        text = "Civil partnerships",
        style = list(
          color = col_neut_grey
        )
      ), 
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 2020,
          y = 423
        ),
        y = 15,
        x = 20,
        text = "Same sex marriage",
        style = list(
          color = col_nrs_purple
        )
      ))) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "same_sex_marriage_civil_partnerships",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )

plots[["SAME_SEX_MARRIAGE_marriages"]]


# Marriage types ----------------------------------------------------------

plots[["SAME_SEX_MARRIAGE_marriage_type"]] <- hchart(
  datasets[["SAME_SEX_MARRIAGE_marriage_type"]],
  "line",
  hcaes(x = year,
        y = count,
        group = type),
  color = c(col_neut_silver,
            col_neut_grey,
            col_neut_tundora,
            col_adopt,
            col_nrs_purple),
  dashStyle = c("dash",
                "dash",
                "dash",
                "solid",
                "solid"
  )
) %>%
  hc_plotOptions(line = list(
    dataLabels = list(enabled = F)
  )) %>% 
  hc_add_theme(hc_theme) %>%
  hc_yAxis(
    min = 0,
    title = list(text = "Number of marriages"),
    labels = list(format = "{value:,.0f}")
  ) %>%
  hc_xAxis(tickLength = 0,
           title = list(enabled = F),
           plotLines = list(
             list(
               label = list(text = "Humanist celebrants <br/>authorised to <br/>conduct marriages",
                            rotation = 0,
                            x = -5,
                            align = "right",
                            style = list(fontFamily = windowsFont("Segoe UI"),
                                         fontSize = "18px",
                                         color = col_adopt
                            )),
               color = col_adopt,
               width = 1,
               value = 2005,
               dashStyle = "dash"
             ))) %>% 
  hc_legend(itemStyle = list(fontSize = "18px",
                             fontStyle = "normal",
                             fontWeight = "light"),
            reversed = T) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "marriages_by_type",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )

plots[["SAME_SEX_MARRIAGE_marriage_type"]]

# 7 LONG TERM CHANGES -----------------------------------------------------

# Births and deaths -------------------------------------------------------

wwi <- datasets[["CHANGES_births_deaths"]] %>%
  filter(year %in% c(1914:1918)) %>%
  spread(type, count)
wwii <- datasets[["CHANGES_births_deaths"]] %>%
  filter(year %in% c(1938:1945)) %>%
  spread(type, count)
hide_lines <- datasets[["CHANGES_births_deaths"]] %>%
  filter(year %in% c(1960:1976)) %>%
  spread(type, count)


plots[["CHANGES_births_deaths"]] <- hchart(
  datasets[["CHANGES_births_deaths"]],
  "line",
  hcaes(x = year,
        y = count,
        group = type),
  color = c(col_nrs_purple,
            col_neut_silver),
  zIndex = 2
) %>%
  hc_yAxis(
    min = 0,
    labels = list(format = "{value:,.0f}"),
    title = list(text = "Number of births and deaths")
  ) %>%
  hc_xAxis(
    title = F,
    tickPositions = c(
      min(datasets[["CHANGES_births_deaths"]]$year),
      1918,
      1961, 
      1975,
      max(datasets[["CHANGES_births_deaths"]]$year)
    ),
    plotLines = list(
      list(
        label = list(text = 'Influenza epidemic known <br/>as the "Spanish Flu"',
                     rotation = 0,
                     align = "right",
                     x = -5,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px"
                     )),
        color = col_neut_silver,
        width = 1,
        value = 1918,
        dashStyle = "dash"
      ),
      list(
        label = list(
          text = "Equal Pay Act <br/>and Sex<br/>
                  Discrimination <br/>Act",
          rotation = 0,
          style = list(
            fontFamily = windowsFont("Segoe UI"),
            fontSize = "18px"
          )
        ),
        color = col_neut_silver,
        width = 1,
        value = 1975,
        dashStyle = "dash"
      ),
      list(
        label = list(
          text = "Contraceptive <br/>pill
                             first available<br/>
                             on the NHS",
          rotation = 0,
          x = -5,
          align = "right",
          style = list(
            fontFamily = windowsFont("Segoe UI"),
            fontSize = "18px"
          )
        ),
        color = col_neut_silver,
        width = 1,
        value = 1961,
        dashStyle = "dash"
      )
    )
  ) %>%
  hc_add_series(
    wwi,
    "arearange",
    hcaes(x = year,
          low = 0,
          high = Births),
    color = col_nrs_light,
    zIndex = 1
  ) %>%
  hc_add_series(wwii,
                "arearange",
                hcaes(x = year,
                      low = 0,
                      high = Births),
                color = col_nrs_light) %>%
  hc_add_series(hide_lines,
                "arearange",
                hcaes(
                  x = year,
                  low = 0,
                  high = Births,
                  zIndex = 1
                ),
                color = "white") %>%
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      borderColor = "transparent",
      shape = 'connector',
      justify = 'right',
      style = list(
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "18px"
    )
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1855,
          y = 93349
        ),
        y = 35,
        text = "Births",
        style = list(color = col_nrs_purple)
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1855,
          y = 62004
        ),
        y = 45,
        text = "Deaths",
        style = list(color = col_neut_grey)),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1914,
          y = 20000
        ),
        align = "right",
        text = "First<br>
                World<br/> 
                War"),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1938,
          y = 20000
        ),
        align = "right",
        text = "Second<br>
                World<br/> 
                War"
      )
    )
  ) %>%
  hc_legend(enabled = F) %>%
  hc_plotOptions(arearange = list(
    fillOpacity = 1,
    animation = F,
    enableMouseTracking = F
  )) %>%
  hc_add_theme(hc_theme) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "births_and_deaths",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )

plots[["CHANGES_births_deaths"]]


# Stillbirths and infant deaths -------------------------------------------

plots[["CHANGES_child_mortality"]] <-
  hchart(datasets[["CHANGES_child_mortality"]], 
         "line",
         hcaes(x = year,
               y = count,
               group = age),
         color = c(col_neut_silver,
                   col_neut_silver,
                   col_neut_silver,
                   col_neut_silver,
                   col_nrs_purple)
         ) %>%
  hc_yAxis(
    min = 0,
    max = 17500,
    labels = list(format = "{value:,.0f}"),
    title = list(text = "Deaths"
                 )
  ) %>%
  hc_xAxis(
    tickPositions = c(1901, 1918, 2020),
    title = F,
    plotLines = list(
      list(
        label = list(text = 'NHS Scotland <br/>formed',
                     rotation = 0,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px"
                     )),
        color = col_neut_grey,
        width = 1,
        value = 1948,
        dashStyle = "shortdash"
      ),
      list(
        label = list(text = "Midwives' Act <br>
                     implemented in <br>
                     Scotland",
                     rotation = 0,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px"
                     )),
        color = col_neut_grey,
        width = 1,
        value = 1915,
        dashStyle = "shortdash"
      # ),
      # list(
      #   label = list(text = 'Discovery of <br>modern penicillin',
      #                rotation = 0,
      #                style = list(
      #                  fontFamily = windowsFont("Segoe UI"),
      #                  fontSize = "18px"
      #                )),
      #   color = col_neut_grey,
      #   width = 1,
      #   value = 1928,
      #   dashStyle = "shortdash"
      )
      )
    ) %>% 
  hc_legend(enabled = F) %>% 
  hc_add_theme(hc_theme) %>% 
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      borderColor = "transparent",
      shape = 'connector',
      align = 'left',
      style = list(
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "16px",
        color = col_neut_grey
      )
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1901,
          y = 17104
        ),
        text = "Age 0",
        x = 5,
        style = list(color = col_neut_grey)
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1901,
          y = 9880
        ),
        text = "Age 1-4",
        y = 0,
        x = -5,
        style = list(color = col_neut_grey)
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1901,
          y = 2334
        ),
        text = "Age 5-9",
        y = 0,
        x = -5,
        style = list(color = col_neut_grey)
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1939,
          y = 3832
        ),
        text = "Stillbirths",
        align = "center",
        y = 0,
        x = -5,
        style = list(color = col_nrs_purple)
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1901,
          y = 0
        ),
        text = "Age 10-14",
        y = 0,
        x = -5,
        style = list(color = col_neut_grey)
      )
      )) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "infant_mortality",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  )

plots[["CHANGES_child_mortality"]]


# Marriage age -------------------------------------------------------

hide_lines <- datasets[["CHANGES_marriage_age"]] %>% 
  filter(year %in% c("1951-60", "1961-70", "1971-80", "1981-90"),
         sex == "Females")

plots[["CHANGES_marriage_age"]] <-
  hchart(datasets[["CHANGES_marriage_age"]], 
         "line",
         hcaes(x = year,
               y = `mean_age`,
               group = sex),
         color = c(col_nrs_purple,
                   col_neut_silver),
         zIndex = 6
  )  %>%
  hc_plotOptions(line = list(
    marker = list(enabled = T,
                  radius = 5),
    dataLabels = list(enabled = F)
  )) %>%
  hc_yAxis(
    min = 0,
    labels = list(format = "{value:,.0f}"),
    title = list(text = "Mean age at marriage")
  ) %>%
  hc_xAxis(
    tickPositions = c(0, 4, 8, 12, 16),
    title = F,
    plotLines = list(
      list(
        label = list(text = "Contraceptive <br/>pill
                             first available<br/>
                             on the NHS (1961)",
                     rotation = 0,
                     align = "right",
                     x = -5,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px")),
        color = col_neut_silver,
        width = 1,
        value = 11,
        dashStyle = "dash"
      ),
      list(
        label = list(text = "Equal Pay Act<br>
                     and Sex <br>
                     Discrimination Act <br>
                     (1975)",
                     rotation = 0,
                     style = list(
                       fontFamily = windowsFont("Segoe UI"),
                       fontSize = "18px")),
        color = col_neut_silver,
        width = 1,
        value = 12,
        dashStyle = "dash"
      )
    )
  ) %>% 
  hc_legend(enabled = T) %>% 
  hc_add_theme(hc_theme) %>% 
  hc_add_annotation(
    draggable = '',
    labelOptions = list(
      borderColor = "transparent",
      shape = 'connector',
      align = 'left',
      style = list(
        fontFamily = windowsFont("Segoe UI"),
        fontSize = "18px",
        color = col_neut_silver
      )
    ),
    labels = list(
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1901,
          y = 17104
        ),
        text = "Age 0",
        x = 5,
        style = list(color = col_nrs_purple)
      ),
      list(
        point = list(
          xAxis = 0,
          yAxis = 0,
          x = 1901,
          y = 9880
        ),
        text = "Age 1-4",
        y = 0,
        x = -5,
        style = list(color = col_neut_silver)
      )
    )) %>% 
  hc_add_series(hide_lines,
                "arearange",
                showInLegend = F,
                hcaes(
                  x = year,
                  low = 0,
                  high = mean_age),
                zIndex = 4,
                color = "white") %>%
  hc_plotOptions(arearange = list(
    fillOpacity = 3,
    animation = F,
    enableMouseTracking = F
  )) %>% 
  hc_exporting(
    enabled = TRUE,
    filename = "marriage_age",
    buttons = list(
      contextButton = list(
        menuItems = c('downloadPNG',
                      'downloadSVG',
                      'separator',
                      'downloadCSV',
                      'downloadXLS')))
  ) %>% 
  hc_legend(reversed = T,
             itemStyle = list(fontSize = "18px",
                              fontStyle = "normal",
                              fontWeight = "light"))

plots[["CHANGES_marriage_age"]]

