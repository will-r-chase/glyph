#' Launch a gui to make glyphs
#'
#' @import shiny shinyjs shinythemes shinyWidgets shinydashboard miniUI
#'
#' @export

glyphGeneratorAddin <- function() {

  cleanup <- function(x) {
    for (i in 1:x) {
      id <- paste0("div#input", i, " > div")
      removeUI(selector = id)
    }
  }
  path <- system.file("www/glyph_instructions.html", package="glyph")
  ui <- dashboardBody(theme = shinytheme("flatly"),
                      h1(id="title", "Glyph generator"),
                      dashboardSidebar(disable = TRUE),
                      setBackgroundColor(color = "black"),
                      useShinydashboard(),
                      useShinyjs(),
                      includeCSS("glyph_css.css"),
                      fluidRow(
                        column(3,
                               box(title = "What do all these parameters mean???", width = NULL,
                                   div("Read the", a(href = "https://www.williamrchase.com/", "description of parameters", target = "_blank"))),
                               box(title = "Type of glyph to generate", id = "glyph_choice", width = NULL,
                                   div(id = "glyph_type",
                                       selectInput("input_type", label = NA,
                                                   c("Orbital glyph", "Summoning glyph",
                                                     "Glitched glyph"
                                                   )
                                       )
                                   ),
                                   div(id = "input19")),
                               div(id = "seed_probs",
                                   box(title = "Seed probabilities", width = NULL,
                                       div(id = "input1"),
                                       div(id = "input2"),
                                       div(id = "input3"),
                                       div(id = "input4"))),
                               div(id = "sec_pareto_glitch",
                                   box(title = "Probability of second set of orbits", width = NULL,
                                       div(id = "input29"),
                                       div(id = "input30")))),
                        column(3,
                               div(id = "planet_prob",
                                   box(title = "Planet probabilities", width = NULL,
                                       div(id = "input5"),
                                       div(id = "input6"),
                                       div(id = "input7"),
                                       div(id = "input8"))),
                               div(id = "sec_pareto",
                                   box(title = "Probability of second set of orbits", width = NULL,
                                       div(id = "input9"),
                                       div(id = "input10"))),
                               div(id = "num_inscribed",
                                   box(title = "Probabilities of number of inscribed shapes", width = NULL,
                                       div(id = "input11"),
                                       div(id = "input12"),
                                       div(id = "input13"),
                                       div(id = "input14"))),
                               div(id = "more_inscribed",
                                   box(title = "Probabilities of more sets of inscribed shapes", width = NULL,
                                       div(id = "input15"),
                                       div(id = "input16"),
                                       div(id = "input17"),
                                       div(id = "input18"))),
                               div(id = "glitch_box",
                                   box(title = "Glitch parameters", width = NULL,
                                       div(id = "input20"),
                                       div(id = "input21"),
                                       div(id = "input22"),
                                       div(id = "input23"),
                                       div(id = "input24"),
                                       div(id = "input25"),
                                       div(id = "input26"),
                                       div(id = "input27"),
                                       div(id = "input28")))),
                        column(6,

                               plotOutput("glyph_plot", width = "500px", height = "500px"),
                               actionButton("run", "Generate plot"),
                               tags$br(),
                               h4(class="boxed", "File name:"),
                               #h4(class="boxed", "File name:"),
                               textInput('filename', label = NA),
                               downloadButton('downloadPlot','Download Plot'))
                      ))

  server <- function(input, output, session) {
    observeEvent(input$input_type, {
      if(input$input_type == "Orbital glyph"){
        cleanup(30)
        shinyjs::hide(id = "glitch_box", anim = TRUE)
        shinyjs::hide(id = "more_inscribed", anim = TRUE)
        shinyjs::hide(id = "num_inscribed", anim = TRUE)
        shinyjs::hide(id = "planet_prob", anim = TRUE)
        shinyjs::hide(id = "sec_pareto_glitch", anim = TRUE)
        shinyjs::show(id = "sec_pareto", anim = TRUE)
        insertUI(
          # insert inside the div input
          selector = "#input1",
          where = "afterBegin",
          ui = numericInput("seedprob_1", "none", value = 0.3, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input2",
          where = "afterBegin",
          ui = numericInput("seedprob_2", "circle", value = 0.5, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input3",
          where = "afterBegin",
          ui = numericInput("seedprob_3", "diamond", value = 0.1, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input4",
          where = "afterBegin",
          ui = numericInput("seedprob_4", "square", value = 0.1, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input5",
          where = "afterBegin",
          ui = numericInput("planetprob_1", '0 planets', value = 0.4, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input6",
          where = "afterBegin",
          ui = numericInput("planetprob_2", '1 planet', value = 0.3, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input7",
          where = "afterBegin",
          ui = numericInput("planetprob_3", '2 planets', value = 0.2, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input8",
          where = "afterBegin",
          ui = numericInput("planetprob_4", '3 planets', value = 0.2, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input9",
          where = "afterBegin",
          ui = numericInput("paretoprob_1", "No second set", value = 0.2, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input10",
          where = "afterBegin",
          ui = numericInput("paretoprob_2", "Second set", value = 0.8, min = 0, max = 1, step = 0.1),
        )
        glyph_data <- eventReactive(input$run, {
          orbit_app(seed_probs = c(input$seedprob_1, input$seedprob_2, input$seedprob_3, input$seedprob_4),
                       planet_probs = c(input$planetprob_1, input$planetprob_2, input$planetprob_3, input$planetprob_4),
                       pareto2_prob = c(input$paretoprob_1, input$paretoprob_2)
          )
        })
        output$glyph_plot <- renderPlot({  glyph_data() })
      }

      if(input$input_type == "Summoning glyph"){
        cleanup(30)
        shinyjs::hide(id = "glitch_box", anim = TRUE)
        shinyjs::show(id = "more_inscribed", anim = TRUE)
        shinyjs::show(id = "num_inscribed", anim = TRUE)
        shinyjs::hide(id = "planet_prob", anim = TRUE)
        shinyjs::hide(id = "sec_pareto_glitch", anim = TRUE)
        shinyjs::hide(id = "sec_pareto", anim = TRUE)
        insertUI(
          # insert inside the div input
          selector = "#input1",
          where = "afterBegin",
          ui = numericInput("seedprob_1", "none", value = 0.3, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input2",
          where = "afterBegin",
          ui = numericInput("seedprob_2", "circle", value = 0.5, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input3",
          where = "afterBegin",
          ui = numericInput("seedprob_3", "diamond", value = 0.1, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input4",
          where = "afterBegin",
          ui = numericInput("seedprob_4", "square", value = 0.1, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input11",
          where = "afterBegin",
          ui = numericInput("inscribedprob_1", '0 shapes', value = 0.2, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input12",
          where = "afterBegin",
          ui = numericInput("inscribedprob_2", '1 shape', value = 0.3, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input13",
          where = "afterBegin",
          ui = numericInput("inscribedprob_3", '2 shapes', value = 0.3, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input14",
          where = "afterBegin",
          ui = numericInput("inscribedprob_4", '3 shapes', value = 0.2, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input15",
          where = "afterBegin",
          ui = numericInput("sec_inscribedprob_1", 'No second set of shapes', value = 0.4, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input16",
          where = "afterBegin",
          ui = numericInput("sec_inscribedprob_2", 'Yes second set of shapes', value = 0.6, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input17",
          where = "afterBegin",
          ui = numericInput("third_inscribedprob_1", 'No thrid set of shapes', value = 0.5, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input18",
          where = "afterBegin",
          ui = numericInput("third_inscribedprob_2", 'Yes third set of shapes', value = 0.5, min = 0, max = 1, step = 0.1),
        )
        glyph_data <- eventReactive(input$run, {
          summon_app(seed_probs = c(input$seedprob_1, input$seedprob_2, input$seedprob_3, input$seedprob_4),
                        inscribed_probs = c(input$inscribedprob_1, input$inscribedprob_2, input$inscribedprob_3, input$inscribedprob_4),
                        sec_shape_probs = c(input$sec_inscribedprob_1, input$sec_inscribedprob_2),
                        third_shape_probs = c(input$third_inscribedprob_1, input$third_inscribedprob_2))

        })
        output$glyph_plot <- renderPlot({  glyph_data() })


      }

      if(input$input_type == "Glitched glyph"){
        cleanup(30)
        shinyjs::show(id = "glitch_box", anim = TRUE)
        shinyjs::hide(id = "more_inscribed", anim = TRUE)
        shinyjs::hide(id = "num_inscribed", anim = TRUE)
        shinyjs::hide(id = "planet_prob", anim = TRUE)
        shinyjs::show(id = "sec_pareto_glitch", anim = TRUE)
        shinyjs::hide(id = "sec_pareto", anim = TRUE)
        insertUI(
          # insert inside the div input
          selector = "#input19",
          where = "afterBegin",
          ui = selectInput("type", "Glitch type",
                           c("spike", "connected", "shattered")),
        )
        insertUI(
          # insert inside the div input
          selector = "#input1",
          where = "afterBegin",
          ui = numericInput("seedprob_1", "none", value = 1, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input2",
          where = "afterBegin",
          ui = numericInput("seedprob_2", "circle", value = 0, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input3",
          where = "afterBegin",
          ui = numericInput("seedprob_3", "diamond", value = 0, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input4",
          where = "afterBegin",
          ui = numericInput("seedprob_4", "square", value = 0, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input29",
          where = "afterBegin",
          ui = numericInput("paretoprob_1", "No second set", value = 0.2, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input30",
          where = "afterBegin",
          ui = numericInput("paretoprob_2", "Second set", value = 0.8, min = 0, max = 1, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input20",
          where = "afterBegin",
          ui = sliderInput("num_glitches", 'Number of glitches', value = 10, min = 0, max = 150, step = 1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input21",
          where = "afterBegin",
          ui = sliderInput("glitch_r_min", 'Minimum glitch radius', value = 0.5, min = 0, max = 10, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input22",
          where = "afterBegin",
          ui = sliderInput("glitch_r_max", 'Maximum glitch radius', value = 2, min = 0, max = 10, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input23",
          where = "afterBegin",
          ui = sliderInput("min_spikes", 'Minimum number of spikes', value = 30, min = 0, max = 199, step = 1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input24",
          where = "afterBegin",
          ui = sliderInput("max_spikes", 'Maximum number of spikes', value = 60, min = 0, max = 199, step = 1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input25",
          where = "afterBegin",
          ui = sliderInput("min_spikes2", 'Minimum number of spikes in second set of orbits', value = 15, min = 0, max = 199, step = 1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input26",
          where = "afterBegin",
          ui = sliderInput("max_spikes2", "Maximum number of spikes in second set of orbits", value = 40, min = 0, max = 199, step = 1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input27",
          where = "afterBegin",
          ui = sliderInput("min_spike_jitter", "Minimum jitter of spikes", value = -0.2, min = -2, max = 2, step = 0.1),
        )
        insertUI(
          # insert inside the div input
          selector = "#input28",
          where = "afterBegin",
          ui = sliderInput("max_spike_jitter", "Maximum jitter of spikes", value = 0.2, min = -2, max = 2, step = 0.1),
        )
        glyph_data <- eventReactive(input$run, {
          orbit_glitch_app(seed_probs = c(input$seedprob_1, input$seedprob_2, input$seedprob_3, input$seedprob_4),
                              glitch_type = input$type,
                              pareto2_prob = c(input$paretoprob_1, input$paretoprob_2),
                              glitch_params = list(num_glitches = input$num_glitches, glitch_r_min = input$glitch_r_min, glitch_r_max = input$glitch_r_max,
                                                   min_spikes = input$min_spikes, max_spikes = input$max_spikes, min_spikes2 = input$min_spikes2,
                                                   max_spikes2 = input$max_spikes2, min_spike_jitter = input$min_spike_jitter, max_spike_jitter = input$max_spike_jitter)
          )
        })
        output$glyph_plot <- renderPlot({  glyph_data() })
      }
      output$downloadPlot <- downloadHandler(
        filename = function(){paste(input$filename,'.png',sep='')},
        content = function(file){
          ggsave(file,plot=glyph_data())
        })
    })
  }

  runGadget(ui, server, viewer = browserViewer())
}

