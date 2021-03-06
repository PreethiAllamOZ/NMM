library(DT)
shinyUI(
  fluidPage(     theme = 'oz_theme.css',
                 # enable shinyalert modals:
                 useShinyalert(),
                 shinyjs::useShinyjs(),
                 
                 list(tags$head(HTML('<link rel="icon", href="Oz_ICON.png", type="image/png" />'))),
                 
                 titlePanel(title="", windowTitle="Model Applications"),
                 # load js to image on top right of title bar:
                 tags$head(tags$script(type="text/javascript", src = "right_image.js")),
                 
                 # offset body of page due to fixed-top menu:
                 tags$style(type="text/css", "body {padding-top: 70px;}"),  
                 
                 tags$head(tags$style("#PH_3d_grid{height:60vh !important;}")),
                 tags$head(tags$style("#PH_squished_raster{height:60vh !important;}")),
                 tags$head(tags$style("#PH_location_samples{height:60vh !important;}")),
                 
                 navbarPage(
                   title = div(img(src = "Oz_MU.svg",
                                   height = "50"),
                               style = "position: relative; left: 0%; top: -70%"),
                   id = 'sidepanel',
                   position = "fixed-top",
                   
                   tabPanel("Near Miss ML Model",
                            sidebarPanel(
                              fileInput("file1", "Upload CSV File",
                                        multiple = FALSE,
                                        accept = c(".csv"),),
                              tags$hr(),
                              
                              downloadButton("DownloadData",label = "Download Results")
                            ),
                            mainPanel(
                              DT::dataTableOutput("mytable",)
                            )
                   ),
                   
                   tabPanel("User Manual",
                           
                            navlistPanel(
                              "Introduction",
                              tabPanel("Near Miss", h2("Near Miss Models"),br(), p("Over the past four years, my administration delivered for Americans of all backgrounds like never before. Save America is about building on those accomplishments, supporting the brave conservatives who will define the future of the America First Movement, the future of our party, and the future of our beloved country.  Save America is also about ensuring that we always keep America First, in our foreign and domestic policy.  We take pride in our country, we teach the truth about our history, we celebrate our rich heritage and national traditions, and of course, we respect our great American Flag.
                              We are committed to defending innocent life and to upholding the Judeo-Christian values of our founding.
                              We believe in the promise of the Declaration of Independence, that we are all made EQUAL by our Creator, and that must all be TREATED equal under the law.
                              We know that our rights do not come from government, they come from God, and no earthly force can ever take those rights away. That includes the right to religious liberty and the right to Keep and Bear Arms.
                              We embrace free thought, we welcome robust debate, and we are not afraid to stand up to the oppressive dictates of political correctness.
                              We know that the rule of law is the ultimate safeguard of our freedoms, and we affirm that the Constitution means exactly what it says AS WRITTEN.")),
                             
                              "How to Use",
                              tabPanel("Upload File Format", h2("Upload File Format"), br(),h4("The Applications requires Data to be in specific file format"), br(),
                                       h4(" 1. File should be in .csv format only"), br(),
                                     #  h4(" - PROJECT,SiteID,DEPTH_FROM,DEPTH_TO,SampleID"), br(),
                                       h4(" 2. All the Assay concentrations should be in PPM"), br(),
                                     
                                       h4(" 3. Element name should be followed by PPM, for example Cu should be named CuPPM"), br(),
                                       h4(" 4. Here is the expected file"), br(),
                                       downloadButton("DownloadData2",label = "Download Results")),
                              tabPanel("Different Models", h2("Required Variables Models"),br(), 
                                       h4("Each of the Models require a set of variables to predict"), br(),
                                       h4("Carrapateena Model") ,h5("AgPPM, AsPPM, CuPPM, LaPPM, MoPPM, NiPPM, PbPPM, SrPPM, ThPPM, UPPM, VPPM, ZnPPM, ZrPPM"), br(),
                                       h4("Carrapateena(No Vanadium) Model"),h5("AgPPM, AsPPM, CuPPM, LaPPM, MoPPM, NiPPM, PbPPM, SrPPM, ThPPM, UPPM,ZnPPM, ZrPPM"), br(),
                                       h4("Prominent Hill Distance Regression Model"), h5("CuPPM, AuPPM, AgPPM, UPPM, FePPM, AsPPM, BaPPM, BiPPM, CaPPM, CePPM, CoPPM, KPPM, LaPPM, MgPPM, MnPPM,MoPPM, NaPPM,NiPPM,PPPM,PbPPM, SbPPM, SPPM,SrPPM,TiPPM ,YPPM, ZnPPM"), br(),
                                       h4("Starra Near Miss Model"),h5("AlPPM,CaPPM,KPPM,NaPPM,SbPPM,TiPPM,AgPPM,AsPPM,CoPPM, FePPM, MnPPM,MoPPM,NiPPM, PbPPM, SPPM,UPPM,ZnPPM"), br(),
                                       h4("West Mustgraves Model"), h5("CrPPM,ZrPPM,ZnPPM,CoPPM,VPPM,SPPM,MgPPM ,KPPM,TiPPM, FePPM, PPPM"))
                            )
                             
                            
                            
                     
                   )
                 )
  ))