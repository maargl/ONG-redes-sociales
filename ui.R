library(shiny);library(shinydashboard);library(visNetwork);library(shinyWidgets);library(shinyjs);library(shinybusy);library(plotly);

choicesONG<-readRDS(file=paste('prememo/','cat1.Rda',sep=''))
choicesONG<-data.frame(id=c('0',choicesONG$category),names=c('Seleccione una Categoría',choicesONG$title),stringsAsFactors=FALSE)
ui <- fluidPage(useShinyjs(),
	includeScript(path = "prememo/jquery-ui.min.js"),
	includeCSS("prememo/jquery-ui.min.css"),
	tags$script(HTML(
		'$(document).tooltip({show: null});
		$( document ).on( "click", "input[type=\'checkbox\']", function() {var a="0"; if(this.checked){a="1"};
  Shiny.onInputChange("checkboxC_clicked", this.value+";"+a ); });'
	)),
	tags$head(
		tags$style(HTML("table.dataTable tbody td, table tbody td { border: 1px solid black;padding:5px 5px;}
		#infoTab tbody td, #statG tbody td, #statN tbody td { border: 1px solid black;}
		.modal-body {padding: 10px}
       .modal-content  {-webkit-border-radius: 6px;-moz-border-radius: 6px;border-radius: 6px;}
       .modal-header {background-color: #3c8dbc; border-top-left-radius: 6px; border-top-right-radius: 6px}
       .dot { height: 10px; width: 10px; border-radius: 50%; display: inline-block;}
       .xclose {position: absolute;right: .3em;top: 3%;width: 30px;margin: -10px 0 0 0;padding: 1px;height: 30px;}
       .btn {font-size:16px;}
       .irs-grid-text {font-size:11px !important;}
       .irs-single, .irs-max {font-size:12px !important;}
       .box.box-solid.box-danger>.box-header {color:#fff;background:#004d99}
       .box.box-solid.box-danger{border-bottom-color:#004d99;border-left-color:#004d99;border-right-color:#004d99;border-top-color:#004d99;}
       .navbar-brand{display:none;}
       #tohide {left: -999px;position: absolute;}
       .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:focus, .navbar-default .navbar-nav > .active > a:hover {color:white; background-color:#33759B;}
       .navbar-default .navbar-nav > li > a, .navbar-default .navbar-nav > li > a:hover {color:white; background-color:#3c8dbc;}
       .navbar-default .navbar-nav > li > a {border-right-style: solid;border-right-width: 2px;border-right-color:#33759B;}
       .navbar .navbar-nav {background-color: #3c8dbc;} 
      .navbar.navbar-default.navbar-static-top{font-size: 20px;font-family: 'Source Sans Pro'; background-color: #3c8dbc;}
      .navbar .navbar-header {float: left;} 
      .navbar-default .navbar-brand {font-size: 20px;font-family: 'Source Sans Pro'; background-color: #3c8dbc;}
      [data-value='#tabTitle'] {background-color:#3c8dbc !important;font-size: 25px;}
      .navbar {margin-bottom:0px;}
      .navbar-default .navbar-toggle {background-color:white;margin-left:20px;}
      .container-fluid {padding-right:0;padding-left:0;}
      .wrapper {padding-left:15px;padding-right:15px;}
      .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {background-color:#ECF0F5;}
      .navbar-default {border-width:0px;}
      .checkbox > label:hover {background-color:#cccccc;}
      .modebar {display: none;}
      .btn-warning.active, .btn-warning:active, .open > .dropdown-toggle.btn-warning {background-color:#d58512;}
      .box.box-solid.box-warning>.box-header {color:#fff;background:#d58512}
      .box.box-solid.box-warning{border-bottom-color:#d58512;border-left-color:#d58512;border-right-color:#d58512;border-top-color:#d58512;}"))
    ),div(id="tohide",
           sliderInput("FirstSlider",label="FirstSlider",1,3,1,1)
           ),
           navbarPage(
            'Atención ciudadana a ONGs chilenas', collapsible=TRUE,
            tabPanel(div("Atención ciudadana a ONGs chilenas",style='text-decoration:underline;'), value="#tabTitle",
            dashboardPage(
		dashboardHeader(disable=T
		),
		dashboardSidebar(disable = TRUE),
		dashboardBody(style='font-size: 14px;',
			fluidRow(mainPanel(width='100%',
				column(width = 3),
				column(width = 6,
				box(solidHeader=T,status="warning",title='¿Para qué visualizar redes?',width='100%',
					'En este sistema es posible ver redes formadas al usar Facebook, que permite dar una idea acerca de la participación de los usuarios que usan este medio para interactuar con las publicaciones que algunas ONGs realizan mediante esta tecnología.'
				))
			)),
			fluidRow(mainPanel(width='100%',
				column(width = 6,
				box(solidHeader=T,status="danger",title='Sobre estas redes',width='100%',
				div(HTML('<p>Las redes disponibles en este sistema son redes en donde los <b>nodos</b> tienen los nombres de <b>cuentas de Facebook</b>, aquellos nodos conectados por un <b>arco</b> (o relación) son los que realizaron una <b>misma acción</b> (que puede ser escribir un comentario o dar "me gusta") sobre una publicación realizada por una ONG en Facebook.</p><p>Y los arcos tienen un <b>peso</b> cuyo valor es proporcional a las veces que la relación se produjo en un periodo de <b>tiempo</b>, un arco es más transparente mientras menor sea su peso.</p><p>En estas redes hay nodos de dos tipos: los nodos que son personas u ONGs con cuentas de Facebook, y nodos que son <b>grupos</b>, los cuales son un <b>grupo de nodos</b>. Al abrir un grupo se muestran los nodos que dicho grupo tiene.</p>'))
				)),

				column(width = 6,
				div(HTML('<iframe width="512" height="288" src="https://youtube.com/embed/LPCwpOexiIY" frameborder="0" allowfullscreen></iframe>'))
				))))
			)
            ),
            tabPanel(div("Ver red"),
            	dashboardPage(
		dashboardHeader(disable=T
		),
		dashboardSidebar(disable = TRUE),
		dashboardBody(
			fluidRow(mainPanel(width='100%',
				div(radioGroupButtons(justified=T,inputId="categoryTab",status="warning",choiceNames = c('Categorías','ONGs','Regiones'),choiceValues = c(1,2,3)),style='margin-top:-15px;'),
					div(selectInput("selectONG",NULL,choices= setNames(choicesONG$id,choicesONG$names) ),style='margin-top:-10px;'),
					div(conditionalPanel(condition = "input.selectONG != '0'",
					column(width = 3,
						box(solidHeader=T,status="danger",title='Descripción',width='100%',
						div(textOutput("desc"),style='max-height: 150px;overflow-y: scroll;')
						),
						div(box(solidHeader=T,status="primary",title=div('Buscador de nodo',HTML('<i class="fa fa-question-circle"></i>'), title='Ordenado por el grado, de mayor a menor.'),width='100%',
							selectizeInput('selectNodo',label=NULL,choices=NULL, options = list(placeholder='Escriba Nombre') )
							,div(textOutput("posNode"),style='margin-top:-14px;')
						),style='margin-top:-14px;'),
						div(box(solidHeader=T,status="danger",title=div('ONGs presentes',textOutput("n_listONG",inline =T)),width='100%',
							div(uiOutput('listONG'),style='max-height: 100px;overflow-y: scroll;')
						),style='margin-top:-10px;')
					),
					box(solidHeader=T,status="danger",title=NULL,width=6, 
					div(radioGroupButtons(justified=T,inputId="graphTab",status="warning",choiceNames = c(paste(icon('facebook-square'),'Red de Comentarios'),paste(icon('facebook-square'),'Red de Me gusta')),choiceValues = c(1,2)),style='margin-top:-7px;'),
					div(visNetworkOutput("network"),style='margin-top:-14px;'), div(uiOutput("sliderTime"),style='margin-top:-14px;')
					),
					column(width = 3,
						box(solidHeader=T,status="danger",title=div('Medidas de la red',HTML('<i class="fa fa-question-circle"></i>'),title='Pasar sobre medidas para ver sus definiciones.'),align="center",width='100%',
						div(uiOutput("statG"),style='margin-bottom:-20px;')
						),
						div(box(solidHeader=T,status="primary",title=div('Medidas de nodo',HTML('<i class="fa fa-question-circle"></i>'),title='Clic sobre nodo para ver medidas de nodo.'),align="center",width='100%',
						textOutput("name_statN"),div(uiOutput("statN"),style='margin-bottom:-20px;')
						),style='margin-top:-10px;')
					)
					),style='margin-top:-10px;')
				)
			)
		)
	)
            ), tabPanel("Comparar tendencias",
            dashboardPage(
		dashboardHeader(disable=T
		),
		dashboardSidebar(disable = TRUE),
		dashboardBody(
			fluidRow(mainPanel(width='100%',
				div(radioGroupButtons(justified=T,inputId="categoryTabC",status="warning",choiceNames = c('Categorías','ONGs','Regiones'),choiceValues = c(1,2,3),selected=1),style='margin-top:-15px;'),
					div(selectInput("funTabC",NULL,choices= setNames(c(0,1,2,3),c('Seleccione algún criterio','Cantidad de personas','Frecuencia media','Coincidencia media')),selected=0 ),style='margin-top:-10px;'),
					column(width = 3,
						box(solidHeader=T,status="danger",title=div('Selección de ',textOutput("op_catC",inline=T),HTML('<i class="fa fa-question-circle"></i>'),title='Puede elegir máximo 3.'),width='100%',
						div(checkboxGroupInput(
							inputId="selectONGC",label=NULL,choices=setNames(1:2,c('1','2')),selected=NULL,width='100%'
							),width='100%',style='margin-top:-7px;margin-bottom:-10px;max-height: 160px;overflow-y: scroll;'
						)
						),
						box(solidHeader=T,status="primary",title=div('Top 5 más participativos',HTML('<i class="fa fa-question-circle"></i>'),title='Clic sobre nodo para ver nombre de usuarios.'),width='100%',
						div(uiOutput('top_w'),width='100%',style='margin-top:-5px;margin-bottom:-13px;'
						)
						)
					),
					box(solidHeader=T,status="danger",title=NULL,width=9,
					div(radioGroupButtons(justified=T,inputId="graphTabC",status="warning",choiceNames = c(paste(icon('facebook-square'),'Red de Comentarios'),paste(icon('facebook-square'),'Red de Me gusta')),choiceValues = c(1,2),selected=1),style='margin-top:-7px;'), 
					div(plotlyOutput("compare_graph"),style='margin-top:-14px;')
					)
				)
			)
		)
	)
            )
          )
)
