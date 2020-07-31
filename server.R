library(shiny);library(shinydashboard);library(visNetwork);library(shinyWidgets);library(shinyjs);library(shinybusy);library(plotly);

getMacroDir <- function() {
	return('prememo/')
}
getFacebookDir <- function(type, subtype) {
	if(type==1){ #Categorías
		if(subtype==1){
			dir<-'prememo/categoria_comment/'
		} else{
			dir<-'prememo/categoria_like/'
		}
	} else if(type==2){ #ONGs
		if(subtype==1){
			dir<-'prememo/ong_comment/'
		} else{
			dir<-'prememo/ong_like/'
		}
	} else if(type==3){ #Regiones
		if(subtype==1){
			dir<-'prememo/regiones_comment/'
		} else{
			dir<-'prememo/regiones_like/'
		}
	}
	return(dir)
}
getFacebookTab <- function(type) {
	if(type==1){
		return('Categoría')
	} else if(type==2){
		return('ONG')
	} else if(type==3){
		return('Región')
	}
}
getFacebookTabC <- function(type) {
	if(type==1){
		return('Categorías')
	} else if(type==2){
		return('ONGs')
	} else if(type==3){
		return('Regiones')
	}
}
getYlabelC <- function(type) {
	if(type==1){
		return('Cantidad de personas')
	} else if(type==2){
		return('Frecuencia media')
	} else if(type==3){
		return('Coincidencia media')
	}
}
server<-shinyServer(function(input, output, session) {
	toReturn<-reactiveValues(cluster_selected=NULL, nodes_data=NULL, edges_data=NULL, ventana_selected=NULL, type_op=NULL, change_graph=NULL, change_all=NULL, first_exec=NULL, no_render=NULL, description=NULL, stat_g=NULL, stat_n=NULL, n_pos=NULL, stat_g_metric=NULL,slider_aux=NULL)
	
	output$sliderTime <- renderUI({
		sliderTextInput(inputId="nVentana",label=NULL,grid=T,
		choices = c('1','2'),
		selected = '1')
	})

	output$network <- renderVisNetwork({
		if( (input$selectONG!='0') & is.null(toReturn$change_all) ){
			dir<-getMacroDir()
			a<-readRDS(file=paste(dir,'cat',input$categoryTab,'-',input$graphTab,'.Rda',sep=''))
			pos<-which(a$category==input$selectONG)
			if(length(pos)>0){
				toReturn$first_exec<-0
				toReturn$stat_n<-toReturn$stat_g<-NULL
				a<-a[pos,]$ventana
				updateSliderTextInput(session=session, inputId="nVentana", choices=a, selected=a[[1]])
				a<-readRDS(file=paste(dir,'cat',input$categoryTab,'.Rda',sep=''))
				toReturn$description<-a[which(a$category==input$selectONG),]$description
				toReturn$no_render<-0
				pos<-'0'
				toReturn$cluster_selected<-NULL
				visNetwork(data.frame(id=pos,group=0,is_cluster=0,level=0,hidden=T,stringsAsFactors=FALSE), data.frame(from=pos,to=pos,weight=0,hidden=T,title='0',width=0,id='0',color='',group=0,stringsAsFactors=FALSE), height = "500px", width = "100%") %>% visEdges(smooth = FALSE) %>% visEvents(type = "once", startStabilizing = "function() { this.moveTo({scale:0.1}) }") %>% visPhysics(stabilization = FALSE) %>%
					visEvents(click = "function(nodes) { Shiny.onInputChange('id_node', nodes.nodes);}") %>% 
					visEvents(doubleClick = "function(nodes) { Shiny.onInputChange('id_cluster', nodes.nodes);}") %>% visIgraphLayout(layout='layout_nicely') %>% 
  					visInteraction(navigationButtons = TRUE)
			}
		}
	})
	
	update_network<-reactive({
		if( !is.null(input$network_nodes) & !is.null(input$network_edges) & !is.null(input$id_cluster) & !is.null(toReturn$nodes_data) & !is.null(toReturn$edges_data) ){
			toReturn$n_pos<-toReturn$stat_n<-NULL
			a<-as.data.frame(input$network_nodes[input$id_cluster],stringsAsFactors=FALSE)
			a<-a[grepl('is_cluster$',colnames(a))]
			if(length(a)>0){if(a==1){
				toReturn$cluster_selected <- input$id_cluster
				nodes<-toReturn$nodes_data;edges<-toReturn$edges_data
				a<-as.integer(strsplit(toReturn$cluster_selected,'-')[[1]])
				cluster<-a[[1]]
				cont<-a[[2]]
				if(nchar(nodes[which(nodes$id==toReturn$cluster_selected),]$parent)>0){
					pp<-c(cluster,as.integer(strsplit(nodes[which(nodes$id==toReturn$cluster_selected),]$parent,';')[[1]]))
					pos<-which(nodes$is_cluster==1 & (nodes$group %in% pp) & !(nodes$id %in% paste(pp,'-',cont,sep='')) )
				}else{
					pos<-which(nodes$is_cluster==1 & ((nodes$level>0 & nodes$group==cluster) | (nodes$level==0 & nodes$id!=toReturn$cluster_selected)) )
				}
				fn<-function(a){
					a<-as.data.frame(a,stringsAsFactors=FALSE);a<-a[grepl('id$|group$|is_cluster$|level$',colnames(a))]
					return(a)
				}
				nodes_visual<-do.call("rbind",lapply(input$network_nodes, fn))
				if(nchar(nodes[which(nodes$id==toReturn$cluster_selected),]$parent)>0){
					pos<-which(!(nodes$id %in% paste(pp,'-',cont,sep='')) & (nodes$level==0 | (nodes$group %in% pp)) )
					nodes_remove<-nodes_visual[which( !(nodes_visual$group %in% pp) | (nodes_visual$id %in% paste(pp,'-',cont,sep='')) ),]$id
				}else{
					pp<-c(cluster)
					pos<-which((nodes$level>0 & nodes$group==cluster) | (nodes$level==0 & nodes$id!=toReturn$cluster_selected) )
					nodes_remove<-nodes_visual[which((nodes_visual$level>0 & nodes_visual$group!=cluster) | (nodes_visual$id==toReturn$cluster_selected) ),]$id
				}
				if(length(pos)>0){
					nodes<-nodes[pos,]
				}
				fn<-function(a){
					a<-as.data.frame(a[1:9],stringsAsFactors=FALSE);a<-a[grepl('id$|group$',colnames(a))]
					return(a)
				}
				edges_visual<-do.call("rbind",lapply(input$network_edges, fn))
				edges<-edges[which(edges$group %in% c(0,pp)),]
				ppid<-paste(pp,'-',cont,sep='')
				edges_remove<-edges[which((edges$from %in% ppid) | (edges$to %in% ppid)),]$id
				edges<-edges[which(!(edges$from %in% ppid) & !(edges$to %in% ppid)),]
				edges_remove<-c(edges_remove,edges_visual[which(!(edges_visual$group %in% c(0,pp))),]$id)
				pos3<-which(nodes$group==cluster & nodes$level>0)
				if(length(pos3)>0){
					i<-as.data.frame(input$network_nodes[toReturn$cluster_selected],stringsAsFactors=FALSE);j<-as.integer(i[grepl('y$',colnames(i))]);i<-as.integer(i[grepl('x$',colnames(i))])
					nodes[pos3,]$x<-nodes[pos3,]$x+i
					nodes[pos3,]$y<-nodes[pos3,]$y+j
				}
				pos3<-which((nodes$group==cluster & nodes$level>0) | (nodes$id!=paste(pp[[length(pp)]],'-',cont,sep='') & nodes$level==0) )
				pos2<-which(nodes$group!=cluster & nodes$level>0)
				visNetworkProxy("network") %>%
					visRemoveNodes(id = nodes_remove) %>%
					visUpdateEdges(edges = edges) %>%
					visUpdateNodes(nodes = nodes[pos3,1:11]) %>%
					visUpdateNodes(nodes = nodes[pos2,1:9])
			}}
		}
	})

	observeEvent(input$id_cluster, {
		toReturn$type_op <- 2
		if(!is.null(toReturn$nodes_data)){
			nodes<-toReturn$nodes_data[,1:9]
			pos<-which(nodes$id==input$id_cluster)
			if(length(pos)>0){
				if(nodes[pos,]$is_cluster==0){
					toReturn$type_op <- NULL
				}else{
					visNetworkProxy("network") %>%
					visGetNodes() %>%
					visGetEdges()
				}
			}else{
				visNetworkProxy("network") %>%
				visGetNodes() %>%
				visGetEdges()
			}
		}else{
			visNetworkProxy("network") %>%
			visGetNodes() %>%
			visGetEdges()
		}
	})

	observeEvent(input$id_node, {
		toReturn$stat_n<-input$id_node
	})

	observeEvent(input$FirstSlider, {
		toReturn$slider_aux<-input$FirstSlider
		if(toReturn$slider_aux>1){
			remove_modal_spinner()
		}
	})

	update_ventana<-reactive({
		if( !is.null(input$network_nodes) & !is.null(input$network_edges) & !is.null(toReturn$ventana_selected) & (input$selectONG!='0') ){
			show_modal_spinner(spin = "circle",color = "blue",text = "Está cargando...")
			toReturn$n_pos<-toReturn$stat_n<-NULL
			fn<-function(a){
				a<-as.data.frame(a,stringsAsFactors=FALSE);a<-a[grepl('id$|group$',colnames(a))]
				return(a)
			}
			nodes_visual<-do.call("rbind",lapply(input$network_nodes, fn))
			fn<-function(a){
				a<-as.data.frame(a[1:7],stringsAsFactors=FALSE);a<-a[grepl('from$|id$',colnames(a))]
				return(a)
			}
			edges_visual<-do.call("rbind",lapply(input$network_edges, fn))
			if(length(edges_visual)>0) { visNetworkProxy("network") %>% visRemoveEdges(id = edges_visual$id) }
			if(length(nodes_visual)>0) { visNetworkProxy("network") %>% visRemoveNodes(id = nodes_visual$id) }
			edges_visual<-nodes_visual<-NULL
			dir<-getFacebookDir(input$categoryTab,input$graphTab)
			nodes<-readRDS(file=paste(dir,'nodesA-',input$selectONG,'-',ifelse(input$graphTab==1,'Comments','Likes'),'-AdjMatrix-Period-',toReturn$ventana_selected,'.Rda',sep=''))
			edges<-readRDS(file=paste(dir,'edgesA-',input$selectONG,'-',ifelse(input$graphTab==1,'Comments','Likes'),'-AdjMatrix-Period-',toReturn$ventana_selected,'.Rda',sep=''))
			toReturn$stat_g_metric<-readRDS(file=paste(getFacebookDir(input$categoryTab,input$graphTab),'metric-',input$selectONG,'-',ifelse(input$graphTab==1,'Comments','Likes'),'-AdjMatrix-Period-',toReturn$ventana_selected,'.Rda',sep=''))
			pos<-which(nodes$is_cluster==1)
			if(length(pos)>0){
				nodes[pos,]$title<-paste('Grupo',1:length(pos))
			}
			toReturn$edges_data<-edges
			toReturn$nodes_data<-nodes
			toReturn$stat_g<-0
			edges<-edges[which(edges$group==0),]
			visNetworkProxy("network") %>%
				visUpdateEdges(edges = edges) %>%
				visUpdateNodes(nodes = nodes[which(nodes$level==0),1:11])
		}
	})

	update_com_like<-reactive({
		if(input$selectONG!='0'){
			dir<-getMacroDir()
			a<-readRDS(file=paste(dir,'cat',input$categoryTab,'.Rda',sep=''))
			choicesONG<-data.frame(id=c('0',a$category),names=c(paste('Seleccione una ',getFacebookTab(input$categoryTab),sep=''),a$title),stringsAsFactors=FALSE)
			updateSelectInput(session,"selectONG",choices=setNames(choicesONG$id,choicesONG$names),selected=ifelse(length(choicesONG$id[grepl(input$selectONG,choicesONG$id)])>0,input$selectONG,'0') )

			a<-readRDS(file=paste(dir,'cat',input$categoryTab,'-',input$graphTab,'.Rda',sep=''))
			a<-a[which(a$category==input$selectONG),]$ventana
			if(!is.null(toReturn$ventana_selected)){
				sel=ifelse(length(a[grepl(toReturn$ventana_selected,a)])>0,toReturn$ventana_selected,a[[1]])
			}else{
				sel=a[[1]]
			}
			updateSliderTextInput(session=session, inputId="nVentana", choices=a, selected=sel)
			if(sel== sort(c(a[[1]],toReturn$ventana_selected))[[1]] ){
				toReturn$ventana_selected<-sel
				update_ventana()
			}
			toReturn$change_graph<-NULL
		}
	})

	update_all<-reactive({
		dir<-getMacroDir()
		a<-readRDS(file=paste(dir,'cat',input$categoryTab,'.Rda',sep=''))
		choicesONG<-data.frame(id=c('0',a$category),names=c(paste('Seleccione una ',getFacebookTab(input$categoryTab),sep=''),a$title),stringsAsFactors=FALSE)
		updateSelectInput(session,"selectONG",choices=setNames(choicesONG$id,choicesONG$names),selected='0' )
		updateSliderTextInput(session=session, inputId="nVentana", choices=c('3','4'), selected='3')
		toReturn$change_all<-NULL
	})

	observeEvent(input$nVentana, {
		if(input$selectONG!='0'){
			toReturn$stat_g_metric <- toReturn$edges_data <- toReturn$nodes_data <- NULL
			toReturn$type_op <- 1
			toReturn$ventana_selected <- input$nVentana
			visNetworkProxy("network") %>%
			visGetNodes() %>%
			visGetEdges()
		}
	})

	observeEvent({
	input$network_edges
	input$network_nodes
	}, {
		if(!is.null(toReturn$change_all)){
			update_all()
		} else {
			if(!is.null(toReturn$change_graph)){
				update_com_like()
			} else {
				if(!is.null(toReturn$type_op)){
					if(toReturn$type_op==1){
						toReturn$type_op<-NULL
						update_ventana()
					} else if(toReturn$type_op==2){
						toReturn$type_op<-NULL
						update_network()
					}
				}
			}
		}
	})

	observeEvent(input$graphTab, {#print('........... o comment/like ...........')
		if(input$selectONG!='0'){
			toReturn$change_graph <- 0
			toReturn$stat_g_metric <- toReturn$edges_data <- toReturn$nodes_data <- NULL
			visNetworkProxy("network") %>%
			visGetNodes() %>%
			visGetEdges()
		}
	})

	observeEvent(input$categoryTab, {#print('........... o all_options ...........')
		if(!is.null(toReturn$first_exec)){
			toReturn$change_all <- 0
			toReturn$stat_g_metric <- toReturn$edges_data <- toReturn$nodes_data <- NULL
			visNetworkProxy("network") %>%
			visGetNodes() %>%
			visGetEdges()
		}
		if(is.null(toReturn$no_render)){
			update_all()
		}
	})

	observeEvent(input$selectONG, {
		if(input$selectONG!='0'){
			dir<-getFacebookDir(input$categoryTab,input$graphTab)
			a<-list.files(dir);a<-a[grepl('.Rda$',a)]
			if(length(a[grepl(paste('^edgesA\\-',input$selectONG,'\\-',sep=''),a)])>0){
				a<-a[grepl(paste('^edgesA\\-',input$selectONG,'\\-',sep=''),a)];a<-sub('.*\\-Period\\-','',sub('.Rda$','',a))
				if(!is.null(toReturn$ventana_selected)){if(toReturn$ventana_selected==a[[1]]){
					toReturn$type_op<-1
					visNetworkProxy("network") %>%
						visGetNodes() %>%
						visGetEdges()
				}}
			}
		}
		toReturn$no_render<-NULL
	})

	output$desc<-renderText({
		toReturn$description
	})

	observeEvent(input$selectNodo, {
		toReturn$stat_n<-input$selectNodo
		toReturn$n_pos<-input$selectNodo
	})

	output$statG<-renderUI({
		if((input$selectONG!='0') & !is.null(toReturn$stat_g) & !is.null(toReturn$nodes_data) & !is.null(toReturn$stat_g_metric)){
			nodes<-toReturn$nodes_data[,1:9]
			nodes<-nodes[which(nodes$is_cluster==0),]
			if(nrow(nodes)>4000){
				a<-which(nodes$is_ong==1);a<-a[which(a>4000)]
				nodes<-nodes[c(1:4000,a),]
			}
			updateSelectizeInput(session,'selectNodo',label=NULL, selected='',choices=setNames(nodes$id,nodes$title), server=F)
			showElement('selectNodo',time=0)
				if(isolate(toReturn$slider_aux)==1){
					updateSliderInput(session,"FirstSlider",value=2)
				}else{
					if(isolate(input$FirstSlider)==2){
						updateSliderInput(session,"FirstSlider",value=3)
					}else{
						updateSliderInput(session,"FirstSlider",value=2)
					}
				}
            a<-toReturn$stat_g_metric
            HTML(paste('<table id="statG" class="table shiny-table table-striped table-hover table-bordered spacing-s" style="width:100%;" align="center"><tbody><tr><td align="left">Nodos</td><td align="left">',a[[1]],'</td></tr><tr><td align="left">Arcos</td><td align="left">',a[[2]],'</td></tr><tr><td align="left">ONGs</td><td align="left">',a[[3]],'</td></tr><tr title="A mayor valor, mayor es la densidad de la red."><td align="left">Densidad</td><td align="left">',a[[4]],'</td></tr><tr title="A mayor valor, más conectados están los nodos."><td align="left">Transitividad</td><td align="left">',a[[5]],'</td></tr><tr title="Cantidad de arcos promedio de los nodos."><td align="left">Grado medio</td><td align="left">',a[[6]],'</td></tr><tr title="Cantidad de arcos máxima de un nodo."><td align="left">Grado máximo</td><td align="left">',a[[7]],'</td></tr><tr title="Suma promedio de los pesos de los arcos de los nodos."><td align="left">Grado pesado medio</td><td align="left">',a[[8]],'</td></tr><tr title="Suma máxima de los pesos de los arcos de un nodo."><td align="left">Grado pesado máximo</td><td align="left">',a[[9]],'</td></tr></tbody></table>',sep='')
	            )
		}else{hideElement('selectNodo',time=0)}
	})

	output$name_statN<-renderText({
		if((input$selectONG!='0') & !is.null(toReturn$stat_n) & !is.null(toReturn$nodes_data) & !is.null(toReturn$ventana_selected)){
			a<-toReturn$nodes_data[,1:9]
			pos<-which(a$id==toReturn$stat_n)
			if(length(pos)>0){if(a[pos,]$is_cluster==0){
				a[pos,]$title
			}}
		}
	})

	output$statN<-renderUI({
		if((input$selectONG!='0') & !is.null(toReturn$stat_n) & !is.null(toReturn$nodes_data) & !is.null(toReturn$ventana_selected)){
			a<-toReturn$nodes_data
			pos<-which(a$id==toReturn$stat_n)
			if(length(pos)>0){if(a[pos,]$is_cluster==0){
				HTML(paste('<table id="statN" class="table shiny-table table-striped table-hover table-bordered spacing-s" style="width:100%;" align="center"><tbody><tr title="Cantidad de arcos de un nodo."><td align="left">Grado</td><td align="left">',format(a[pos,]$degree, digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3),'</td></tr><tr title="Suma de los pesos asociados a los arcos de un nodo."><td align="left">Grado pesado</td><td align="left">',format(a[pos,]$w_degree, digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3),'</td></tr></tbody></table>',sep='')
	            )
			}else{NULL}
			}
		}
	})

	output$posNode<-renderText({
		if(!is.null(toReturn$n_pos) & !is.null(toReturn$nodes_data)){
			a<-toReturn$nodes_data
			posu<-pos<-which(a$id==toReturn$n_pos)
			if(length(pos)>0){
				gro<-pos<-a[pos,]$group
				if(pos==0){
					visNetworkProxy("network") %>% visFit(nodes=c(a[posu,]$id),animation=F)
					'  Presente, no en grupo.'
				}
				else{
					conto<-as.integer((strsplit(toReturn$n_pos,'-')[[1]])[[2]])
					pos<-which(a$id==paste(pos,'-',conto,sep=''))
					if(!is.null(toReturn$cluster_selected)){
						b<-as.integer(strsplit(toReturn$cluster_selected,'-')[[1]])
						cluster<-b[[1]]
						cont<-b[[2]]
						if(cont==conto){
							ch_f<-T
							ch_pp<-c(as.integer(strsplit(a[pos,]$children,';')[[1]]))
							if(length(ch_pp)>0){if(cluster %in% ch_pp){
								ch_f<-F
							}}
							b<-which(a$id==toReturn$cluster_selected)
							if(ch_f){
								if(nchar(a[pos,]$parent)>0){
									pp<-c(gro,as.integer(strsplit(a[pos,]$parent,';')[[1]]))
									if(cluster %in% pp){
										pos2<-match(cluster,pp)
										if(pos2==1){
											visNetworkProxy("network") %>% visFit(nodes=c(a[posu,]$id),animation=F)
											'  Presente en el lienzo.'
										}else{
											pos2<-which(a$id==paste(pp[pos2-1],'-',conto,sep=''))
											visNetworkProxy("network") %>% visFit(nodes=c(a[pos2,]$id),animation=F)
											paste('  Abrir ',a[pos2,]$title,' (faltan ',a[pos,]$level-a[pos2,]$level+1,' grupos por abrir).',sep='')
										}
									}else{
										pp<-which(a$id==paste(pp[length(pp)],'-',conto,sep=''))
										visNetworkProxy("network") %>% visFit(nodes=c(a[pp,]$id),animation=F)
										paste('  Abrir ',a[pp,]$title,' (faltan ',a[pos,]$level+1,' grupos por abrir).',sep='')
									}
								}else{
									if(pos==b){
										visNetworkProxy("network") %>% visFit(nodes=c(a[posu,]$id),animation=F)
										'  Presente en el lienzo.'
									}else{
										visNetworkProxy("network") %>% visFit(nodes=c(a[pos,]$id),animation=F)
										paste('  Abrir ',a[pos,]$title,'.',sep='')
									}
								}
							}else{
								visNetworkProxy("network") %>% visFit(nodes=c(a[posu,]$id),animation=F)
								'  Presente en el lienzo.'
							}
						}else{
							if(nchar(a[pos,]$parent)>0){
								pp<-c(as.integer(strsplit(a[pos,]$parent,';')[[1]]))
								pp<-which(a$id==paste(pp[length(pp)],'-',conto,sep=''))
								visNetworkProxy("network") %>% visFit(nodes=c(a[pp,]$id),animation=F)
								paste('  Abrir ',a[pp,]$title,' (faltan ',a[pos,]$level+1,' grupos por abrir).',sep='')
							}else{
								visNetworkProxy("network") %>% visFit(nodes=c(a[pos,]$id),animation=F)
								paste('  Abrir ',a[pos,]$title,'.',sep='')
							}
						}
					}else{
						if(nchar(a[pos,]$parent)>0){
							pp<-c(as.integer(strsplit(a[pos,]$parent,';')[[1]]))
							pp<-which(a$id==paste(pp[length(pp)],'-',conto,sep=''))
							visNetworkProxy("network") %>% visFit(nodes=c(a[pp,]$id),animation=F)
							paste('  Abrir ',a[pp,]$title,' (faltan ',a[pos,]$level+1,' grupos por abrir).',sep='')
						}else{
							visNetworkProxy("network") %>% visFit(nodes=c(a[pos,]$id),animation=F)
							paste('  Abrir ',a[pos,]$title,'.',sep='')
						}
					}
				}
			}else{''}
		}
	})

	output$listONG<-renderUI({
		if(!is.null(toReturn$stat_g) & !is.null(toReturn$nodes_data)){
			nodes<-toReturn$nodes_data[,1:9]
			pos<-which(nodes$is_ong==1)
			if(length(pos)>0){
				nodes<-nodes[pos,]
				HTML(paste(paste('<p><span class="dot" style="background-color: ',nodes$color,'"></span> ',nodes$title,'</p>',sep=''),collapse=''))
			}else{'No hay.'}
		}
	})

	output$n_listONG<-renderText({
		if(!is.null(toReturn$stat_g) & !is.null(toReturn$nodes_data) & !is.null(toReturn$stat_g_metric)){
			a<-toReturn$stat_g_metric
			paste(' (',a[[3]],')',sep='')
		}
	})

	observeEvent(input$aboutGraphs, {
		showModal(
			modalDialog(
				withTags({
				HTML('<p>Las redes disponibles en este sistema son redes en donde los <b>nodos</b> tienen los nombres de <b>cuentas de Facebook</b>, aquellos nodos conectados por un <b>arco</b> (o relación) son los que realizaron una <b>misma acción</b> (que puede ser escribir un comentario o dar "me gusta") sobre una publicación realizada por una ONG en Facebook.</p><p>Y los arcos tienen un <b>peso</b> cuyo valor es proporcional a las veces que la relación se produjo en un periodo de <b>tiempo</b>, un arco es más transparente mientras menor sea su peso.</p><p>En estas redes hay nodos de dos tipos: los nodos que son personas u ONGs con cuentas de Facebook, y nodos que son <b>grupos</b>, los cuales son un <b>grupo de nodos</b>. Al abrir un grupo se muestran los nodos que dicho grupo tiene.</p><br/><h4 align="center">Características interactivas de las redes</h4><table id="infoTab" class="table shiny-table table-striped table-hover table-bordered spacing-s" style="width:auto;" align="center"><tbody><tr><td align="left">Doble clic sobre un nodo</td><td align="left">Para abrir un grupo.</td></tr> <tr><td align="left">Clic sobre un nodo</td><td align="left">Para ver medidas asociadas a un nodo.</td></tr> <tr><td align="left">Deslizamiento vertical en lienzo</td><td align="left">Para achicar o agrandar la red.</td></tr> <tr><td align="left">Arrastrar y soltar nodo</td><td align="left">Para mover un nodo.</td></tr> <tr><td align="left">Arrastrar y soltar lienzo</td><td align="left">Para desplazarse en la red.</td></tr> <tr><td align="left">Pasar sobre un nodo</td><td align="left">Para ver nombre del nodo.</td></tr> <tr><td align="left">Pasar sobre un arco</td><td align="left">Para ver peso del arco.</td></tr> 
</tbody> </table>')
				}),
				title = div(span('Información sobre estas redes', style="color:white"),HTML('<button type="button" class="btn btn-primary xclose" data-dismiss="modal" align="right"><span style="color:white;font-size:25px;position: absolute;right:6px;margin: -19px 0 0 0;padding: 1px;">&times<span></button>')),
			footer=NULL,size="m",easyClose=T)
		)
    })

######################## COMPARE ########################
	toReturnC<-reactiveValues(choices_sel=NULL,date_format=NULL,line_data=NULL,show_topw=NULL)

    observeEvent(input$categoryTabC, {
    	toReturnC$show_topw<-NULL
    	if(is.null(toReturnC$date_format)){
    		toReturnC$date_format<-data.frame(num=(a<-0:23),date=paste(2008+as.integer(a/2),'-',a%%2+1,sep=''),stringsAsFactors=F)
    	}
		if(!is.null(input$categoryTabC)){
			a<-readRDS(file=paste(getMacroDir(),'cat',input$categoryTabC,'.Rda',sep=''))
			updateCheckboxGroupInput(session, "selectONGC", choices=setNames(a$category,a$title))
			toReturnC$choices_sel<-NULL
			toReturnC$line_data<-readRDS(file=paste(getMacroDir(),'compare',input$categoryTabC,'-',ifelse(is.null(input$graphTabC),1,input$graphTabC),'.Rda',sep=''))
		}
	})

	observeEvent(input$graphTabC, {
		toReturnC$show_topw<-NULL
		toReturnC$line_data<-readRDS(file=paste(getMacroDir(),'compare',input$categoryTabC,'-',input$graphTabC,'.Rda',sep=''))
	})

	observeEvent(input$checkboxC_clicked, {
		a<-strsplit(input$checkboxC_clicked,';')[[1]]
		if(a[[2]]=='0'){
			if(!is.null(toReturnC$choices_sel)){
				b<-toReturnC$choices_sel
				b<-b[which(b!=a[[1]])]
				toReturnC$choices_sel<-b
			}
		}else{
			toReturnC$choices_sel<-head(c(a[[1]],toReturnC$choices_sel),3)
			updateCheckboxGroupInput(session, "selectONGC", selected=toReturnC$choices_sel)
		}
	})

	observeEvent(input$selectONGC, {
		if(!is.null(input$categoryTabC) && !is.null(input$selectONGC) && (input$funTabC != 0)){
			dir<-getMacroDir()
			a<-readRDS(file=paste(dir,'cat',input$categoryTabC,'.Rda',sep=''))
		}
	})

	output$op_catC<-renderText({
		if(!is.null(input$categoryTabC)){
			getFacebookTabC(input$categoryTabC)
		}
	})

	output$compare_graph <- renderPlotly({
		if(!is.null(input$categoryTabC) && !is.null(toReturnC$choices_sel) && (input$funTabC != 0)){if(length(toReturnC$choices_sel) > 0){
			a<-toReturnC$line_data
			date<-toReturnC$date_format
			if(input$funTabC != 0){
				a<-a[which(a$id %in% toReturnC$choices_sel),]
			}
			if(input$funTabC == 1){
				p<-ggplot(data=a, aes(x=num,y=personas,colour=name,group=1,text=paste(personas,' personas<br>Semestre: ',2008+as.integer(num/2),'-',num%%2+1,sep=''))) + geom_line()+geom_point()+xlab("Semestre")+ylab(getYlabelC(input$funTabC))
			}else if(input$funTabC == 2){
				p<-ggplot(data=a, aes(x=num,y=w_med,colour=name,group=1,text=paste('Valor: ',w_med,'<br>Semestre: ',2008+as.integer(num/2),'-',num%%2+1,sep=''))) + geom_line()+geom_point()+xlab("Semestre")+ylab(getYlabelC(input$funTabC))
			}else{
				p<-ggplot(data=a, aes(x=num,y=d_med,colour=name,group=1,text=paste('Valor: ',d_med,'<br>Semestre: ',2008+as.integer(num/2),'-',num%%2+1,sep=''))) + geom_line()+geom_point()+xlab("Semestre")+ylab(getYlabelC(input$funTabC))
			}
			b<-c(min(a$num),max(a$num));if((b[2]-b[1])>1){b<-seq(b[1],b[2],2)};p<-p+scale_x_continuous(breaks=b,labels=date[match(b, date$num),]$date)+labs(colour='')
			ggplotly(p, tooltip = "text", source='lgbtn_node') %>% event_register('plotly_click')
		}}
	})

	observeEvent(event_data("plotly_click", source="lgbtn_node",session=session), {
		if(!is.null(input$categoryTabC) && !is.null(toReturnC$choices_sel) && (input$funTabC != 0)){if(length(input$selectONGC) > 0){
			a<-toReturnC$line_data
			p<-event_data("plotly_click", source="lgbtn_node",session=session)
			toReturnC$show_topw<-c(p$curveNumber, p$x)
		}}
	})

	output$top_w<-renderUI({
		if(!is.null(input$categoryTabC) && !is.null(input$selectONGC) && !is.null(toReturnC$show_topw)){
			p<-toReturnC$show_topw
			a<-toReturnC$line_data
			sel<-input$selectONGC;sel<-sel[p[[1]]+1]
			a<-a[which((a$num==p[[2]]) & (a$id==sel)),]$u_w
			HTML(paste('<p>',gsub(';','</p><p>',a),'</p>',sep=''))
		}
	})

})