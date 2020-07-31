####################################################################
# PASOS PARA REALIZAR PREPROCESAMIENTO (RESPETAR EL ORDEN)
####################################################################
# 1. Ejecutar makeRDA() para convertir los archivos .graphml en .Rda
# 2. Ejecutar processNE() para obtener los data frames de nodos y arcos a partir de los GraphML, además de la matriz de distancias.
# 3. Ejecutar processC() para obtener la jerarquia de clusters, además de los valores de silhouette asociados.
# 4. Ejecutar procNodesA() para obtener las posiciones de los nodos, así como la lista de padres e hijos de cada cluster.
# 5. Ejecutar procEdgesA() para obtener el grupo de cada arco.
# 6. Ejecutar procMetric() para obtener las medidas mostradas en la pestaña de "Ver red".
# 7. Ejecutar procNode() para agregar el degree y weighted degree de cada nodo mostrado en la pestaña de "Ver red".
# 8. Ejecutar genCat() para permitir el acceso de cada item perteneciente a cierto tipo de red.
# 9. Ejecutar genCatSub() para indicar los grafos correspondientes a cada item (de cada tipo de red).
# 10. Ejecutar procCompare() para obtener las medidas mostradas en la pestaña "Comparar tendencias".
####################################################################
library(igraph)
makeRDA<-function(){
	dirs<-c('Desktop/preproc/categoria_comment/'
	,'Desktop/preproc/categoria_like/'
	,'Desktop/preproc/regiones_comment/'
	,'Desktop/preproc/regiones_like/')
	for(dir in dirs){
		a<-list.files(dir);a<-a[grepl('.graphml$',a)]
		for(i in a){
			aux<-read_graph(paste(dir,i,sep=''),format='graphml')
			saveRDS(aux,file=paste(dir,sub('\\..*','',i),'.Rda',sep=''))
	}}
}
makeRDA()
####################################################################
library(igraph);library(cluster)
processNE<-function(){
	dirs<-c('Desktop/preproc/ong_comment/'
	,'Desktop/preproc/ong_like/')
	#dirs<-c('Desktop/preproc/categoria_comment/'
	#,'Desktop/preproc/categoria_like/')
	#dirs<-c('Desktop/preproc/regiones_comment/'
	#,'Desktop/preproc/regiones_like/')
	ong_colors<-readRDS(file='Desktop/preproc/ong_colors.Rda')
	cont_a<-0;xzxz<-''
	for(dir in dirs){
		aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^Dataset\\-',aa)]
		for(ii in aa){
			aux<-readRDS(file=paste(dir,ii,sep=''))
			azaz<-sub('\\-.*','',sub('[a-zA-Z]*\\-','',ii))
			if(xzxz != azaz){
				xzxz<-azaz
				cont_a<-ifelse(length(dir[grepl('_like/$',dir)])>0,50,0)
			}
			a<-get.data.frame(aux, what="both")
			nodes<-a[[1]];colnames(nodes)<-c('id','id2')
			nodes<-data.frame(id=paste(nodes$id2,'-',cont_a,sep=''),title=nodes$id,stringsAsFactors=FALSE)
			nodes$size<-20;nodes$level<-1;nodes$color<-'#97C2FC'
			edges<-a[[2]];edges$title<-edges$weight
			a<-unique(nodes$title)
			for(j in a){
				b=nodes[which(nodes$title==j),]$id[[1]]
				pos=which(edges$from==j)
				if(length(pos)>0){
					edges[pos,]$from<-b
				}
				pos=which(edges$to==j)
				if(length(pos)>0){
					edges[pos,]$to<-b
				}
			}
			nodes$is_ong<-0
			for(j in 1:nrow(ong_colors)){
				pos=which( (nodes$title==ong_colors[j,]$Organización) | (nodes$title==ong_colors[j,]$nombreONGFB) )
				if(length(pos)>0){
					nodes[pos,]$color<-ong_colors[j,]$color
					nodes[pos,]$size<-25
					nodes[pos,]$is_ong<-1
				}
			}
			cont_a<-cont_a+1
			aux<-graph.data.frame(edges, directed=FALSE, vertices=nodes)
			fn<-function(a){
				peso=as.integer(10*as.double(a['weight']))*.1
				a['color']<-paste('rgba(151,194,252,',ifelse(peso<.3,.2, ifelse(peso>1,1,peso)),')',sep='')
				return(a['color'])
			}
			if(ecount(aux)>0){
				edges$width<-13;edges$hidden<-FALSE;edges$id<-paste(edges$from,'-',edges$to,sep='');edges$color<-'#97C2FC'
				edges$color<-apply(edges, 1, fn)
				d<-distances(aux,v=V(aux),weights=1/E(aux)$weight)
				saveRDS(d,file=paste(dir,'dist-',sub('^Dataset\\-','',ii),sep=''))
				d[is.infinite(d)]<-10^200
				cont_groups<-max(cluster_fast_greedy(aux,weights=E(aux)$weight)$membership);a<-walktrap.community(aux,weights=E(aux)$weight);tree<-as.hclust(a);a<-cutree(tree,k=cont_groups);nodes$group<-a;a<-as.data.frame(rep(table(a)));a<-cbind(a,as.integer(rownames(a)));colnames(a)<-c('Freq','Community.sizes');max_size<-max(a$Freq)
			}
			saveRDS(edges,file=paste(dir,'edges-',sub('^Dataset\\-','',ii),sep=''))
			sigue<-FALSE;grupos_transform<-c();max_cluster<-100;min_cluster<-3
			if(ecount(aux)>0){
				if(max_size > max_cluster) { sigue<-TRUE;grupos_transform<-unique(a[which(a$Freq>max_cluster),]$Community.sizes) }
				for(i in a[which(a$Freq < min_cluster),]$Community.sizes){
					nodes[which(nodes$group==i),]$group<-0
				}
			} else {
				nodes$group<-0
			}
			a<-which(nodes$group==0)
			if(length(a)>0) { nodes[a,]$level<-0 }
			nodes$is_cluster<-0
			nodes$hidden<-FALSE
			saveRDS(nodes,file=paste(dir,'nodes2-',sub('^Dataset\\-','',ii),sep=''))
		}
	}
}
processNE()
####################################################################
library(igraph);library(cluster)
processC<-function(){
	dirs<-c('Desktop/preproc/ong_comment/'
	,'Desktop/preproc/ong_like/')
	#dirs<-c('Desktop/preproc/categoria_comment/'
	#,'Desktop/preproc/categoria_like/'
	#,'Desktop/preproc/regiones_comment/'
	#,'Desktop/preproc/regiones_like/')
	for(dir in dirs){
		aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^nodes2\\-',aa)]#;if(dir==dirs[[1]]){aa<-aa[13:length(aa)]}
		for(ii in aa){
			nodes<-readRDS(file=paste(dir,ii,sep=''))
			edges<-readRDS(file=paste(dir,'edges-',sub('^nodes2\\-','',ii),sep=''))
			aux<-graph.data.frame(edges, directed=FALSE, vertices=nodes)
			if(ecount(aux)>0){
				d<-readRDS(file=paste(dir,'dist-',sub('^nodes2\\-','',ii),sep=''))
				d[is.infinite(d)]<-10^200
				cont_groups<-max(cluster_fast_greedy(aux,weights=E(aux)$weight)$membership);a<-walktrap.community(aux,weights=E(aux)$weight);tree<-as.hclust(a);a<-cutree(tree,k=cont_groups);nodes$group<-a;a<-as.data.frame(rep(table(a)));a<-cbind(a,as.integer(rownames(a)));colnames(a)<-c('Freq','Community.sizes');max_size<-max(a$Freq)
			}
			sigue<-FALSE;grupos_transform<-c();max_cluster<-100;min_cluster<-3
			if(ecount(aux)>0){
				if(max_size > max_cluster) { sigue<-TRUE;grupos_transform<-unique(a[which(a$Freq>max_cluster),]$Community.sizes) }
				for(i in a[which(a$Freq < min_cluster),]$Community.sizes){
					nodes[which(nodes$group==i),]$group<-0
				}
			} else {
				nodes$group<-0
			}
			a<-which(nodes$group==0);aux<-edges<-NULL
			if(length(a)>0) { nodes[a,]$level<-0 }
			a<-unique(nodes$group);a<-a[which(a!=0)]
			if(length(a)>0) {
				group_nodes<-data.frame(matrix(ncol=length(colnames(nodes)), nrow=length(a) ));colnames(group_nodes)<-colnames(nodes)
				group_nodes$is_cluster<-1;group_nodes$group<-a;group_nodes$id<-a;group_nodes$title<-paste('Cluster ',1:length(a),sep='')
				group_nodes$color<-'#97C2FC';group_nodes$level<-0;group_nodes$is_ong<-0
				for(i in 1:length(a)){
					ids=nodes[which(nodes$group==a[[i]]),]$id
					group_nodes[i,]$size<-35+as.integer(60*log10(length(ids)))
				}
				nodes$hidden<-FALSE;group_nodes$hidden<-FALSE
				sub_nodes<-list();sub_i<-0;level_i<-1
				b=silhouette(cutree(tree,k=cont_groups),dmatrix=d)
				if(sigue){b=b[which(nodes$group %in% grupos_transform),3]}
				silsil<-ifelse(sigue, c(mean(b[!is.na(b)])), c(summary(b)$si.summary['Mean']));silsil_i<-1
				while(sigue){
					if(nrow(nodes)<500){
						choose_k<-(5:1)*10+40+cont_groups
					} else if(nrow(nodes)<10000){
						choose_k<-(5:1)*10+50+cont_groups
					} else{
						choose_k<-(3:1)*10+70+cont_groups
					}
					choose_k<-choose_k[which(choose_k<nrow(nodes))]
					if(length(choose_k)==0){ break }
					sil_val<-c()
					for(i in 1:length(choose_k)){
						a<-cutree(tree,k=choose_k[[i]])
						aux<-nodes$group
						b=max(nodes$group)
						for(j in grupos_transform){
							pos=which(nodes$group==j)
							if(length(pos)>0){
								aux[pos]<-a[pos]+b
							}
						}
						b=silhouette(aux,dmatrix=d)
						b=b[which(nodes$group %in% grupos_transform),3]
						sil_val[[i]]<-mean(b[!is.na(b)])
					}
					k_val<-choose_k[ which(sil_val==max(sil_val)) ];if(length(k_val)>1){k_val<-k_val[[1]]}
					silsil[(silsil_i<-silsil_i+1)]<-max(sil_val)
					cont_groups<-max(nodes$group);a<-cutree(tree,k=k_val)
					for(i in grupos_transform){
						pos=which(nodes$group==i)
						if(length(pos)>0){
							nodes[pos,]$level<-level_i+1
							nodes[pos,]$group<-a[pos]+cont_groups
							b=unique(a[pos])+cont_groups
							j_title<-0
							for(j in 1:length(b)){
								pos=which(nodes$group==b[[j]])
								ids=nodes[pos,]$id
								if(length(ids) < min_cluster){
									nodes[pos,]$group<-i
									nodes[pos,]$level<-level_i
								}else{
									sub_nodes[[(sub_i<-sub_i+1)]]<-data.frame(is_cluster=ifelse(length(b)==1,0,1),group=i,id=b[[j]],title=paste(group_nodes[which(group_nodes$group==i),]$title,'.',(j_title<-j_title+1),sep=''),color='#97C2FC',is_ong=0,level=level_i,size=( 35+as.integer(60*log10(length(ids))) ),hidden=FALSE,stringsAsFactors=FALSE)
								}
							}
						}
					}
					a<-nodes$group;a<-as.data.frame(rep(table(a)));a<-cbind(a,as.integer(rownames(a)));colnames(a)<-c('Freq','Community.sizes')
					cont_groups<-k_val;level_i<-level_i+1
					grupos_transform<-c();pos=which( (a$Freq>max_cluster) & (a$Community.sizes!=0) )
					if(length(pos)>0){ grupos_transform<-unique(a[pos,]$Community.sizes) }
					if(length(grupos_transform)==0){ break }
				}
				saveRDS(silsil,file=paste(dir,'sil-',sub('^nodes2\\-','',ii),sep=''))
				if(length(sub_nodes)>0){
					sub_nodes<-do.call("rbind", lapply(sub_nodes, as.data.frame))
					pos=which(sub_nodes$is_cluster==0)
					if(length(pos)>0){
						ids=sub_nodes[pos,]$id;ids=ids[length(ids):1]
						for(i in ids){
							pos=which(sub_nodes$group==i)
							if(length(pos)>0){
								sub_nodes[pos,]$group<-sub_nodes[which(sub_nodes$id==i),]$group
								sub_nodes[pos,]$title<-sub_nodes[which(sub_nodes$id==i),]$title
								sub_nodes[pos,]$level<-sub_nodes[which(sub_nodes$id==i),]$level
							}
							pos=which(nodes$group==i)
							if(length(pos)>0){
								nodes[pos,]$group<-sub_nodes[which(sub_nodes$id==i),]$group
								nodes[pos,]$level<-sub_nodes[which(sub_nodes$id==i),]$level
							}
						}
						sub_nodes<-sub_nodes[which(!(sub_nodes$id %in% ids)),]
					}
					ids=unique(sub_nodes[grepl('^\\.',sub_nodes$title),]$group)
					for(i in ids){
						pos=which( (sub_nodes$group==i) & (sub_nodes$id!=i) )
						sub_nodes[pos,]$title<-paste(sub_nodes[which(sub_nodes$id==i),]$title,sub_nodes[pos,]$title,sep='')
					}
					nodes<-do.call("rbind", lapply(list(nodes,group_nodes,sub_nodes), as.data.frame))
				}else{
					nodes<-do.call("rbind", lapply(list(nodes,group_nodes), as.data.frame))
				}
			}
			saveRDS(nodes,file=paste(dir,'nodes-',sub('^nodes2\\-','',ii),sep=''))
		}
	}
}
processC()
####################################################################
library(igraph)
procNodesA<-function(){
	dirs<-c('Desktop/preproc/ong_comment/'
	,'Desktop/preproc/ong_like/')
	#dirs<-c('Desktop/preproc/categoria_comment/'
	#,'Desktop/preproc/categoria_like/'
	#,'Desktop/preproc/regiones_comment/'
	#,'Desktop/preproc/regiones_like/')
	for(dir in dirs){
		aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^nodes\\-',aa)];#if(dir==dirs[[1]]){aa<-aa[1:length(aa)]}
		for(ii in aa){
			nodes<-readRDS(file=paste(dir,ii,sep=''))
			edges2<-readRDS(file=paste(dir,'edges-',sub('^nodes\\-','',ii),sep=''))
			cont<-sub('\\-.*','',sub('^n\\d+\\-','',nodes[1,]$id))
			pos<-which(nodes$is_cluster==1)
			if(length(pos)>0){
				nodes[pos,]$id<-paste(nodes[pos,]$id,'-',cont,sep='')
				for(i in pos){
					nodes[i,]$level<-nchar(nodes[i,]$title)-nchar(gsub('\\.','',nodes[i,]$title))
				}
			}
			nodes$x<-NA;nodes$y<-NA

			nodes$children<-nodes$parent<-''
			if(length(pos)>0){
				a<-max(nodes$level)
				for(i in pos){
					if(nodes[i,]$level<a){
						ids<-c()
						for(j in (nodes[i,]$level+1):a){
							pos2<-which(nodes$is_cluster==1 & nodes$level==j & (nodes$group %in% c(ids,as.integer(sub('\\-.*','',nodes[i,]$id)))) )
							if(length(pos2)>0){
								ids<-c(ids,as.integer(sub('\\-.*','',nodes[pos2,]$id)))
							}else{break}
						}
						nodes[i,]$children<-gsub('\\, ',';',toString(ids,sep=';'))
					}
					if(nodes[i,]$level>0){
						ids<-c(nodes[i,]$group)
						for(j in (nodes[i,]$level-1):0){
							pos2<-which(nodes$is_cluster==1 & nodes$level==j & (nodes$id %in% paste(ids,'-',cont,sep='')) )
							if(length(pos2)>0){
								ids<-c(ids,as.integer(sub('\\-.*','',nodes[pos2,]$group)))
							}
						}
						nodes[i,]$parent<-gsub('\\, ',';',toString(unique(ids),sep=';'))
					}
				}
			}

			edges<-edges2;pos<-which(nodes$level==0 & nodes$is_cluster==1)
			if(length(pos)>0){
				for(i in pos){
					a<-c(as.integer(sub('\\-.*','',nodes[i,]$id)))
					if(nchar(nodes[i,]$children)>0){
						a<-c(a,as.integer(strsplit(nodes[i,]$children,';')[[1]]))
					}
					pos2<-which(nodes$level>0 & nodes$is_cluster==0 & (nodes$group %in% a))
					if(length(pos2)>0){
						ids=nodes[pos2,]$id
						pos2<-which(edges$from %in% ids)
						if(length(pos2)>0){edges[pos2,]$from<-nodes[i,]$id}
						pos2<-which(edges$to %in% ids)
						if(length(pos2)>0){edges[pos2,]$to<-nodes[i,]$id}
						ids=c();pos2=which(edges$from==nodes[i,]$id)
						if(length(pos2)>0) { ids=edges[pos2,]$to }
						pos2=which(edges$to==nodes[i,]$id)
						if(length(pos2)>0) { ids=c(ids,edges[pos2,]$from) }
						ids=unique(ids)
						if(length(ids)>0){
							ids=ids[which(ids!=nodes[i,]$id)]
							for(j in ids){
								pos2=which( ((edges$from==j) & edges$to==nodes[i,]$id) | ((edges$to==j) & edges$from==nodes[i,]$id) )
								if(length(pos2)>0){
									peso=median(edges[pos2,]$weight)*length(pos2)
									edges[pos2,]$weight<-peso
								}
							}
						}
					}
				}
			}
			edges<-edges[which(!duplicated(edges[c('from','to')]) & (edges$from!=edges$to) ),]
			if(nrow(edges)>0){
				edges<-unique(get.data.frame(graph.data.frame(edges, directed=FALSE),"edges"))
			}
			pos<-which(nodes$level==0)
			a <- graph.data.frame(edges, directed=FALSE, vertices=nodes[pos,])
			a<-layout_with_graphopt(a)
			nodes[pos,]$x<-as.integer(a[,1])*10
			nodes[pos,]$y<-as.integer(a[,2])*10

			####### DEMAS x y
			pos<-which(nodes$is_cluster==1)
			if(length(pos)>0){
				for(j in pos){
					pos3<-which(nodes$is_cluster==1 & nodes$group==as.integer(sub('\\-.*','',nodes[j,]$id)) & nodes$level>0)
					if(length(pos3)>0){
						edges<-edges2
						for(i in pos3){
							a<-c(as.integer(sub('\\-.*','',nodes[i,]$id)))
							if(nchar(nodes[i,]$children)>0){
								a<-c(a,as.integer(strsplit(nodes[i,]$children,';')[[1]]))
							}
							pos2<-which(nodes$level>0 & nodes$is_cluster==0 & (nodes$group %in% a))
							if(length(pos2)>0){
								ids=nodes[pos2,]$id
								pos2<-which(edges$from %in% ids)
								if(length(pos2)>0){edges[pos2,]$from<-nodes[i,]$id}
								pos2<-which(edges$to %in% ids)
								if(length(pos2)>0){edges[pos2,]$to<-nodes[i,]$id}
								ids=c();pos2=which(edges$from==nodes[i,]$id)
								if(length(pos2)>0) { ids=edges[pos2,]$to }
								pos2=which(edges$to==nodes[i,]$id)
								if(length(pos2)>0) { ids=c(ids,edges[pos2,]$from) }
								ids=unique(ids)
								if(length(ids)>0){
									ids=ids[which(ids!=nodes[i,]$id)]
									for(k in ids){
										pos2=which( ((edges$from==k) & edges$to==nodes[i,]$id) | ((edges$to==k) & edges$from==nodes[i,]$id) )
										if(length(pos2)>0){
											peso=median(edges[pos2,]$weight)*length(pos2)
											edges[pos2,]$weight<-peso
										}
									}
								}
							}
						}
						edges<-edges[which(!duplicated(edges[c('from','to')]) & (edges$from!=edges$to) ),]
						if(nrow(edges)>0){
							edges<-unique(get.data.frame(graph.data.frame(edges, directed=FALSE),"edges"))
						}
					}
					pos3<-which(nodes$group==as.integer(sub('\\-.*','',nodes[j,]$id)) & nodes$level>0)
					if(length(pos3)>0){
						a <- graph.data.frame(edges[which( (edges$from %in% nodes[pos3,]$id) & (edges$to %in% nodes[pos3,]$id) ),], directed=FALSE, vertices=nodes[pos3,])
						a<-layout_with_graphopt(a)
						nodes[pos3,]$x<-as.integer(a[,1])*10
						nodes[pos3,]$y<-as.integer(a[,2])*10
					}
				}
			}
			saveRDS(nodes,file=paste(dir,'nodesA-',sub('^nodes\\-','',ii),sep=''))
		}
	}
}
procNodesA()
####################################################################
library(igraph)
procEdgesA<-function(){
	dirs<-c('Desktop/preproc/categoria_comment/'
	,'Desktop/preproc/categoria_like/'
	,'Desktop/preproc/regiones_comment/'
	,'Desktop/preproc/regiones_like/')
	for(dir in dirs){
		aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^nodesA\\-',aa)];#if(dir==dirs[[1]]){aa<-aa[1:length(aa)]}
		for(ii in aa){
			nodes<-readRDS(file=paste(dir,ii,sep=''))
			edges2<-readRDS(file=paste(dir,'edges-',sub('^nodesA\\-','',ii),sep=''))
			cont<-sub('\\-.*','',sub('^n\\d+\\-','',nodes[1,]$id))
			newEdges<-list();newEdges_i<-0

			edges<-edges2;pos<-which(nodes$level==0 & nodes$is_cluster==1)
			if(length(pos)>0){
				for(i in pos){
					a<-c(as.integer(sub('\\-.*','',nodes[i,]$id)))
					if(nchar(nodes[i,]$children)>0){
						a<-c(a,as.integer(strsplit(nodes[i,]$children,';')[[1]]))
					}
					pos2<-which(nodes$level>0 & nodes$is_cluster==0 & (nodes$group %in% a))
					if(length(pos2)>0){
						ids=nodes[pos2,]$id
						pos2<-which(edges$from %in% ids)
						if(length(pos2)>0){edges[pos2,]$from<-nodes[i,]$id}
						pos2<-which(edges$to %in% ids)
						if(length(pos2)>0){edges[pos2,]$to<-nodes[i,]$id}
						ids=c();pos2=which(edges$from==nodes[i,]$id)
						if(length(pos2)>0) { ids=edges[pos2,]$to }
						pos2=which(edges$to==nodes[i,]$id)
						if(length(pos2)>0) { ids=c(ids,edges[pos2,]$from) }
						ids=unique(ids)
						if(length(ids)>0){
							ids=ids[which(ids!=nodes[i,]$id)]
							for(j in ids){
								pos2=which( ((edges$from==j) & edges$to==nodes[i,]$id) | ((edges$to==j) & edges$from==nodes[i,]$id) )
								if(length(pos2)>0){
									peso=median(edges[pos2,]$weight)*length(pos2)
									apeso=as.integer(10*peso)*.1
									apeso<-paste('rgba(151,194,252,',ifelse(apeso<.3,.2, ifelse(apeso>1,1,apeso)),')',sep='')
									edges[pos2,]$weight<-peso
									edges[pos2,]$title<-peso
									edges[pos2,]$color<-apeso
								}
							}
						}
					}
				}
			}
			edges<-edges[which(!duplicated(edges[c('from','to')]) & (edges$from!=edges$to) ),]
			if(nrow(edges)>0){
				edges$id<-''
				edges<-unique(get.data.frame(graph.data.frame(edges, directed=FALSE),"edges"))
				edges$group<-0
				newEdges[[(newEdges_i<-newEdges_i+1)]]<-edges
			}

			####### DEMAS
			pos<-which(nodes$is_cluster==1)
			if(length(pos)>0){
				for(j in pos){
					pe<-as.integer(sub('\\-.*','',nodes[j,]$id))
					if(nchar(nodes[i,]$parent)>0){
						pe<-c(pe,as.integer(strsplit(nodes[i,]$parent,';')[[1]]))
					}
					pos3<-which(nodes$is_cluster==1 & !(nodes$id %in% paste(pe,'-',cont,sep='')) & ((nodes$group %in% pe) | nodes$level==0) )
					if(length(pos3)>0){
						edges<-edges2
						for(i in pos3){
							a<-c(as.integer(sub('\\-.*','',nodes[i,]$id)))
							if(nchar(nodes[i,]$children)>0){
								a<-c(a,as.integer(strsplit(nodes[i,]$children,';')[[1]]))
							}
							pos2<-which(nodes$level>0 & nodes$is_cluster==0 & (nodes$group %in% a))
							if(length(pos2)>0){
								ids=nodes[pos2,]$id
								pos2<-which(edges$from %in% ids)
								if(length(pos2)>0){edges[pos2,]$from<-nodes[i,]$id}
								pos2<-which(edges$to %in% ids)
								if(length(pos2)>0){edges[pos2,]$to<-nodes[i,]$id}
								ids=c();pos2=which(edges$from==nodes[i,]$id)
								if(length(pos2)>0) { ids=edges[pos2,]$to }
								pos2=which(edges$to==nodes[i,]$id)
								if(length(pos2)>0) { ids=c(ids,edges[pos2,]$from) }
								ids=unique(ids)
								if(length(ids)>0){
									ids=ids[which(ids!=nodes[i,]$id)]
									for(k in ids){
										pos2=which( ((edges$from==k) & edges$to==nodes[i,]$id) | ((edges$to==k) & edges$from==nodes[i,]$id) )
										if(length(pos2)>0){
											peso=median(edges[pos2,]$weight)*length(pos2)
											edges[pos2,]$weight<-peso
										}
									}
								}
							}
						}
						edges<-edges[which(!duplicated(edges[c('from','to')]) & (edges$from!=edges$to) ),]
						if(nrow(edges)>0){
							edges$id<-''
							edges<-unique(get.data.frame(graph.data.frame(edges, directed=FALSE),"edges"))
							agroup<-as.integer(sub('\\-.*','',nodes[j,]$id))
							edges$group<-agroup
							newEdges[[(newEdges_i<-newEdges_i+1)]]<-edges
						}
						if((newEdges_i %% 15)==0){
							newEdges<-do.call("rbind", lapply(newEdges, as.data.frame))
							newEdges$id<-paste(newEdges$from,'-',newEdges$to,sep='')
							newEdges<-newEdges[which(!duplicated(newEdges$id)),]
							newEdges$id<-''
							newEdges<-list(newEdges)
							newEdges_i<-1
						}
					}else{
						edges<-edges2
						edges<-edges[which(!duplicated(edges[c('from','to')]) & (edges$from!=edges$to) ),]
						if(nrow(edges)>0){
							edges$id<-''
							edges<-unique(get.data.frame(graph.data.frame(edges, directed=FALSE),"edges"))
							agroup<-as.integer(sub('\\-.*','',nodes[j,]$id))
							edges$group<-agroup
							newEdges[[(newEdges_i<-newEdges_i+1)]]<-edges
						}
					}
				}
			}
			#######
			if(length(newEdges)>0){
				newEdges<-do.call("rbind", lapply(newEdges, as.data.frame))
			}else{
				newEdges<-edges
			}
			if(nrow(newEdges)>0){
				newEdges$id<-paste(newEdges$from,'-',newEdges$to,sep='')
				newEdges<-newEdges[which(!duplicated(newEdges$id)),]
				newEdges$title<-gsub('\\.',',',newEdges$title)
			}
			saveRDS(newEdges,file=paste(dir,'edgesA-',sub('^nodesA\\-','',ii),sep=''))
		}
	}
}
procEdgesA()
####################################################################
library(igraph)
procMetric<-function(){
	dirs<-c('Desktop/preproc/categoria_comment/'
	,'Desktop/preproc/categoria_like/'
	,'Desktop/preproc/regiones_comment/'
	,'Desktop/preproc/regiones_like/'
	,'Desktop/preproc/ong_comment/'
	,'Desktop/preproc/ong_like/')
	for(dir in dirs){
		aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^edgesA\\-',aa)];#if(dir==dirs[[1]]){aa<-aa[1:length(aa)]}
		for(ii in aa){
			nodes<-readRDS(file=paste(dir,'nodesA-',sub('^edgesA\\-','',ii),sep=''))
			edges<-readRDS(file=paste(dir,'edges-',sub('^edgesA\\-','',ii),sep=''))
			a <- graph.data.frame(edges, directed=FALSE, vertices=nodes)
			b<-strength(a)
			c<-degree(a)
			aux<-gsub("^Na.*", "0", c(
			format(nrow(nodes[which(nodes$is_cluster==0),]), digits=6, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(nrow(edges), digits=6, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(length(which(nodes$is_ong==1)), digits=6, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(edge_density(a,loops=F), digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(transitivity(a), digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(median(c), digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(max(c), digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(median(b), digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			,format(ifelse(is.infinite((m<-max(b))),'0',m), digits=5, decimal.mark=",",big.mark=".",small.mark="", small.interval=3)
			))
			saveRDS(aux,file=paste(dir,'metric-',sub('^edgesA\\-','',ii),sep=''))
		}
	}
}
procMetric()
####################################################################
library(igraph)
procNode<-function(){
	dirs<-c('Desktop/preproc/categoria_comment/'
	,'Desktop/preproc/categoria_like/'
	,'Desktop/preproc/regiones_comment/'
	,'Desktop/preproc/regiones_like/'
	,'Desktop/preproc/ong_comment/'
	,'Desktop/preproc/ong_like/')
	for(dir in dirs){
		aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^nodesA\\-',aa)];#if(dir==dirs[[1]]){aa<-aa[1:length(aa)]}
		for(ii in aa){
			nodes<-readRDS(file=paste(dir,ii,sep=''))
			edges<-readRDS(file=paste(dir,'edges-',sub('^nodesA\\-','',ii),sep=''))
			nod<-nodes[which(nodes$is_cluster==0),]
			clu<-nodes[which(nodes$is_cluster==1),]
			a <- graph.data.frame(edges, directed=FALSE, vertices=nod)
			b<-strength(a)
			c<-degree(a)
			a<-order(c);a<-a[length(a):1]
			nod<-nod[a,];c<-c[a];b<-b[a]
			nod$degree<-c;nod$w_degree<-b
			if(nrow(clu)>0){
				clu$degree<-NA;clu$w_degree<-NA
			}
			nodes<-do.call("rbind", lapply(list(nod,clu), as.data.frame))
			saveRDS(nodes,file=paste(dir,ii,sep=''))
		}
	}
}
procNode()
####################################################################
genCat<-function(){
	dir<-'Desktop/preproc/ong_like/'
	if(length(dir[grepl('/categoria_',dir)])>0){type<-1
	}else if(length(dir[grepl('/ong_',dir)])>0){type<-2
	}else{type<-3}
	aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^edgesA\\-',aa)];aa<-unique(sub('\\-.*','',sub('[a-zA-Z]*\\-','',aa)))
	aa<-as.data.frame(aa,stringsAsFactors=FALSE);colnames(aa)<-'category'
	############ LLENAR INDIVIDUALMENTE: ############
	aa$description<-aa$title<-NA
	#################################################
	saveRDS(aa,file=paste('Desktop/preproc/cat',type,'.Rda',sep=''))
}
####################################################################
genCatSub<-function(){
	dir<-'Desktop/preproc/categoria_like/'
	if(length(dir[grepl('/categoria_',dir)])>0){type<-1
	}else if(length(dir[grepl('/ong_',dir)])>0){type<-2
	}else{type<-3}
	subtype<-ifelse(length(dir[grepl('_like/$',dir)])>0,2,1)
	aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^edgesA\\-',aa)]
	aa<-as.data.frame(aa,stringsAsFactors=FALSE);colnames(aa)<-'file'
	aa$ventana<-sub('\\.Rda$','',sub('.*\\-Period\\-','',aa$file))
	aa$category<-sub('\\-.*','',sub('[a-zA-Z]*\\-','',aa$file))
	saveRDS(aa,file=paste('Desktop/preproc/cat',type,'-',subtype,'.Rda',sep=''))
}
genCatSub()
####################################################################
procCompare<-function(){
	dir<-'Desktop/preproc/categoria_like/'
	if(length(dir[grepl('/categoria_',dir)])>0){type<-1
	}else if(length(dir[grepl('/ong_',dir)])>0){type<-2
	}else{type<-3}
	subtype<-ifelse(length(dir[grepl('_like/$',dir)])>0,2,1)
	aa<-list.files(dir);aa<-aa[grepl('.Rda$',aa)];aa<-aa[grepl('^nodesA\\-',aa)]
	bb<-aa;aa<-as.data.frame(aa,stringsAsFactors=FALSE);colnames(aa)<-'id'
	cat<-readRDS(file=paste(Desktop/preproc/cat,type,'.Rda',sep=''))
	i<-1;aa$id<-aa$name<-aa$date<-aa$num<-aa$personas<-aa$ongs<-aa$d_med<-aa$w_med<-aa$u_d<-aa$u_w<-NA
	for(ii in bb){
		nodes<-readRDS(file=paste(dir,ii,sep=''));nodes<-nodes[which(nodes$is_cluster==0),]
		edges<-readRDS(file=paste(dir,sub('^nodesA\\-','edges-',ii),sep=''))
		aa[i,]$id<-sub('\\-.*','',sub('^nodesA\\-','',bb[i]))
		aa[i,]$name<-cat[which(cat$category==a[i,]$id),]$title
		aa[i,]$date<-sub('\\.Rda$','',sub('.*\\-Period\\-','',bb[i]))
		aa[i,]$num<-(as.integer(sub('\\-.*','',aa[i,]$date))-2008)*2+as.integer(as.integer(sub('.*\\-','',aa[i,]$date))/2)
		aa[i,]$personas<-nrow(nodes[which(nodes$is_ong==0),])
		aa[i,]$ongs<-nrow(nodes[which(nodes$is_ong==1),])
		a <- graph.data.frame(edges, directed=FALSE, vertices=nodes)
		c<-degree(a);b<-strength(a)
		aa[i,]$d_med<-mean(c)
		aa[i,]$w_med<-mean(b)
		a<-order(c);a<-a[length(a):1]
		nodes<-nodes[a,]
		if(length(a)>5){
			aa[i,]$u_d<-gsub('\\, ',';',toString(nodes[1:5,]$title))
		}else{
			aa[i,]$u_d<-gsub('\\, ',';',toString(nodes$title))
		}
		a<-order(b);a<-a[length(a):1]
		nodes<-nodes[a,]
		if(length(a)>5){
			aa[i,]$u_w<-gsub('\\, ',';',toString(nodes[1:5,]$title))
		}else{
			aa[i,]$u_w<-gsub('\\, ',';',toString(nodes$title))
		}
		i<-i+1
	}
	saveRDS(aa,file=paste('Desktop/preproc/compare',type,'-',subtype,'.Rda',sep=''))
}
procCompare()
####################################################################