#SAIR 2023: Directions to a Degree: the Efficient Visualization and Analysis of Curriculum Data using Network Graphs in R
#Zhen Zhang, zhen.zhang2@uga.edu
#University of Georgia Office of Institutional Research

# metrics -----------------------------------------------------------------
path_search <- function(u) {
  #print(u)
  if (visited$visited[visited$node == u]) {
    return()
  }
  visited$visited[visited$node == u] <<- TRUE
  current_path <<- append(current_path,u)
  if (u %in% source_list) {
    simple_paths[m] <<- list(current_path)
    m <<- m + 1
    visited$visited[visited$node == u] <<- FALSE
    current_path <<- head(current_path,-1)
    return()
  }
  step_list <- c()
  step_list <- edge_list$from[edge_list$to == u]
  #print(step_list)
  for (k in 1:length(step_list)) {
    path_search(step_list[k])
  }
  current_path <<- head(current_path,-1)
  visited$visited[visited$node == u] <<- FALSE
}

graph_metrics <- function(node_metrics,edge_list) {
  
  for (i in 1:length(sink_list)) {
    visited <<- data.frame(node=node_metrics$from)
    visited$visited <<- FALSE
    current_path <<- c()
    path_search(sink_list[i])
    #print(paste("path loop ",i))
  }
  
  node_metrics$centrality <<- NA
  for (i in 1:length(node_metrics$from)) {
    if (node_metrics$from[i] %in% append(sink_list,source_list)) {
      node_metrics$centrality[i] <<- 0
    } else {
      centrality_list <- c()
      for (j in 1:length(simple_paths)) {
        if (node_metrics$from[i] %in% simple_paths[[j]]) {
          centrality_list <- append(centrality_list,simple_paths[[j]])
        }
      }
      node_metrics$centrality[i] <<- length(centrality_list)
    }
    #print(paste("centrality ",i))
  }
  
  node_metrics$type <<- NA
  for (i in 1:length(node_metrics$from)) {
    if (node_metrics$from[i] %in% source_list) {
      node_metrics$type[i] <<- "source"
    } else if (node_metrics$from[i] %in% sink_list){
      node_metrics$type[i] <<- "sink"
    } else {
      node_metrics$type[i] <<- "central"
    }
  }
  
  delay <- c()
  node_metrics$delay <<- 1
  simple_paths_ordered <- simple_paths[order(sapply(simple_paths,length),decreasing = TRUE)]
  for (i in 1:length(node_metrics$from)) {
    for (j in 1:length(simple_paths_ordered)) {
      if (node_metrics$from[i] %in% simple_paths_ordered[[j]]) {
        node_metrics$delay[i] <<- length(simple_paths_ordered[[j]])
        delay[i] <- length(simple_paths_ordered[[j]])
        break
      } else {
        delay[i] <- 1
      }
    }
  }
  
  node_metrics$blocking <<- 0
  blocking <- c()
  for (i in 1:length(node_metrics$from)) {
    blocking_list <- c()
    for (j in 1:length(simple_paths)) {
      if (node_metrics$from[i] %in% simple_paths[[j]]) {
        blocking_list <- append(blocking_list,simple_paths[[j]][1:match(node_metrics$from[i],simple_paths[[j]])-1])
      }
    }
    node_metrics$blocking[i] <<- length(unique(blocking_list))
    blocking[i] <- length(unique(blocking_list))
  }
  
  node_metrics$complexity <<- NA
  for (i in 1:length(node_metrics$from)) {
    node_metrics$complexity[i] <<- blocking[i] + delay[i]
  }
}

node_metrics <- node_list
node_metrics <- node_metrics%>%rename(from=id)


simple_paths <- list(list())
m <- 1
visited <- data.frame(node=node_metrics$from)
visited$visited <- FALSE
current_path <- c()

source_list <- c()
for(i in 1:length(node_metrics$from)) {
  if (!(node_metrics$from[i] %in% edge_list$to)) {
    source_list <- append(source_list,node_metrics$from[i])
  }
}

sink_list <- c()
to_list <- unique(edge_list$to)
for(i in 1:length(to_list)) {
  if (!(to_list[i] %in% edge_list$from)) {
    sink_list <- append(sink_list,to_list[i])
  }
}

graph_metrics(node_metrics,edge_list)

node_list <- node_metrics