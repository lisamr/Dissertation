if(!"DiagrammeR" %in% rownames(installed.packages())) devtools::install_github("rich-iannone/DiagrammeR")
library(DiagrammeR)

paths <- bcn_fit %>%
  parameterestimates %>%
  select(lhs, op, rhs, est)

node_df <- paths %>%
  filter(op != "~1") %>%
  select(label = lhs) %>%
  distinct %>%
  mutate(type = "a",
         id = 1:nrow(.)) %>%
  dplyr::select(id, type, label)

# Edges will be labeled by the parameter estimates
regressions <- paths %>%
  filter(op != "~1") %>%
  mutate(label = round(est, 2)) %>%
  select(-est) %>%
  filter(op == "~") %>%
  rename(edge_to = lhs, edge_from = rhs) %>%
  mutate(style = "solid") %>%
  select(edge_from, edge_to, style, label)

from <- left_join(x = dplyr::select(regressions, edge_from),
                  y = dplyr::select(node_df, id, edge_from = label)
                  )
to <- left_join(x = dplyr::select(regressions, edge_to),
                  y = dplyr::select(node_df, id, edge_to = label)
)
edge_df <- create_edge_df(from = as.numeric(from$id),
                          to = as.numeric(to$id),
                          rel = as.numeric(regressions$label))
my_graph <- create_graph(
  nodes_df = node_df,
  edges_df = edge_df)

# We can plot the graph directly
render_graph(my_graph)

grViz("
      digraph SEM {
      
      graph [layout = neato,
      overlap = true,
      outputorder = edgesfirst]
      
      node [shape = rectangle]
      
      a [pos = '-4,0.5!', label = 'aet']
      b [pos = '-4,-0.5!', label = 'tmin']
      c [pos = '-3,1!', label = 'Litter']
      d [pos = '-2,1!', label = 'AIG']
      e [pos = '-1,1!', label = 'AIF']
      f [pos = '-3,-1!', label = 'BCN']
      g [pos = '-2,-1!', label = 'PCN']
      h [pos = '-1,-1!', label = 'OCN']
      
      a->c
      a->d
      a->e
      a->f
      a->g
      a->h
      b->c
      b->d
      b->e
      b->f
      b->g
      b->h
      c->f
      c->g
      c->h
      d->f
      d->g
      d->h
      e->f
      e->g
      e->h
      
      }
      ")
