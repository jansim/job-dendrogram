# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

occ <- readODS::read_ods("data/ESCO_v1.1/occupations_en.ods")
grps <- readODS::read_ods("data/ESCO_v1.1/ISCOGroups_en.ods")

n_digits <- 4
all_codes_incl_duplicates <- substring(str_remove_all(occ$code, fixed(".")), first=0, last=n_digits)
all_codes <- unique(all_codes_incl_duplicates) # Can be extended

code_counts <- table(all_codes_incl_duplicates)

all_edges <- data.frame()
for (n in 1:n_digits) {
  to = sort(unique(substring(all_codes, first=0, last=n)))

  if (n > 0) {
    from = substring(to, first=0, last=n-1)
  } else {
    from = 'origin'
  }

  all_edges <- rbind(all_edges, data.frame(from=from, to=to))
}
edges <- all_edges

# Replace To with label

# leaf_edges <- nchar(edges$to) == n_digits
# edges$to[leaf_edges] <- paste(edges$to, grps$preferredLabel[match(edges$to, grps$code)])[leaf_edges]
# edges$to[leaf_edges] <- grps$preferredLabel[match(edges$to, grps$code)][leaf_edges]

# create a vertices data.frame. One line per object of our hierarchy
all_nodes <- unique(c(as.character(edges$from), as.character(edges$to)))
leaf_nodes <- nchar(all_nodes) == n_digits

all_node_counts <- as.numeric(code_counts[all_nodes])
# all_nodes[leaf_nodes] <- grps$preferredLabel[match(all_nodes, grps$code)][leaf_nodes]

vertices = data.frame(
  name = all_nodes,
  value = all_node_counts
)
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name, edges$to ) ]


#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 0, 1)

# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# Create a graph object
mygraph <- graph_from_data_frame(edges, vertices=vertices)

node_pos_factor <- 1.035
text_pos_factor <- 1.07

# Make the plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*text_pos_factor, y=y*text_pos_factor, filter = leaf, label=name, angle = angle + 15, hjust=hjust, colour=group), size=2.7, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*node_pos_factor, y=y*node_pos_factor, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

# Also add attribution to graph code
  # labs(
  #   caption="by Jan Simson\nTwitter: _jansimson\nData: This publication uses the ESCO classification of the European Commission. http://data.europa.eu/esco/"
  # )

# grid::grid.text("Classifications of Occupation", x = unit(0, "npc"), y = unit(0, "npc"),
#                 hjust=0, vjust = -0.5, gp = grid::gpar(cex=1.2))
# grid::grid.text("According to the European & International classification systems ESCO/ISCO.\nSize of dots corresponds to the number of occupations falling within a certain class.", x = unit(0, "npc"), y = unit(0, "npc"),
#                 hjust=0, vjust = -0.5, gp = grid::gpar(cex=1.0))

# Poster: width = 59.4, height = 84.1
ggsave("plot.pdf", units = "cm", width = 59.4, height = 59.4)

