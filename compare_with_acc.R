#ACCORDIAN DATABASE
setwd('C:/Users/user/Desktop/scudo/scudo')
Acc_naive <- read.csv('C:/Users/User/Downloads/naive_cd4_acc.csv')
x<- c(Acc_naive$marker,Acc_naive$marker_type)
x_naive<- x[x != "positive"]
x_scudo_int<- intersect(x_naive,cd4naive_scudo)
unique(markers[markers$cluster=='Naive CD4 T',]$gene)
#Read txt file #####################
# Read the file, skipping the first row (which is a meta-header)
scudo_df <- read.delim("./scudo_output/output_scudo_biomarker.txt", header = TRUE, skip = 1)
scudo_df_cd4 <- read.delim("scudo_output_biomarker.txt", header = TRUE, skip = 1)
scudo_df_100<-read.delim("./100_trained/bi_scudo/scudo_output_biomarker.txt")
naive_accordion<- read.csv('C:/Users/user/Desktop/scudo/cleaned_acc/naive_all_fixed.csv')
memory_accordion<- read.csv('C:/Users/user/Desktop/scudo/cleaned_acc/memory_all_fixed.csv')
cd4_markers_accordion<- union(unique(naive_accordion$marker),unique(memory_accordion$marker))


#venn diagram #########################################
library(dplyr)

# Filter significant genes
#significant_markers <- markers %>% filter(p_val_adj <= 0.05)

# Extract gene names as a vector
significant_genes <- significant_markers$gene

intersect(scudo_df$Feature.ID,unique(markers$gene))
intersect(scudo_df_cd4$Feature.ID,unique(rownames(cluster_markers)))
# Features in scudo_df but not in markers
setdiff(unique(rownames(scudo_df_100)),unique(markers$gene))

# Features in markers but not in scudo_df
setdiff(unique(markers$gene), scudo_df$Feature.ID)


# Define the two sets
library(VennDiagram)
set1<- cd4_markers_accordion#unique(scudo_df_cd4$Feature.ID)
#set1 <-unique(rownames(scudo_df_100))
##set2 <- unique(markers$gene)
set2 <-unique(scudo_df_cd4$Feature.ID) #unique(unique(rownames(cluster_markers)))
set3<- unique(unique(rownames(cluster_markers)))
# Create a named list for Venn diagram
venn_list <- list(
  CD4_ACCORDION = set1,
  SCUDO_CD4_Biomarkers = set3
)

# Draw the Venn diagram
venn.plot <- venn.diagram(
  x = venn_list,
  filename = NULL,  # So it returns a grid object
  fill = c("lightblue", "pink"),
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.5,
  cat.pos = 0,
  cat.dist = 0.05,
  margin = 0.1
)

# Plot it
grid::grid.newpage()
grid::grid.draw(venn.plot)
scudo_only_genes <- setdiff(set1, set2)
print(scudo_only_genes)
##End of venn diagram

#######################################################
# Extract the Feature IDs (gene names)
feature_ids <- unique(markers$gene)#unique(scudo_df$Feature.ID)


# View them
print(feature_ids)
#intersect(feature_ids_cd4,top_marker_acc)
x_scudo<-intersect(scudo_df$Feature.ID,top_acc_markers) #intersect(unique(rownames(scudo_df_100)),top_acc_markers)#intersect(feature_ids,top_acc_markers)

scores_scudox<-acc_df[acc_df$marker %in% x_scudo, ]
#new 8/16/2025
library(dplyr)
library(ggplot2)
library(viridis)
marker<-readLines("selected_features_RF.txt")

# order markers within each cell type by decreasing impact
scores_scudox_plot <- scores_scudox %>%
  group_by(accordion_per_cluster) %>%
  arrange(accordion_per_cluster, desc(gene_impact_score_per_celltype_cluster)) %>%
  mutate(.ord = row_number()) %>%
  ungroup() %>%
  arrange(accordion_per_cluster, .ord) %>%
  mutate(
    # set marker factor levels to preserve order
    marker = factor(marker, levels = unique(marker)),
    # optional bubble size scaling
    size_var = scales::rescale(gene_impact_score_per_celltype_cluster, to = c(0.25, 1))
  )

library(ggplot2)
library(viridis)

# calculate the number of unique markers
n_markers <- length(unique(scores_scudox_plot$marker))

ggplot(scores_scudox_plot, aes(x = marker, y = accordion_per_cluster)) +
  geom_point(aes(
    color = gene_impact_score_per_celltype_cluster,
    size  = size_var
  )) +
  scale_color_viridis(
    name = "gene_impact_score_per_celltype_cluster",
    option = "C"
  ) +
  scale_size_continuous(
    name = "SPs",
    range = c(2.5, 7),
    breaks = c(0.25, 0.50, 0.75, 1.00)
  ) +
  labs(
    title = "Grouped Marker Genes by Cell Type",
    x = paste("Marker Gene (Grouped by Cell Type), n =", n_markers),
    y = "Accordion Cell Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
    legend.key = element_blank()
  )

# Optional: save to a text file
#write.table(feature_ids, file = "scudo_feature_ids.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
##############################################################################################################

#Intersect with findallmarkers
x_findmarker<- intersect(markers$gene,top_acc_markers)



# Get the scores of common top
acc_df<-data_acc@misc[["accordion"]][["cluster_resolution"]][["detailed_annotation_info"]][["top_markers_per_celltype_cluster"]]
scores_findmarkersx<- acc_df[acc_df$marker %in% x_findmarker, ]

############################################# Upset PLot
library(dplyr)
library(ggplot2)
acc_df<-data_acc@misc[["accordion"]][["cluster_resolution"]][["detailed_annotation_info"]][["top_markers_per_celltype_cluster"]]
# Step 1: Intersect markers
x_scudo <- intersect(scudo_df$Feature.ID, acc_df$marker)
x_marker<- intersect(markers$gene, acc_df$marker)
scores_scudox <- acc_df[acc_df$marker %in% x_scudo, ]
scores_markerx<-acc_df[acc_df$marker %in% x_marker, ]

# Step 2: Prepare (marker, cluster, SPs)
plot_df <- scores_markerx %>%
  select(marker, accordion_per_cluster, SPs) %>%
  distinct()

# Step 3: Assign each marker to its "main cluster"
marker_primary <- plot_df %>%
  group_by(marker) %>%
  slice_max(order_by = SPs, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(accordion_per_cluster, desc(SPs)) %>%
  mutate(marker_grouped = factor(marker, levels = unique(marker)))

# Step 4: Join back the grouped marker order
plot_df <- left_join(plot_df, marker_primary[, c("marker", "marker_grouped")], by = "marker")

# Step 5: Plot with color by cluster
ggplot(plot_df, aes(x = marker_grouped, y = accordion_per_cluster, size = SPs, color = accordion_per_cluster)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(name = "SPs Score", range = c(2, 8)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    axis.title = element_blank(),
    legend.title = element_text(size = 10),
    legend.position = "right"
  ) +
  ggtitle("Markers Grouped by Cell Type\n(Dot size = SPs, Color = Cell Type)")


###################################################################################library(dplyr)
setwd("C:/Users/user/Desktop/scudo")
cleaned_acc <- read.csv('./naive_in_acc_.csv')
cleaned_acc_2<-read.csv('./memory_in_acc')
library(stringr)
library(tidyverse)

unique(cleaned_acc$marker)
unique(cleaned_acc$CL_celltype)
check_marker<-unique(cleaned_acc$marker)

cleaned_filtered <- cleaned_acc%>%
  filter(
    (str_detect(CL_celltype, regex("CD14-positive", ignore_case = TRUE))))# & 
      # str_detect(CL_celltype, regex("C", ignore_case = TRUE))) |
      
      #(str_detect(CL_celltype, regex("naive", ignore_case = TRUE)) & 
       #  str_detect(CL_celltype, regex("\\bT[- ]?cell\\b|\\bT\\b", ignore_case = TRUE)))
print(cleaned_filtered)
filtered_marker<- unique(cleaned_filtered$marker)


# READ ALL MARKERS for cell type FROM ACC #########3

cleaned_filtered<- read.csv('./naive_all.csv')
cleaned_filtered_2<- read.csv('./memory_all.csv')
filtered_marker=unique(cleaned_filtered$marker)
filtered_marker_2=unique(cleaned_filtered_2$marker)
# READ SCUDO BIOMARKER################################

setwd("C:/Users/user/Desktop/scudo/scudo")


DC_scudo<- readLines("DC_biomarkers.txt")
B_scudo <- readLines("./scudo/B_biomarkers.txt")
FCGRA_scudo<- readLines("./scudo/FCGRA_biomarkers.txt")
cd14_scudo<- readLines("CD14_biomarkers.txt")
cd8_scudo<- readLines('./scudo/CD8T_biomarkers.txt')
NK_scudo<- readLines('./scudo/NK_biomarkers.txt')

cd4naive_scudo <-readLines("./scudo/naive_biomarkers.txt")
cd4memory_scudo<- readLines("./scudo/memory_biomarkers.txt")
setwd("C:/Users/User/Desktop/scudo/scudo")

# Load required library
library(VennDiagram)

find_all_markers <- markers[markers$avg_log2FC > 0.25 & markers$p_val < 0.05, "gene"]
find_all_markers <-unique(find_all_markers)
# Create a Venn diagram
venn.plot <- venn.diagram(
  x = list(
    ` FindAllMarkers` = set2,#check_marker,filtered_marker,
    `SCUDO Biomarkers` = set1#cd4naive_scudo
  ),
  filename = NULL,
  fill = c("lightblue", "lightpink"),
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.2,
  cat.pos = 2,
  cat.dist = 0.009,
  margin = 0.1
)

# Plot to screen
grid.newpage()
grid.draw(venn.plot)
setdiff(check_marker,NK_scudo)
are_you_sure<- intersect(check_marker,cd4naive_scudo)
#################################################################################################################################
left_unmatched<- setdiff(cd4naive_scudo, filtered_marker) ##same
matched<- intersect(cd4naive_scudo, filtered_marker)
mystery<- setdiff(left_unmatched,cleaned_acc$marker)
Ihope<-intersect(are_you_sure,mystery)
not_mystery <- intersect(left_unmatched,cleaned_acc$marker)
library(dplyr)
library(ggplot2)

# Start from the unmatched markers
unmatched_markers <- setdiff(B_scudo,filtered_marker)
#supp_matched <- setdiff(unique(unmatched_df$marker),)
# Subset the data for unmatched markers only
unmatched_df <- cleaned_acc%>%
  filter(marker %in% unmatched_markers)
unique(unmatched_df$marker)
# Remove duplicated (marker, celltype) pairs
unmatched_unique <- unmatched_df %>%
  distinct(marker, CL_celltype)

# Find how many unique markers are covered by top cell types, incrementally
celltype_counts <- unmatched_unique %>%
  count(CL_celltype) %>%
  arrange(desc(n))

# Initialize
selected_celltypes <- c()
covered_markers <- c()

for (ct in celltype_counts$CL_celltype) {
  # Get markers in this cell type
  new_markers <- unmatched_unique %>%
    filter(CL_celltype == ct) %>%
    pull(marker)
  
  # Add new markers not already covered
  covered_markers <- union(covered_markers, new_markers)
  selected_celltypes <- c(selected_celltypes, ct)
  
  # Stop if we'v
  if (length(covered_markers) == length(not_mystery))break
}
# Limit to top 40 most frequent cell types
top_n_ct <- celltype_counts %>%
  slice_head(n = 40) %>%
  pull(CL_celltype)

# Filter data to these cell types
#filtered_df <- unmatched_df %>%
 # filter(CL_celltype %in% top_n_ct)

filtered_df <- unmatched_df %>%
  filter(CL_celltype %in% selected_celltypes)

ggplot(filtered_df, aes(x = marker, y = CL_celltype, size = SPs)) +
  geom_point(
    shape = 21,                         # Hollow circle with border
    fill = "lightblue",                # Transparent light fill
    color = "steelblue",               # Border color
    stroke = 0.4,                      # Border thickness
    alpha = 0.6,                       # Transparent, but NOT darker when overlapped
    position = position_dodge(width = 0.7)
  ) +
  theme_minimal() +
  labs(
    title = paste("Unmatched Markers in", length(selected_celltypes), "Most Mapped Cell Types"),
    x = paste("Marker (not mapped to CD8):",length(unique(filtered_df$marker))),
    y = "Mapped CL_celltype",
    size = "SPs"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
    axis.text.y = element_text(size = 8)
  ) +
  scale_size(range = c(2.2, 3.3))
## from pyhton after upset
upset <- c('CTSB', 'NKG7', 'ASAH1', 'COX4I1', 'FGL2', 'IGFBP7', 'PFDN5', 'PFN1', 'SH3BGRL3', 'TGFBI', 'UBA52', 'WARS', 'CD68', 'MNDA', 'SERPINA1', 'SPI1', 'MS4A1', 'C1orf162', 'C5AR1', 'CEBPB', 'CFD', 'CSTB', 'CTSH', 'GRN', 'HLA-DQB1', 'TNFSF13B', 'TYMP', 'FGR', 'HCK', 'IFI30', 'PILRA', 'ABI3', 'ARPC5', 'ARRB2', 'BID', 'CEBPD', 'CFL1', 'EIF4A1', 'FCGRT', 'GABARAP', 'GSTP1', 'H2AFY', 'HIST1H2AC', 'KLF4', 'LGALS2', 'LILRA5', 'LRRC25', 'MYL6', 'RPL12', 'RPL15', 'RPS23', 'RPS24', 'RPS4X', 'SLC7A7', 'STXBP2', 'TAGLN2', 'ARPC1B', 'CARD16', 'CKB', 'FKBP1A', 'TPI1', 'EIF1', 'OAZ1', 'POLD4', 'BRI3', 'RPL28', 'AP2S1', 'GPBAR1', 'ARPC2', 'ARPC3', 'ATP6V0B', 'BRK1', 'CALM2', 'CHCHD2', 'CLIC1', 'EIF3L', 'LGALS9', 'PGLS', 'PRELID1', 'RAC1', 'RGS19', 'RNF130', 'RPL10', 'RPL18A', 'RPL19', 'RPL23', 'RPL26', 'RPL27', 'RPL29', 'RPL35A', 'RPL37', 'RPL38', 'RPL7A', 'RPLP1', 'RPLP2', 'RPS10', 'RPS13', 'RPS15', 'RPS16', 'RPS28', 'SERF2', 'SLC25A6', 'COX5B', 'C1ORF162', 'RPSAP58')

mystery_l<- setdiff(mystery,upset)


#PLOT
'''
library(ggplot2)
library(ggbeeswarm)

ggplot(filtered_df, aes(x = marker, y = CL_celltype, size = SPs)) +
  geom_quasirandom(
    shape = 21,
    fill = "lightblue",
    color = "steelblue",
    stroke = 0.4,
    alpha = 0.6,
    dodge.width = 0.7,     # Keeps spacing consistent
    groupOnX = TRUE        # Spread along X within each Y level
  ) +
  theme_minimal() +
  labs(
    title = paste("Unmatched Markers in", length(selected_celltypes), "Most Mapped Cell Types"),
    x = "Marker (not mapped to naive T)",
    y = "Mapped CL_celltype",
    size = "SPs Score"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
    axis.text.y = element_text(size = 8)
  ) +
  scale_size(range = c(2, 3))
### PLOT THE UNMATCHED##################################
# Set the correct path to your file
unmatched_naive <- read.csv("unmatched_naive.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Preview the data
head(df)


head(unmatched_naive)

# Filter data to these cell types


ggplot(unmatched_naive, aes(x = marker, y = CL_celltype, size = SPs)) +
  geom_point(
    shape = 21,                         # Hollow circle with border
    fill = "lightblue",                # Transparent light fill
    color = "steelblue",               # Border color
    stroke = 0.4,                      # Border thickness
    alpha = 0.6,                       # Transparent, but NOT darker when overlapped
    position = position_dodge(width = 0.7)
  ) +
  theme_minimal() +
  labs(
    title = paste("Unmatched Markers in", 40, "Most Mapped Cell Types"),
    x = "Marker (not mapped to naive T)",
    y = "Mapped CL_celltype",
    size = "SPs Score"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
    axis.text.y = element_text(size = 8)
  ) +
  scale_size(range = c(2.2, 3.3))
  '''


