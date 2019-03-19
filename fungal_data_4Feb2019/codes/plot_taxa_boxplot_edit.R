plot_taxa_boxplot_edit <- function(x, taxonomic.level, top.otu, VariableA, title, color = NULL){
  
  Abundance <- NULL
  if (!is.null(x@phy_tree)){
    
    message("For plotting purpuses the phy_tree will be removed")
    x@phy_tree = NULL
  }
  else {
    
    message("The phy_tree slot is empty, easy to make the plot")
  }
  
  taxic <- as.data.frame(x@tax_table);
  # using abundances function from microbiome as sometime otu_table can have taxa_are_rows FALSE in phyloseq
  otudf <- as.data.frame(abundances(x));
  taxic$OTU <- row.names(otudf);
  taxmat <- as.matrix(taxic);
  new.tax <- tax_table(taxmat);
  tax_table(x) <- new.tax;
  
  
  # Merge the taxa at a higher taxonomic level
  
  if (!taxonomic.level == "OTU") {
    
    x <- aggregate_top_taxa(x, taxonomic.level, top = top.otu)
    
  }
  
  x1 <- transform(x, "compositional")
  
  x.df0 <- suppressWarnings(suppressMessages(phy_to_ldf(x1, transform.counts = NULL)))
  
  
  p <- ggplot(x.df0, aes(x =x.df0[,taxonomic.level],
                         y=Abundance,
                         fill = x.df0[,VariableA]))
  
  p <- p + geom_boxplot(position = position_dodge(1))
  
  p <- p + ggtitle(title) + theme_bw() +
    theme(axis.text.x = element_text(face ="italic",
                                     angle = 90))
  
  p <- p + ylab("Relative abundance") + xlab(taxonomic.level) +
    scale_fill_brewer(VariableA,
                      palette = color)
  
  return(p)
}
