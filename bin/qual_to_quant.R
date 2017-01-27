convert_qual_to_quant <- function(qual_label, qual_type) {
  # Convert qualitative ratings to quant estimate ranges
  #
  # ARGS:
  #		qual_label - dataframe of qualitative labels (label=H/M/L, etc)
  #   qual_type  - character string of the type (tef, tc, diff, etc)
  #
  # RETURNS:
  #   dataframe of estimate parameters
  
  
  
  filtered_mappings <- filter(mappings, type == qual_type)
  filtered_mappings <- left_join(qual_label, filtered_mappings, 
                                 by = c("label" = "label")) %>%  
    select(l, ml, h, conf)
  return(filtered_mappings)
}
