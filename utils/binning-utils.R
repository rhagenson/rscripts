# Below are two similar functions that break an input into bins based on
# paritally applying the "cut" function with the breaking points stated in "breaks" 
# and names these bins according to "labels"
#
# Requires:
#    library(pryr) for the partial(...) function
# 
# Usage: 
#    HeightBins <- bin_height(EstimatedHeights)
#    DistanceBins <- bin_distance(EstimatedDistances)
bin_height <- partial(cut, breaks = c(-Inf, 5, 10, Inf), labels = c("0-5", "6-10", "10+"))
bin_distance <- partial(cut, breaks = c(-Inf, 5, 10, 15, Inf), labels = c("0-5", "6-10", "10-15", "15+"))
