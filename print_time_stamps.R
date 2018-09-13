#################################################
# Print all the timestamps for audio segmentation
#################################################

# Load in the RData version of the ACQDIV corpus
load("~/Documents/Projects/ACQDIV/acqdiv_corpus_2018-06-19.rda")

# A function to extract and save timestamps for specific 
# languages from ACQDIV
# **** Currently only works on Russian and Chintang *****
TimeStampPrinter = function(utt_df, target_language, output_file){
    # Extract the language-specific subsample
    sample = droplevels(subset(utt_df, 
                               utt_df$language==target_language))
    # Extract a vector of the utterance ids
    #     - allows us to match to the audio files
    source_id = as.vector(sample$utterance_id_source)
    # Clean up the utterance id name
    sample$source = gsub("^([^\\._]+)[\\._].+",
                         "\\1",
                         source_id)
    # Construct the output file
    #   - start time
    #   - end time
    #   - source ID
    #   - the utterance ID
    #   - the cleaned source ID
    stamps = sample[!is.na(sample$start) &
                    !is.na(sample$end) &
                    sample$source != "(empty)",
                    c("start", "end", "utterance_id_source",
                      "utterance_id", "source")]
    # Save the timestamp file
    write.table(stamps, file = output_file,
                sep="\t", row.names=F, quote=F)
}


###################################
# Extract for each of the languages
###################################
# Chintang
out = "/Users/nicholas/Desktop/chintang_ts.txt"
TimeStampPrinter(utterances, "Chintang", out)

# Russian
out = "/Users/nicholas/Desktop/russian_ts.txt"
TimeStampPrinter(utterances, "Russian", out)

