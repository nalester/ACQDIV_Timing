#################################################
# Print all the timestamps for audio segmentation
#################################################
library(plyr)

#####################
# Data pre-processing
#####################

# Load in the RData version of the ACQDIV corpus
load("~/Documents/Projects/ACQDIV/acqdiv_corpus_2018-06-19.rda")

# Get the true source ids
sources = sessions[,c("session_id", "session_id_source")]

# Merge session ids into the utterances
df = merge(utterances, sources, by.x="session_id_fk", by.y="session_id")

#####################################
# Cleaning the Japanese-Miyata corpus
#####################################

## Remove the transcripts that we don't have audio for
df = droplevels(df[df$session_id_source!="tai_45_20421",])
df = droplevels(df[-grep("^(aki_|ryo_).+", df$session_id_source),])

## Alter the filenames to match the new labels
levels(df$session_id_source) = c(levels(df$session_id_source), gsub("_\\d+_", "", as.vector(df$session_id_source[df$corpus=="Japanese_Miyata"])))

df$session_id_source[df$corpus=="Japanese_Miyata"] = gsub("_\\d+_", "", as.vector(df$session_id_source[df$corpus=="Japanese_Miyata"]))

## Load the table that will allow us to convert the new labels to old labels
mapping = read.csv("/Users/nicholas/Documents/Projects/ACQDIV/jpn_Miyata_session_labels.csv", header=T); head(mapping)

### Clean up NewFileNames
mapping$NewFileName = gsub(".cha$", "", mapping$NewFileName, perl=T)

### Clean up OldFileName
mapping$OldFileName = gsub("^T(.+)", "tai\\1", mapping$OldFileName, perl=T)

## Replace new transcript filenames with old filenames
### Check to make sure we have the same names
length(unique(df$session_id_source[df$corpus=="Japanese_Miyata"])) 

nrow(mapping) # Problem! We must still have the bad file in here

### Remove bad file from mapping
mapping = mapping[-grep("tai940831", mapping$OldFileName),];nrow(mapping)

### Double-check
as.vector(mapping$NewFileName)==unique(df$session_id_source[df$corpus=="Japanese_Miyata"]) # Good

### Perform the mapping
levels(df$session_id_source) = c(levels(df$session_id_source), mapping$OldFileName)

df$session_id_source[df$corpus=="Japanese_Miyata"] = mapvalues(df$session_id_source[df$corpus=="Japanese_Miyata"], from = unique(df$session_id_source[df$corpus=="Japanese_Miyata"]), to = mapping$OldFileName)

#############################
# Cleaning the Turkish corpus
#############################

# First remove the tokens we can't use
rows = df$corpus=="Turkish" & !grepl("\\-", df$start_raw)
df = df[!rows,]

# Then, split at the hyphens
turk = as.vector(df$start_raw[df2$corpus=="Turkish"])

# Fix a bad apple
turk[startsWith(turk, "21:32")] = "00:21:32-00:21:50"
starts = sapply(strsplit(turk, "-"), "[" , 1)
stops = sapply(strsplit(turk, "-"), "[" , 2)

# Next, transform timestamps into seconds
starts_secs = as.character(as.numeric(hms(starts)))
stops_secs = as.character(as.numeric(hms(stops)))
starts_secs = paste(starts_secs, ".00", sep="")
stops_secs = paste(stops_secs, ".00", sep="")

# Finally, replace the existing values with the new ones
levels(df$start) = c(levels(df$start), starts_secs)
levels(df$end) = c(levels(df$start), stops_secs)
df$start[df$corpus=="Turkish"] = starts_secs
df$end[df$corpus=="Turkish"] = stops_secs
df = droplevels(df)

##########################
# Cleaning the Cree corpus
##########################

## Fix the filenames
cree = as.vector(df$session_id_source[df$language=="Cree"])
subs = gsub("^[^-]+-[^-]+-([^ms]+)[ms]*$", "\\1", cree, perl=T)

## Replace the filenames
levels(df$session_id_source) = c(levels(df$session_id_source), subs)

df$session_id_source[df$language=="Cree"] = subs

#########################################################
# A function to extract and save timestamps for specific 
# languages from ACQDIV
# **** Currently only works on Russian and Chintang *****
#########################################################
TimeStampPrinter = function(utt_df, target_language, output_file){
  # Extract the language-specific subsample
  sample = droplevels(subset(utt_df, 
                             utt_df$corpus==target_language))
  # Simplify label
  sample$source = as.vector(sample$session_id_source)
  # Construct the output file
  #   - start time
  #   - end time
  #   - the utterance ID
  #   - the source ID
  stamps = sample[!is.na(sample$start) &
                    sample$source != "(empty)",
                  c("start", "end",
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
TimeStampPrinter(df, "Chintang", out)

# Russian
out = "/Users/nicholas/Desktop/russian_ts.txt"
TimeStampPrinter(df, "Russian", out)

# Japanese (MiiPro)
out = "/Users/nicholas/Desktop/jpn_miipro_ts.txt"
TimeStampPrinter(df, "Japanese_MiiPro", out)

# Japanese (Miyata)
out = "/Users/nicholas/Desktop/jpn_miyata_ts.txt"
TimeStampPrinter(df, "Japanese_Miyata", out)

# Turkish
out = "/Users/nicholas/Desktop/turkish_ts.txt"
TimeStampPrinter(df, "Turkish", out)

# Cree
out = "/Users/nicholas/Desktop/cree_ts.txt"
TimeStampPrinter(df, "Cree", out)
