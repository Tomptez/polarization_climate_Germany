require(quanteda)
require(quanteda.textmodels)
require(quanteda.textplots)
require(quanteda.textstats)
require(ggplot2)
library(dplyr)
library(glue)
packageVersion("quanteda")
packageVersion("quanteda.textmodels")


# The folder of the main data analysis has to specified, in this case Data_analysis/

#### Select data to analyze
# Bundestag
title = "Wordfish Bundestag Term 19"
state = "Bundestag Term 19"
df <- arrow::read_feather("../Data_analysis/data/selected_stncs_19_stem_wgov.feather")
bu <- df
# 18
title = "Wordfish Bundestag Term 18"
state = "Bundestag Term 18"
df <- arrow::read_feather("../Data_analysis/data/selected_stncs_18_stem_wgov.feather")
bu <- df

# Landtag
title = "Wordfish North Rhine-Westphalia Term 17"
state = "North Rhine-Westphalia Term 17"
df <- arrow::read_feather("../Data_analysis/Landtag/data/wordfish_nrw_stem_17.feather")
nrw <- df
title = "Wordfish Saxony Term 7"
state = "Saxony Term 7"
df <- arrow::read_feather("../Data_analysis/Landtag/data/wordfish_sn_stem_7.feather")
sn <- df
title = "Wordfish Hamburg Term 22"
state <- "Hamburg Term 22"
df <- arrow::read_feather("../Data_analysis/Landtag/data/wordfish_hh_stem_22.feather")
hh <- df
#####################################################################################################


# Create Corpus
corp <- corpus(df, text_field = "text", docid_field = "party")
summary(corp)

# Do wordfish analysis
toks <- tokens(corp, remove_punct = TRUE)
dfmat <- dfm(toks) %>% dfm_remove(pattern = stopwords("de")) %>% dfm_remove(pattern = c("spd\\w*", "cdu\\w*", "csu\\w*", "grünen\\w*", "linkspartei\\w?", "linke\\w*", "union\\w*", "\\w*fraktion\\w*", "afd\\w*", "sozialdemokrat\\w*"), valuetype = "regex")
pg_dfm <- dfm_trim(dfmat, min_termfreq = 5,  verbose = quanteda_options("verbose"))
summary(pg_dfm)
tmod_wf <- textmodel_wordfish(pg_dfm)
docfreq(dfmat)["bezahlen"]
docfreq(pg_dfm)["klimapolitik"]
freqcy <- colSums(as.matrix(dfmat))

graph <- textplot_scale1d(tmod_wf)
stats <- graph$data

graph + expand_limits(y = c(-3, 3)) +
  geom_point(size=4) +
  theme(text=element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  labs(title=title)

margin <- textplot_scale1d(tmod_wf, margin = "features", highlighted = c("co2-kompensation", "kleben", "russlandsanktion", 
                                                                         "fridays"), highlighted_color = "red")
margin
k <- margin["data"]$data
df1 <- filter(k, beta < -1)
df2 <- filter(k, beta > 1.2)


# Wordcloud (not used)
#textplot_wordcloud(pg_dfm, comparison = TRUE)

# Parties multiuse
party = "DIE LINKE"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-36, 56)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "GRÜNE"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-36, 96)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "SPD"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-26, 46)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "CDU"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-16, 36)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "CSU"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-46, 86)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "AfD"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-36, 56)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "FDP"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-36, 46)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))

# Parties Bundestag xlim
party = "DIE LINKE"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-46, 156)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "GRUENE"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-230, 736)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "SPD"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-76, 176)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "CDU"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-76, 46)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "CSU"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-46, 86)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "CDU/CSU"
tstat_keyness <- textstat_keyness(pg_dfm, target = c("CDU","CSU"))
textplot_keyness(tstat_keyness) +
  xlim(-126, 116)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/CDU-CSU_{state}.png"))
party = "AfD"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-186, 346)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))
party = "FDP"
tstat_keyness <- textstat_keyness(pg_dfm, target = c(party))
textplot_keyness(tstat_keyness) +
  xlim(-46, 46)  + labs(title=paste(state, party, sep=" - ")) + 
  theme(legend.position=c(.1, .9), plot.title = element_text(hjust = 0.5))
ggsave(glue("images/parties/{party}_{state}.png"))

