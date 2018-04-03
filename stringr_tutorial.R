library(tidyverse)
library(stringr)

# data_dir <- 'https://raw.githubusercontent.com/OHI-Science/IUCN-AquaMaps/master/clip_depth'
data_dir <- 'data'
coral_spp_narratives <- read_csv(file.path(data_dir, 'coral_spp_narratives.csv'))
head(coral_spp_narratives)
### interested in species_id, habitat
coral_spp_info <- read_csv(file.path(data_dir, 'coral_spp_areas.csv'))
head(coral_spp_info)
### info for species mapped in both datasets

### create a dataframe with just ID, scientific name, and habitat
coral_habs_raw <- coral_spp_narratives %>%
  rename(iucn_sid = species_id) %>%
  left_join(coral_spp_info, by = 'iucn_sid') %>%
  select(iucn_sid, sciname, habitat)

### examine a few habitat descriptions
coral_habs_raw$habitat[1]
### how can we chop these up to get at the depth information?
### - chop into sentences
### - keep sentences with any numbers in them
### - see what's left after that

### in pseudocode:
# coral_habs <- coral_habs_raw %>%
#   split into individual sentences %>%
#   keep the sentences with numbers in them
  
### Also introduce some basic stringr functions, and pattern vs. vector of strings
### - str_match, str_match_all
### - str_detect
### - str_split
### - str_replace, str_replace_all
### - str_subset, str_count, str_locate
### - str_trim, tolower, toupper, tools::toTitleCase
x <- "Everybody's got something to hide except for me and my monkey"
tools::toTitleCase(x)
tolower(x)
str_split(x, 'hide'); str_split(x, 't')
str_replace(x, 'except for', 'including')
str_replace(x, ' ', '_')
str_replace_all(x, ' ', '_')
str_detect(x, 't'); str_detect(x, 'monk') ### is pattern in the string? T/F
str_match(x, 't'); str_match_all(x, 'y')  ### return every instance of the pattern in the string
  ### more useful when using wildcards as a pattern...
str_locate(x, 't'); str_locate_all(x, 'y')

###########################################################################
### Break down the habitat column into manageable chunks, i.e. sentences
###########################################################################
coral_habs <- coral_habs_raw %>%
  mutate(hab_cut = str_split(habitat, '.'))

coral_habs$hab_cut[1]
### why didn't that work?  Introduce idea of "escape characters" and "." as a 
###   wildcard for any single character, and literal characters

### Also: why is just separating on a period a bad idea?
### - since we're looking for a number, what if there's a decimal?!

coral_habs <- coral_habs_raw %>%
  mutate(hab_cut = str_split(habitat, '\. '))
### Error: '\.' is an unrecognized escape in character string starting "'\."

coral_habs <- coral_habs_raw %>%
  mutate(hab_cut = str_split(habitat, '\\. '))
### creates a cell with a vector of broken up sentences!

### Show book cover...

### use unnest() to separate out those sentences into individual rows to work with them
coral_habs <- coral_habs_raw %>%
  mutate(hab_cut = str_split(habitat, '\\. ')) %>%
  unnest(hab_cut)
### note the number of observations skyrocketed!

####################################################
### NEXT: just keep the sentences with numerics
####################################################
### Without wildcards:
coral_habs <- coral_habs_raw %>%
  mutate(hab_cut = str_split(habitat, '\\. ')) %>%
  unnest(hab_cut) %>%
  filter(str_detect(hab_cut, '1') | str_detect(hab_cut, '2'))

### With wildcards
coral_habs <- coral_habs_raw %>%
  mutate(hab_cut = str_split(habitat, '\\. ')) %>%
  unnest(hab_cut) %>%
  filter(str_detect(hab_cut, '[0-9]'))
    ### also works with [3-7], [a-z], [A-Z], [a-z0-9A-Z]

### How to differentiate further to get at depth info?
### - exclude years? Knowing a bit about corals, can probably exclude any 
###   four-digit numbers; problems with that?
### - match pattern of number followed by " m"
coral_depth <- coral_habs %>%
  filter(str_detect(hab_cut, '[0-9] m')) %>%
  mutate(depth = str_extract(hab_cut, '[0-9] m'))
### Why didn't that work???? Only matched the digit next to the "m"!

### Use a quantifier:
### + means one or more times
### * means zero or more times
### ? means zero or one time
### {3} means exactly three times
### {2,4} means two to four times; {2,} means two or more times
coral_depth <- coral_habs %>%
  filter(str_detect(hab_cut, '[0-9] m')) %>%
  mutate(depth = str_extract(hab_cut, '[0-9]+ m'))
### Still misses the ranges e.g. "3-30 m" - how to capture?

### let it also capture "-" in the brackets
coral_depth <- coral_habs %>%
  filter(str_detect(hab_cut, '[0-9] m')) %>%
  mutate(depth = str_extract(hab_cut, '[0-9-]+ m'))

### split 'em (introduce "not" qualifier), convert to numeric, keep the largest
coral_depth <- coral_habs %>%
  filter(str_detect(hab_cut, '[0-9] m')) %>%
  mutate(depth_char = str_extract(hab_cut, '[0-9-]+ m'),
         depth_num = str_split(depth_char, '[^0-9]')) %>%
  unnest(depth_num)

coral_depth <- coral_depth %>%
  mutate(depth_num = as.numeric(depth_num)) %>%
  filter(!is.na(depth_num)) %>%
  group_by(iucn_sid, sciname) %>%
  mutate(depth_num = max(depth_num),
         n = n()) %>%
  distinct()

### Play with text a little more; let's look at threats instead.
### Combining multiple tests using "or", and adding string start and end characters.
coral_threats <- coral_spp_narratives %>%
  select(iucn_sid = species_id, threats) %>%
  mutate(threats = tolower(threats),
         threats_cut = str_split(threats, '\\. ')) %>%
  unnest(threats_cut) %>%
  filter(str_detect(threats_cut, '^a|s$')) 
    ### NOTE: ^ outside brackets is start of a string, but inside brackets it's a negation

crappy_colname <- 'Per-capita income ($US) (2015 dollars)'
tolower(crappy_colname) %>%
  str_replace_all('[^a-z0-9]+', '_') %>%
  str_replace('_$', '')

### Mention: lookahead and lookbehind assertions, useful to match a pattern 
### led by or followed by another pattern; lazy vs. greedy evaluation of quantifiers
x %>% str_replace('b.+e', '...')
x %>% str_replace('b.+?e', '...')

y <- 'one fish two fish red fish blue fish'
y %>% str_locate('(?<=two) fish')
y %>% str_locate('fish (?=blue)')
y %>% str_replace_all('(?<=two|blue) fish', '...')

list.files('sample_files')
list.files('sample_files', pattern = 'jpg$')
list.files('sample_files', pattern = '[0-9]{4}')
raster_files <- list.files('sample_files', pattern = '^sample.+[0-9]{4}.tif$') 
  ### note: should technically be '\\.tif$'
