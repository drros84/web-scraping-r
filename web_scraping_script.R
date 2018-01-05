
library(crandb)
library(dplyr)

## Get all package names, version, and title using the "crandb" package
all_packages <- list_packages(limit = 20000, archived = FALSE) %>%
  unlist()
# When downloading from crandb, I get a list. Below, unpack it by taking elements
# at specific intervals
all_packages <- data.frame(package_name = names(all_packages)[seq(1, length(all_packages), 2)],
                            package_version = all_packages[seq(1, length(all_packages), 2)],
                            package_title = all_packages[seq(2, length(all_packages), 2)])

# Convert to character
all_packages$package_name <- as.character(all_packages$package_name)
all_packages$package_version <- as.character(all_packages$package_version)
all_packages$package_title <- as.character(all_packages$package_title)

# Only keep the actual name of the package
all_packages$package_name <- substr(all_packages$package_name, 1,
                                     nchar(all_packages$package_name) - 8)

# Re-index the dataframe
row.names(all_packages) <- c(1:nrow(all_packages))

# Save the dataframe as csv
write.csv(all_packages3, "/Users/davidrosenfeld/Documents/web_scraping_r/all_packages.csv")


# # Read the dataframe
# all_packages <- read.csv("/Users/davidrosenfeld/Documents/web_scraping_r/all_packages.csv")

# Load packages for web-scraping and data analysis
library(httr)
library(rvest)
library(XML)
library(reshape2)
library(purrr)
library(dplyr)

# Function for extracting data for each package name as input
get_package_data <- function(name_of_package) {
  
  # Scrape CRAN web page for a given R package 
  stored_html <- read_html(paste0("https://cran.r-project.org/web/packages/", 
                                  name_of_package,
                                  "/index.html"))
  
  # Extract the first table from the stored web page, which includes useful info
  get_table <- html_table(stored_html)[[1]] %>%
    data.frame()
  
  # Remove colons from the first column of the table
  get_table$X1 <- substr(get_table$X1, 1, nchar(get_table$X1) - 1)
  
  # Create a vector including string "name" and the name of the package
  name_vector <- c("name", html_text(html_nodes(stored_html, css = "title")))
  # Clean up vector name
  name_vector[2] <- substr(name_vector[2], 16, nchar(name_vector[2]))
  
  # Create a vector with string "long_name" and the title of the package
  long_name_vector <- c("long_name", html_text(html_nodes(stored_html, css = "h2")))
  
  # Create a vector with string "package_description" and a description of what
  # the package does
  package_description_vec <- c("package_description",
                               html_text(html_nodes(stored_html, css = "p"))[[1]])
  
  # Bind the table with the vectors to create a unique dataframe
  get_table <- rbind(get_table, name_vector, long_name_vector, package_description_vec)
  
  # Name the second column after the package name
  colnames(get_table)[2] <- name_vector[2]
  
  return(get_table)
}

# Create an empty list to fill with package data
combined_tables <- list()
# Loop over package names. For each, create dataframe using the get_package_data
# function above, and add it to the combined_tables list.
# Pause for 5 seconds between every request
for(i in 1:length(all_packages$package_name[1:10])) {
  
  combined_tables[[i]] <- get_package_data(all_packages$package_name[i])
  Sys.sleep(5)
  
}

# Full join all the tables in the list, but maintaining all the rows (which is why 
# I use full_join rather that left_join). Not all packages include the same info,
# it is important to keep all rows. Join by the "X1" column, which is common to
# all dataframes in the list
merged_tables <- reduce(combined_tables, full_join, by = "X1")

# Extract data from the "Depends" column and the package name. This will give us 
# the dependencies for each package
dependencies <- merged_tables[merged_tables$X1 %in% c("Depends", "name"), ] %>%
  t() 
colnames(dependencies) <- dependencies[1, ]
dependencies <- dependencies[-1, ]
dependencies <- data.frame(dependencies)

# Import R packages that facilitate text mining
library(stringr)
library(stringi)
library(rebus)

# Split the string of dependencies by using commas as separators. Use the 
# "simplify = TRUE" argument to store these as a matrix
depend_matrix <- str_split(dependencies$Depends, pattern = fixed(","), simplify = TRUE)

# Clean package names by getting rid of spaces at the start or end of each package name
test <- str_replace_all(depend_matrix, pattern = or(START %R% " ", " " %R% END),
                        replacement = "") %>%
  as.matrix(nrow = nrow(depend_matrix), ncol = ncol(depend_matrix), byrow = FALSE)


# Extract author names for each package
author_matrix <- merged_tables[merged_tables$X1 %in% c("Author", "name"), ] %>%
  t() 
colnames(author_matrix) <- author_matrix[1, ]
author_matrix <- author_matrix[-1, ]
author_matrix <- data.frame(author_matrix)

# # Split the string by using the square brackets (and their content) as a separator. The 
# # square brackets includes strings like "cre, aut". Use the "simplify = TRUE" argument
# # to store results as a matrix
# test <- str_split(author_matrix$Author, pattern = " [" %R% one_or_more(ANY_CHAR) %R%
#                                                        "], ", 
#                   simplify = TRUE)
# 
# # Extract the contents of the square brackets
# test <- str_extract_all(author_matrix$Author, pattern = " [" %R% one_or_more(ANY_CHAR) %R%
#                           "], ")

# str_view_all(author_matrix$Author, pattern = OPEN_BRACKET %R% WRD %R% zero_or_more(", ") 
#              %R% zero_or_more(WRD) %R% CLOSE_BRACKET)
# 
# str_view_all(author_matrix$Author, pattern = OPEN_BRACKET %R% WRD %R% "[:punct:]" %R% 
#                "[:space:]" %R%
#              WRD %R% CLOSE_BRACKET)
# 
# str_view_all(author_matrix$Author, pattern = OPEN_BRACKET %R% one_or_more(WRD) %R% 
#                zero_or_more(fixed(", ")) %R% zero_or_more(WRD) %R% CLOSE_BRACKET)


# Extract the content of the square brackets
author_types <- str_extract_all(author_matrix$Author, pattern = OPEN_BRACKET %R% one_or_more(WRD) %R% 
               zero_or_more(fixed(", ")) %R% zero_or_more(WRD) %R% CLOSE_BRACKET,
               simplify = TRUE)

# Remove the square brackets and their content from the author data
author_clean <- str_replace_all(author_matrix$Author, pattern = SPACE %R% 
                                  OPEN_BRACKET %R% one_or_more(WRD) %R% 
                  zero_or_more(fixed(", ")) %R% zero_or_more(WRD) %R% CLOSE_BRACKET,
                replacement = "")

# Remove the "\n" that seem to have appeared after the previous operation
author_clean <- str_replace_all(author_clean, pattern = fixed("\n "),
                                replacement = "")

# Remove the "and" string
author_clean <- str_replace_all(author_clean, pattern = fixed("and "),
                                replacement = "")

# split the author data based on commas and the ", and" string
author_clean <- str_split(author_clean, pattern = or(fixed(", "), 
                                                     fixed(", and ")), 
                          simplify = TRUE)


