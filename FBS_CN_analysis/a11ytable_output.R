cover_df <- tibble::tribble( 
  ~"subsection_title",  ~"subsection_content",
  "Information", "Data in this workbooks relates to the Farm Business Survey...  
  \nmore?",
  "Useful links",  paste0(
    "=HYPERLINK(\"",
    "https://www.google.com",
    "\",",
    "\"Google\"",
    ")",
    "HYPERLINK(\"",
    "https://www.google.com",
    "\",",
    "\"Google\"",
    ")"
    ),
  "Date of Publication",     "DD MONTH YYYY",
  "Contact", "agric.stats@gov.scot",
  "Media enquiries", "Telephone numbers and contact information"
)

contents_df <- tibble::tribble(
  ~"Sheet name", ~"Sheet title",
  "Notes",       "Notes used in this workbook",
  "Table 1", "Absolute emissions (t CO2-e per ha) by farm type, 2019-20 to 2021-22",
  "Table 2", "Emission intensity (kg CO2-e per kg output) by farm type, 2019-20 to 2021-22",
  "Table 3", "Nitrogen balance (kg nitrogen surplus) by farm type, 2019-20 to 2021-22",
  "Table 4", "Nitrogen use efficiency (% nitrogen outputs / nitrogen inputs) by farm type, 2019-20 to 2021-22"
)

notes_df <- tibble::tribble(
  ~"Note number", ~"Note text",
  "[note 1]",     "All farm types includes lowland cattle and sheep farms.",
  "[note 2]",     "Nitrogen data is not available for all farms in the farm business survey. Specifically, organic farms are excluded. Data are not directly comparable with data elsewhere which may include all farms in the farm business survey sample."
)

Table_1$Notes <- c(  # add 'Notes' column
  rep("[note 1]", 3), 
  rep(NA_character_, 15)
)
Table_2$Notes <- c(  # add 'Notes' column
  rep("[note 1]", 3), 
  rep(NA_character_, 15)
)
Table_3$Notes <- c(  # add 'Notes' column
  rep("[note 1], [note 2]", 3), 
  rep(NA_character_, 15)
)
Table_4$Notes <- c(  # add 'Notes' column
  rep("[note 1], [note 2]", 3), 
  rep(NA_character_, 15)
)

my_a11ytable <- 
  a11ytables::create_a11ytable(
    tab_titles = c(
      "Cover",
      "Contents",
      "Notes",
      "Table 1",
      "Table 2",
      "Table 3",
      "Table 4"
    ),
    sheet_types = c(
      "cover",
      "contents",
      "notes",
      "tables",
      "tables",
      "tables",
      "tables"
    ),
    sheet_titles = c(
      "Farm Business Survey ",
      "Table of contents",
      "Notes",
      "Table 1: Absolute emissions (t CO2-e per ha) by farm type, 2019-20 to 2021-22",
      "Table 2: Emission intensity (kg CO2-e per kg output) by farm type, 2019-20 to 2021-22",
      "Table 3: Nitrogen balance (kg nitrogen surplus) by farm type, 2019-20 to 2021-22",
      "Table 4: Nitrogen use efficiency (% nitrogen outputs / nitrogen inputs) by farm type, 2019-20 to 2021-22"
      
    ),
    sources = rep(
      "Scottish Government - Farm Business Survey 2021-22: Farm level emissions and nitrogen usage",7
    ),
    tables = list(
      cover_df,
      contents_df,
      notes_df,
      Table_1,
      Table_2,
      Table_3,
      Table_4
    )
  )

library(openxlsx)
my_wb <- a11ytables::generate_workbook(my_a11ytable)
openxlsx::saveWorkbook(my_wb, "publication.xlsx",overwrite = TRUE)
