library(pdftools)

# Function to convert PDF to text file
convert_pdf_to_txt <- function(directory) {
  # List all files in the directory
  files <- list.files(directory, pattern = "\\.pdf$", full.names = TRUE)
  
  # Loop through the files and convert PDF to TXT
  for (file in files) {
    # Extract text from PDF
    text <- pdf_text(file)
    
    # Create a TXT filename with the same name as the PDF file
    txt_filename <- sub(".pdf$", ".txt", basename(file))
    
    # Write the text to a TXT file
    writeLines(text, file.path(directory, txt_filename))
  }
  
  cat("Conversion completed.\n")
}

# Use the function on your directory
# Replace '/path/to/your/directory' with the actual path where your PDFs are stored
convert_pdf_to_txt('CCW_EXP_REV_2023/EN/GS/Session 2/')
