library(x3ptools)
library(bulletxtrctr)

# Change this path to your bullet scan directory
target_dir <- "<PATH_TO_BULLET_SCANS>"

if (target_dir == "<PATH_TO_BULLET_SCANS>" || !dir.exists(target_dir)) {
  stop("Please set 'target_dir' to a valid directory path containing your bullet scans.")
}


files_list <- list.files(path = target_dir, pattern = "\\.x3p$", full.names = TRUE, recursive = TRUE)


# loop through each file path
for (file_path in files_list) {
  tryCatch(
    {
      current_x3p <- x3ptools::read_x3p(file_path)
      rotated_x3p <- x3ptools::x3p_rotate(current_x3p, angle = 90) # change angle to be how many degrees you want rotated
      # save it to the same path
      x3ptools::x3p_write(rotated_x3p, file = file_path)
    },
    error = function(e) {
      message(paste("Error processing", file_path, ":", e$message))
    }
  )
}

message("Rotation task complete.")
