# DO NOT RENAME #
# file used for autoreload during app development (see Rakefile)
# requires a credentials.R file in this folder (gitignored!) that defines
# `schedule_gs_id` (the google spreadsheet id) and `key_file` (the path to the
# google service account json key file), and optionally
# `class_files_folder_id` (the google drive folder with the class files)
devtools::load_all("..")
source("credentials.R")
earthschedule_app(
  data_sheet_id = schedule_gs_id,
  gs_key_file = key_file,
  enrollment_data_file = "2026_2_IGroup_Detail_1038_20261008_Detail_only.xlsx",
  class_files_folder_id = class_files_folder_id,
  dev_cache_file = "local_data_schedule.xlsx",
  options = list(port = 4444),
  access_level = "admin",
  start_tab = "schedule"
)
