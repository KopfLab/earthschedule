# earthschedule

A [shiny](https://shiny.posit.co/) app for viewing and planning the teaching
schedule of the Department of Earth Sciences. Faculty can select themselves to
add planned classes and teaching absences for upcoming semesters. The data lives
in a Google spreadsheet that the app reads and writes with a Google service
account. Its sister app is [earthpaths](https://github.com/KopfLab/earthpaths).

## Installation

```r
# install.packages("remotes")
remotes::install_github("KopfLab/earthschedule")
```

## Usage

The package provides a single function, `earthschedule_app()`, which returns
the shiny app. To deploy it, return it from an `app.R` file next to the service
account key file:

```r
# app.R
earthschedule::earthschedule_app(
  data_sheet_id = "<google spreadsheet id>",
  gs_key_file = "gs_key_file.json"
)
```

Share the spreadsheet with the service account's email address (editor access
is required to schedule classes and absences).

## Spreadsheet structure

The app reads the following tabs (additional columns and tabs are ignored):

| tab            | columns                                                                                                                                                                                                  |
|----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `classes`      | `class`, `title`, `credits`, `inactive`, `type`                                                                                                                                                          |
| `instructors`  | `instructor_id`, `last_name`, `first_name`, `department`, `position`, `inactive`                                                                                                                         |
| `not_teaching` | `term`, `instructor_id`, `reason`, `created`, `deleted`                                                                                                                                                  |
| `schedule`     | `term`, `class`, `section`, `subtitle`, `instructor_id`, `preenrollment`, `enrollment`, `enrollment_cap`, `building`, `room`, `days`, `start_time`, `end_time`, `created`, `updated`, `deleted`, `canceled`, `confirmed`, `notes` |

The app appends new rows to `not_teaching` and `schedule`, and marks removed
records with a `deleted` timestamp instead of deleting them. Since it appends
columns in this order, the columns listed for `not_teaching` and `schedule` must
be the first columns of these tabs, in this order.

## Development

```r
devtools::load_all()   # load the package
devtools::document()   # regenerate NAMESPACE and man/ after roxygen changes
devtools::test()       # run the tests
devtools::check()      # full R CMD check
```

Code is formatted with [air](https://posit-dev.github.io/air/) (`air format .`).

To run the app with auto-reload during development, put a `credentials.R` file
defining `schedule_gs_id` and `key_file` (plus the key file itself) into `dev/`
(both are gitignored), then from the `dev/` folder (requires Ruby and
`bundle install`):

- terminal 1: `rake dev` runs `dev/app.R` in shiny dev mode with debug logging
- terminal 2: `rake guard` watches `R/` and triggers the reload

In dev mode, the downloaded spreadsheet is cached in
`dev/local_data_schedule.xlsx` (the Reload button clears the cache).
