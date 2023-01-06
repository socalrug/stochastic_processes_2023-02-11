message('Installing required packages')
package <- c('devtools', 'tidyverse', 'plot3D', 'diagram', 'markovchain',
             'rstudioapi', 'Hmisc', 'rgl', 'rootSolve')

lapply(package, FUN = function(pkg) {
  if (!require(pkg, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
  }
})

# load the packages that this script needs
stopifnot(require(rstudioapi))
stopifnot(require(stringr))

message('Checking the version of R that you are using')
if ((R.Version()$major |> as.integer() < 4) ||
    (R.Version()$major |> as.integer() == 4 &
     stringr::str_split_i(R.Version()$minor, "\\.", 1) |>
       as.integer() < 0)) {
  warning('Your version of R is quite old, consider upgrading')
}


message('Updating all the packages to the latest version')
update.packages(ask = FALSE)


message('Checking the version of R Studio')
if (rstudioapi::versionInfo()$version |>
      as.character() |>
      stringr::str_split_i('\\.', 1) |>
      as.integer() < 2021) {
  warning('Using an old version of RStudio, consider upgrading')
}

