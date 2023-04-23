<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# rrclust 1.0.5.9002

## Bug fixes

- Fix the original names.

- Use 80% of the data on the training set to determine kstar.

- Rename variables with their original names.

- Fix the name of the tibble random data.

- Add subfolders to be able to store data specific for kamila and general data in 'all'.

- Add 'all' folder.

## Features

- Add new parameters files.

- Create randomly demo data.

## Chore

- Stop tracking .Rprofile.

## Documentation

- Add the complete examples to the README.

- Remove unnecessary spaces.

- Use rewrite comments.

- Update Rd file with new title.

- Fix options R chunk.

- Start example with demo_data.

- Add argument method_name to gen_demo_data().

- Add documentation to NAMESPACE and man folder for gen_demo_data().


# rrclust 1.0.5.9001

## Chore

- Set options(warnPartialMatchArgs = FALSE) in the .Rprofile of the project.


# rrclust 1.0.5.9000

## Bug fixes

- Replace length.out with by.

- Fix partial argument matching in seq().

- Fix partial matching argument.

- Delete multiplot.R.

- Fix partial matching problems.

- Add plotOps functino definition.

- Remove 'fs::dir_create' and replace with dir_create.

- Remove 'fs::dir_copy' and replace with dir_copy.

- Remove 'library(grid)'.

- Remove 'library(Hmisc)'.

- Remove 'library(fst) read_fst' and replace with read_fst.

- Remove 'data.table::fwrite' and replace with fwrite.

- Remove 'library(fst) write_fst' and replace with write_fst.

- Remove 'Hmisc::bystats' and replace with bystats.

- Remove 'Hmisc::describe' and replace with describe.

- Remove 'DiagrammeR::render_graph' and replace with render_graph.

- Remove 'DiagrammeR::add_global_graph_attrs' and replace with add_global_graph_attrs.

- Remove 'DiagrammeR::create_graph' and replace with create_graph.

- Remove 'DiagrammeR::create_node_df' and replace with create_node_df.

- Fix email and title in DESCRIPTION.

- Replace  by the native pipe.

- Remove default container.

- Move scripts to inst/scripts/.

- Fix url rrclust logo.

## Features

- Add missing functions read_utf8 and invalid_utf8.

- Style all package files.

## Build system, external dependencies

- Add missing dependencies for plotOps.

- Add dependency to fs dir_create.

- Add dependency to fs dir_copy.

- Add dependency to Hmisc bystats.

- Add dependency to Hmisc describe.

- Add dependency to DiagrammeR render_graph.

- Add dependency to DiagrammeR add_global_graph_attrs.

- Add dependency to DiagrammeR create_graph.

- Add dependency to DiagrammeR create_edge_df.

- Add dependency to DiagrammeR create_edge_df.

- Add dependency to DiagrammeR create_node_df.

- Add new global variables and dependencies.

- Add new globals.

- Add more dependencies.

- Remove import caTools kamila and data.table.

- Add more global variables.

- Add more dependencies.

- Add dependencies and global variables with.

## Chore

- Add inst/scripts/* to .gitignore.

## Documentation

- Update Rd file.

- Remove unncessary return in DESCRIPTION.

- Rewrite DESCRITPTION description field.

- Update Rd files.

- Replace 'layalchristine.lettry@unifr.ch' by 'layal.lettry@gmail.com'.

- Update Rd files.

- Add missing documentation.

- Update Rd files.

- Fix non working link.

- Update Rd files.

- Write all missing arguments documentation.

- Remove documented arguments not in usage.

- Update Rd files.

- Style file.

- Fix missing parameter definition.

- Update Rd files and NAMESPACE.

- Fix missing parameter definition and fix typos.

- Fix missing parameter definition and fix typos.

- Update Rd file.

- Update Rd file.

- Update Rd file.

- Add parameter doc.

- Update the man Rd file for the new title.

- Update man folder.

- Add precision to README.

- Fix titles size.

- Update README.

- Update NEWS.

## Uncategorized

- Merge pull request #1 from asam-group/rrclust_lyl.

Add logo and R CMD check package

- Merge branch 'rrclust_llc' of github.com:asam-group/rrclust into rrclust_llc.



## Bug fixes

- Fix url rrclust logo.

## Features

- Style all package files.

## Uncategorized

- Merge pull request #1 from asam-group/rrclust_lyl.

Add logo and R CMD check package

- Merge branch 'rrclust_llc' of github.com:asam-group/rrclust into rrclust_llc.



## Bug fixes

- Remove obsolete examples.

## Build system, external dependencies

- Update .gitignore.

- Build .Rbuildignore.

- Add license.

## Documentation

- Create NAMESPACE.

- Build README.

- Create all Rd files in man folder.

## Uncategorized

- Merge branch 'rrclust_llc' of github.com:asam-group/rrclust into rrclust_llc.



# rrclust 1.0.5

Add a new table for descriptive statistics

Correct condition in function_kamplot

# rrclust 1.0.4

Correction  "monthly_rent" into "monthly_pension" and "Register of Rents" into "Pension register" in the graphs
 

# rrclust 1.0.3

correction "monthly_rent" into "monthly_pension"

# rrclust 1.0.2

adaptation to the large pension register (more variables available)

# rrclust 1.0.1

new graphs to add in the working paper

# rrclust 1.0.0

first operational version used for the working paper 

# rrclust 0.1.4

kstar instead of gstar

# rrclust 0.1.3

mod_gstar

# rrclust 0.1.2

Kamila with 3 clusters with gstar = 3 

# rrclust 0.1.1

Kamila package used for mixed-type data

# rrclust 0.1.0

Creation of the package rrclust
