## The following example demonstrates

## how a search and replace string task

## can be peformed with R across several files



## Create two text files with content

filenames <- list.files("C:/research/rrclust_llc_subwork/inst/graphs",
                        full.names = TRUE,  pattern = "\\.R$")

## Replace Merry Christmas with Happy New Year

for( f in filenames ){



  x <- readLines(f)

  y <- gsub( 'Other types of pensioners',
             'Other types of OASI pensioners',
             x )

  cat(y, file=f, sep="\n")



}

#
#
# ## Review output
#
# for( f in filenames ){
#
#   cat(readLines(f), sep="\n")
#
# }
