## The following example demonstrates

## how a search and replace string task

## can be peformed with R across several files



## Create two text files with content

filenames <- list.files("/Users/Layal/OFAS/doctorat/package_tools/source/rrclust/R",
                        full.names = TRUE,  pattern = "\\.R$")

## Replace Merry Christmas with Happy New Year

for( f in filenames ){



  x <- readLines(f)

  y <- gsub( "mailto:layalchristine.pipoz@bsv.admin.ch",
             "mailto:layalchristine.lettry@unifr.ch",
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
