#!/bin/bash

# Processing iNaturalist data downloaded from GBIF
# A *very* large file (>2.5GB) with all taxa from iNaturalist requiring 
# cleanup, some of which will be quicker in bash
# Source: https://doi.org/10.15468/dl.ux4jcf 
# Note the file is advertised (and named) as a csv, but is actually 
# tab-delimited

# Set up where archive is and where we'll store the large csv file 
# (temporarily)
BASENAME="0001721-180824113759888"
DATADIR="../data/gbif"
ARCHIVE="$DATADIR/$BASENAME.zip"

# Extract the archive to the data directory
unzip $ARCHIVE -d $DATADIR
INFILE="$DATADIR/$BASENAME.csv"
TMP="$DATADIR/tmp.txt"
OUTFILE="../data/iNaturalist.txt"

# Start with a limitation to lepidoptera
grep -w "Lepidoptera" $INFILE > $TMP

# Get header row for our output file
head -n1 $INFILE > $OUTFILE

# Iterate over all families and copy those lines to output file
FAMILIES=("Hesperiidae" "Papilionidae" "Pieridae" "Nymphalidae" "Lycaenidae" "Riodinidae")
for FAMILY in "${FAMILIES[@]}";
do
  echo "Processing $FAMILY"
  grep $FAMILY $TMP >> $OUTFILE
done

rm $TMP
rm $INFILE