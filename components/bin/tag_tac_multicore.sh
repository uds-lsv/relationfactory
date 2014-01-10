# tag_tac.sh <directory>
# This tags all gzipped sgml files as provided by TAC.
# For each file of the form some-file.gz a file some-file.dtag.gz is created.
directory=$1

find $directory -type f -name \*.gz \
| grep -v '.dtag.gz$' \
| parallel -j+0 --eta "$TAC_ROOT/components/bin/tag_tac_file.sh"

