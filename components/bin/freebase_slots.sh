#!/bin/sh
# Queries freebase for slot fillers.

# freebase.FreebaseSlots <freebase_index> <tac_to_freebase_relmap> <query_expanded.xml> <slots_out>
freebase_index=/data/users/beroth/freebase_index/
tac_to_freebase_relmap=$TAC_ROOT/resources/manual_annotation/freebase_relations_types_top_precision.txt
query_expanded=$1
slots_out=$2

$TAC_ROOT/components/bin/run.sh freebase.FreebaseSlots $freebase_index $tac_to_freebase_relmap $query_expanded $slots_out
