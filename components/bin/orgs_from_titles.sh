#!/bin/sh
# orgs_from_titles.sh <query_expanded.xml> <candidates> <response_in> <title_org_tabs>
# a response is written to stdout

# MergeResponses <query_expanded_xml> <teamid> <response>*
$TAC_ROOT/components/bin/run.sh run.OrganizationsFromTitles $1 $2 $3 $4
