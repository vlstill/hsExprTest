#!/usr/bin/env bash

HW=10post4

select_test() {
	echo "select author from eval_out_pts join assignment on (assignment_id = assignment.id) where active and assignment.name = '$HW' and eval_out_pts.name = '$1'"
}

tag() {
cat <<EOF
select tag, array_agg(author) from submission_in_tag join submission on (submission.id = submission_id) join assignment on (assignment_id = assignment.id) where assignment.name = '$1' group by tag order by tag;
EOF
}

gen_sql() {
	for i in evalConsts removeConsts toNNF toIF; do
		tag $i $(select_test $i)
	done
	tag "CNFDNF" "$(select_test isCNF) intersect $(select_test isDNF)"
}

gen_sql 
#    | psql -q --csv | tail +2 \
#    | sed -e 's/"//g' -e 's/{/[/g' -e 's/}/]/g' -e 's/^\([^,]*\),\(.*\)$/    ("\1", \2),/'
