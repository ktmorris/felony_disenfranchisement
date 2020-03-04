graph <- "digraph flowchart {
# node definitions with substituted label text
node [fontname = \"LM Roman 10\", shape = rectangle, width = 4]
tab1 [label = '@@1']
tab2 [label = '@@2']
tab3 [label = '@@3']
tab4 [label = '@@4']
tab5 [label = '@@5']
# edge definitions with the node IDs
tab1 -> tab2;
tab1 -> tab3;
tab3 -> tab4;
tab3 -> tab5
}
[1]: 'Parolee Discharges, 1/1/2017 - 10/12/2018\\n18,423'
[2]: 'Discharged Before 5/18/2018\\n(Assigned to Control)\\n14,155'
[3]: 'Discharged On or After 5/18/2018\\n(Assigned to Treatment)\\n4,268'
[4]: 'Actually Treated\\n(Rights Restored at In-Person Meeting)\\n3,093'
[5]: 'Non-Compliers\\n1,175'
"
grViz(graph) %>%
export_svg %>% charToRaw %>% rsvg_pdf("./temp/flowchart.pdf", width = 750)
