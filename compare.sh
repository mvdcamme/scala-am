echo "Comparing Concrete nodes' size"
diff "Analysis/Concrete nodes/concrete_nodes_size.txt" "Analysis/Concrete nodes/$1/ka popped and pushed.txt"
echo "Comparing Graph size"
diff "Analysis/Graph size/graph_size.txt" "Analysis/Graph size/$1/ka popped and pushed.txt"
