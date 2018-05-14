logfile=${1:-log.txt}
echo "ErrorPath aborts:"
cat $logfile | grep -ni "follows" | wc -l
echo "Merges:"
cat $logfile | grep -ni "Path should be merged" | wc -l
echo "Usererrors:"
cat $logfile | grep -ni usererror | wc -l


