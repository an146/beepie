omake $@ 2>.omake.stderr
cat .omake.stderr |
while read line; do
	if echo $line | grep -q '^File ".*", line '; then
		prefix=`echo $line | sed 's/^\(File "\).*".*/\1/'`
		file=`echo $line | sed 's/^File "\(.*\)".*/\1/'`
		suffix=`echo $line | sed 's/^File ".*\(".*\)/\1/'`
		fixedfile=`find src -name $file`
		echo "$prefix$fixedfile$suffix"
	else
		echo "$line"
	fi
done >&2
rm .omake.stderr
