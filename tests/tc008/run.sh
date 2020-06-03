set -e
set -u

../../bin/dcd-client $1 $(readlink -f file.d) --linnum 5 --charnum 20 > actual1.txt
diff actual1.txt expected1.txt
