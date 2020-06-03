set -e
set -u

../../bin/dcd-client $1 $(readlink -f file.d) --linnum 13 --charnum 11 > actual.txt
diff actual.txt expected.txt
