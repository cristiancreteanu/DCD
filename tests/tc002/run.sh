set -e
set -u

../../bin/dcd-client $1 $(readlink -f file.d) --linnum 1 --charnum 52 > actual.txt
diff actual.txt expected.txt
