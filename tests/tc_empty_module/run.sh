set -e
set -u

../../bin/dcd-client $1 file.d --extended -c$(stat -c %s file.d) > actual.txt
diff actual.txt expected.txt
