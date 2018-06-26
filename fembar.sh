RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

rm *.o *.exe *.mod main
clear
echo compiling all modules...
gfortran -Wall -c nodemodule.f95 elementmodule.f95 summarymain.f95
echo -e "${GREEN}Success compiling all modules"
echo -e "${NC}"

echo compiling main...
gfortran -Wall -c main.f95
echo -e "${GREEN}Success compiling main"
echo -e "${NC}"

gfortran -Wall *.o -o main.exe
echo -e "${GREEN}The program is ready to run!!! "
echo *.exe
echo -e "${NC}"
