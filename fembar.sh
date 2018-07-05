RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

rm *.o *.exe *.mod 
clear
echo compiling all modules...
gfortran -Wall -g -c nodemodule.f95 elementmodule.f95 systemmodule.f95  summarymain.f95 
echo -e "${GREEN}Success compiling all modules"
echo -e "${NC}"

echo compiling main...
gfortran -Wall -g -c  main.f95
echo -e "${GREEN}Success compiling main"
echo -e "${NC}"

gfortran -Wall -g *.o -o fidzfem.exe
echo -e "${GREEN}The program is ready to run!!! "
echo *.exe
echo -e "${NC}"

#echo -e "${GREEN}Plotting the result...${NC}"
#gnuplot
#plot 'output.txt'

