#!/bin/zsh


# Where to put the output data
RESULT=results.txt

# Programs to run
BINARIES=(ex1 ex2 ex3 ex4 ex5)
# Max number of cores to use in a run
CORES=10

# Increase granulatity of zsh internal timer
typeset -F SECONDS

#for PROG in $BINARIES
#echo $PROG " "
#do
#    for N in {1..$CORES}
#    do
#        local START=$SECONDS
#        echo -n $N " "
#        echo $((SECONDS-START))
#    done
#    echo
#done

echo -n "Cores , "
for P in $BINARIES
do
  echo -n $P ", "
done
echo

for N in {1..$CORES}
do
    echo -n $N ", "
    for P in $BINARIES
    do
      local START=$SECONDS
      stack exec $P -- +RTS -ls -N$N -RTS > /dev/null 2> /dev/null
      echo -n $((SECONDS-START)) ", "
    done
    echo
done