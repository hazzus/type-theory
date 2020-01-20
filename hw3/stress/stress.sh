#!/bin/sh

while (true);
do
    generated="$(./gen)"
    echo $generated
    echo $generated | stack run > my.out
    echo $generated | ./main.exe > correct.out
    diff my.out correct.out
    read -n 1 -p "okay?[Y/n]" x
    if [x eq 'n']; then
        break
    fi
done
