#!/usr/bin/env bash
#replace middle spaces, probably unnecessary

sed -i 's/ /","/g' $file

