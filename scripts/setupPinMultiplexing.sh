#!/bin/bash
# See doc/pwm.md
PINS=("P9.22" "P9.21" "P9.14" "P9.16" "P8.19" "P8.13")
#config-pin overlay cape-universala
for PIN in ${PINS[@]}; do
	echo "Setting up pin ${PIN} for pwm"
	config-pin ${PIN} pwm
done
exit 0

