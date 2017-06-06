#!/bin/bash
#run as root
#see doc/pwm.md
#Frequency in Hz (supported by Motordriver 50-450Hz)
FREQUENCY=400
#period in ns
PERIOD=$(expr 1000000000 / $FREQUENCY)
CHIPS=(0 2 4)


echo "Set up pwmchips:"
for CHIP in ${CHIPS[@]}; do
	echo "Setup pwmchip${CHIP} pwm 0"
	echo 0 > /sys/class/pwm/pwmchip${CHIP}/export #OutputA
	echo "Setup pwmchip${CHIP} pwm 1"
	echo 1 > /sys/class/pwm/pwmchip${CHIP}/export #OutputB
done

echo "Set up pwm period:"
for CHIP in ${CHIPS[@]}; do
	echo "Setup period  and chmod for pwmchip${CHIP} pwm0"
	echo $PERIOD > /sys/class/pwm/pwmchip${CHIP}/pwm0/period
	chmod o+w /sys/class/pwm/pwmchip${CHIP}/pwm0/duty_cycle /sys/class/pwm/pwmchip${CHIP}/pwm0/enable
	echo "Setup period and chmod for pwmchip${CHIP} pwm1"
	echo $PERIOD > /sys/class/pwm/pwmchip${CHIP}/pwm1/period
	chmod o+w /sys/class/pwm/pwmchip${CHIP}/pwm1/duty_cycle /sys/class/pwm/pwmchip${CHIP}/pwm1/enable
done
#duty and enabling the pwm is done in erlang
exit 0
