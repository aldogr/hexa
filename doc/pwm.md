## Filesystem Overlay
* [http://processors.wiki.ti.com/index.php/Linux_Core_PWM_User%27s_Guide]()
* [https://www.missinglinkelectronics.com/devzone/index.php/source-menu-processing-sysfs]()
* [Linux PWM Module Introduction](https://www.kernel.org/doc/Documentation/pwm.txt)

Update FS-overlay. Code is in `/opt/source/beaglebone-universal-io` issue `git pull origin master` and then `make install`

* 2 * 3 EHRPWN outputs (same base frequency)
* 2 ECAPPW outputs

Adresse  | pwmchip | PWM-Controller |Pins A/B
--------|--------|--------------|---------------|
48300200 | 0	| epwm0 A/B		| P9_22, P9_31 / P9_21, P9_29
48302200 | 2	| epwm1	A/B		| P8_36, P9_14 / P8_34, P9_16
48304200 | 4	| epwm2	A/B		| P8_19 / P8_13
48300100 | 6	| ecap			|
48304100 | 7	| ecap			|

To route PWM signals to output use e.g. `config-pin P9.22 pwm`

Memory Adresses from AM335x ARM Cortex-A8 Manual and `ls -al /sys/class/pwm/`

From [https://github.com/cdsteinkuehler/beaglebone-universal-io/blob/master/config-pin]():

* P8_13 ehrpwm2B
* P8_19 ehrpwm2A
* P8_34 ehrpwm1B
* P8_36 ehrpwm1A
* P8_46 ehrpwm2B
* P9_14 ehrpwm1A
* P9_16 ehrpwm1B
* P9_21 ehrpwm0B
* P9_22 ehrpwm0A
* P9_29 ehrpwm0B
* P9_31 ehrpwm0A


Additional various "tripzone" Inputs (Überlastschutz), various sync inputs

To enable e.g. pwmchip0:

* `echo 0 > /sys/class/pwm/pwmchip0/export` `0` for ehrpwm0A `1` for ehrpwm0B
* directory pwm0 should appear
* set period to 1sec: `echo 1000000000 > /sys /class/pwm/pwmchip0/pwm0/period`
* set duty in ns: `echo val > /sys/class/pwm/pwmchip0/pwm0/duty_cycle`
* set polarity: `echo 1 > /sys/class/pwm/pwmchip0/pwm0/polarity`
* enable : `echo 1 > /sys/class/pwm/pwmchip0/pwm0/enable`



## PWM Spec
From F550 User Manual:

ECU Spec				| Value
----------------------- |--------
Max Voltage				| 17.4V
Max Current (persitent)	| 20A
Max Peak Current		| 30A
PWM Input Signal Level	| 3.3V/5V compatible
Signal Frequency		| 30-450Hz
Battery					| 3S-4S LiPo

## Desired Pin Mapping

*Not final yet, needs to be checked if compatible with possibly needed SPI or I2C bus interface*

pwmOut		| Pin		| Motor
-----		|-----		|-----
epwm0A		| P9_22 	| a
epwm0B		| P9_21		| b
epwm1A		| P9_14		| c 
epwm1B		| P9_16		| d
epwm2A		| P8_19		| e
epwm2B		| P8_13		| f

