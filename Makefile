all: idiot

idiot:	*.asm
	@rm -f idiot4.hex
	asmx -e -w -l idiot4.lst -i -o idiot4.idiot idiot4.asm
	
hex:    *.asm
	@rm -f idiot4.idiot
	asmx -e -w -l idiot4.lst    -o idiot4.hex idiot4.asm

install:idiot
	screen -X slowpaste 5
	screen -X readreg p "`pwd`/idiot4.idiot"
	screen -X paste p

eeprom: hex
	eeprom_programmer -w idiot4.hex

.PHONY: clean
clean:
	rm idiot4.lst idiot4.idiot idiot4.hex
