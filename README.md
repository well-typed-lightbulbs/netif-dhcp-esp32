### Network interface with DHCP handling 

This is a mirage layer that allows to get both a NETWORK type and a DHCP lease stream.
This is made for ESP32 Wifi library, as the wifi configuration is done at unikernel runtime 
(thus after tcp/ip stack is setup.). This is part of the dynamic ip rework that I had to make.

I know this isn't ideal but I had to do this to test network features of Mirage/ESP32.