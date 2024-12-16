# Vendor dependencies

## libsodium

Download libsodium and build it for ARM:

```
tar zxvf libsodium-1.0.20.tar.gz
cd libsodium-1.0.20
sudo apt install gcc-arm-linux-gnueabihf
mkdir libsodium-build
./configure --host=arm-linux-gnueabihf --prefix=$(cd ../libsodium-build; pwd)
make
make install
```
